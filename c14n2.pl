:- module(c14n2,
	  [ xml_write_canonical/2	% +Stream, +Term
	  ]).
:- use_module(library(error)).
:- use_module(library(sgml_write)).
:- use_module(library(dicts)).
:- use_module(library(ordsets)).
:- use_module(library(apply)).
:- use_module(library(lists)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C14n2 specifies a _canonical_ XML  document.   This  library writes such
files from an XML DOM as  returned  by   the  XML  (or SGML) parser. The
process takes two steps:

  - Normalise the DOM
  - Call xml_write/3 with appropriate flags
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	xml_write_canonical(+Stream, +Term) is det.
%
%	Write an XML DOM using the   canonical conventions as defined by
%	C14n2.

xml_write_canonical(Stream, DOM) :-
	xml_canonical_dom(DOM, CannDOM),
	xml_write(Stream, CannDOM,
		  [ header(false),
		    net(false)
		  ]).

xml_canonical_dom(DOM, CDOM) :-
	xml_canonical_dom(DOM, CDOM, xml{in_ns:ns{}, out_ns:ns{}}).

xml_canonical_dom(Var, _, _) :-
	var(Var), !,
	instantiation_error(Var).
xml_canonical_dom(DOM, CDOM, Options) :-
	is_list(DOM), !,
	xml_canonical_list(DOM, CDOM, Options).
xml_canonical_dom(element(Name,  Attrs,  Content),
		  element(Name, CAttrs, CContent),
		  Options) :- !,
	InNS0  = Options.in_ns,
	OutNS0 = Options.out_ns,
	take_ns(Attrs, Attrs1, InNS0, InNS),
	put_ns(Name, InNS, OutNS0, OutNS1),
	put_ns_attrs(Attrs1, InNS, OutNS1, OutNS),
	sort(1, @<, Attrs1, CAttrs0),
	ns_attrs(OutNS0, OutNS, NSAttrs),
	append(NSAttrs, CAttrs0, CAttrs),
	xml_canonical_list(Content, CContent,
			   Options.put(_{in_ns:InNS, out_ns:OutNS})).

xml_canonical_list([], [], _).
xml_canonical_list([H0|T0], [H|T], Options) :-
	xml_canonical_dom(H0, H, Options),
	xml_canonical_list(T0, T, Options).

take_ns([], [], Options, Options).
take_ns([H|T0], T, Options0, Options) :-
	xml_ns(H, NS, URL), !,
	take_ns(T0, T, Options0.put(NS, URL), Options).
take_ns([H|T0], [H|T], Options0, Options) :-
	take_ns(T0, T, Options0, Options).


put_ns_attrs([], _, OutNS, OutNS).
put_ns_attrs([Name=_Value|T], InNS, OutNS0, OutNS) :-
	put_ns(Name, InNS, OutNS0, OutNS1),
	put_ns_attrs(T, InNS, OutNS1, OutNS).

put_ns(NS:_Name, _InNS, OutNS, OutNS) :-
	get_dict(NS, OutNS, _), !.
put_ns(NS:_Name, InNS, OutNS0, OutNS) :-
	URL = InNS.NS, !,
	OutNS = OutNS0.put(NS, URL).
put_ns(_, _, OutNS, OutNS).

ns_attrs(OutNS, OutNS, []) :- !.
ns_attrs(OutNS0, OutNS, NSAttrs) :- !,
	dict_keys(OutNS, Keys),
	dict_keys(OutNS0, Keys0),
	ord_subtract(Keys, Keys0, NewKeys),
	maplist(ns_attr(OutNS), NewKeys, NSAttrs0),
	sort(NSAttrs0, NSAttrs).

ns_attr(Dict, '', xmlns=Dict.'') :- !.
ns_attr(Dict, NS, xmlns:NS=Dict.NS).


xml_ns(xmlns=URL, '', URL) :- !.
xml_ns(xmlns:NS=URL, NS, URL) :- !.
xml_ns(Name=URL, NS, URL) :-
	atom_concat('xmlns:', NS, Name).
