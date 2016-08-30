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
		    layout(false),
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
xml_canonical_dom(element( Name,  Attrs,  Content),
		  element(CName, CAttrs, CContent),
		  Options) :- !,
	InNS0  = Options.in_ns,
	OutNS0 = Options.out_ns,
	take_ns(Attrs, Attrs1, InNS0, InNS),
	partition(has_ns, Attrs1, AttrsWithNS0, AttrsSans0),
	sort(1, @<, AttrsWithNS0, AttrsWithNS1),
	sort(1, @<, AttrsSans0, AttrsSans),
	put_elemns(Name, CName, InNS, OutNS0, OutNS1, KillDefault),
	put_ns_attrs(AttrsWithNS1, AttrsWithNS, InNS, OutNS1, OutNS),
	ns_attrs(OutNS0, OutNS, NSAttrs),
	append([KillDefault, NSAttrs, AttrsSans, AttrsWithNS], CAttrs),
	must_be(list, Content),
	xml_canonical_list(Content, CContent,
			   Options.put(_{in_ns:InNS, out_ns:OutNS})).
xml_canonical_dom(CDATA, CDATA, _) :-
	atomic(CDATA).

has_ns(ns(_,_URL):_Name=_Value).

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


put_ns_attrs([], [], _, OutNS, OutNS).
put_ns_attrs([Name=Value|T0], [CName=Value|T], InNS, OutNS0, OutNS) :-
	put_ns(Name, CName, InNS, OutNS0, OutNS1),
	put_ns_attrs(T0, T, InNS, OutNS1, OutNS).

put_elemns(Name, Name, _InNS, OutNS0, OutNS1, [xmlns='']) :-
	atom(Name),
	dict_pairs(OutNS0, _, Pairs),
	memberchk(URL-'', Pairs), !,
	del_dict(URL, OutNS0, '', OutNS1).
put_elemns(Name, CName, InNS, OutNS0, OutNS, []) :-
	put_ns(Name, CName, InNS, OutNS0, OutNS).

put_ns(ns(NS, URL):Name, CName, _InNS, OutNS, OutNS) :-
	get_dict(URL, OutNS, NS), !,
	make_cname(NS:Name, CName).
put_ns(ns(NS, URL):Name, CName, _InNS, OutNS0, OutNS) :- !,
	make_cname(NS:Name, CName),
	OutNS = OutNS0.put(URL, NS).
put_ns(URL:Name, CName, _InNS, OutNS, OutNS) :-
	get_dict(URL, OutNS, NS), !,
	make_cname(NS:Name, CName).
put_ns(URL:Name, CName, InNS, OutNS0, OutNS) :-
	dict_pairs(InNS, _, Pairs),
	memberchk(NS-URL, Pairs), !,
	make_cname(NS:Name, CName),
	OutNS = OutNS0.put(URL, NS).
put_ns(Name, Name, _, OutNS, OutNS).

ns_attrs(OutNS, OutNS, []) :- !.
ns_attrs(OutNS0, OutNS, NSAttrs) :- !,
	dict_keys(OutNS, URLs),
	dict_keys(OutNS0, URLs0),
	ord_subtract(URLs, URLs0, NewURLs),
	maplist(ns_attr(OutNS), NewURLs, NSAttrs0),
	sort(NSAttrs0, NSAttrs).

ns_attr(Dict, URL, NSAttr) :-
	ns_simplify(xmlns:Dict.URL=URL, NSAttr).

ns_simplify(xmlns:''=URL, xmlns=URL) :- !.
ns_simplify(xmlns:NS=URL, XMLNS=URL) :-
	make_cname(xmlns:NS, XMLNS).

xml_ns(xmlns=URL, '', URL) :- !.
xml_ns(xmlns:NS=URL, NS, URL) :- !.
xml_ns(Name=URL, NS, URL) :-
	atom(Name),
	atom_concat('xmlns:', NS, Name).

make_cname('':Name, Name) :- !.
make_cname(NS:Name, CName) :-
	atomic_list_concat([NS,Name], :, CName).
