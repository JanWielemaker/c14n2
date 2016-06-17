:- use_module(library(sgml)).
:- use_module(c14n2).

test :-
	test_file(In, Out),
	test(In, Out).

test_file(Dir, In, Out) :-
	directory_files(Dir, Files),
	member(In, Files),
	file_name_extension(Base, xml, In),
	sub_atom(In, 0, _, _, in),
	format(atom(Out), 'out_~w_c14nDefault.xml', [Base]),
	memberchk(Out, Files).

test_file(In, Out) :-
	Dir = 'test/w3c',
	test_file(Dir, In0, Out0),
	directory_file_path(Dir, In0, In),
	directory_file_path(Dir, Out0, Out).

test(In, Out) :-
	load_xml(In, DOM,
		 [ dialect(xmlns)
		 ]),
	with_output_to(string(Cann),
		       xml_write_canonical(current_output, DOM)),
	read_file_to_string(Out, OK, [encoding(utf8)]),
	(   OK == Cann
	->  true
	;   format('~NOK~`-t~60|~n', []),
	    format('~s', [OK]),
	    format('~NUS~`-t~60|~n', []),
	    format('~s', [Cann])
	).

