% ---------------------------------------------
% RDF PlainLiteral plugin for Eye -- Jos De Roo
% ---------------------------------------------

% functions according to http://www.w3.org/TR/rdf-plain-literal/#Functions_on__rdf:PlainLiteral_Data_Values
'<http://www.w3.org/2009/rdf-PlainLiteral-functions#PlainLiteral-from-string-lang>'([literal(A,void)],literal(A,void)) :- !.
'<http://www.w3.org/2009/rdf-PlainLiteral-functions#PlainLiteral-from-string-lang>'([literal(A,void),literal(B,void)],literal(A,lang(C))) :-
	downcase_string(B,D), atom_codes(C,D).

'<http://www.w3.org/2009/rdf-PlainLiteral-functions#string-from-PlainLiteral>'([literal(A,void)],literal(A,void)) :- !.
'<http://www.w3.org/2009/rdf-PlainLiteral-functions#string-from-PlainLiteral>'([literal(A,lang(_))],literal(A,void)).

'<http://www.w3.org/2009/rdf-PlainLiteral-functions#lang-from-PlainLiteral>'([literal(_,void)],literal("",void)) :- !.
'<http://www.w3.org/2009/rdf-PlainLiteral-functions#lang-from-PlainLiteral>'([literal(_,lang(A))],literal(B,void)) :-
	atom_codes(A,B).

% @@partial implementation: no collation
'<http://www.w3.org/2009/rdf-PlainLiteral-functions#compare>'([literal(A,void),literal(C,void)],D) :- !,
	(A @< C -> D = -1; (A == C -> D = 0; (A @> C -> D = 1))).
'<http://www.w3.org/2009/rdf-PlainLiteral-functions#compare>'([literal(A,lang(B)),literal(C,lang(B))],D) :-
	(A @< C -> D = -1; (A == C -> D = 0; (A @> C -> D = 1))).

'<http://www.w3.org/2009/rdf-PlainLiteral-functions#length>'([literal(A,void)],C) :- !,
	length(A,C).
'<http://www.w3.org/2009/rdf-PlainLiteral-functions#length>'([literal(A,lang(B))],C) :-
	length(A,C).

% @@partial implementation: no false results
'<http://www.w3.org/2009/rdf-PlainLiteral-functions#matches-language-range>'([literal(A,lang(B)),literal(C,void)],true) :-
	A \= "", atom_codes(B,D), regexp_wildcard(C,E), append("^",E,F), regexp(F,D,[nocase]).


regexp_wildcard([],[]) :- !.
regexp_wildcard([0'*|A],[0'.,0'*|B]) :- !,
	regexp_wildcard(A,B).
regexp_wildcard([A|B],[A|C]) :-
	regexp_wildcard(B,C).
