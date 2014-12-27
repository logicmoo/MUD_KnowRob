% -----------------------------------
% Crypto plugin for Eye -- Jos De Roo
% -----------------------------------

:- use_module(library(semweb/rdf_db)).
:- use_module(library(sha)).

:- set_prolog_flag(unknown,fail).


'<http://www.w3.org/2000/10/swap/crypto#md5>'(literal(A,void),literal(B,void)) :-
	rdf_atom_md5(A,1,C),
	atom_codes(C,B).

'<http://www.w3.org/2000/10/swap/crypto#sha>'(literal(A,void),literal(B,void)) :-
	sha_hash(A,C,[algorithm(sha1)]),
	phrase(hash_to_ascii(C),B).


% -------
% support
% -------

hash_to_ascii([]) -->
	[].
hash_to_ascii([B|T]) -->
	hex_byte(B),
	hash_to_ascii(T).

hex_byte(Byte) -->
	{	High is (Byte>>4) /\ 0xf,
		Low  is Byte /\ 0xf
	},
	hex(High),
	hex(Low).

hex(Digit) -->
	{	code_type(Code,xdigit(Digit))
	},
	[Code].
