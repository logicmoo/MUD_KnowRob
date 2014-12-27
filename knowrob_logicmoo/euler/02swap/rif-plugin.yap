% --------------------------------
% RIF plugin for Eye -- Jos De Roo
% --------------------------------

% according to RIF Datatypes and Built-Ins 1.0 -- http://www.w3.org/TR/rif-dtb/

% 4.1.1.1 pred:literal-not-identical
'<http://www.w3.org/2007/rif-builtin-predicate#literal-not-identical>'([literal(A,B),literal(C,B)],D) :-
	when(
		(	ground([A,B,C])
		),
		(	A \== C
		->	D = true
		;	D = false
		)
	).

% 4.4.4 pred:iri-string
'<http://www.w3.org/2007/rif-builtin-predicate#iri-string>'([A,literal(B,void)],C) :-
	when(
		(	nonvar(A)
		;	nonvar(B)
		),
		(	atom(A),
			sub_atom(A,1,_,1,U),
			atomic_list_concat(['<',U,'>'],A),
			!,
			(	atom_codes(U,B)
			->	C = true
			;	C = false
			)
		;	nonvar(B),
			atom_codes(U,B),
			(	atomic_list_concat(['<',U,'>'],A)
			->	C = true
			;	C = false
			)
		)
	).

% 4.5.1 Numeric Functions
'<http://www.w3.org/2007/rif-builtin-function#numeric-add>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	sum([A,B],C)
		)
	).

'<http://www.w3.org/2007/rif-builtin-function#numeric-subtract>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			C is U-V
		)
	).

'<http://www.w3.org/2007/rif-builtin-function#numeric-multiply>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			C is U*V
		)
	).

'<http://www.w3.org/2007/rif-builtin-function#numeric-divide>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			(	V =\= 0
			->	C is U/V
			;	throw(zero_division('<http://www.w3.org/2007/rif-builtin-function#numeric-divide>'([A,B],C)))
			)
		)
	).

'<http://www.w3.org/2007/rif-builtin-function#numeric-integer-divide>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			(	V =\= 0
			->	C is integer(floor(U/V))
			;	throw(zero_division('<http://www.w3.org/2007/rif-builtin-function#numeric-integer-divide>'([A,B],C)))
			)
		)
	).

'<http://www.w3.org/2007/rif-builtin-function#numeric-mod>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			(	V =\= 0
			->	C is U-V*integer(floor(U/V))
			;	throw(zero_division('<http://www.w3.org/2007/rif-builtin-function#numeric-mod>'([A,B],C)))
			)
		)
	).

% 4.5.2.1 pred:numeric-equal
'<http://www.w3.org/2007/rif-builtin-predicate#numeric-equal>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			(	U =:= V
			->	C = true
			;	C = false
			)
		)
	).

% 4.5.2.2 pred:numeric-less-than
'<http://www.w3.org/2007/rif-builtin-predicate#numeric-less-than>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			(	U < V
			->	C = true
			;	C = false
			)
		)
	).

% 4.5.2.3 pred:numeric-greater-than
'<http://www.w3.org/2007/rif-builtin-predicate#numeric-greater-than>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			(	U > V
			->	C = true
			;	C = false
			)
		)
	).

% 4.5.2.4 pred:numeric-not-equal
'<http://www.w3.org/2007/rif-builtin-predicate#numeric-not-equal>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			(	U =\= V
			->	C = true
			;	C = false
			)
		)
	).

% 4.5.2.5 pred:numeric-less-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#numeric-less-than-or-equal>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			(	U =< V
			->	C = true
			;	C = false
			)
		)
	).

% 4.5.2.6 pred:numeric-greater-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#numeric-greater-than-or-equal>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getnumber(A,U),
			getnumber(B,V),
			(	U >= V
			->	C = true
			;	C = false
			)
		)
	).

% 4.6.1.1 func:not
'<http://www.w3.org/2007/rif-builtin-function#not>'([A],B) :-
	when(
		(	ground(A)
		),
		(	getbool(A,U),
			(	ground(B)
			->	getbool(B,V)
			;	V = B
			),
			inv(U,V)
		)
	).

% 4.6.2.1 pred:boolean-equal
'<http://www.w3.org/2007/rif-builtin-predicate#boolean-equal>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getbool(A,U),
			getbool(B,U)
		->	C = true
		;	C = false
		)
	).

% 4.6.2.2 pred:boolean-less-than
'<http://www.w3.org/2007/rif-builtin-predicate#boolean-less-than>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getbool(A,false),
			getbool(B,true)
		->	C = true
		;	C = false
		)
	).

% 4.6.2.3 pred:boolean-greater-than
'<http://www.w3.org/2007/rif-builtin-predicate#boolean-greater-than>'([A,B],C) :-
	when(
		(	ground([A,B])
		),
		(	getbool(A,true),
			getbool(B,false)
		->	C = true
		;	C = false
		)
	).

% 4.7.1.1 func:compare @@partial implementation: no collation
'<http://www.w3.org/2007/rif-builtin-function#compare>'([literal(A,B),literal(C,B)],D) :-
	!,
	(	A @< C
	->	D = -1
	;	(	A == C
		->	D = 0
		;	(	A @> C
			->	D = 1
			)
		)
	).

% 4.7.1.2 func:concat
'<http://www.w3.org/2007/rif-builtin-function#concat>'(A,literal(B,void)) :-
	when(
		(	ground(A)
		),
		(	findall(S,
				(	member(literal(S,void),A)
				),
				C
			),
			flatten(C,B)
		)
	 ).

% 4.7.1.3 func:string-join
'<http://www.w3.org/2007/rif-builtin-function#string-join>'([A,literal(B,void)],literal(C,void)) :-
	when(
		(	ground([A,B])
		),
		(	findall([S,B],
				(	member(literal(S,void),A)
				),
				D
			),
			flatten(D,C)
		)
	).

% 4.7.1.4 func:substring
'<http://www.w3.org/2007/rif-builtin-function#substring>'([literal(A,_),B,C],literal(D,void)) :-
	!,
	when(
		(	ground([A,B,C])
		),
		(	getint(B,I),
			getint(C,J),
			(	I < 1
			->	G is 0,
				H is J+I-1
			;	G is I-1,
				H is J
			),
			(	H < 0
			->	D = ""
			;	atom_codes(U,A),
				sub_atom(U,G,H,_,V),
				atom_codes(V,D)
			)
		)
	).
'<http://www.w3.org/2007/rif-builtin-function#substring>'([literal(A,_),B],literal(D,void)) :-
	when(
		(	ground([A,B])
		),
		(	getint(B,I),
			length(A,E),
			J is E-I+1,
			(	I < 1
			->	G is 0,
				H is J+I-1
			;	G is I-1,
				H is J
			),
			(	H < 0
			->	D = ""
			;	atom_codes(U,A),
				sub_atom(U,G,H,_,V),
				atom_codes(V,D)
			)
		)
	).

% 4.7.1.5 func:string-length
'<http://www.w3.org/2007/rif-builtin-function#string-length>'([literal(A,_)],B) :-
	when(
		(	ground(A)
		),
		(	length(A,B)
		)
	).

% 4.7.1.6 func:upper-case
'<http://www.w3.org/2007/rif-builtin-function#upper-case>'([literal(A,B)],literal(C,B)) :-
	when(
		(	ground([A,B])
		),
		(	upcase_string(A,C)
		)
	).

% 4.7.1.7 func:lower-case
'<http://www.w3.org/2007/rif-builtin-function#lower-case>'([literal(A,B)],literal(C,B)) :-
	when(
		(	ground([A,B])
		),
		(	downcase_string(A,C)
		)
	).

% 4.7.1.8 func:encode-for-uri
'<http://www.w3.org/2007/rif-builtin-function#encode-for-uri>'([literal(A,B)],literal(C,B)) :-
	when(
		(	ground([A,B])
		),
		(	atom_codes(D,A),
			www_form_encode(D,E),
			atom_codes(E,C)
		)
	).

% 4.7.1.11 func:substring-before @@partial implementation: no collation
'<http://www.w3.org/2007/rif-builtin-function#substring-before>'([literal(A,void),literal(B,void)],literal(C,void)) :-
	when(
		(	ground([A,B])
		),
		(	atom_codes(U,A),
			atom_codes(V,B),
			sub_atom(U,W,_,_,V),
			sub_atom(U,0,W,_,X),
			atom_codes(X,C)
		)
	).

% 4.7.1.12 func:substring-after @@partial implementation: no collation
'<http://www.w3.org/2007/rif-builtin-function#substring-after>'([literal(A,void),literal(B,void)],literal(C,void)) :-
	when(
		(	ground([A,B])
		),
		(	atom_codes(U,A),
			atom_codes(V,B),
			sub_atom(U,_,_,W,V),
			sub_atom(U,_,W,0,X),
			atom_codes(X,C)
		)
	).

% 4.7.2.1 pred:contains @@partial implementation: no collation
'<http://www.w3.org/2007/rif-builtin-predicate#contains>'([literal(A,void),literal(B,void)],C) :-
	when(
		(	ground([A,B])
		),
		(	append(D,_,A),
			append(_,B,D)
		->	C = true
		;	C = false
		)
	).

% 4.7.2.2 pred:starts-with @@partial implementation: no collation
'<http://www.w3.org/2007/rif-builtin-predicate#starts-with>'([literal(A,void),literal(B,void)],C) :-
	when(
		(	ground([A,B])
		),
		(	append(B,_,A)
		->	C = true
		;	C = false
		)
	).

% 4.7.2.3 pred:ends-with @@partial implementation: no collation
'<http://www.w3.org/2007/rif-builtin-predicate#ends-with>'([literal(A,void),literal(B,void)],C) :-
	when(
		(	ground([A,B])
		),
		(	append(_,B,A)
		->	C = true
		;	C = false
		)
	).

% 4.7.2.4 pred:matches @@partial implementation: no flags
'<http://www.w3.org/2007/rif-builtin-predicate#matches>'([literal(A,void),literal(B,void)],C) :-
	when(
		(	ground([A,B])
		),
		(	regex(B,A,_)
		->	C = true
		;	C = false
		)
	).

% 4.8.1.1 func:year-from-dateTime
'<http://www.w3.org/2007/rif-builtin-function#year-from-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(datetime(C,_,_,_,_,_,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.2 func:month-from-dateTime
'<http://www.w3.org/2007/rif-builtin-function#month-from-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(datetime(_,C,_,_,_,_,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.3 func:day-from-dateTime
'<http://www.w3.org/2007/rif-builtin-function#day-from-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(datetime(_,_,C,_,_,_,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.4 func:hours-from-dateTime
'<http://www.w3.org/2007/rif-builtin-function#hours-from-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(datetime(_,_,_,C,_,_,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.5 func:minutes-from-dateTime
'<http://www.w3.org/2007/rif-builtin-function#minutes-from-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(datetime(_,_,_,_,C,_,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.6 func:seconds-from-dateTime
'<http://www.w3.org/2007/rif-builtin-function#seconds-from-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(datetime(_,_,_,_,_,C,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.7 func:year-from-date
'<http://www.w3.org/2007/rif-builtin-function#year-from-date>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(date(C,_,_,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.8 func:month-from-date
'<http://www.w3.org/2007/rif-builtin-function#month-from-date>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(date(_,C,_,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.9 func:day-from-date
'<http://www.w3.org/2007/rif-builtin-function#day-from-date>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(date(_,_,C,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.10 func:hours-from-time
'<http://www.w3.org/2007/rif-builtin-function#hours-from-time>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(time(C,_,_,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.11 func:minutes-from-time
'<http://www.w3.org/2007/rif-builtin-function#minutes-from-time>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(time(_,C,_,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.12 func:seconds-from-time
'<http://www.w3.org/2007/rif-builtin-function#seconds-from-time>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(time(_,_,C,_),A),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).

% 4.8.1.13 func:years-from-duration
'<http://www.w3.org/2007/rif-builtin-function#years-from-duration>'([literal(_,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],0) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#years-from-duration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(yearmonthduration(C),A),
			D is C//12,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).

% 4.8.1.14 func:months-from-duration
'<http://www.w3.org/2007/rif-builtin-function#months-from-duration>'([literal(_,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],0) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#months-from-duration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(yearmonthduration(C),A),
			D is C-(C//12)*12,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).

% 4.8.1.15 func:days-from-duration
'<http://www.w3.org/2007/rif-builtin-function#days-from-duration>'([literal(_,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],_) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#days-from-duration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(daytimeduration(C),A),
			D is integer(C)//86400,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).

% 4.8.1.16 func:hours-from-duration
'<http://www.w3.org/2007/rif-builtin-function#hours-from-duration>'([literal(_,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],_) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#hours-from-duration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(daytimeduration(C),A),
			D is (integer(C)-(integer(C)//86400)*86400)//3600,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).

% 4.8.1.17 func:minutes-from-duration
'<http://www.w3.org/2007/rif-builtin-function#minutes-from-duration>'([literal(_,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],_) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#minutes-from-duration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(daytimeduration(C),A),
			D is (integer(C)-(integer(C)//3600)*3600)//60,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).

% 4.8.1.18 func:seconds-from-duration
'<http://www.w3.org/2007/rif-builtin-function#seconds-from-duration>'([literal(_,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],_) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#seconds-from-duration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],B) :-
	when(
		(	ground(A)
		),
		(	phrase(daytimeduration(C),A),
			D is C-(integer(C)//60)*60,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).

% 4.8.1.19 func:timezone-from-dateTime
'<http://www.w3.org/2007/rif-builtin-function#timezone-from-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground(A)
		),
		(	phrase(datetime(_,_,_,_,_,_,C),A),
			(	ground(B)
			->	phrase(daytimeduration(D),B),
				D =:= C
			;	daytimeduration(C,B)
			)
		)
	).

% 4.8.1.20 func:timezone-from-date
'<http://www.w3.org/2007/rif-builtin-function#timezone-from-date>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>'))],
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground(A)
		),
		(	phrase(date(_,_,_,C),A),
			(	ground(B)
			->	phrase(daytimeduration(D),B),
				D =:= C
			;	daytimeduration(C,B)
			)
		)
	).

% 4.8.1.21 func:timezone-from-time
'<http://www.w3.org/2007/rif-builtin-function#timezone-from-time>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>'))],
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground(A)
		),
		(	phrase(time(_,_,_,C),A),
			(	ground(B)
			->	phrase(daytimeduration(D),B),
				D =:= C
			;	daytimeduration(C,B)
			)
		)
	).

% 4.8.1.22 func:subtract-dateTimes
'<http://www.w3.org/2007/rif-builtin-function#subtract-dateTimes>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D),A),
			phrase(datetime(E),B),
			F is D-E,
			(	ground(C)
			->	phrase(daytimeduration(G),C),
				G =:= F
			;	daytimeduration(F,C)
			)
		)
	).

% 4.8.1.23 func:subtract-dates
'<http://www.w3.org/2007/rif-builtin-function#subtract-dates>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#date>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D),A),
			phrase(date(E),B),
			F is D-E,
			(	ground(C)
			->	phrase(daytimeduration(G),C),
				G =:= F
			;	daytimeduration(F,C)
			)
		)
	).

% 4.8.1.24 func:subtract-times
'<http://www.w3.org/2007/rif-builtin-function#subtract-times>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#time>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(time(D),A),
			phrase(time(E),B),
			F is D-E,
			(	ground(C)
			->	phrase(daytimeduration(G),C),
				G =:= F
			;	daytimeduration(F,C)
			)
		)
	).

% 4.8.1.25 func:add-yearMonthDurations
'<http://www.w3.org/2007/rif-builtin-function#add-yearMonthDurations>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(yearmonthduration(D),A),
			phrase(yearmonthduration(E),B),
			F is D+E,
			(	ground(C)
			->	phrase(yearmonthduration(G),C),
				G =:= F
			;	yearmonthduration(F,C)
			)
		)
	).

% 4.8.1.26 func:subtract-yearMonthDurations
'<http://www.w3.org/2007/rif-builtin-function#subtract-yearMonthDurations>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(yearmonthduration(D),A),
			phrase(yearmonthduration(E),B),
			F is D-E,
			(	ground(C)
			->	phrase(yearmonthduration(G),C),
				G =:= F
			;	yearmonthduration(F,C)
			)
		)
	).

% 4.8.1.27 func:multiply-yearMonthDuration
'<http://www.w3.org/2007/rif-builtin-function#multiply-yearMonthDuration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),B],
	literal(C,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(yearmonthduration(D),A),
			getnumber(B,E),
			F is integer(round(D*E-1)+1),
			(	ground(C)
			->	phrase(yearmonthduration(G),C),
				G =:= F
			;	yearmonthduration(F,C)
			)
		)
	).

% 4.8.1.28 func:divide-yearMonthDuration
'<http://www.w3.org/2007/rif-builtin-function#divide-yearMonthDuration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),B],
	literal(C,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(yearmonthduration(D),A),
			getnumber(B,E),
			F is integer(round(D/E-1)+1),
			(	ground(C)
			->	phrase(yearmonthduration(G),C),
				G =:= F
			;	yearmonthduration(F,C)
			)
		)
	).

% 4.8.1.29 func:divide-yearMonthDuration-by-yearMonthDuration
'<http://www.w3.org/2007/rif-builtin-function#divide-yearMonthDuration-by-yearMonthDuration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(yearmonthduration(D),A),
			phrase(yearmonthduration(E),B),
			F is D/E,
			(	ground(C)
			->	C =:= F
			;	C = F
			)
		)
	).

% 4.8.1.30 func:add-dayTimeDurations
'<http://www.w3.org/2007/rif-builtin-function#add-dayTimeDurations>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(daytimeduration(D),A),
			phrase(daytimeduration(E),B),
			F is D+E,
			(	ground(C)
			->	phrase(daytimeduration(G),C),
				G =:= F
			;	daytimeduration(F,C)
			)
		)
	).

% 4.8.1.31 func:subtract-dayTimeDurations
'<http://www.w3.org/2007/rif-builtin-function#subtract-dayTimeDurations>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(daytimeduration(D),A),
			phrase(daytimeduration(E),B),
			F is D-E,
			(	ground(C)
			->	phrase(daytimeduration(G),C),
				G =:= F
			;	daytimeduration(F,C)
			)
		)
	).

% 4.8.1.32 func:multiply-dayTimeDuration
'<http://www.w3.org/2007/rif-builtin-function#multiply-dayTimeDuration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),B],
	literal(C,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(daytimeduration(D),A),
			getnumber(B,E),
			F is integer(round(D*E-1)+1),
			(	ground(C)
			->	phrase(daytimeduration(G),C),
				G =:= F
			;	daytimeduration(F,C)
			)
		)
	).

% 4.8.1.33 func:divide-dayTimeDuration
'<http://www.w3.org/2007/rif-builtin-function#divide-dayTimeDuration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),B],
	literal(C,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(daytimeduration(D),A),
			getnumber(B,E),
			F is integer(round(D/E-1)+1),
			(	ground(C)
			->	phrase(daytimeduration(G),C),
				G =:= F
			;	daytimeduration(F,C)
			)
		)
	).

% 4.8.1.34 func:divide-dayTimeDuration-by-dayTimeDuration
'<http://www.w3.org/2007/rif-builtin-function#divide-dayTimeDuration-by-dayTimeDuration>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(daytimeduration(D),A),
			phrase(daytimeduration(E),B),
			F is D/E,
			(	ground(C)
			->	C =:= F
			;	C = F
			)
		)
	).

% 4.8.1.35 func:add-yearMonthDuration-to-dateTime
'<http://www.w3.org/2007/rif-builtin-function#add-yearMonthDuration-to-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D,E,F,G,H,I,J),A),
			phrase(yearmonthduration(K),B),
			L is E+K-1,
			Q is D+integer(floor(L/12)),
			R is L-integer(floor(L/12))*12+1,
			memotime(datime(Q,R,F,G,H,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			O is M+I+31536000-N-J,
			(	ground(C)
			->	phrase(datetime(P),C),
				O =:= P
			;	Offset is -J,
				stamp_date_time(O,date(Year,Month,Day,Hour,Minute,Second,_,_,_),Offset),
				datetime(Year,Month,Day,Hour,Minute,Second,Offset,C)
			)
		)
	).

% 4.8.1.36 func:add-yearMonthDuration-to-date
'<http://www.w3.org/2007/rif-builtin-function#add-yearMonthDuration-to-date>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#date>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D,E,F,G),A),
			phrase(yearmonthduration(K),B),
			L is E+K-1,
			Q is D+integer(floor(L/12)),
			R is L-integer(floor(L/12))*12+1,
			memotime(datime(Q,R,F,0,0,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			O is (integer(floor(M+31536000-N-G))//60)*60,
			(	ground(C)
			->	phrase(date(P),C),
				O =:= P
			;	date(O,C)
			)
		)
	).

% 4.8.1.37 func:add-dayTimeDuration-to-dateTime
'<http://www.w3.org/2007/rif-builtin-function#add-dayTimeDuration-to-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D,E,F,G,H,I,J),A),
			phrase(daytimeduration(K),B),
			L is I+K,
			memotime(datime(D,E,F,G,H,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			O is M+L+31536000-N-J,
			(	ground(C)
			->	phrase(datetime(P),C),
				O =:= P
			;	Offset is -J,
				stamp_date_time(O,date(Year,Month,Day,Hour,Minute,Second,_,_,_),Offset),
				datetime(Year,Month,Day,Hour,Minute,Second,Offset,C)
			)
		)
	).

% 4.8.1.38 func:add-dayTimeDuration-to-date
'<http://www.w3.org/2007/rif-builtin-function#add-dayTimeDuration-to-date>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#date>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D,E,F,G),A),
			phrase(daytimeduration(K),B),
			L is integer(K),
			memotime(datime(D,E,F,0,0,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			O is (integer(floor(M+L+31536000-N))//86400)*86400-G,
			(	ground(C)
			->	phrase(date(P),C),
				O =:= P
			;	date(O,C)
			)
		)
	).

% 4.8.1.39 func:add-dayTimeDuration-to-time
'<http://www.w3.org/2007/rif-builtin-function#add-dayTimeDuration-to-time>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#time>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(time(D,E,F,G),A),
			phrase(daytimeduration(K),B),
			L is F+K,
			memotime(datime(1972,12,31,D,E,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			Z is M+L+31536000-N-G,
			O is Z-86400*integer(floor(Z/86400)),
			(	ground(C)
			->	phrase(time(P),C),
				O =:= P-86400*integer(floor(P/86400))
			;	Offset is -G,
				stamp_date_time(O,date(_,_,_,Hour,Minute,Second,_,_,_),Offset),
				time(Hour,Minute,Second,Offset,C)
			)
		)
	).

% 4.8.1.40 func:subtract-yearMonthDuration-from-dateTime
'<http://www.w3.org/2007/rif-builtin-function#subtract-yearMonthDuration-from-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D,E,F,G,H,I,J),A),
			phrase(yearmonthduration(K),B),
			L is E-K-1,
			Q is D+integer(floor(L/12)),
			R is L-integer(floor(L/12))*12+1,
			memotime(datime(Q,R,F,G,H,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			O is M+I+31536000-N-J,
			(	ground(C)
			->	phrase(datetime(P),C),
				O =:= P
			;	Offset is -J,
				stamp_date_time(O,date(Year,Month,Day,Hour,Minute,Second,_,_,_),Offset),
				datetime(Year,Month,Day,Hour,Minute,Second,Offset,C)
			)
		)
	).

% 4.8.1.41 func:subtract-yearMonthDuration-from-date
'<http://www.w3.org/2007/rif-builtin-function#subtract-yearMonthDuration-from-date>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#date>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D,E,F,G),A),
			phrase(yearmonthduration(K),B),
			L is E-K-1,
			Q is D+integer(floor(L/12)),
			R is L-integer(floor(L/12))*12+1,
			memotime(datime(Q,R,F,0,0,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			O is (integer(floor(M+31536000-N-G))//60)*60,
			(	ground(C)
			->	phrase(date(P),C),
				O =:= P
			;	date(O,C)
			)
		)
	).

% 4.8.1.42 func:subtract-dayTimeDuration-from-dateTime
'<http://www.w3.org/2007/rif-builtin-function#subtract-dayTimeDuration-from-dateTime>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D,E,F,G,H,I,J),A),
			phrase(daytimeduration(K),B),
			L is I-integer(K),
			memotime(datime(D,E,F,G,H,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			O is M+L+31536000-N-J,
			(	ground(C)
			->	phrase(datetime(P),C),
				O =:= P
			;	Offset is -J,
				stamp_date_time(O,date(Year,Month,Day,Hour,Minute,Second,_,_,_),Offset),
				datetime(Year,Month,Day,Hour,Minute,Second,Offset,C)
			)
		)
	).

% 4.8.1.43 func:subtract-dayTimeDuration-from-date
'<http://www.w3.org/2007/rif-builtin-function#subtract-dayTimeDuration-from-date>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#date>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D,E,F,G),A),
			phrase(daytimeduration(K),B),
			L is -integer(K),
			memotime(datime(D,E,F,0,0,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			O is (integer(floor(M+L+31536000-N))//86400)*86400-G,
			(	ground(C)
			->	phrase(date(P),C),
				O =:= P
			;	date(O,C)
			)
		)
	).

% 4.8.1.44 func:subtract-dayTimeDuration-from-time
'<http://www.w3.org/2007/rif-builtin-function#subtract-dayTimeDuration-from-time>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],literal(C,type('<http://www.w3.org/2001/XMLSchema#time>'))) :-
	when(
		(	ground([A,B])
		),
		(	phrase(time(D,E,F,G),A),
			phrase(daytimeduration(K),B),
			L is F-K,
			memotime(datime(1972,12,31,D,E,0),M),
			memotime(datime(1971,1,1,0,0,0),N),
			Z is M+L+31536000-N-G,
			O is Z-86400*integer(floor(Z/86400)),
			(	ground(C)
			->	phrase(time(P),C),
				O =:= P-86400*integer(floor(P/86400))
			;	Offset is -G,
				stamp_date_time(O,date(_,_,_,Hour,Minute,Second,_,_,_),Offset),
				time(Hour,Minute,Second,Offset,C)
			)
		)
	).

% 4.8.2.1 pred:dateTime-equal
'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D),A),
			phrase(datetime(E),B),
			(	D =:= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.2 pred:dateTime-less-than
'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-less-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D),A),
			phrase(datetime(E),B),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.3 pred:dateTime-greater-than
'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-greater-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D),A),
			phrase(datetime(E),B),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.4 pred:date-equal
'<http://www.w3.org/2007/rif-builtin-predicate#date-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#date>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D),A),
			phrase(date(E),B),
			(	D =:= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.5 pred:date-less-than
'<http://www.w3.org/2007/rif-builtin-predicate#date-less-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#date>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D),A),
			phrase(date(E),B),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.6 pred:date-greater-than
'<http://www.w3.org/2007/rif-builtin-predicate#date-greater-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#date>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D),A),
			phrase(date(E),B),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.7 pred:time-equal
'<http://www.w3.org/2007/rif-builtin-predicate#time-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#time>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(time(D),A),
			phrase(time(E),B),
			(	D =:= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.8 pred:time-less-than
'<http://www.w3.org/2007/rif-builtin-predicate#time-less-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#time>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(time(D),A),
			phrase(time(E),B),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.9 pred:time-greater-than
'<http://www.w3.org/2007/rif-builtin-predicate#time-greater-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#time>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(time(D),A),
			phrase(time(E),B),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.10 pred:duration-equal
'<http://www.w3.org/2007/rif-builtin-predicate#duration-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#duration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#duration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(duration(D),A),
			phrase(duration(E),B),
			(	D =:= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.11 pred:dayTimeDuration-less-than
'<http://www.w3.org/2007/rif-builtin-predicate#dayTimeDuration-less-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(daytimeduration(D),A),
			phrase(daytimeduration(E),B),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.12 pred:dayTimeDuration-greater-than
'<http://www.w3.org/2007/rif-builtin-predicate#dayTimeDuration-greater-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(daytimeduration(D),A),
			phrase(daytimeduration(E),B),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.13 pred:yearMonthDuration-less-than
'<http://www.w3.org/2007/rif-builtin-predicate#yearMonthDuration-less-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(yearmonthduration(D),A),
			phrase(yearmonthduration(E),B),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.14 pred:yearMonthDuration-greater-than
'<http://www.w3.org/2007/rif-builtin-predicate#yearMonthDuration-greater-than>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(yearmonthduration(D),A),
			phrase(yearmonthduration(E),B),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.15 pred:dateTime-not-equal
'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-not-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D),A),
			phrase(datetime(E),B),
			(	D =\= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.16 pred:dateTime-less-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-less-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D),A),
			phrase(datetime(E),B),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.17 pred:dateTime-greater-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-greater-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(datetime(D),A),
			phrase(datetime(E),B),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.18 pred:date-not-equal
'<http://www.w3.org/2007/rif-builtin-predicate#date-not-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#date>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D),A),
			phrase(date(E),B),
			(	D =\= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.19 pred:date-less-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#date-less-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#date>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D),A),
			phrase(date(E),B),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.20 pred:date-greater-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#date-greater-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#date>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(date(D),A),
			phrase(date(E),B),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.21 pred:time-not-equal
'<http://www.w3.org/2007/rif-builtin-predicate#time-not-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#time>'))],C) :-
	when(
		(	ground([A,B])
		),
	(	phrase(time(D),A),
			phrase(time(E),B),
			(	D =\= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.22 pred:time-less-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#time-less-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#time>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(time(D),A),
			phrase(time(E),B),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	 ).

% 4.8.2.23 pred:time-greater-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#time-greater-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#time>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(time(D),A),
			phrase(time(E),B),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.24 pred:duration-not-equal
'<http://www.w3.org/2007/rif-builtin-predicate#duration-not-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#duration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#duration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(duration(D),A),
			phrase(duration(E),B),
			(	D =\= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.25 pred:dayTimeDuration-less-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#dayTimeDuration-less-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(daytimeduration(D),A),
			phrase(daytimeduration(E),B),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.26 pred:dayTimeDuration-greater-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#dayTimeDuration-greater-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(daytimeduration(D),A),
			phrase(daytimeduration(E),B),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.27 pred:yearMonthDuration-less-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#yearMonthDuration-less-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(yearmonthduration(D),A),
			phrase(yearmonthduration(E),B),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	).

% 4.8.2.28 pred:yearMonthDuration-greater-than-or-equal
'<http://www.w3.org/2007/rif-builtin-predicate#yearMonthDuration-greater-than-or-equal>'([literal(A,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B,type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))],C) :-
	when(
		(	ground([A,B])
		),
		(	phrase(yearmonthduration(D),A),
			phrase(yearmonthduration(E),B),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).

% 4.10.1.1 func:PlainLiteral-from-string-lang
'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-from-string-lang>'([literal(A,void)],literal(A,void)) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-from-string-lang>'([literal(A,void),literal(B,void)],literal(A,lang(C))) :-
	downcase_string(B,D),
	atom_codes(C,D).

% 4.10.1.2 func:string-from-PlainLiteral
'<http://www.w3.org/2007/rif-builtin-function#string-from-PlainLiteral>'([literal(A,void)],literal(A,void)) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#string-from-PlainLiteral>'([literal(A,lang(_))],literal(A,void)).

% 4.10.1.3 func:lang-from-PlainLiteral
'<http://www.w3.org/2007/rif-builtin-function#lang-from-PlainLiteral>'([literal(_,void)],literal("",void)) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#lang-from-PlainLiteral>'([literal(_,lang(A))],literal(B,void)) :-
	atom_codes(A,B).

% 4.10.1.4 func:PlainLiteral-compare @@partial implementation: no collation
'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-compare>'([literal(A,void),literal(C,void)],D) :-
	!,
	(	A @< C
	->	D = -1
	;	(	A == C
		->	D = 0
		;	(	A @> C
			->	D = 1
			)
		)
	).
'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-compare>'([literal(A,lang(B)),literal(C,lang(B))],D) :-
	(	A @< C
	->	D = -1
	;	(	A == C
		->	D = 0
		;	(	A @> C
			->	D = 1
			)
		)
	).

% 4.10.1.5 func:PlainLiteral-length
'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-length>'([literal(A,void)],C) :-
	!,
	length(A,C).
'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-length>'([literal(A,lang(_))],C) :-
	length(A,C).

% 4.10.2.1 pred:matches-language-range @@partial implementation: no false results
'<http://www.w3.org/2007/rif-builtin-predicate#matches-language-range>'([literal(A,lang(B)),literal(C,void)],true) :-
	A \= "",
	atom_codes(B,D),
	regexp_wildcard(C,E),
	append("^",E,F),
	downcase_string(F,G),
	downcase_string(D,H),
	regex(G,H,_).

% 4.11.3.1 pred:is-list
'<http://www.w3.org/2007/rif-builtin-predicate#is-list>'([A],B) :-
	(	is_list(A)
	->	B = true
	;	B = false
	).

% 4.11.3.2 pred:list-contains
'<http://www.w3.org/2007/rif-builtin-predicate#list-contains>'([A,B],C) :-
	when(
		(	nonvar(A)
		),
		(	member(B,A)
		->	C = true
		;	C = false
		)
	).

% 4.11.4.1 func:make-list
'<http://www.w3.org/2007/rif-builtin-function#make-list>'(A,A).

% 4.11.4.2 func:count
'<http://www.w3.org/2007/rif-builtin-function#count>'([A],B) :-
	when(
		(	nonvar(A)
		),
		(	length(A,B)
		)
	).

% 4.11.4.3 func:get
'<http://www.w3.org/2007/rif-builtin-function#get>'([A,B],C) :-
	when(
		(	nonvar(A),
			ground(B)
		),
		(	getnumber(B,U),
			nth0(U,A,C)
		)
	).

% 4.11.4.4 func:sublist
'<http://www.w3.org/2007/rif-builtin-function#sublist>'([A,B,C],D) :-
	!,
	when(
		(	nonvar(A),
			ground([B,C])
		),
		(	getint(B,U),
			getint(C,V),
			length(A,W),
			(	U < 0
			->	I is W+U
			;	I is U
			),
			(	V < 0
			->	J is W+V
			;	J is V
			),
			append(E,F,A),
			length(E,I),
			append(D,G,F),
			K is J-I,
			(	length(D,K)
			->	true
			;	G = []
			),
			!
		)
	).
'<http://www.w3.org/2007/rif-builtin-function#sublist>'([A,B],C) :-
	when(
		(	nonvar(A),
			ground(B)
		),
		(	getint(B,U),
			length(A,W),
			(	U < 0
			->	I is W+U
			;	I is U
			),
			append(E,C,A),
			length(E,I),
			!
		)
	).

% 4.11.4.5 func:append
'<http://www.w3.org/2007/rif-builtin-function#append>'([A|B],C) :-
	append(A,B,C).

% 4.11.4.6 func:concatenate
'<http://www.w3.org/2007/rif-builtin-function#concatenate>'(A,B) :-
	when(
		(	nonvar(A)
		),
		(	append(A,B)
		)
	).

% 4.11.4.7 func:insert-before
'<http://www.w3.org/2007/rif-builtin-function#insert-before>'([A,B,C],D) :-
	when(
		(	nonvar(A),
			ground([B,C])
		),
		(	getint(B,U),
			length(A,W),
			(	U < 0
			->	I is W+U
			;	I is U
			),
			append(G,H,A),
			length(G,I),
			append([G,[C],H],D)
		)
	).

% 4.11.4.8 func:remove
'<http://www.w3.org/2007/rif-builtin-function#remove>'([A,B],C) :-
	when(
		(	nonvar(A),
			ground(B)
		),
		(	getint(B,U),
			length(A,W),
			(	U < 0
			->	I is W+U
			;	I is U
			),
			append(G,[_|T],A),
			length(G,I),
			append(G,T,C)
		)
	).

% 4.11.4.9 func:reverse
'<http://www.w3.org/2007/rif-builtin-function#reverse>'([A],B) :-
	reverse(A,B).

% 4.11.4.10 func:index-of
'<http://www.w3.org/2007/rif-builtin-function#index-of>'([A,B],C) :-
	when(
		(	nonvar(A),
			ground(B)
		),
		(	findall(I,
				(	nth0(I,A,B)
				),
				C
			)
		)
	).

% 4.11.4.11 func:union
'<http://www.w3.org/2007/rif-builtin-function#union>'(A,B) :-
	when(
		(	nonvar(A)
		),
		(	append(A,C),
			distinct(C,B)
		)
	).

% 4.11.4.12 func:distinct-values
'<http://www.w3.org/2007/rif-builtin-function#distinct-values>'([A],B) :-
	when(
		(	nonvar(A)
		),
		(	distinct(A,B)
		)
	).

% 4.11.4.13 func:intersect
'<http://www.w3.org/2007/rif-builtin-function#intersect>'([A,B],C) :-
	when(
		(	ground(A),
			ground(B)
		),
		(	findall(I,
				(	member(I,A),
					member(I,B)
				),
				C
			)
		)
	).

% 4.11.4.14 func:except
'<http://www.w3.org/2007/rif-builtin-function#except>'([A,B],C) :-
	when(
		(	ground(A),
			ground(B)
		),
		(	findall(I,
				(	member(I,A),
					\+member(I,B)
				),
				C
			)
		)
	).


%%% support predicates
regexp_wildcard([],[]) :-
	!.
regexp_wildcard([0'*|A],[0'.,0'*|B]) :-
	!,
	regexp_wildcard(A,B).
regexp_wildcard([A|B],[A|C]) :-
	regexp_wildcard(B,C).

getbool(literal("false",type('<http://www.w3.org/2001/XMLSchema#boolean>')),false).
getbool(literal("true",type('<http://www.w3.org/2001/XMLSchema#boolean>')),true).
getbool(false,false).
getbool(true,true).

inv(false,true).
inv(true,false).

getint(A,B) :-
	getnumber(A,C),
	B is integer(round(C)).

