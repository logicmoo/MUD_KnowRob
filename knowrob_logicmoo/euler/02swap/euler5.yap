% -----------------------------------
% Euler proof mechanism -- Jos De Roo
% -----------------------------------

id('$Id: euler5.yap 2537 2008-12-30 15:03:43Z josd $').

:-
	use_module(library(lists)),
	use_module(library(pillow)),
	use_module(library(regexp)),
	use_module(library(charsio)),
	unix(argv(V)),
	(	memberchk(profile,V)
	->	yap_flag(profiling,on)
	;	true
	).

main:-
	(	unix(argv(V)),
		args(V,web,W)
	->	assert(flag('e:nope')),
		web(W,A),
		phrase(object(B),A),
		memberchk(pair(prefixes,C),B),
		pp(C),
		memberchk(pair(triples,D),B),
		pt(D),
		memberchk(pair(query,E),B),
		pq(E)
	;	consult(user)
	),
	id(X),
	sub_atom(X,1,_,1,Y),
	write('#Processed by '), write(Y), nl, nl,
	(	pfx(X1,X2),
		write('@prefix '), write(X1), write(' '), write(X2), write('.'), nl,
		fail
	;	nl
	),
	(	flag('e:bchain')
	->	feed(U,_),
		write('_: owl:sameAs '), wt(U), write('.'), nl, nl,
		why,
		write_proof
	;	assert(tabs('')),
		assert(scope(0)),
		assert(limit(1)),
		step,
		bnet,
		step(0),
		w3
	),
	statistics(runtime,[_,Z]),
	write('#ENDS '), write(Z), write(' msec.'), nl, nl,
	(	unix(argv(V)),
		memberchk(profile,V),
		profile
	;	true
	).


% -------------
% proof engines
% -------------

% why/0 for backward chaining
why:-
	goal(C,P),
	step(','(C,P),A,[],[],S,1,D),
	abstract(','(C,P),H),
	why(H,A,S,D),
	fail.
why.

% why/4 using goal,proof,state,value
why(H,A,S,D) :-
	proof_state(H,U,T,V),
	subset(T,S), !,					% minimal model
	(	subset(S,T),
		'e:binaryEntropy'(D,E,_),
		'e:binaryEntropy'(V,W,_),
		(	E > W					% maximal entropy
		;	E =:= W,
			(	positive(H)
			->	D > V				% maximal belief
			;	D < V				% minimal disbelief
			)
		)
	->	retract(proof_state(H,U,T,V)),
		assert(proof_state(H,A,S,D))
	;	true
	).
why(H,_,S,_) :-
	proof_state(H,_,T,_),
	subset(S,T),
	retract(proof_state(H,_,T,_)),
	fail.
why(H,A,S,D) :-
	assert(proof_state(H,A,S,D)).

% step/7 using goal,proof,pending,state,state,value,value
step(','(A,B),conjunction(C,D),U,V,W,I,J) :- !,
	step(A,C,U,V,X,I,K),
	step(B,D,U,X,W,K,J).
step('e:true'(_,A,_),fact('e:true'(_,A,_)),_,V,V,I,J) :- !,
	J is I*A.
step(A,_,U,_,_,_,_) :-
	inverse(A,B),
	memberchk(B,U), !,				% euler path fail case
	fail.
step(A,_,U,_,_,_,_) :-
	memberchk(A,U), !,				% euler path fail case
	fail.
step(A,lemma(A),_,V,V,I,I) :-
	abstract(A,C),
	ground(C),
	memberchk(C,V), !.				% euler path lemma case
step(A,fact(A),_,V,V,I,I) :-
	A.
step(A,Y,U,V,X,I,J) :-
	rule(A,C),
	step(C,B,[A|U],V,W,I,K),
	abstract(A,Z),
	step(A,Z,B,Y,V,W,X,I,K,J).

% step/10 using goal,goal,proof,proof,state,state,state,value,value,value
step(_,Z,_,_,V,_,_,_,_,_) :-
	inversa(Z,U),
	memberchk(U,V), !,				% euler path fail case
	fail.
step(A,Z,_,lemma(A),V,W,W,I,_,I) :-
	memberchk(Z,V), !.				% euler path lemma case
step(A,Z,B,rule(A,B),_,W,[Z|W],_,K,K).

% step/0 for forward chaining
step:-
	(	'log:implies'(lf(X),lf(Y),_),
		X,
		\+Y,						% euler path lemma case
		assert(Y),
		assert(step(X,Y)),
		\+retractall(brake)
	;	brake
	;	assert(brake),
		step
	).

% step/1 for recursive scoping during forward chaining
step(A) :-
	(	limit(B),
		A < B,
		C is A + 1,
		assert(scope(C)),
		step,
		step(C)
	;	true
	).


% ------------
% proof output
% ------------

write_proof:-
	proof_state(G,_,_,_),
	findall(A,(retract(proof_state(G,U,_,A)), write_proof(U,A)),B),
	B \= [],
	sum(B,C),
	assert(goal_rank(G,C)),
	fail.
write_proof:-
	goal_rank(','(I,H),A),
	(	inversa(I,J)
	->	goal_rank(','(J,H),B),
		D is A+B,
		D > 0,
		C is A/D
	;	C is 1
	),
	(	flag('e:be')
	->	cappend(H,'e:true'('_:',C,_),F),
		\+proof_conclusion(I,F),
		assert(proof_conclusion(I,F)),
		wt(lf(F)),
		write(' => '),
		wt(lf(I)),
		write('.'), nl
	;	(	C < 1
		->	true
		;	\+proof_conclusion(I),
			assert(proof_conclusion(I)),
			wt(I),
			write('.'), nl
		)
	),
	fail.
write_proof:-
	nl.

write_proof(A,B) :-
	flag('e:pe'), !,
	write('{#e:possibility'), nl,
	wp(A,'\t'),
	format('} e:possibility ~w.~2n',[B]).
write_proof(_,_).

wp(rule(A,fact('e:true'(_,B,_))),C) :-
	B =:= 1, !,
	wp(fact(A),C).
wp(rule(A,B),C) :- !,
	write(C),
	write('{'),
	wt(A),
	write('} e:because {'), nl,
	atom_concat(C,'\t',D),
	wp(B,D),
	write(C),
	write('}; e:source '),
	arg(3,A,S),
	write(S),
	write('.'), nl.
wp(conjunction(A,B),C) :- !,
	wp(A,C),
	wp(B,C).
wp(A,B) :-
	write(B),
	wt(A),
	write('.'), nl.

w3:-
	flag('e:nope'), !,
	(	step(_,case(Y)),
		wt(Y),
		write('.'), nl,
		fail
	;	nl
	).
w3:-
	write('[ a r:Proof, r:Conjunction;'),
	indent,
	ws,
	(	step(X,case(Y)),
		write('r:component '),
		wi(X,Y),
		write(';'),
		ws,
		fail
	;	true
	),
	write('r:gives {'),
	indent,
	(	step(_,case(Y)),
		ws,
		ww(Y),
		wt(Y),
		write('.'),
		fail
	;	true
	),
	write('}].'), nl, nl.

wi(X,Y) :-
	write('[ a r:Inference; r:gives {'),
	ww(Y),
	wt(Y),
	write('}; r:evidence ('),
	indent,
	wr(X),
	write(');'),
	ws,
	clast(X,instrument(R,B)),
	wb(B),
	dedent, !,
	write('r:rule [ a r:Extraction; r:gives '),
	wt(lf(R)),
	write('; r:because [ a r:Parsing; r:source '),
	arg(3,Y,W),
	write(W),
	write(']]]').

wr(','(atom(_),Y)) :- !,
	wr(Y).
wr(','(_=..[_,_,_,_],Y)) :- !,
	wr(Y).
wr(','(X,Y)) :- !,
	wr(X),
	wr(Y).
wr(atom(_)) :- !.
wr(_=..[_,_,_,_]) :- !.
wr(instrument(_,_)) :- !.
wr('='(X,Y)) :- !,
	ws,
	write('[ a r:Fact; r:gives '),
	wt(lf('='(X,Y))),
	write(']').
wr(Y) :-
	step(X,Y), !,
	ws,
	write('[ a r:Extraction; r:gives {'),
	ww(Y),
	wt(Y),
	write('}; r:because '),
	wi(X,Y),
	write(']').
wr(Y) :-
	arg(3,Y,W),
	ground(W), !,
	ws,
	write('[ a r:Extraction; r:gives {'),
	ww(Y),
	wt(Y),
	write('}; r:because [ a r:Parsing; r:source '),
	write(W),
	write(']]').
wr(Y) :-
	auri(Y,Z),
	ws,
	write('[ a r:Fact; r:gives '),
	wt(lf(Z)),
	write(']').

wt(X) :-
	var(X), !,
	write('var:'),
	write(X).
wt(fpath(X,Y)) :- !,
	wt(X),
	write('!'),
	wt(Y).
wt(bpath(X,Y)) :- !,
	wt(X),
	write('^'),
	wt(Y).
wt(tlit(X,Y)) :- !,
	wt(X),
	write('^^'),
	wt(Y).
wt(plit(X,Y)) :- !,
	wt(X),
	write('@'),
	wt(Y).
wt(fact(X)) :- !,
	write('{'),
	wt(X),
	(	arg(3,X,S)
	->	(	ground(S)
		->	write('} a e:Fact; e:source '),
			write(S)
		;	write('} a e:Builtin')
		)
	;	true
	).
wt(lemma(X)) :- !,
	write('{'),
	wt(X),
	write('} a e:Lemma').
wt(lf(','('e:true'(_,1),Y))) :- !,
	wt(lf(Y)).
wt(lf(','(X,Y))) :- !,
	write('{'),
	wt(X),
	wf(Y),
	write('}').
wt(lf(case(X))) :- !,
	write('{'),
	wt(X),
	write('}').
wt(lf(X)) :- !,
	write('{'),
	wt(X),
	write('}').
wt(','(X,Y)) :- !,
	wt(X),
	write('. '),
	wt(Y).
wt([]) :- !,
	write('()').
wt([X|Y]) :- !,
	write('('),
	wt(X),
	wl(Y),
	write(')').
wt(X) :-
	functor(X,P,A),
	A > 1, !,
	arg(1,X,S),
	arg(2,X,O),
	wt(S),
	write(' '),
	wt(P),
	write(' '),
	wt(O).
wt(X) :-
	flag('e:nope'),
	atom(X),
	atom_concat('var:',Y,X), !,
	write('_:'),
	write(Y).
wt(X) :-
	write(X).

wf(','(ground(_),Y)) :- !,
	wf(Y).
wf(','(_=..[_,_,_,_],Y)) :- !,
	wf(Y).
wf(','(X,Y)) :- !,
	write('. '),
	wt(X),
	wf(Y).
wf(ground(_)) :- !.
wf(_=..[_,_,_,_]) :- !.
wf(instrument(_,_)) :- !.
wf(X) :-
	write('. '),
	wt(X).

wl([]) :- !.
wl([X|Y]) :-
	write(' '),
	wt(X),
	wl(Y).

wb([]) :- !.
wb([X,Y|Z]) :- !,
	write('r:binding [ r:variable [ n3:uri '),
	write(X),
	write(']; r:boundTo '),
	wv(Y),
	write(']; '),
	ws,
	wb(Z).

wv(X) :-
	atom(X),
	sub_atom(X,0,A,B,'var:'), !,
	write('[ a r:Existential; n3:nodeId "http://localhost/var#'),
	sub_atom(X,A,B,_,Q),
	write(Q),
	write('"'),
	write(']').
wv(X) :-
	quri(X,Y), !,
	sub_atom(Y,1,_,1,U),
	write('[ n3:uri "'),
	write(U),
	write('"'),
	write(']').
wv(X) :-
	wt(X).

ww(X) :-
	(	arg(1,X,Y),
		atom(Y),
		sub_atom(Y,0,_,_,'var:'),
		write('@forSome '),
		wt(Y),
		write('. '),
		fail
	;	arg(2,X,Y),
		atom(Y),
		sub_atom(Y,0,_,_,'var:'),
		write('@forSome '),
		wt(Y),
		write('. '), !
	;	true
	).

ws:-
	nl,
	tabs(A),
	write(A).


% --------
% builtins
% --------

'e:bayesian'([fpath(U,'log:conclusion'),V],[A,B,C],_) :-
	level(V),
	scope(V),
	brake,
	bayesian(A,B,C),
	feed(U,_).

'e:binaryEntropy'(A,B,_) :-
	(	A =:= 0
	->	B is 0
	;	(	A =:= 1
		->	B is 0
		;	B is -(A*log(A)+(1-A)*log(1-A))/log(2)
		)
	).

'e:findall'([fpath(U,'log:conclusion'),V],[X,lf(Y),L],_) :-
	level(V),
	scope(V),
	brake,
	findall(X,Y,L),
	feed(U,_).

'e:tuple'(X,Y,_) :-
	(	tuple(X,Y)
	->	true
	;	findall(Z,tuple(Z,_),L),
		length(L,N),
		number_atom(N,A),
		atom_concat('var:e',A,X),
		assert(tuple(X,Y))
	).

'e:length'(A,B,_) :-
	nonvar(A),
	length(A,B).

'e:prune'([A,B,C],D,_) :-
	prune(A,B,C,D).

'e:reverse'(A,B,_) :-
	reverse(A,B).

'e:sort'(A,B,_) :-
	nonvar(A),
	sort(A,B).

'e:max'(A,B,_) :-
	nonvar(A),
	max_list(A,B).

'e:min'(A,B,_) :-
	nonvar(A),
	min_list(A,B).

'e:ruleOfThree'([A,B,C],D,_) :-
	(	C =:= 0
	->	D is B
	;	D is A*B/C
	).

'e:instrument'(_,_,_).

'e:true'(_,_,_).

'e:trace'(_,X,_) :-
	wt(X), nl.

'fn:resolve-uri'([A,B],C,_) :-
	sub_atom(A,1,_,1,U),
	sub_atom(B,1,_,1,V),
	url_info(V,I),
	url_info_relative(U,I,J),
	url_info(W,J),
	atom_codes(X,W),
	atom_concat('"',X,Y),
	atom_concat(Y,'"',C).

'fn:substring-after'([A,B],C,_) :-
	sub_atom(A,1,_,1,U),
	sub_atom(B,1,_,1,V),
	sub_atom(U,_,_,W,V),
	sub_atom(U,_,W,Z,X),
	Z = 0,
	atom_concat('"',X,Y),
	atom_concat(Y,'"',C).

'fn:substring-before'([A,B],C,_) :-
	sub_atom(A,1,_,1,U),
	sub_atom(B,1,_,1,V),
	sub_atom(U,W,_,_,V),
	sub_atom(U,0,W,_,X),
	atom_concat('"',X,Y),
	atom_concat(Y,'"',C).

'list:first'([A|B],A,_) :-
	(	flag('e:bchain')
	->	ground(B)
	;	true
	).

'list:rest'([A|B],B,_) :-
	(	flag('e:bchain')
	->	ground(B)
	;	true
	).

'list:last'(A,B,_) :-
	nonvar(A),
	last(A,B).

'list:in'(A,B,_) :-
	nonvar(B),
	member(A,B).

'list:member'(A,B,_) :-
	nonvar(A),
	member(B,A).

'list:append'([A,B],C,_) :-
	append(A,B,C).

'log:conjunction'(X,Y,_) :-
	when(nonvar(X),
		(	conjunction(X,Y)
		)
	).

'log:equalTo'(X,Y,_) :-
	when(ground([X,Y]),
		(	X == Y
		)
	).

'log:includes'(X,Y,_) :-
	when((nonvar(X),nonvar(Y)),
		(	includes(X,Y)
		)
	).

'log:notEqualTo'(X,Y,_) :-
	when(ground([X,Y]),
		(	X \== Y
		)
	).

'log:notIncludes'(X,Y,_) :-
	when((nonvar(X),nonvar(Y)),
		(	\+'log:includes'(X,Y,_)
		)
	).

'log:semantics'(X,Y,_) :-
	quri(X,Q),
	fact('log:semantics'(Q,Y,_)), !.
'log:semantics'(X,Y,_) :-
	(	assert(nsp('')),
		pfx(X1,X2),
		retract(nsp(U1)),
		atom_concat(U1,'@prefix%2B',V1),
		atom_concat(V1,X1,V2),
		atom_concat(V2,'%2B',V3),
		atom_concat(V3,X2,V4),
		atom_concat(V4,'.%2B',V5),
		assert(nsp(V5)),
		fail
	;	true
	),
	retract(nsp(W)),
	base(B),
	quri(X,Q),
	sub_atom(Q,1,_,1,U),
	atom_concat(B,'.euler+-pterm+',C1),
	atom_concat(C1,B,C2),
	atom_concat(C2,'.context%2B',C3),
	atom_concat(C3,W,C4),
	atom_concat(C4,'+',C5),
	atom_concat(C5,U,V),
	web(V,C),
	read_from_chars(C,Y),
	assert(fact('log:semantics'(Q,Y,_))).

'log:uri'(X,Y,_) :-
	quri(X,Q), !,
	sub_atom(Q,1,_,1,U),
	atom_concat('"',U,C),
	atom_concat(C,'"',Y).
'log:uri'(X,Y,_) :-
	when(ground(Y),
		(	sub_atom(Y,1,_,1,U),
			atom_concat('<',U,C),
			atom_concat(C,'>',X)
		)
	).

'math:absoluteValue'(X,Z,_) :-
	number(X),
	Z is abs(X).

'math:cos'(X,Z,_) :-
	(	number(X),
		Z is cos(X)
	;	number(Z),
		X is acos(Z)
	).

'math:degrees'(X,Z,_) :-
	(	number(X),
		Z is X * 180 / pi
	;	number(Z),
		X is Z * pi / 180
	).

'math:difference'([X,Y],Z,_) :-
	number(X),
	number(Y),
	Z is X - Y.

'math:exponentiation'([X,Y],Z,_) :-
	number(X),
	(	number(Y),
		Z is X ** Y
	;	number(Z),
		Y is log(Z)/log(X)
	).

'math:equalTo'(X,Y,_) :-
	number(X),
	number(Y),
	X =:= Y.

'math:greaterThan'(X,Y,_) :-
	number(X),
	number(Y),
	X > Y.

'math:integerQuotient'([X,Y],Z,_) :-
	integer(X),
	integer(Y),
	Z is X // Y.

'math:lessThan'(X,Y,_) :-
	number(X),
	number(Y),
	X < Y.

'math:memberCount'(X,Y,_) :-
	length(X,Y).

'math:negation'(X,Z,_) :-
	(	number(X),
		Z is -X
	;	number(Z),
		X is -Z
	).

'math:notEqualTo'(X,Y,_) :-
	number(X),
	number(Y),
	X =\= Y.

'math:notGreaterThan'(X,Y,_) :-
	number(X),
	number(Y),
	X =< Y.

'math:notLessThan'(X,Y,_) :-
	number(X),
	number(Y),
	X >= Y.

'math:product'(X,Z,_) :-
	product(X,Z).

'math:quotient'([X,Y],Z,_) :-
	number(X),
	number(Y),
	Z is X / Y.

'math:remainder'([X,Y],Z,_) :-
	integer(X),
	integer(Y),
	Z is X mod Y.

'math:rounded'(X,Z,_) :-
	number(X),
	Z is round(X).

'math:sin'(X,Z,_) :-
	(	number(X),
		Z is sin(X)
	;	number(Z),
		X is asin(Z)
	).

'math:sum'(X,Z,_) :-
	sum(X,Z).

'math:tan'(X,Z,_) :-
	(	number(X),
		Z is tan(X)
	;	number(Z),
		X is atan(Z)
	).

'rdf:first'([X|Y],X,_) :-
	(	flag('e:bchain')
	->	ground(Y)
	;	'rdf:type'([X|Y],'rdf:List',_)
	).

'rdf:rest'([X|Y],Y,_) :-
	(	flag('e:bchain')
	->	ground(Y)
	;	'rdf:type'([X|Y],'rdf:List',_)
	).

'str:concatenation'([X,Y],Z,_) :-
	when(ground([X,Y]),
		(	sub_atom(X,1,_,1,S),
			sub_atom(Y,1,_,1,T),
			atom_concat('"',S,C1),
			atom_concat(C1,T,C2),
			atom_concat(C2,'"',Z)
		)
	).

'str:contains'(X,Y,_) :-
	when(ground([X,Y]),
		(	sub_atom(X,1,_,1,S),
			sub_atom(Y,1,_,1,T),
			atom_codes(S,U),
			atom_codes(T,V),
			regexp(V,U,[])
		)
	).

'str:containsIgnoringCase'(X,Y,_) :-
	when(ground([X,Y]),
		(	sub_atom(X,1,_,1,S),
			sub_atom(Y,1,_,1,T),
			atom_codes(S,U),
			atom_codes(T,V),
			regexp(V,U,[nocase])
		)
	).

'str:endsWith'(X,Y,_) :-
	when(ground([X,Y]),
		(	sub_atom(X,1,_,1,S),
			sub_atom(Y,1,_,1,T),
			sub_atom(S,_,_,0,T)
		)
	).

'str:equalIgnoringCase'(X,Y,_) :-
	when(ground([X,Y]),
		(	sub_atom(X,1,_,1,S),
			sub_atom(Y,1,_,1,T),
			atom_codes(S,U),
			atom_codes(T,V),
			regexp(V,U,[nocase],[W]),
			W == U
		)
	).

'str:greaterThan'(X,Y,_) :-
	when(ground([X,Y]),
		(	X @> Y
		)
	).

'str:lessThan'(X,Y,_) :-
	when(ground([X,Y]),
		(	X @< Y
		)
	).

'str:matches'(X,Y,_) :-
	when(ground([X,Y]),
		(	sub_atom(X,1,_,1,S),
			sub_atom(Y,1,_,1,T),
			atom_codes(S,U),
			atom_codes(T,V),
			regexp(V,U,[],[W]),
			W == U
		)
	).

'str:notEqualIgnoringCase'(X,Y,_) :-
	when(ground([X,Y]),
		(	\+'str:equalIgnoringCase'(X,Y,_)
		)
	).

'str:notGreaterThan'(X,Y,_) :-
	when(ground([X,Y]),
		(	X @=< Y
		)
	).

'str:notLessThan'(X,Y,_) :-
	when(ground([X,Y]),
		(	X @>= Y
		)
	).

'str:notMatches'(X,Y,_) :-
	when(ground([X,Y]),
		(	\+'str:matches'(X,Y,_)
		)
	).

'str:startsWith'(X,Y,_) :-
	when(ground([X,Y]),
		(	sub_atom(X,1,_,1,S),
			sub_atom(Y,1,_,1,T),
			sub_atom(S,0,_,_,T)
		)
	).

'time:year'(tlit(X,_),Y,_) :-
	when(ground(X),
		(	sub_atom(X,1,4,_,Y)
		)
	).

'time:month'(tlit(X,_),Y,_) :-
	when(ground(X),
		(	sub_atom(X,6,2,_,Y)
		)
	).

'time:day'(tlit(X,_),Y,_) :-
	when(ground(X),
		(	sub_atom(X,9,2,_,Y)
		)
	).


% -------
% support
% -------

web(X,Y) :-
	url_info(X,http(H,P,D)),
	fetch_url(http(H,P,D),['Host'(H)],R),
	member(content(Y),R).

args([A,B|_],A,B) :- !.
args([_,_|C],A,B) :-
	args(C,A,B).

base(A) :-
	unix(argv(B)),
	args(B,port,C), !,
	atom_concat('http://localhost:',C,D),
	atom_concat(D,'/',A).
base('http://localhost/').

quri(A,A) :-
	atom(A),
	sub_atom(A,1,_,1,U),
	atom_concat('<',U,C),
	atom_concat(C,'>',A), !.
quri(A,B) :-
	atom(A),
	pfx(A,B), !.
quri(A,B) :-
	atom(A),
	pfx(U,V),
	sub_atom(A,0,X,Y,U),
	sub_atom(V,0,_,1,W),
	sub_atom(A,X,Y,_,Q),
	atom_concat(W,Q,C),
	atom_concat(C,'>',B).

auri('log:semantics'(A,B,C),'log:semantics'(D,B,C)) :-
	quri(A,D), !.
auri('log:uri'(A,B,C),'log:uri'(D,B,C)) :-
	quri(A,D), !.
auri(A,A).

level(A) :-
	number(A), !,
	limit(B),
	(	B < A
	->	retract(limit(B)),
		assert(limit(A))
	;	true
	).
level(1).

indent :-
	retract(tabs(A)),
	atom_concat(A,'\t',B),
	assert(tabs(B)).

dedent :-
	retract(tabs(A)),
	sub_atom(A,_,_,1,B),
	assert(tabs(B)).

instrument(_,_).

profile:-
	setof(D-[M:P|D1],(current_module(M),profile_data(M:P,calls,D),profile_data(M:P,retries,D1)),LP),
	profile(LP).

profile([]).
profile([D-[M:P|R]|SLP]) :-
	format('~a:~w: ~32+~t~d~12+~t~d~12+~n', [M,P,D,R]),
	profile(SLP).

conjunction([lf(X)],lf(X)) :- !.
conjunction([lf(X)|Y],lf(Z)) :-
	conjunction(Y,lf(U)),
	conjoin(X,U,Z), !.

conjoin(','(A,B),C,D) :-
	conjoin(B,C,D),
	includes(lf(D),lf(A)), !.
conjoin(','(A,B),C,','(A,D)) :-
	conjoin(B,C,D), !.
conjoin(A,B,B) :-
	includes(lf(B),lf(A)), !.
conjoin(A,B,','(A,B)).

includes(lf(X),lf(X)).
includes(lf(','(X,Y)),lf(Z)) :-
	(	X=Z
	;	includes(lf(Y),lf(Z))
	).
includes(lf(X),lf(','(Y,Z))) :-
	includes(lf(X),lf(Y)),
	includes(lf(X),lf(Z)).

cmember(A,A) :- !.
cmember(A,','(A,_)) :- !.
cmember(A,','(_,A)) :- !.
cmember(A,','(_,B)) :-
	cmember(A,B).

cappend(','(A,B),C,','(A,D)) :-
	cappend(B,C,D), !.
cappend(A,B,','(A,B)).

clast(','(_,X),I) :- !,
	clast(X,I).
clast(I,I).

subset([],_) :- !.
subset([A|B],C) :-
	memberchk(A,C),
	subset(B,C).

abstract(','(A,B),','(C,D)) :-
	A =.. [U,V,W,_],
	C =.. [U,V,W],
	abstract(B,D), !.
abstract(A,B) :-
	A =.. [U,V,W,_],
	B =.. [U,V,W].

positive(','('e:boolean'(_,'e:T'),_)).

inverse('e:boolean'(A,'e:T',_),'e:boolean'(A,'e:F',_)) :- !.
inverse('e:boolean'(A,'e:F',_),'e:boolean'(A,'e:T',_)).

inversa('e:boolean'(A,'e:T'),'e:boolean'(A,'e:F')) :- !.
inversa('e:boolean'(A,'e:F'),'e:boolean'(A,'e:T')).

prune([],_,_,[]) :- !.
prune([A|B],C,D,[A|E]) :-
	copy_term([C,D],[F,G]),
	pdelete(B,F,G,A,H),
	prune(H,C,D,E).

pdelete([],_,_,_,[]) :- !.
pdelete([A|B],C,D,E,F) :-
	copy_term([C,D],[G,H]),
	A = C,
	E = D, !,
	pdelete(B,G,H,E,F).
pdelete([A|B],C,D,E,[A|F]) :-
	pdelete(B,C,D,E,F).

sum([],0) :- !.
sum([A|B],C) :-
	number(A),
	sum(B,D),
	C is A+D.

product([],1) :- !.
product([A|B],C) :-
	number(A),
	product(B,D),
	(	A =:= 0
	->	C is 0.0
	;	(	D =:= 0
		->	C is 0.0
		;	C is A*D
		)
	).

bnet:-
	(	retractall(bref(_,_)),
		'e:depends'([A|_],[B|_],_),
		member([C|_],B),
		\+bref(C,A),
		\+assert(bref(C,A))
	;	true
	).

brel([A|_],[B|_]) :-
	bref(A,B), !.
brel(A,[B|_]) :-
	bref(C,B),
	brel(A,[C|_]).

bget(A,B,1) :-
	memberchk(A,B), !.
bget([A,'e:T'],B,0) :-
	memberchk([A,'e:F'],B), !.
bget([A,'e:F'],B,C) :-
	(	memberchk([A,'e:T'],B), !,
		C is 0
	;	!, bget([A,'e:T'],B,D),
		C is 1-D
	).
bget(A,B,C) :-
	sort(B,D),
	write_to_chars([A|D],E),
	atom_codes(F,E),
	(	bb_get(F,C)
	->	true
	;	(	member(X,D),
			brel(A,X),
			member(G,D),
			findall(Y,(member(Y,[A|D]), brel(G,Y)),[]),
			delete(D,G,H), !,
			bget(G,[A|H],U),
			bget(A,H,V),
			bget(G,H,W),
			'e:ruleOfThree'([U,V,W],C,_)
		;	findall(Z,('e:depends'(A,[O,P],_), bayesian(O,D,Q), Z is P*Q),L),
			sum(L,C)
		),
		bb_put(F,C)
	).

bayesian([],_,1) :- !.
bayesian([A|B],C,D) :-
	bget(A,C,E),
	bayesian(B,[A|C],F),
	D is E*F.


% ------------
% JSON support
% ------------

object([])--> wsp, "{", wsp, "}", wsp.
object(X)--> wsp, "{", wsp, members(X), wsp, "}", wsp.
 
members([X|Y])--> pair(X), wsp, ",", wsp, !, members(Y).
members([X])--> pair(X), !.

pair(pair(X,Y))--> string(X), wsp, ":", wsp, value(Y).

array([])--> "[", wsp, "]".
array(X)--> "[", wsp, elements(X), wsp, "]".

elements([X|Y])--> value(X), wsp, ",", wsp, elements(Y).
elements([X])--> value(X).

value(X)--> string(X), !.
value(X)--> number(X), !.
value(X)--> object(X), !.
value(X)--> array(X), !.
value(true)--> "true", !.
value(false)--> "false", !.
value(null)--> "null".

string(X)--> "\"", chars(Y), "\"", {atom_codes(X,Y)}.

chars([X|Y])--> char(X), !, chars(Y).
chars([])--> [].

char(0'")--> "\\\"".
char(0'\\)--> "\\\\".
char(0'/)--> "\\/".
char(0'\b)--> "\\b".
char(0'\f)--> "\\f".
char(0'\n)--> "\\n".
char(0'\r)--> "\\r".
char(0'\t)--> "\\t".
char(X)--> [X], {X >= 32, X \== 0'"}.

number(X)--> int(A), frac(B), exp(C), !, {append(A,B,D), append(D,C,E), number_codes(X,E)}.
number(X)--> int(A), frac(B), !, {append(A,B,C), number_codes(X,C)}.
number(X)--> int(A), exp(B), !, {append(A,B,C), number_codes(X,C)}.
number(X)--> int(A), {number_codes(X,A)}.

int([0'-,X|Y])--> "-", digit(X), digits(Y).
int([0'-,X])--> "-", digit(X), !.
int([X|Y])--> digit(X), digits(Y), !.
int([X])--> digit(X), !.

frac([0'.|X])--> ".", digits(X).

exp(X) --> e(A), digits(B), {append(A,B,X)}.

digits([X|Y])--> digit(X), !, digits(Y).
digits([])--> [].

e("e+")--> "e+".
e("e+")--> "E+".
e("e-")--> "e-".
e("e-")--> "E-".
e("e+")--> "e".
e("e+")--> "E".

digit(X)--> [X], {0'0 =< X, X =< 0'9}.

wsp--> wschar, !, wsp.
wsp--> "//", !, comment, wsp.
wsp--> [].

wschar--> " ".
wschar--> "\t".
wschar--> "\n".
wschar--> "\r".

comment--> "\n", !.
comment--> "\r", !.
comment--> [_], !, comment.

pp([]) :- !.
pp([[A,B]|C]) :-
	assert(pfx(A,B)),
	pp(C).

pt([]) :- !.
pt([A|B]) :-
	triple(A,C),
	write_to_chars(C,D),
	append(D,".",E),
	read_from_chars(E,F),
	assert(F),
	pt(B).

pq([]) :- !.
pq([A|B]) :-
	triple(A,C),
	write_to_chars('\'log:implies\''(lf(C),lf(case(C)),_),D),
	append(D,".",E),
	read_from_chars(E,F),
	assert(F),
	pq(B).

triple([S,a,O],'\'rdf:type\''(A,B,_)) :- !,
	res(S,A),
	res(O,B).
triple([S,'=>',O],'\'log:implies\''(lf(A),lf(B),_)) :- !,
	triples(S,A),
	triples(O,B).
triple([S,'e:findall',[V,W,R]],'\'e:findall\''(A,[B,lf(C),D],_)) :- !,
	res(S,A),
	res(V,B),
	triples(W,C),
	res(R,D).
triple([S,P,O],X) :-
	res(S,A),
	res(P,B),
	res(O,C),
	X =.. [B,A,C,_].

triples([A],B) :-
	triple(A,B), !.
triples([A|B],(C,D)) :-
	triple(A,C), !,
	triples(B,D).

res([],[]) :- !.
res([A|B],[C|D]) :-
	res(A,C),
	res(B,D), !.
res(A,A) :-
	number(A), !.
res(A,B) :-
	sub_atom(A,0,_,_,'?'), !,
	atom_length(A,I),
	J is I-1,
	sub_atom(A,1,J,_,C),
	atom_concat('_',C,B).
res(A,B) :-
	atom_concat('\'',A,C),
	atom_concat(C,'\'',B).
