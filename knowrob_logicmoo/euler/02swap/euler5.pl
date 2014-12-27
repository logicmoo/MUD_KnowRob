% -----------------------------------
% Euler proof mechanism -- Jos De Roo
% -----------------------------------

id('$Id: euler5.pl 2537 2008-12-30 15:03:43Z josd $').

:- use_module(library('http/http_open')).

:- dynamic(flag/1).
:- dynamic(counter/1).
:- dynamic(tabs/1).
:- dynamic(scope/1).
:- dynamic(limit/1).
:- dynamic(step/2).
:- dynamic(brake/0).
:- dynamic(delta/2).
:- dynamic(model/7).
:- dynamic(model/5).
:- dynamic(model/3).
:- dynamic(case/1).
:- dynamic(tuple/2).
:- dynamic(fact/1).
:- dynamic(pfx/2).
:- dynamic(rule/2).
:- dynamic(goal/2).

:- set_prolog_flag(float_format,'%.15e').

main:-
	write('#Consulting '), consult(user), nl,
	catch((flag(profile), profile(go); go),E,(write('#'), write(E), nl, fail)).

go:-
	id(X),
	sub_atom(X,1,_,1,Y),
	write('#Processed by '), write(Y), nl, nl,
	(	pfx(X1,X2),
		write('@prefix '), write(X1), write(' '), write(X2), write('.'), nl,
		fail
	;	nl
	),
	(	flag('e:bchain')
	->	write('_: owl:sameAs '), feed(F,_), wt(F), write('.'), nl, nl,
		retractall(counter(_)),
		assert(counter(0)),
		why
	;	retractall(tabs(_)),
		retractall(scope(_)),
		retractall(limit(_)),
		retractall(brake),
		assert(tabs('')),
		assert(scope(0)),
		assert(limit(1)),
		step,
		step(0),
		w3
	),
	statistics(runtime,[_,Z]),
	write('#ENDS '), write(Z), write(' msec.'), nl, nl.


% -------------
% proof engines
% -------------

% why/0 for backward chaining
why:-
	goal(C,P),
	cappend(P,C,G),
	(	why(pc,G),
		why(p,P),
		fail
	;	true
	),
	wp,
	fail.
why.

% why/2 for backward chaining
why(W,G) :-
	step(G,A,[],[],T,1.0,D),
	abstract(G,H),
	skeleton(A,N),
	(	model(W,H,B,N,T,E,J)
	->	Z is abs(D-E),
		Z > 1e-15,
		\+delta([J,_],Z),
		count(I),
		assert(delta([J,I],Z)),
		(	D > E
		->	wp(B,E,J),
			retract(model(W,H,B,N,T,E,J)),
			assert(model(W,H,A,N,T,D,I))
		;	wp(A,D,I)
		)
	;	count(I),
		assert(model(W,H,A,N,T,D,I))
	),
	fail.
why(_,_).

% step/7 using goal,proof,pending,state,state,value,value
step(','(A,B),conjunction(C,D),U,V,W,I,J) :- !,
	step(A,C,U,V,X,I,K),
	step(B,D,U,X,W,K,J).
step('e:true'(_,A,_),fact('e:true'(_,A,_)),_,V,V,I,J) :- !,
	J is I*A.
step(A,fact(A),_,V,V,I,I) :-
	A.
step(A,Y,U,V,X,I,J) :-
	rule(A,C),
	step(A,U,Z),
	step(C,B,[A|U],V,W,I,K),
	step(A,Z,B,Y,V,W,X,I,K,J).

% step/3 using goal,state,goal
step(A,U,_) :-
	inverse(A,B),
	memberchk(B,U), !,			% euler path fail case
	fail.
step(A,U,_) :-
	memberchk(A,U), !,			% euler path fail case
	fail.
step(A,_,Z) :-
	abstract(A,Z).

% step/10 using goal,goal,proof,proof,state,state,state,value,value,value
step(_,Z,_,_,V,_,_,_,_,_) :-
	inversa(Z,U),
	memberchk(U,V), !,			% euler path fail case
	fail.
step(A,Z,_,lemma(A),V,_,V,I,_,I) :-
	memberchk(Z,V), !.			% euler path lemma case
step(A,Z,B,rule(A,B),_,W,[Z|W],_,K,K).

% step/0 for forward chaining
step:-
	'log:implies'(lf(X),lf(Y),_),
	X,
	\+Y,					% euler path lemma case
	assert(Y),
	assert(step(Y,X)),
	retract(brake),
	fail.
step:-
	brake, !.
step:-
	assert(brake),
	step.

% step/1 for recursive scoping during forward chaining
step(A) :-
	limit(B),
	A < B, !,
	C is A + 1,
	assert(scope(C)),
	step,
	step(C).
step(_).


% ------------
% proof output
% ------------

wp :-
	model(W,H,_,N,_,_,_),
	findall([D,I],(model(W,H,A,N,_,D,I), wp(A,D,I)),L),
	L \= [],
	findall(U,(member([U,_],L)),J),
	findall(V,(member([_,V],L)),K),
	sum(J,B),
	retractall(model(W,H,_,N,_,_,_)),
	w5(W,H,N,B,K),
	fail.
wp :-
	model(pc,G,N,A,U),
	model(p,H,M,B,V),
	cappend(H,I,G),
	(	append(M,[_],N)
	;	N = []
	),
	B > 0,
	C is A/B,
	(	model(','(I,H),D,_),
		abs(C-D) < 1e-15
	->	true
	;	assert(model(','(I,H),C,[U,V]))
	),
	fail.
wp :-
	model(','(I,H),D,[U,V]),
	inversa(I,J),
	E is 1.0-D,
	(	model(','(J,H),F,[X,Y])
	->	Z is abs(E-F),
		Z > 1e-15,
		assert(delta([[X,Y],[U,V]],Z)),
		(	model(','(J,H),W,_),
			abs(E-W) < 1e-15
		->	true
		;	assert(model(','(J,H),E,[U,V]))
		)
	;	assert(model(','(J,H),E,[U,V]))
	),
	fail.
wp :-
	delta(A,B),
	wt('e:delta'(A,B,_)), write('.'), nl, nl,
	fail.
wp :-
	model(','(I,H),D,R),
	cappend(H,'e:true'('_:',D,_),F),
	(	flag('e:pe')
	->	write('[ e:bayesRule '), wt(R), write('; r:gives {'), nl,
		write('\t'), wt(lf(F)), write(' => '), wt(lf(I)), write('}].'), nl, nl
	;	(	flag('e:be')
		->	wt(lf(F)), write(' => '), wt(lf(I)), write('.'), nl
		;	(	D =:= 1
			->	wt(I), write('.'), nl
			;	true
			)
		)
	),
	fail.
wp :-
	(	flag('e:nope')
	->	nl
	;	true
	),
	retractall(delta(_,_)),
	retractall(model(_,_,_,_,_)),
	retractall(model(_,_,_)).

wp(A,D,I) :-
	(	flag('e:pe')
	->	write('[ e:proofID '), write(I), write('; e:proof {'), nl,
		wp(A,'\t'),
		write('\t}; e:possibility '), write(D), write('].'), nl, nl
	;	true
	).

wp(rule(A,fact('e:true'(_,B,_))),C) :-
	B =:= 1, !,
	wp(fact(A),C).
wp(rule(A,B),C) :- !,
	write(C),
	write('{'),
	wt(A),
	write('} e:because {'), nl,
	concat_atom([C,'\t'],D),
	wp(B,D),
	write(D),
	write('}; e:source '),
	arg(3,A,S),
	write(S), write('.'), nl.
wp(conjunction(A,B),C) :- !,
	wp(A,C),
	wp(B,C).
wp(fact(atom(_)),_) :- !.
wp(fact(_=..[_,_,_,_]),_) :- !.
wp(A,B) :-
	write(B),
	wt(A), write('.'), nl.

w5(pc,H,N,B,K) :-
	scount(N,Q),
	model(pc,H,M,C,P),
	scount(M,R),
	(	Q < R, !,
		retractall(model(pc,H,_,_,_)),
		assert(model(pc,H,N,B,K))
	;	Q > R, !
	;	Z is abs(B-C),
		Z > 1e-15,
		\+delta([P,K],Z),
		assert(delta([P,K],Z)),
		assert(model(pc,H,N,B,K))
	;	true
	).
w5(W,H,N,B,K) :-
	assert(model(W,H,N,B,K)).

w3:-
	flag('e:nope'), !,
	(	step(case(Y),_),
		wt(Y), write('.'), nl,
		fail
	;	nl
	).
w3:-
	write('[ a r:Proof, r:Conjunction;'),
	indent,
	ws,
	(	step(case(Y),X),
		write('r:component '),
		wi(X,Y),
		write(';'),
		ws,
		fail
	;	true
	),
	write('r:gives {'),
	indent,
	(	step(case(Y),_),
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
	step(Y,X), !,
	ws,
	wi(X,Y).
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
wt('rdf:type') :- !,
	write('a').
wt('owl:sameAs') :- !,
	write('=').
wt('log:implies') :- !,
	write('=>').
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
	concat_atom(['var:',Y],X), !,
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

'e:binaryEntropy'(A,B,_) :-
	when(nonvar(A),
		(	A =:= 0
		->	B is 0
		;	(	A =:= 1
			->	B is 0
			;	B is -(A*log(A)+(1-A)*log(1-A))/log(2)
			)
		)
	).

'e:findall'([fpath(U,'log:conclusion'),V],[X,lf(Y),L],_) :-
	(	flag('e:bchain')
	->	findall(X,step(Y,_,[],[],_,1.0,_),L)
	;	level(V),
		scope(V),
		brake,
		findall(X,Y,L),
		feed(U,_)
	).

'e:distinct'(A,B) :-
	when(nonvar(A),
		list_to_set(A,B)
	).

'e:instrument'(_,_,_).

'e:length'(A,B,_) :-
	when(nonvar(A),
		length(A,B)
	).

'e:max'(A,B,_) :-
	when(nonvar(A),
		max(A,B)
	).

'e:min'(A,B,_) :-
	when(nonvar(A),
		min(A,B)
	).

'e:optional'(_,lf(A),_) :-
	(	flag('e:bchain')
	->	step(A,_,[],[],_,1.0,_)
	;	(	A
		->	true
		;	true
		)
	).

'e:pair'(A,[B,C],_) :-
	(	'e:sublist'(A,[B,C],_)
	;	'e:sublist'(A,[C,B],_)
	).

'e:prune'([A,B,C],D,_) :-
	prune(A,B,C,D).

'e:reverse'(A,B,_) :-
	reverse(A,B).

'e:sort'(A,B,_) :-
	when(nonvar(A),
		sort(A,B)
	).

'e:sublist'(A,B,_) :-
	when(nonvar(A),
		(	append(C,_,A),
			append(_,B,C)
		)
	).

'e:tokenize'([A,B],C,_) :-
	when(ground([A,B]),
		(	unquote(A,D),
			unquote(B,E),
			atom_codes(D,F),
			atom_codes(E,G),
			tokenize(F,G,C), !
		)
	).

'e:trace'(_,X,_) :-
	write('#TRACE '), wt(X), nl.

'e:true'(_,A,_) :-
	when(nonvar(A),
		A =:= 1.0
	).

'e:tuple'(X,Y,_) :-
	(	tuple(X,Y)
	->	true
	;	findall(Z,tuple(Z,_),L),
		length(L,N),
		atom_number(A,N),
		concat_atom(['var:e',A],X),
		assert(tuple(X,Y))
	).

'fn:resolve-uri'([A,B],C,_) :-
	when(ground([A,B]),
		(	unquote(A,U),
			unquote(B,V),
			global_url(U,V,W),
			concat_atom(['"',W,'"'],C)
		)
	).

'fn:substring-after'([A,B],C,_) :-
	when(ground([A,B]),
		(	unquote(A,U),
			unquote(B,V),
			sub_atom(U,_,_,W,V),
			sub_atom(U,_,W,Z,X),
			Z = 0,
			concat_atom(['"',X,'"'],C)
		)
	).

'fn:substring-before'([A,B],C,_) :-
	when(ground([A,B]),
		(	unquote(A,U),
			unquote(B,V),
			sub_atom(U,W,_,_,V),
			sub_atom(U,0,W,_,X),
			concat_atom(['"',X,'"'],C)
		)
	).

'list:append'([A,B],C,_) :-
	when((nonvar(A), nonvar(B)),
		append(A,B,C)
	).

'list:first'([A|B],A,_) :-
	(	flag('e:bchain')
	->	ground(B)
	;	true
	).

'list:in'(A,B,_) :-
	when(nonvar(B),
		member(A,B)
	).

'list:last'(A,B,_) :-
	when(nonvar(A),
		last(A,B)
	).

'list:member'(A,B,_) :-
	when(nonvar(A),
		member(B,A)
	).

'list:rest'([_|B],B,_) :-
	(	flag('e:bchain')
	->	ground(B)
	;	true
	).

'log:conjunction'(X,Y,_) :-
	when(nonvar(X),
		conjunction(X,Y)
	).

'log:equalTo'(X,Y,_) :-
	X = Y.

'log:includes'(X,Y,_) :-
	when((nonvar(X), nonvar(Y)),
		includes(X,Y)
	).

'log:notEqualTo'(X,Y,_) :-
	X \= Y.

'log:notIncludes'(X,Y,_) :-
	when((nonvar(X), nonvar(Y)),
		\+'log:includes'(X,Y,_)
	).

'log:semantics'(X,Y,_) :-
	when(nonvar(X),
		(	quri(X,Q),
			(	fact('log:semantics'(Q,Y,_)), !
			;	findall(U,(pfx(X1,X2), concat_atom(['@prefix ',X1,' ',X2,'. '],U)),L),
				concat_atom(L,W),
				base(B),
				sub_atom(Q,1,_,1,Z),
				concat_atom(['.context ',W],C1),
				www_form_encode(C1,C2),
				concat_atom(['.euler -pterm ',B,C2,' ',Z],C3),
				www_form_encode(C3,C4),
				concat_atom([B,C4],V),
				http_open(V,S,[]),
				catch(read(S,Y),_,Y=fail),
				close(S),
				assert(fact('log:semantics'(Q,Y,_)))
			)
		)
	).

'log:uri'(X,Y,_) :-
	when((nonvar(X); ground(Y)),
		(	quri(X,Q), !,
			sub_atom(Q,1,_,1,U),
			concat_atom(['"',U,'"'],Y)
		;	unquote(Y,U),
			concat_atom(['<',U,'>'],X)
		)
	).

'math:absoluteValue'(X,Z,_) :-
	when(ground(X),
		(	getnumber(X,U),
			Z is abs(U)
		)
	).

'math:cos'(X,Z,_) :-
	when((ground(X); ground(Z)),
		(	getnumber(X,U),
			Z is cos(U), !
		;	getnumber(Z,W),
			X is acos(W)
		)
	).

'math:degrees'(X,Z,_) :-
	when((ground(X); ground(Z)),
		(	getnumber(X,U),
			Z is U * 180 / pi,!
		;	getnumber(Z,W),
			X is W * pi / 180
		)
	).

'math:difference'([X,Y],Z,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			Z is U - V
		)
	).

'math:equalTo'(X,Y,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			U =:= V
		)
	).

'math:exponentiation'([X,Y],Z,_) :-
	when((ground([X,Y]); ground([X,Z])),
		(	getnumber(X,U),
			(	getnumber(Y,V),
				Z is U ** V
			;	getnumber(Z,W),
				Y is log(W)/log(U)
			)
		)
	).

'math:greaterThan'(X,Y,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			U > V
		)
	).

'math:integerQuotient'([X,Y],Z,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			Z is U // V
		)
	).

'math:lessThan'(X,Y,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			U < V
		)
	).

'math:memberCount'(X,Y,_) :-
	when(nonvar(X),
		length(X,Y)
	).

'math:negation'(X,Z,_) :-
	when((ground(X); ground(Z)),
		(	getnumber(X,U),
			Z is -U
		;	getnumber(Z,W),
			X is -W
		)
	).

'math:notEqualTo'(X,Y,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			U =\= V
		)
	).

'math:notGreaterThan'(X,Y,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			U =< V
		)
	).

'math:notLessThan'(X,Y,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			U >= V
		)
	).

'math:product'(X,Z,_) :-
	when(ground(X),
		product(X,Z)
	).

'math:quotient'([X,Y],Z,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			Z is U / V
		)
	).

'math:remainder'([X,Y],Z,_) :-
	when(ground([X,Y]),
		(	getnumber(X,U),
			getnumber(Y,V),
			Z is U mod V
		)
	).

'math:rounded'(X,Z,_) :-
	when(ground(X),
		(	getnumber(X,U),
			Z is round(U)
		)
	).

'math:sin'(X,Z,_) :-
	when((ground(X); ground(Z)),
		(	getnumber(X,U),
			Z is sin(U), !
		;	getnumber(Z,W),
			X is asin(W)
		)
	).

'math:sum'(X,Z,_) :-
	when(ground(X),
		sum(X,Z)
	).

'math:tan'(X,Z,_) :-
	when((ground(X); ground(Z)),
		(	getnumber(X,U),
			Z is tan(U), !
		;	getnumber(Z,W),
			X is atan(W)
		)
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
		(	unquote(X,S),
			unquote(Y,T),
			concat_atom(['"',S,T,'"'],Z)
		)
	).

'str:contains'(X,Y,_) :-
	when(ground([X,Y]),
		(	unquote(X,S),
			unquote(Y,T),
			sub_atom(S,_,_,_,T)
		)
	).

'str:containsIgnoringCase'(X,Y,_) :-
	when(ground([X,Y]),
		(	unquote(X,S),
			unquote(Y,T),
			downcase_atom(S,U),
			downcase_atom(T,V),
			sub_atom(U,_,_,_,V)
		)
	).

'str:endsWith'(X,Y,_) :-
	when(ground([X,Y]),
		(	unquote(X,S),
			unquote(Y,T),
			sub_atom(S,_,_,0,T)
		)
	).

'str:equalIgnoringCase'(X,Y,_) :-
	when(ground([X,Y]),
		(	downcase_atom(X,U),
			downcase_atom(Y,V),
			U == V
		)
	).

'str:greaterThan'(X,Y,_) :-
	when(ground([X,Y]),
		X @> Y
	).

'str:lessThan'(X,Y,_) :-
	when(ground([X,Y]),
		X @< Y
	).

'str:matches'(X,Y,_) :-
	'e:tokenize'([Y,X],_,_).

'str:notEqualIgnoringCase'(X,Y,_) :-
	when(ground([X,Y]),
		\+'str:equalIgnoringCase'(X,Y,_)
	).

'str:notGreaterThan'(X,Y,_) :-
	when(ground([X,Y]),
		X @=< Y
	).

'str:notLessThan'(X,Y,_) :-
	when(ground([X,Y]),
		X @>= Y
	).

'str:notMatches'(X,Y,_) :-
	when(ground([X,Y]),
		\+'str:matches'(X,Y,_)
	).

'str:startsWith'(X,Y,_) :-
	when(ground([X,Y]),
		(	unquote(X,S),
			unquote(Y,T),
			sub_atom(S,0,_,_,T)
		)
	).

'time:day'(tlit(X,_),Y,_) :-
	when(ground(X),
		sub_atom(X,9,2,_,Y)
	).

'time:month'(tlit(X,_),Y,_) :-
	when(ground(X),
		sub_atom(X,6,2,_,Y)
	).

'time:year'(tlit(X,_),Y,_) :-
	when(ground(X),
		sub_atom(X,1,4,_,Y)
	).


% -------
% support
% -------

base(A) :-
	flag(port(B)), !,
	concat_atom(['http://localhost:',B,'/'],A).
base('http://localhost/').

quri(A,A) :-
	atom(A),
	sub_atom(A,1,_,1,U),
	concat_atom(['<',U,'>'],A), !.
quri(A,B) :-
	atom(A),
	pfx(A,B), !.
quri(A,B) :-
	atom(A),
	pfx(U,V),
	sub_atom(A,0,X,Y,U),
	sub_atom(V,0,_,1,W),
	sub_atom(A,X,Y,_,Q),
	concat_atom([W,Q,'>'],B).

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
	concat_atom([A,'\t'],B),
	assert(tabs(B)).

dedent :-
	retract(tabs(A)),
	sub_atom(A,_,_,1,B),
	assert(tabs(B)).

count(C) :-
	retract(counter(A)),
	C is A+1,
	assert(counter(C)).

instrument(_,_).

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

cappend(','(A,B),C,','(A,D)) :-
	cappend(B,C,D), !.
cappend(A,B,','(A,B)).

clast(','(_,X),I) :- !,
	clast(X,I).
clast(I,I).

abstract(A,A) :-
	var(A), !.
abstract(','(A,B),','(C,D)) :- !,
	abstract(A,C),
	abstract(B,D).
abstract(lf(A),lf(B)) :- !,
	abstract(A,B).
abstract(A,B) :-
	A =.. [U,V,W,_], !,
	abstract(V,X),
	abstract(W,Y),
	B =.. [U,X,Y].
abstract(A,A).

skeleton(conjunction(A,B),C) :- !,
	skeleton(A,D),
	skeleton(B,E),
	append(D,E,C).
skeleton(rule(A,B),[]) :-
	skeleton(A,C),
	skeleton(B,[]),
	C = [], !.
skeleton(rule(A,B),[[C|D]]) :- !,
	skeleton(A,C),
	skeleton(B,D).
skeleton(lemma(A),[[B]]) :- !,
	skeleton(A,B).
skeleton(fact(_),[]) :- !.
skeleton('e:boolean'(A,_,_),A) :- !.
skeleton(_,[]).

scount(A,B) :-
	flatten(A,C),
	list_to_set(C,D),
	length(D,B).

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

transpose([[]|_],[]) :- !.
transpose(A,[B|C]) :-
	decap(A,B,D),
	transpose(D,C).

decap([],[],[]) :- !.
decap([[A|B]|C],[A|D],[B|E]) :-
	decap(C,D,E).

sum([],0) :- !.
sum([A|B],C) :-
	getnumber(A,X),
	sum(B,D),
	C is X+D.

product([],1) :- !.
product([A|B],C) :-
	getnumber(A,X),
	product(B,D),
	(	X =:= 0
	->	C is 0.0
	;	(	D =:= 0
		->	C is 0.0
		;	C is X*D
		)
	).

max([A|B],C) :-
	max(B,A,C).

max([],A,A).
max([A|B],C,D) :-
	(	A @> C 
	->	max(B,A,D)
	;	max(B,C,D)
	).

min([A|B],C) :-
	min(B,A,C).

min([],A,A).
min([A|B],C,D) :-
	(	A @< C 
	->	min(B,A,D)
	;	min(B,C,D)
	).

getnumber(A,A) :-
	ground(A),
	number(A), !.
getnumber(tlit(A,'xsd:dateTime'),B) :- !,
	ground(A),
	datetime(A,B).
getnumber(tlit(A,'xsd:duration'),B) :- !,
	ground(A),
	duration(A,B).
getnumber(plit(A,_),B) :- !,
	ground(A),
	unquote(A,C),
	atom_number(C,B).
getnumber(A,B) :-
	ground(A),
	unquote(A,C),
	atom_number(C,B).

unquote(A,B) :-
	sub_atom(A,0,1,_,'"'),
	sub_atom(A,_,1,0,'"'),
	sub_atom(A,1,_,1,B).	

datetime(A,B) :-
	unquote(A,C),
	atom_codes(C,D),
	datetime(B,D,[]).

duration(A,B) :-
	unquote(A,C),
	atom_codes(C,D),
	duration(B,D,[]).

datetime(A) --> int(B), "-", int(C), "-", int(D), "T", int(E), ":", int(F), ":", decimal(G), timezone(H),
	{I is -H, date_time_stamp(date(B,C,D,E,F,G,I,-,-),A)}.

timezone(A) --> int(B), !, ":", int(C), {A is B*3600+C*60}.
timezone(0) --> "Z".
timezone(0) --> [].

duration(A) --> "P", years(B), months(C), days(D), dtime(E), {A is B*31556952+C*2629746+D*86400+E}.

dtime(A) --> "T", !, hours(B), minutes(C), seconds(D), {A is B*3600+C*60+D}.
dtime(0) --> [].

years(A) --> int(A), "Y".
years(0) --> [].

months(A) --> int(A), "M".
months(0) --> [].

days(A) --> int(A), "D".
days(0) --> [].

hours(A) --> int(A), "H".
hours(0) --> [].

minutes(A) --> int(A), "M".
minutes(0) --> [].

seconds(A) --> decimal(A), "S".
seconds(0) --> [].

int(A) --> sign(B), digit(C), digits(D), {number_chars(A,[B,C|D])}.

decimal(A) --> sign(B), digit(C), digits(D), fraction(E), {append([B,C|D],E,F), number_chars(A,F)}.

sign(0'+) --> "+".
sign(0'-) --> "-".
sign(0'+) --> [].

fraction([0'.,A|B]) --> ".", !, digit(A), digits(B).
fraction([]) --> [].

digits([A|B]) --> digit(A), digits(B).
digits([]) --> [].

digit(A) --> [A], {code_type(A,digit)}.


% Perl Style Regular Expressions
% original code from http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html

tokenize(RE,Input,Output) :-
	re(Parsed_RE,RE,[]),
	tokenize2(Parsed_RE,Input,Output).
  
tokenize2(_P_RE,[],[]).
tokenize2(P_RE,Input,Output) :-
	rematch1(P_RE,Input,Unmatched,SelStrings),
	names(Tokens,SelStrings),
	tokenize2(P_RE,Unmatched,MoreTokens),
	append(Tokens,MoreTokens,Output).
  
names([],[]).
names([Sym1|MoreSymbols],[Str1|MoreStrings]) :-
	name(Sym1,Str1),
	names(MoreSymbols,MoreStrings).

rematch1(union(RE1,_RE2),S,U,Selected) :- 
	rematch1(RE1,S,U,Selected).
rematch1(union(_RE1,RE2),S,U,Selected) :- 
	rematch1(RE2,S,U,Selected).
rematch1(conc(RE1,RE2),S,U,Selected) :- 
	rematch1(RE1,S,U1,Sel1),
	rematch1(RE2,U1,U,Sel2),
	append(Sel1,Sel2,Selected).
rematch1(star(RE),S,U,Selected) :-
	rematch1(RE,S,U1,Sel1),
	rematch1(star(RE),U1,U,Sel2),
	append(Sel1,Sel2,Selected).
rematch1(star(_RE),S,S,[]).
rematch1(plus(RE),S,U,Selected) :-
	rematch1(RE,S,U1,Sel1),
	rematch1(star(RE),U1,U,Sel2),
	append(Sel1,Sel2,Selected).
rematch1(group(RE),S,U,Selected) :-
	rematch1(RE,S,U,Sel1),
	append(P,U,S),
	append(Sel1,[P],Selected).
rematch1(any,[_C1|U],U,[]).
rematch1(char(C),[C|U],U,[]).
rematch1(eos,[],[],[]).
rematch1(negSet(Set),[C|U],U,[]) :-
	\+charSetMember(C,Set).
rematch1(posSet(Set),[C|U],U,[]) :-
	charSetMember(C,Set).

charSetMember(C,[char(C)|_]).
charSetMember(C,[range(C1,C2)|_]) :-
	C1 =< C,
	C =< C2.
charSetMember(C,[_|T]) :-
	charSetMember(C,T).

re(Z) --> basicRE(W), reTail(W,Z).

reTail(W,Z) --> "|", basicRE(X), reTail(union(W,X),Z).
reTail(W,W) --> [].

basicRE(Z) --> simpleRE(W), basicREtail(W,Z).

basicREtail(W,Z) --> simpleRE(X), basicREtail(conc(W,X),Z).
basicREtail(W,W) --> [].

simpleRE(Z) --> elementalRE(W), simpleREtail(W,Z).

simpleREtail(W,star(W)) --> "*".
simpleREtail(W,plus(W)) --> "+".
simpleREtail(W,W) --> [].

elementalRE(any) --> ".".
elementalRE(group(X)) --> "(", re(X), ")".
elementalRE(eos) --> "$".
elementalRE(char(C)) --> [C], {\+re_metachar([C])}.
elementalRE(char(C)) --> "\\", [C], {re_metachar([C])}.
elementalRE(negSet(X)) --> "[^", !, setItems(X), "]".
elementalRE(posSet(X)) --> "[", setItems(X), "]".

re_metachar("\\").
re_metachar("\|").
re_metachar("*").
re_metachar("+").
re_metachar("\.").
re_metachar("[").
re_metachar("$").
re_metachar("(").
re_metachar(")").

setItems([Item1|MoreItems]) --> setItem(Item1), setItems(MoreItems).
setItems([Item1]) --> setItem(Item1).

setItem(char(C)) --> [C], {\+set_metachar([C])}.
setItem(char(C)) --> "\\", [C], {set_metachar([C])}.
setItem(range(A,B)) --> setItem(char(A)), "-", setItem(char(B)).

set_metachar("\\").
set_metachar("]").
set_metachar("-").
