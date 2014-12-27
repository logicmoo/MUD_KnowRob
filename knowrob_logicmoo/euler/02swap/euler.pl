% ---------------------------------------
% Euler Yap engine for JLog -- Jos De Roo
% ---------------------------------------

:- op(1200,xfx,':=').
:- op(1199,xfx,'=>').

main :-
	write('#Processed by $Id: euler.pl 3211 2009-12-17 20:58:07Z josd $'), nl,
	(flag(keywords) -> write('@keywords is, of, a.'), nl, nl; nl),
	forall(pfx(A,B),(write('@prefix '), write(A), write(' '), write(B), write('.'), nl)),
	(pfx('e:',_) -> true; write('@prefix e: <http://eulersharp.sourceforge.net/2003/03swap/log-rules#>.'), nl),
	(pfx('r:',_) -> true; write('@prefix r: <http://www.w3.org/2000/10/swap/reason#>.'), nl), nl,
	(\+flag(nope) -> assert((['<>',X,Y] := 'e:conditional'(X,Y) => answer('e:conditional'(X,Y)))); true),
	assert(limit(1)), (([] := answer(_) => goal) -> true; assert(([] := answer(_) => goal))), time((sem(0), write('#ENDS '))),
	map(tc,U), map(tp,V), write('#Trunk : '), write(U), write('/'), write(V), write(' = '), (V =\= 0 -> W is U/V*100, write(W), write(' %'); true), nl,
	map(bc,J), map(bp,K), write('#Branch: '), write(J), write('/'), write(K), write(' = '), (K =\= 0 -> L is J/K*100, write(L), write(' %'); true), nl, nl.

main(Query) :-
	assert(Query), main, retractall(span(_)), retractall(limit(_)), retractall(brake),
	retractall((_ := _ => answer(_))), retractall(answer(_)), retractall(steps(_,_,answer(_),_)), retractall(answers(_,_)).

sem(Span) :-
	(Src := Prem => Conc), copy_term((Src := Prem => Conc),Rule),
	\+'e:tactic'('log:implies'(Prem,Conc),_),
	Prem, cnt(tp), (flag(step(StepLim)), map(tp,Step), Step >= StepLim -> w3(trunk), throw(maximimum_step_count(Step)); true),
	%%% euler path detection
	\+Conc, cnt(tc),
	ground(Conc), Conc \= (_;_), Conc \= goal, Conc \= false,
	astep(Src,Prem,Conc,Rule), retract(brake), fail
	; brake, (limit(Limit), Span < Limit, S is Span+1, assert(span(S)), sem(S); w3(trunk), \+flag('no-branch'), sem([],0,[],[]); true), !
	; assert(brake), sem(Span).

%%% Coherent Logic inspired by http://www.cs.vu.nl/~diem/research/ht/CL.pl
sem(Grd,Pnum,Stack,Env) :-
	(Src := Prem => Conc), copy_term((Src := Prem => Conc),Rule),
	(\+'e:tactic'('log:implies'(Prem,Conc),_) -> Gnew = Grd; 'e:tactic'('log:implies'(Prem,Conc),[Grd,Gnew])),
	Prem, cnt(bp), (flag(step(StepLim)), map(tp,Tp), map(bp,Bp), Step is Tp+Bp, Step >= StepLim -> throw(maximimum_step_count(Step)); true),
	%%% euler path detection
	\+Conc, cnt(bc),
	(Conc = false -> \+false(Prem), C = false(Prem); C = Conc), !,
	((C = goal -> true; C = false(_), flag(quick)) -> end(C,Env)
	; (C = (E;D) -> split(Src,Prem,Gnew,Pnum,[D|Stack],E,Env,Rule); memo(Src,Prem,Gnew,Pnum,Stack,C,Env,Rule))).

split(Src,Prem,Grd,Pnum,[T|Stack],C,Env,Rule) :-
	\+member(C,Env), memo(Src,Prem,Grd,Pnum,[T|Stack],C,[C|Env],Rule),
	(T = (E;D) -> split(Src,Prem,Grd,Pnum,[D|Stack],E,Env,Rule); \+member(T,Env), memo(Src,Prem,Grd,Pnum,Stack,T,[T|Env],Rule)).

memo(Src,Prem,Grd,Pnum,Stack,Conc,Env,Rule) :-
	(Conc = answer('log:implies'(_,_)) -> Q = all; Q = some), numbervars(Conc,Pnum,Pnew,Q),
	astep(Src,Prem,Conc,Rule), (sem(Grd,Pnew,Stack,Env) -> true; end(countermodel,Env)), dstep(Src,Prem,Conc,Rule).

astep(A,B,C,Rule) :-
	C = (D,E) -> match(D,F), (\+F -> assert(F), assert(steps(A,B,F,Rule)); true), astep(A,B,E,Rule)
	; match(C,F), (\+F -> assert(F), assert(steps(A,B,F,Rule)); true).

dstep(A,B,C,Rule) :-
	C = (D,E) -> match(D,F), (\+F -> true; retract(F), retract(steps(A,B,F,Rule))), dstep(A,B,E,Rule)
	; match(C,F), (\+F -> true; retract(F), retract(steps(A,B,F,Rule))).

ancestor(A,B) :-
	steps(_,C,D,_), D \= false(_), D \= answer(_), unif(B,D), cmember(E,C), E \= true, (unif(A,E); ancestor(A,E)).

cgives(A,B) :-
	\+steps(_,_,B,_), !; steps(_,C,B,_), (cmember(D,C), cmember(E,A), (unif(E,D); \+cgives(E,D)) -> fail; true).

end(goal,Env) :-
	\+false(_), !,
	(Env = [] -> w3(knot); write('[ e:possibleModel '), clist(Env,G), wg(G), nl,
	write('; r:gives {'), nl, retractall(answers(_,branch)), w3(branch), write('}].'), nl, nl).
end(countermodel,Env) :-
	\+false(_), !,
	write('[ e:counterModel '), clist(Env,G), wg(G), write('].'), nl, nl.
end(_,Env) :-
	write('[ e:falseModel '), clist(Env,G), wg(G), nl, (false(F),
	write('; e:because [ e:integrityConstraint '), write('{'), wg(F), write(' => false}'),
	(flag(quiet) -> true; (cmember(A,F), nl,
	write('  ; e:selected [ e:triple '), wg(A), nl,
	findall(X,ancestor(X,A),L), clist(L,U),
	findall(X,ancestor(A,X),M), clist(M,V),
	findall(X,(false(Y), cmember(X,Y)),I), remove_duplicates(I,P), clist(P,Q),
	findall(X,(cmember(X,U), cmember(Y,Q), unif(X,Y)),J), clist(J,R),
	write('    ; e:falseAncestors '), wg(R), nl,
	findall(X,(cmember(X,V), cmember(Y,Q), unif(X,Y)),K), clist(K,S),
	write('    ; e:falseDescendents '), wg(S), nl,
	(flag(think) -> findall(X,(steps(_,_,X,_), X \= false(_), X \= answer(_), \+unif(X,A), cgives(A,X)),N), clist(N,W),
	write('    ; e:consistentGives '), wg(W), nl; true),
	write('    ]'), fail; true)), nl, write('  ]'), nl, fail; true),
	(flag(think) -> findall(X,(steps(_,_,X,_), X \= false(_), X \= answer(_),
	forall((false(H), cmember(B,H)), (\+unif(X,B), cgives(B,X)))),O), clist(O,Z),
	write('; e:consistentGives '), wg(Z), nl; true),
	write('; r:gives {'), nl, retractall(answers(_,branch)), w3(branch), write('}].'), nl, nl.


% ------------
% proof output
% ------------

w3(U) :-
	flag(nope), !,
	(steps(_,_,answer(C),_), \+answers(C,_), assert(answers(C,U)), wt(C), write('.'), nl, fail; nl).
w3(U):-
	steps(_,_,answer(X),_), \+answers(X,_), !, redent, write('[ a r:Proof, r:Conjunction;'), indent, ws,
	(steps(A,B,answer(C),(Src := Prem => answer(Conc))), \+answers(C,_), assert(answers(C,U)),
	write('r:component '), wi(A,B,C,(Src := Prem => Conc)), write(';'), nl, ws, fail; true),
	write('r:gives {'), indent, (answers(C,U), ws, getvars(C,D), wq(D,some), wt(C), write('.'), fail; true),
	dedent, ws, write('}].'), dedent, nl, nl; true.

wi(A,true,C,_) :- !,
	A = [S|_], write('[ a r:Extraction; r:gives {'), numbervars((A,C),0,_,var), getvars(C,D), wq(D,some), wt(C),
	write('}; r:because [ a r:Parsing; r:source '), wt(S), write(']]').
wi(A,B,C,(Src := Prem => Conc)) :-
	numbervars((A,B,C),0,_,some), numbervars((Src := Prem => Conc),0,_,var), A = [S|Vbnd], Src = [_|Vars],
	write('[ a r:Inference; r:gives {'), getvars(C,D), wq(D,some), wt(C),
	write('}; r:evidence ('), indent, wr(B), write(');'), ws, wb(Vars,Vbnd),
	write('r:rule [ a r:Extraction; r:gives {'), wq(Vars,all), wt('log:implies'(Prem,Conc)),
	write('}; r:because [ a r:Parsing; r:source '), wt(S), write(']]]'), dedent, fail; true.

wr(varpred(S,P,O)) :- !,
	U =.. [P,S,O], wr(U).
wr((X,Y)) :- !,
	wr(X), wr(Y).
wr('='(X,Y)) :- !,
	ws, write('[ a r:Fact; r:gives '), wt('='(X,Y)), write(']').
wr(Z) :-
	steps(X,Y,Z,Rule), !, ws, wi(X,Y,Z,Rule).
wr(Y) :-
	auri(Y,Z), ws, write('[ a r:Fact; r:gives {'), numbervars(Z,0,_,var), getvars(Z,V), wq(V,some), wt(Z), write('}]').

wt(X) :-
	number(X), !, write(X).
wt((X,Y)) :- !,
	wt(X), write('. '), wt(Y).
wt([]) :- !,
	write('()').
wt([X|Y]) :- !,
	write('('), wg(X), wl(Y), write(')').
wt(X) :-
	functor(X,_,A), (A = 0, !, wt0(X); A = 1, !, wt1(X); A = 2, !, wt2(X); wtn(X)).

wt0(empty) :- !.
wt0(a) :- !,
	write(':a').
wt0(X) :-
	flag(nope), concat_atom(['var:',Y],X), !, write('_:'), write(Y).
wt0(X) :-
	(atom(X), sub_atom(X,_,1,_,'.'), quri(X,Y) -> write(Y); write(X)).

wt1(some(X)) :- !,
	write('_:sk'), write(X).
wt1(all(X)) :- !,
	write('?U'), write(X).
wt1(var(X)) :- !,
	write('var:x'), write(X).
wt1(X) :-
	X =.. [B|C], write('_: '), wp(B), write(' '), wt(C).

wt2(fpath(X,Y)) :- !,
	wg(X), write('!'), wt(Y).
wt2(bpath(X,Y)) :- !,
	wg(X), write('^'), wt(Y).
wt2(tlit(X,Y)) :- !,
	wt(X), write('^^'), wt(Y).
wt2(plit(X,Y)) :- !,
	wt(X), write('@'), wt(Y).
wt2('e:biconditional'([X|Y],Z)) :-
	flag(tquery), !, 'log:conjunction'(Y,U), write('{'), wt(U), write('. _: e:true '), wt(Z), write('} => {'), wt(X), write('}').
wt2('e:conditional'([X|Y],Z)) :-
	flag(tquery), !, 'log:conjunction'(Y,U), write('{'), wt(U), write('. _: e:true '), wt(Z), write('} => {'), wt(X), write('}').
wt2(X =.. [P,S,O]) :- !,
	wt(X), write(' = {'), wg(S), write(' '), wp(P), write(' '), wg(O), write('}').
wt2(X) :-
	X =.. [P,S,O], wg(S), write(' '), wp(P), write(' '), wg(O).

wtn(varpred(S,P,O)) :- !,
	wg(S), write(' '), wp(P), write(' '), wg(O).
wtn(X) :-
	X =.. [B|C], write('_: '), wp(B), write(' '), wt(C).

wg(X) :-
	number(X), !, write(X).
wg((X;Y)) :- !,
	wt([X,Y]), write('!'), wt('e:disjunction').
wg((X,Y)) :- !,
	write('{'), wt((X,Y)), write('}').
wg([]) :- !,
	write('()').
wg([X|Y]) :- !,
	write('('), wg(X), wl(Y), write(')').
wg(X) :-
	functor(X,F,A), ((F = varpred, !; A = 2, F \= fpath, F \= bpath, F \= tlit, F \= plit) -> write('{'), wt(X), write('}'); wt(X)).

wp('rdf:type') :- !,
	write('a').
wp('log:implies') :- !,
	write('=>').
wp(X) :-
	wt(X).

wk([]) :- !.
wk([X|Y]) :-
	write(', '), wt(X), wk(Y).

wl([]) :- !.
wl([X|Y]) :-
	write(' '), wg(X), wl(Y).

wq([],_) :- !.
wq([X|Y],all) :- !,
	write('@forAll '), wt(X), wk(Y), write('. ').
wq([X|Y],some) :-
	write('@forSome '), wt(X), wk(Y), write('. ').

wb([],[]) :- !.
wb([X|Y],[U|V]) :-
	write('r:binding [ r:variable '), wv(X), write('; r:boundTo '), wv(U), write('];'), ws, wb(Y,V).

wv(var(I)) :- !,
	write('[ n3:uri "http://localhost/var#x'), write(I), write('"'), write(']').
wv(some(I)) :- !,
	write('[ a r:Existential; n3:nodeId "_:sk'), write(I), write('"'), write(']').
wv((X,Y)) :- !,
	write('{'), wt((X,Y)), write('}').
wv([]) :- !,
	write('()').
wv([X|Y]) :- !,
	write('('), wg(X), wl(Y), write(')').
wv(X) :-
	atom(X), sub_atom(X,0,4,B,'var:'), !, write('[ a r:Existential; n3:nodeId "http://localhost/var#'),
	sub_atom(X,4,B,_,Q), write(Q), write('"'), write(']').
wv(X) :-
	quri(X,R), sub_atom(R,1,_,1,U), concat_atom(['<',U,'>'],R), !, write('[ n3:uri "'), write(U), write('"'), write(']').
wv(X) :-
	wg(X).

ws :-
	nl, map(tabs,A), tab(A).

redent :-
	map(tabs,0).

indent :-
	map(tabs,A), B is A+1, map(tabs,B).

dedent :-
	map(tabs,A), B is A-1, map(tabs,B).


% --------
% builtins
% --------

'e:allAncestors'(A,B) :-
	within_span(1), A, findall(X,ancestor(X,A),L), clist(L,B).

'e:allDescendents'(A,B) :-
	within_span(1), A, findall(X,ancestor(A,X),L), clist(L,B).

'e:biconditional'(['e:boolean'(A,B)|C],D) :-
	within_span(1), (\+bset, assert(bset), bcln, bnet; true), fail; bvar(A), bval(B), bcon(['e:boolean'(A,B)],C,D).

'e:binaryEntropy'(A,B) :-
	getnumber(A,C), (C =:= 0.0 -> B is 0.0; (C =:= 1.0 -> B is 0.0; B is -(C*log(C)+(1-C)*log(1-C))/log(2))).

'e:distinct'(A,B) :-
	nonvar(A), remove_duplicates(A,B).

'e:findall'([Scope,Span],[A,B,C]) :-
	(var(Span) -> Span = 1; true), within_span(Span), flag(scope(Scope)), findall(A,B,C).

'e:graphDifference'(X,Y) :-
	nonvar(X), difference(X,Y).

'e:graphIntersection'(X,Y) :-
	nonvar(X), intersection(X,Y).

'e:graphList'(A,B) :-
	clist(B,A).

'e:label'(A,B) :-
	nonvar(A), (atom(A) -> sub_atom(A,0,4,_,'var:'), sub_atom(A,4,_,0,C), concat_atom(['"',C,'"'],B);
	A = some(C), number_chars(C,D), atom_chars(E,D), concat_atom(['"sk',E,'"'],B)).

'e:length'(A,B) :-
	nonvar(A), length(A,B).

'e:max'(A,B) :-
	nonvar(A), bmax(A,B).

'e:min'(A,B) :-
	nonvar(A), bmin(A,B).

'e:notLabel'(A,B) :-
	nonvar(A) ,\+'e:label'(A,B).

'e:optional'(_,A) :-
	A -> true; true.

'e:pair'(A,[B,C]) :-
	'e:sublist'(A,[B,C]); 'e:sublist'(A,[C,B]).

'e:propertyChainExtension'([A],[B,C]) :- !,
	D =.. [A,B,C], D.
'e:propertyChainExtension'([A|B],[C,D]) :-
	E =.. [A,C,F], E, 'e:propertyChainExtension'(B,[F,D]).

'e:reverse'(A,B) :-
	reverse(A,B).

'e:roc'(St,[Sen,Asp]) :-
	getnumber(St,K), (getnumber(Sen,S) -> Asp is 1-(1-exp(-K*(S-1)))*(1+exp(K))/(1+exp(-K*(S-1)))/(1-exp(K))
	; getnumber(Asp,A), Sen is (1-exp(-K*A))*(1+exp(-K))/(1+exp(-K*A))/(1-exp(-K))).

'e:sigmoid'(A,B) :-
	getnumber(A,C), B is 1/(1+exp(-C)).

'e:sort'(A,B) :-
	nonvar(A), sort(A,B).

'e:sublist'(A,B) :-
	nonvar(A), append(C,_,A), append(_,B,C).

'e:trace'(_,X) :-
	write('#TRACE '), copy_term(X,Y), numbervars(Y,0,_,var), wg(Y), nl.

'e:true'(_,A) :-
	nonvar(A), A =:= 1.0.

'e:tuple'(X,Y) :-
	tuple(X,Y) -> true; findall(Z,tuple(Z,_),L), length(L,N), number_chars(N,C), atom_chars(A,C),
	concat_atom(['var:t',A],X), assert(tuple(X,Y)).

'e:wwwFormEncode'(X,Y) :-
	ground(X), unquote(X,U), www_form_encode(U,V), concat_atom(['"',V,'"'],Y), !
	; ground(Y), unquote(Y,V), www_form_encode(U,V), concat_atom(['"',U,'"'],X).

'fl:pi'([A,B],C) :-
	within_span(1), (fset -> true; forall('fl:mu'([X,Y],Z),((fm(X) -> true; assert(fm(X))), assert(pi(X,Y,Z)))),
	forall('fl:sigma'([X,Y],_),((fs(X) -> true; assert(fs(X))), (fs(Y) -> true; assert(fs(Y))))), fnet, assert(fset)), fail; pi(A,B,C).

'fn:resolve-uri'([A,B],C) :-
	ground([A,B]), unquote(A,U), unquote(B,V), global_url(U,V,W), concat_atom(['"',W,'"'],C).

'fn:substring'([A,B|C],D) :-
	ground([A,B,C]), sub_atom(A,1,E,1,U), (C = [] -> F is E-B; C = [F]), sub_atom(U,B,F,_,V), concat_atom(['"',V,'"'],D).

'fn:substring-after'([A,B],C) :-
	ground([A,B]), unquote(A,U), unquote(B,V), sub_atom(U,_,_,W,V), sub_atom(U,_,W,Z,X), Z = 0, concat_atom(['"',X,'"'],C).

'fn:substring-before'([A,B],C) :-
	ground([A,B]), unquote(A,U), unquote(B,V), sub_atom(U,W,_,_,V), sub_atom(U,0,W,_,X), concat_atom(['"',X,'"'],C).

'list:append'(A,B) :-
	nonvar(A), append(A,B).

'list:first'([A|_],A).

'list:in'(A,B) :-
	nonvar(B), member(A,B).

'list:last'(A,B) :-
	nonvar(A), last(A,B).

'list:member'(A,B) :-
	nonvar(A), member(B,A).

'list:rest'([_|B],B).

'log:conjunction'(X,Y) :-
	nonvar(X), conjoin(X), findall(Z,graph(Z),L), clist(L,Y), retractall(graph(_)).

'log:dtlit'([A,B],tlit(A,B)).

'log:equalTo'(X,X).

'log:implies'(X,Y) :-
	(_ := X => Y), X \= true, Y \= answer(_), Y \= goal.

'log:includes'(X,Y) :-
	nonvar(X), nonvar(Y), includes(X,Y).

'log:notEqualTo'(X,Y) :-
	X \= Y.

'log:notIncludes'(X,Y) :-
	nonvar(X), nonvar(Y), \+'log:includes'(X,Y).

'log:semantics'(X,Y) :-
	nonvar(X), quri(X,Q), (fact('log:semantics'(Q,Y)), !;
	findall(U,(pfx(X1,X2), concat_atom(['@prefix ',X1,' ',X2,'. '],U)),L), concat_atom(L,W),
	base(B), sub_atom(Q,1,_,1,Z), concat_atom(['.context ',W],C1), www_form_encode(C1,C2),
	concat_atom(['.euler --semterm ',B,C2,' ',Z],C3), www_form_encode(C3,C4), concat_atom([B,C4],V),
	semantics(V,Y), assert(fact('log:semantics'(Q,Y)))).

'log:uri'(X,Y) :-
	(nonvar(X); ground(Y)), quri(X,Q), !, sub_atom(Q,1,_,1,U), concat_atom(['"',U,'"'],Y);
	unquote(Y,U), concat_atom(['<',U,'>'],X).

'math:absoluteValue'(X,Z) :-
	getnumber(X,U), Z is abs(U).

'math:atan2'([X,Y],Z) :-
	getnumber(X,U), getnumber(Y,V), Z is atan(U/V).

'math:cos'(X,Z) :-
	getnumber(X,U), Z is cos(U), !; getnumber(Z,W), X is acos(W).

'math:cosh'(X,Z) :-
	getnumber(X,U), Z is (exp(U)+exp(-U))/2, !; getnumber(Z,W), X is log(W+sqrt(W**2-1)).

'math:degrees'(X,Z) :-
	getnumber(X,U), Z is U*180/3.141592653589793, !; getnumber(Z,W), X is W*3.141592653589793/180.

'math:difference'([X,Y],Z) :-
	getnumber(X,U), getnumber(Y,V), Z is U-V.

'math:equalTo'(X,Y) :-
	getnumber(X,U), getnumber(Y,V), U =:= V.

'math:exponentiation'([X,Y],Z) :-
	getnumber(X,U), (getnumber(Y,V), Z is U**V, !; getnumber(Z,W), W =\= 0, U =\= 0, Y is log(W)/log(U)).

'math:greaterThan'(X,Y) :-
	getnumber(X,U), getnumber(Y,V), U > V.

'math:integerQuotient'([X,Y],Z) :-
	getnumber(X,U), getnumber(Y,V), Z is truncate(floor(U/V)).

'math:lessThan'(X,Y) :-
	getnumber(X,U), getnumber(Y,V), U < V.

'math:memberCount'(X,Y) :-
	nonvar(X), length(X,Y).

'math:negation'(X,Z) :-
	getnumber(X,U), Z is -U, !; getnumber(Z,W), X is -W.

'math:notEqualTo'(X,Y) :-
	getnumber(X,U), getnumber(Y,V), U =\= V.

'math:notGreaterThan'(X,Y) :-
	getnumber(X,U), getnumber(Y,V), U =< V.

'math:notLessThan'(X,Y) :-
	getnumber(X,U), getnumber(Y,V), U >= V.

'math:product'(X,Z) :-
	product(X,Z).

'math:quotient'([X,Y],Z) :-
	getnumber(X,U), getnumber(Y,V), Z is 1.0*U/V.

'math:remainder'([X,Y],Z) :-
	getnumber(X,U), getnumber(Y,V), Z is U-V*truncate(floor(U/V)).

'math:rounded'(X,Z) :-
	getnumber(X,U), Z is truncate(round(U)).

'math:sin'(X,Z) :-
	getnumber(X,U), Z is sin(U), !; getnumber(Z,W), X is asin(W).

'math:sinh'(X,Z) :-
	getnumber(X,U), Z is (exp(U)-exp(-U))/2, !; getnumber(Z,W), X is log(W+sqrt(W**2+1)).

'math:sum'(X,Z) :-
	sum(X,Z).

'math:tan'(X,Z) :-
	getnumber(X,U), Z is sin(U)/cos(U), !; getnumber(Z,W), X is atan(W).

'math:tanh'(X,Z) :-
	getnumber(X,U), Z is (exp(2*U)-1)/(exp(2*U)+1), !; getnumber(Z,W), X is 0.5*log((1+W)/(1-W)).

'rdf:first'([X|Y],X) :-
	'rdf:type'([X|Y],'rdf:List').

'rdf:rest'([X|Y],Y) :-
	'rdf:type'([X|Y],'rdf:List').

'str:concatenation'(X,Y) :-
	ground(X), findall(S,(member(R,X), unquote(R,S)),Z), concat_atom(Z,T), concat_atom(['"',T,'"'],Y).

'str:contains'(X,Y) :-
	ground([X,Y]), unquote(X,S), unquote(Y,T), sub_atom(S,_,_,_,T).

'str:containsIgnoringCase'(X,Y) :-
	ground([X,Y]), unquote(X,S), unquote(Y,T), downcase_atom(S,U), downcase_atom(T,V), sub_atom(U,_,_,_,V).

'str:endsWith'(X,Y) :-
	ground([X,Y]), unquote(X,S), unquote(Y,T), sub_atom(S,_,_,0,T).

'str:equalIgnoringCase'(X,Y) :-
	ground([X,Y]), unquote(X,S), unquote(Y,T), downcase_atom(S,U), downcase_atom(T,V), U == V.

'str:greaterThan'(X,Y) :-
	ground([X,Y]), X @> Y.

'str:lessThan'(X,Y) :-
	ground([X,Y]), X @< Y.

'str:matches'(X,Y) :-
	ground([X,Y]), unquote(X,S), unquote(Y,T), matches(S,T).

'str:notEqualIgnoringCase'(X,Y) :-
	ground([X,Y]), \+'str:equalIgnoringCase'(X,Y).

'str:notGreaterThan'(X,Y) :-
	ground([X,Y]), X @=< Y.

'str:notLessThan'(X,Y) :-
	ground([X,Y]), X @>= Y.

'str:notMatches'(X,Y) :-
	ground([X,Y]), \+'str:matches'(X,Y).

'str:startsWith'(X,Y) :-
	ground([X,Y]), unquote(X,S), unquote(Y,T), sub_atom(S,0,_,_,T).

'time:day'(tlit(X,_),Y) :-
	ground(X), sub_atom(X,9,2,_,Z), concat_atom(['"',Z,'"'],Y).

'time:month'(tlit(X,_),Y) :-
	ground(X), sub_atom(X,6,2,_,Z), concat_atom(['"',Z,'"'],Y).

'time:year'(tlit(X,_),Y) :-
	ground(X), sub_atom(X,1,4,_,Z), concat_atom(['"',Z,'"'],Y).


% -------
% support
% -------

base(A) :-
	flag(port(P)), !, concat_atom(['http://localhost:',P,'/'],A).
base('http://localhost/').

auri('log:semantics'(A,B),'log:semantics'(D,B)) :-
	quri(A,D), !.
auri('log:uri'(A,B),'log:uri'(D,B)) :-
	quri(A,D), !.
auri(A,A).

quri(A,A) :-
	atom(A), sub_atom(A,1,_,1,U), concat_atom(['<',U,'>'],A), !.
quri(A,B) :-
	atom(A), pfx(A,B), !.
quri(A,B) :-
	atom(A), pfx(U,V), sub_atom(A,0,X,Y,U), sub_atom(V,0,_,1,W), sub_atom(A,X,Y,_,Q), concat_atom([W,Q,'>'],B).

within_span(A) :-
	(limit(B) -> (B < A -> retract(limit(B)), assert(limit(A)); true); assert(limit(A))), span(A).

varpred(S,P,O) :-
	atom(P) -> U =.. [P,S,O], U; steps(_,_,U,_), U =.. [P,S,O], P \= false.

unif(varpred(S,P,O),A) :-
	A =.. [P,S,O], !.
unif(A,varpred(S,P,O)) :-
	A =.. [P,S,O], !.
unif(A,A).

match(varpred(S,P,O),A) :-
	A =.. [P,S,O], !.
match(A,A).

clist([],true) :- !.
clist([A],B) :-
	(nonvar(B), B = (_,_) -> fail; B = A), !.
clist([A|B],(A,C)) :-
	clist(B,C).

cmember(A,(A,_)).
cmember(A,(_,B)) :-
	cmember(A,B).
cmember(A,A) :-
	A \= (_,_).

conjoin([]) :- !.
conjoin([true|Y]) :- !,
	conjoin(Y).
conjoin([X|Y]) :-
	agraph(X), conjoin(Y).

agraph((X,Y)) :- !,
	(\+graph(X) -> assert(graph(X)); true), agraph(Y).
agraph(X) :-
	\+graph(X) -> assert(graph(X)); true.

includes(_,true) :- !.
includes(X,Y) :-
	X \= (_,_), unif(X,Y), !.
includes((X,Y),Z) :-
	unif(X,Z); includes(Y,Z).
includes(X,(Y,Z)) :-
	includes(X,Y), includes(X,Z).

difference([true,_],true) :- !.
difference([X,true],X) :- !.
difference([X,Y],Z) :-
	findall(U,(cmember(U,X), (cmember(V,Y), unif(U,V) -> fail; true)),W), (W = [] -> Z = true; clist(W,G), Z = G).

intersection([X],X) :- !.
intersection([true|_],true) :- !.
intersection([X|Y],Z) :-
	intersection(Y,I), (I = true -> Z = true; findall(U,(cmember(U,X), cmember(V,I), unif(U,V)),W), clist(W,Z)).

delete([],_,[]) :- !.
delete([A|B],A,C) :- !,
	delete(B,A,C).
delete([A|B],C,[A|D]) :-
	delete(B,C,D).

append([],A,A).
append([A|B],C,[A|D]) :-
	append(B,C,D).

append([],[]).
append([A|B],C) :-
	append(A,D,C), append(B,D).

flatten(A,B) :-
	flatten(A,[],C), !, B = C.

flatten(A,B,[A|B]) :-
	var(A), !.
flatten([],A,A) :- !.
flatten([A|B],C,D) :- !,
	flatten(A,E,D), flatten(B,C,E).
flatten(A,B,[A|B]).

remove_duplicates([],[]) :- !.
remove_duplicates([A|B],[A|C]) :-
	delete(B,A,D), remove_duplicates(D,C).

member(A,[A|_]).
member(A,[_|B]) :-
	member(A,B).

memberchk(A,[A|_]) :- !.
memberchk(A,[_|B]) :-
	memberchk(A,B).

reverse(A,B) :-
	reverse(A,[],B).

reverse([],A,A).
reverse([A|B],C,D) :-
	reverse(B,[A|C],D).

sum([],0) :- !.
sum([A|B],C) :-
	getnumber(A,X), sum(B,D), C is X+D.

product([],1) :- !.
product([A|B],C) :-
	getnumber(A,X), product(B,D), C is X*D.

rms(A,B) :-
	findall(C,(member(D,A), getnumber(D,E), C is E*E),F), sum(F,G), length(F,H), B is sqrt(G/H).

bmax([A|B],C) :-
	getnumber(A,X), bmax(B,X,C).

bmax([],A,A).
bmax([A|B],C,D) :-
	getnumber(A,X), (X > C -> bmax(B,X,D); bmax(B,C,D)).

bmin([A|B],C) :-
	getnumber(A,X), bmin(B,X,C).

bmin([],A,A).
bmin([A|B],C,D) :-
	getnumber(A,X), (X < C -> bmin(B,X,D); bmin(B,C,D)).

last([A|B],C) :-
	last(B,A,C).

last([],A,A).
last([A|B],_,C) :-
	last(B,A,C).

inconsistent(['e:boolean'(A,'e:T')|B]) :-
	memberchk('e:boolean'(A,'e:F'),B), !.
inconsistent(['e:boolean'(A,'e:F')|B]) :-
	memberchk('e:boolean'(A,'e:T'),B), !.
inconsistent([_|B]) :-
	inconsistent(B).

inverse('e:boolean'(A,'e:T'),'e:boolean'(A,'e:F')) :- !.
inverse('e:boolean'(A,'e:F'),'e:boolean'(A,'e:T')).

bcln :-
	'e:conditional'([A|B],U), (flag(quick) -> Z = U; sort(B,C), findall(Y,('e:conditional'([A|X],Y), sort(X,C)),L),
	sum(L,S), length(L,N), Z is S/N), \+map([A|B],_), map([A|B],Z), assert(bcnd([A|B],Z)),
	inverse(A,D), \+map([D|B],_), E is 1-Z, map([D|B],E), assert(bcnd([D|B],E)), fail; true.

bnet :-
	bcnd(['e:boolean'(A,_)|B],_), (\+bvar(A), assert(bvar(A)); true),
	member('e:boolean'(C,_),B), \+bref(C,A), assert(bref(C,A)), \+bvar(C), assert(bvar(C)), fail; true.

bval('e:T').
bval('e:F').

brel('e:boolean'(A,_),'e:boolean'(B,_)) :-
	bref(A,B), !.
brel(A,'e:boolean'(B,_)) :-
	bref(C,B), brel(A,'e:boolean'(C,_)).

bpar([],[]) :- !.
bpar(['e:boolean'(A,_)|B],[A|C]) :-
	bpar(B,C).

bget(A,B,1.0) :-
	memberchk(A,B), !.
bget('e:boolean'(A,'e:T'),B,0.0) :-
	memberchk('e:boolean'(A,'e:F'),B), !.
bget('e:boolean'(A,'e:F'),B,C) :-
	memberchk('e:boolean'(A,'e:T'),B), !, C is 0.0; !, bget('e:boolean'(A,'e:T'),B,D), C is 1-D.
bget(A,B,C) :-
	bgot(A,B,C) -> true; (member(X,B), brel(A,X), member(G,B),
	findall(Y,(member(Z,[A|B]), brel(G,Z)),[]), delete(B,G,H), !,
	bget(G,[A|H],U), bget(A,H,V), bget(G,H,W), (W < 1e-15 -> C is 0.5; E is U*V/W, bmin([E,1.0],C));
	findall([Z,Y],(bcnd([A|O],P), bcon(O,B,Q), Z is P*Q, bpar(O,Y)),L),
	findall(Z,(member([_,Z],L)),N), remove_duplicates(N,I),
	findall(Z,(member(Y,I), findall(P,(member([P,Y],L)),Q), sum(Q,R), length(Q,S), length(Y,T),
		(Q = [] -> Z is 0.0; D is 2**(T-ceiling(log(S)/log(2))), (D < 1 -> Z is R*D; Z is R))),J),
	(J = [] -> C is 0.0; bmax(J,C))), assert(bgot(A,B,C)).

bcon([],_,1.0) :- !.
bcon(_,B,0.5) :-
	inconsistent(B), !.
bcon([A|B],C,D) :-
	bget(A,C,E), bcon(B,[A|C],F), D is E*F.

fnet :-
	repeat(20), fm(X), fs(Y), findall(I,('fl:sigma'([P,Y],W), pi(X,P,M), I is (2*M-1)*(2*W-1)),L),
	(L = [] -> true; sum(L,A), Z is 1/(1+exp(-A)), retractall(pi(X,Y,_)), assert(pi(X,Y,Z))), fail; true.

repeat(_).
repeat(N) :- N > 1, N1 is N-1, repeat(N1).

forall(A,B) :-
	A, \+B -> fail; true.

numbervars(some(A),A,B,some) :- !,
	B is A+1.
numbervars(all(A),A,B,all) :- !,
	B is A+1.
numbervars(var(A),A,B,var) :- !,
	B is A+1.
numbervars([],A,A,_) :- !.
numbervars([A|B],C,D,Q) :- !,
	numbervars(A,C,E,Q), numbervars(B,E,D,Q).
numbervars((A,B),C,D,Q) :- !,
	numbervars(A,C,E,Q), numbervars(B,E,D,Q).
numbervars((A;B),C,D,Q) :- !,
	numbervars(A,C,E,Q), numbervars(B,E,D,Q).
numbervars(A,B,B,_) :-
	atomic(A), !.
numbervars(A,B,C,Q) :-
	functor(A,_,D), numbervars(0,D,A,B,C,Q).

numbervars(A,A,_,B,B,_) :- !.
numbervars(A,B,C,D,E,Q) :-
	F is A+1, arg(F,C,G), numbervars(G,D,H,Q), numbervars(F,B,C,H,E,Q).

getvars([],[]) :- !.
getvars([A|B],C) :-
	getvars(A,D), getvars(B,E), append(D,E,F), !, remove_duplicates(F,C).
getvars((A,B),C) :-
	getvars(A,D), getvars(B,E), append(D,E,F), !, remove_duplicates(F,C).
getvars(A,B) :-
	atomic(A), !, (atom(A), sub_atom(A,0,4,_,'var:') -> B = [A]; B = []).
getvars(A,B) :-
	A =.. C, getvars(C,B).

concat_atom([],'') :- !.
concat_atom([A,B],C) :-
	nonvar(A), var(B), nonvar(C), !, atom_chars(A,D), atom_chars(C,E), append(D,F,E), atom_chars(B,F).
concat_atom([A|B],C) :-
	nonvar(A), ground(B), concat_atom(B,D), atom_chars(A,E), atom_chars(D,F), append(E,F,G), atom_chars(C,G).

sub_atom(A,B,C,D,E) :-
	nonvar(A), nonvar(E), atom_chars(A,F), atom_chars(E,G), !, sub_list(G,F,B), length(G,C), length(F,H), D is H-(B+C).
sub_atom(A,B,C,D,E) :-
	nonvar(A), atom_chars(A,F), sub_list(G,F,B), atom_chars(E,G), length(G,C), length(F,H), D is H-(B+C).

sub_list([],_,0).
sub_list([A|B],[A|C],0) :-
	sub_list_seq(B,C).
sub_list(A,[_|B],C) :-
	sub_list(A,B,D), C is D + 1.

sub_list_seq([],_).
sub_list_seq([A|B],[A|C]) :-
	sub_list_seq(B,C).

unquote(A,B) :-
	ground(A), sub_atom(A,0,1,_,'"'), sub_atom(A,_,1,0,'"'), sub_atom(A,1,_,1,B).	

getnumber(A,A) :-
	ground(A), number(A), !.
getnumber(tlit(A,'xsd:dateTime'),B) :- !,
	ground(A), unquote(A,C), datetime(C,B).
getnumber(tlit(A,'xsd:duration'),B) :- !,
	ground(A), unquote(A,C), duration(C,B).
getnumber(tlit(A,_),B) :- !,
	ground(A), unquote(A,C), atom_chars(C,D), number_chars(B,D).
getnumber(plit(A,_),B) :- !,
	ground(A), unquote(A,C), atom_chars(C,D), number_chars(B,D).
getnumber(fpath(A,B),C) :- !,
	ground(A), U =.. [B,A,C], U.
getnumber(bpath(A,B),C) :- !,
	ground(A), U =.. [B,C,A], U.
getnumber(A,B) :-
	ground(A), unquote(A,C), atom_chars(C,D), number_chars(B,D).
