% ----------------------------------------------------
% Euler yap engine for SWI-Prolog (Eyes) -- Jos De Roo
% ----------------------------------------------------

version_info('$Id: eyes.pl 3238 2009-12-23 13:54:34Z josd $').

help_info('Usage: plcon -q -g [eyes],main -- <options>* <data>* <query>\n\
<options>\n\
\	--nope			no proof explanation\n\
\	--no-branch		no branch engine\n\
\	--no-blank		no blank nodes in output\n\
\	--quiet			incomplete e:falseModel explanation\n\
\	--quick			do not prove all e:falseModel\n\
\	--think			generate e:consistentGives\n\
\	--step <count>		set maximimum step count (default 500000)\n\
\	--debug			output debug info\n\
\	--version		show version info\n\
\	--help			show help info\n\
<data>\n\
\	<n3_resource>\n\
\	--trules <n3_resource>\n\
<query>\n\
\	--query <n3_resource>\n\
\	--tquery <n3_resource>\n\
\	--pass').

:- use_module(library('http/http_open')).

:- set_prolog_flag(encoding,utf8).
:- set_prolog_flag(float_format,'%.15g').

:- dynamic(answer/1).
:- dynamic(answers/2).
:- dynamic(bcnd/2).
:- dynamic(bgot/3).
:- dynamic(brake/0).
:- dynamic(bref/2).
:- dynamic(bvar/1).
:- dynamic(fact/1).
:- dynamic(false/0).
:- dynamic(false/1).
:- dynamic(flag/1).
:- dynamic(fm/1).
:- dynamic(fs/1).
:- dynamic(goal/0).
:- dynamic(graph/1).
:- dynamic(keywords/1).
:- dynamic(nodepth/0).
:- dynamic(pcl/3).
:- dynamic(pfx/2).
:- dynamic(pi/3).
:- dynamic(span/1).
:- dynamic(steps/4).
:- dynamic(tuple/2).
:- dynamic(wtcache/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#reflexive>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#mu>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#sigma>'/2).

main :-
	statistics(runtime,[T0,_]), format(user_error,'starting ~w msec~n',[T0]),
	nb_setval(scope,[]), nb_setval(limit,1), nb_setval(bnet,not_done), nb_setval(fnet,not_done),
	unix(argv(Argv)), append(_,[--|Args],Argv), !, (Args = [] -> args(['--help']); args(Args)),
	(flag(step(_)) -> true; assert(flag(step(500000)))),
	statistics(runtime,[T1,_]), TN is T1-T0, format(user_error,'networking ~w msec~n',[TN]),
	version_info(Version), format('#Processed by ~w~n~n',[Version]),
	forall(pfx(A,B),format('@prefix ~w ~w.~n',[A,B])),
	(pfx('var:',_) -> true; write('@prefix var: <http://localhost/var#>.'), nl),
	(pfx('e:',_) -> true; write('@prefix e: <http://eulersharp.sourceforge.net/2003/03swap/log-rules#>.'), nl),
	(pfx('r:',_) -> true; write('@prefix r: <http://www.w3.org/2000/10/swap/reason#>.'), nl),
	(pfx('n3:',_) -> true; write('@prefix n3: <http://www.w3.org/2004/06/rei#>.'), nl), nl,
	nb_setval(tc,0), nb_setval(tp,0), nb_setval(bc,0), nb_setval(bp,0),
	catch(sem(0),Exc,(format(user_error,'** ERROR ** sem ** ~w~n',[Exc]), halt)), !,
	statistics(runtime,[T2,_]), TR is T2-T1, format('#ENDS ~w msec~n',[TR]), format(user_error,'reasoning ~w msec~n~n',[TR]),
	nb_getval(tc,U), nb_getval(tp,V), (V =\= 0 -> W is U/V*100, format('#Trunk : ~w/~w = ~w %~n',[U,V,W]); true),
	nb_getval(bc,J), nb_getval(bp,K), (K =\= 0 -> L is J/K*100, format('#Branch: ~w/~w = ~w %~n',[J,K,L]); true), nl,
	halt.

args([]) :- !.
args(['--trules',Arg|T]) :- !,
	n3_pcl(Arg,trules), concat_atom(['<',Arg,'>'],R), nb_getval(scope,S), nb_setval(scope,[R|S]), args(T).
args(['--tquery',Arg|T]) :- !,
	assert(flag(tquery)), n3_pcl(Arg,tquery),
	(\+flag(nope) -> assert(pcl('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'(X,Y),
	answer('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'(X,Y)),'<>')); true),
	assert(pcl(answer(_),goal,'<>')), args(T).
args(['--query',Arg|T]) :- !,
	n3_pcl(Arg,query),
	(\+flag(nope) -> assert(pcl('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'(X,Y),
	answer('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'(X,Y)),'<>')); true),
	assert(pcl(answer(_),goal,'<>')), args(T).
args(['--pass'|T]) :- !,
	assert(pcl(varpred(S,P,O),answer(varpred(S,P,O)),'<>')), assert(pcl(answer(_),goal,'<>')), args(T).
args(['--version'|_]) :- !,
	version_info(Version), write(Version), nl, nl, halt.
args(['--help'|_]) :- !,
	help_info(Help), write(Help), nl, nl, halt.
args(['--step',Limit|T]) :- !,
	assert(flag(step(Limit))), args(T).
args([Arg|T]) :-
	sub_atom(Arg,0,2,_,'--'), !, sub_atom(Arg,2,_,0,Opt), assert(flag(Opt)), args(T).
args([Arg|T]) :-
	n3_pcl(Arg,data), args(T), concat_atom(['<',Arg,'>'],R), nb_getval(scope,S), nb_setval(scope,[R|S]).


% ------------------
% N3 to PCL compiler
% ------------------

n3_pcl(Arg,Mode) :-
	(format(user_error,'GET ~w~n',[Arg]), sub_atom(Arg,0,5,_,'http:') ->
	catch(http_open(Arg,In,[]),Exc,(format(user_error,'** ERROR ** ~w ** ~w~n',[Arg,Exc]), fail));
	catch(open(Arg,read,In),Exc,(format(user_error,'** ERROR ** ~w ** ~w~n',[Arg,Exc]), fail))),
	catch((turtle_tokens(In,Tokens), close(In)),Exc,(format(user_error,'** ERROR ** ~w ** ~w~n',[Arg,Exc]), close(In), fail)),
	(flag(debug) -> format(user_error,'>>> ~w tokens <<<~n~w~n~n',[Arg,Tokens]); true),
	retractall(base_uri(_)), assert(base_uri(Arg)),
	retractall(ns(_,_)), assert(ns('','#')),
	retractall(quvar(_,_,_)), retractall(qevar(_,_,_)), retractall(evar(_,_,_)),
	nb_setval(fdepth,0), nb_setval(past_triples,[]),
	catch(phrase(document(Triples),Tokens),Exc,(format(user_error,'** ERROR ** ~w ** ~w~n',[Arg,Exc]), fail)),
	(flag(debug) -> format(user_error,'>>> ~w triples <<<~n~w~n~n',[Arg,Triples]); true),
	(Mode = semantics -> clist(Triples,Term), write_to_chars_swi(Term,G), append(G,".",H), read_from_chars_swi(H,T),
	concat_atom(['<',Arg,'>'],S), assert(fact('<http://www.w3.org/2000/10/swap/log#semantics>'(S,T)))
	; concat_atom(['\'<',Arg,'>\''],Src), n3_pcl(Triples,Src,Mode)), !.
n3_pcl(_,_).

n3_pcl([],_,_) :- !.
n3_pcl(['\'<http://www.w3.org/2000/10/swap/log#implies>\''(X,Y)|Z],Src,trules) :- !,
	(clast(X,'\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#true>\''(_,T)) -> true; T = 1.0),
	clist(L,X), tr_pcl(L,K,M), clist(K,N),
	write_to_chars_swi(pcl(N,'\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>\''([Y|M],T),Src),U),
	append(U,".",V), read_from_chars_swi(V,W), assert(W), n3_pcl(Z,Src,trules).
n3_pcl(['\'<http://www.w3.org/2000/10/swap/log#implies>\''(X,Y)|Z],Src,tquery) :- !,
	clist(L,X), tr_pcl(L,K,M),
	append(K,['\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>\''([Y|M],T)],J), clist(J,N),
	write_to_chars_swi(pcl(N,answer('\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>\''([Y|M],T)),Src),U),
	append(U,".",V), read_from_chars_swi(V,W), assert(W), n3_pcl(Z,Src,tquery).
n3_pcl(['\'<http://www.w3.org/2000/10/swap/log#implies>\''(X,Y)|Z],Src,query) :- !,
	write_to_chars_swi(pcl(X,answer(Y),Src),U), append(U,".",V), read_from_chars_swi(V,W), assert(W), n3_pcl(Z,Src,query).
n3_pcl(X,Src,query) :- !,
	clist(X,Y), write_to_chars_swi(pcl(Y,answer(Y),Src),U), append(U,".",V), read_from_chars_swi(V,W), assert(W).
n3_pcl(['\'<http://www.w3.org/2000/10/swap/log#implies>\''(X,Y)|Z],Src,Mode) :- !,
	write_to_chars_swi(pcl(X,Y,Src),U), append(U,".",V), read_from_chars_swi(V,W), assert(W), n3_pcl(Z,Src,Mode).
n3_pcl([X|Z],Src,Mode) :-
	write_to_chars_swi(X,U), append(U,".",V), read_from_chars_swi(V,W), sub_atom(Src,1,_,1,A),
	assert(W), assert(steps(A,true,W,_)), n3_pcl(Z,Src,Mode).

tr_pcl([],[],[]) :- !.
tr_pcl([A|B],C,[A|D]) :-
	functor(A,'\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>\'',_), !, tr_pcl(B,C,D).
tr_pcl([A|B],C,D) :-
	functor(A,'\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#true>\'',_), !, tr_pcl(B,C,D).
tr_pcl([A|B],[A|C],D) :-
	tr_pcl(B,C,D).


% --------------------
% Skolem Euler Machine
% --------------------

sem(Span) :-
	(flag(debug) -> format(user_error,'enter trunk span ~w~n',[Span]); true),
	pcl(Prem,Conc,Src), copy_term('<http://www.w3.org/2000/10/swap/log#implies>'(Prem,Conc),Rule),
	\+'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'('<http://www.w3.org/2000/10/swap/log#implies>'(Prem,Conc),_),
	(flag(debug) -> format(user_error,'. selecting rule ~w~n',[pcl(Prem,Conc,Src)]); true),
	Prem, cnt(tp), (flag(step(StepLim)), nb_getval(tp,Step), Step >= StepLim -> w3(trunk), throw(maximimum_step_count(Step)); true),
	(\+Conc -> cnt(tc); (flag(debug) -> format(user_error,'.. sem/1 euler path so do not step in your own step ~w~n',[Conc]), fail)),
	ground(Conc), Conc \= (_;_), Conc \= goal, Conc \= false,
	(flag(debug) -> format(user_error,'... assert step ~w~n',[Conc]); true), astep(Src,Prem,Conc,Rule), retract(brake), fail
	; brake, (nb_getval(limit,Limit), Span < Limit, S is Span+1, assert(span(S)), sem(S); w3(trunk), \+flag('no-branch'), sem([],0,[],[]); true), !
	; assert(brake), sem(Span).

% Coherent Logic inspired by http://www.cs.vu.nl/~diem/research/ht/CL.pl
sem(Grd,Pnum,Stack,Env) :-
	(flag(debug) -> format(user_error,'enter branch ~w~n',[Env]); true),
	pcl(Prem,Conc,Src), copy_term('<http://www.w3.org/2000/10/swap/log#implies>'(Prem,Conc),Rule),
	(\+'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'('<http://www.w3.org/2000/10/swap/log#implies>'(Prem,Conc),_) -> Gnew = Grd
	; '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'('<http://www.w3.org/2000/10/swap/log#implies>'(Prem,Conc),[Grd,Gnew])),
	(flag(debug) -> format(user_error,'. selecting rule ~w~n',[pcl(Prem,Conc,Src)]); true),
	Prem, cnt(bp), (flag(step(StepLim)), nb_getval(tp,Tp), nb_getval(bp,Bp), Step is Tp+Bp, Step >= StepLim -> throw(maximimum_step_count(Step)); true),
	(\+Conc -> cnt(bc); (flag(debug) -> format(user_error,'.. sem/4 euler path so do not step in your own step ~w~n',[Conc]), fail)),
	(Conc = false -> \+false(Prem), C = false(Prem); C = Conc), !,
	((C = goal -> true; C = false(_), flag(quick)) -> end(C,Env)
	; (C = (E;D) -> split(Src,Prem,Gnew,Pnum,[D|Stack],E,Env,Rule); memo(Src,Prem,Gnew,Pnum,Stack,C,Env,Rule))).

split(Src,Prem,Grd,Pnum,[T|Stack],C,Env,Rule) :-
	(\+member(C,Env) -> true; (flag(debug) -> format(user_error,'.. split/8-left euler path so do not step in your own step ~w~n',[C]), fail)),
	memo(Src,Prem,Grd,Pnum,[T|Stack],C,[C|Env],Rule),
	(T = (E;D) -> split(Src,Prem,Grd,Pnum,[D|Stack],E,Env,Rule);
	(\+member(T,Env) -> true; (flag(debug) -> format(user_error,'.. split/8-right euler path so do not step in your own step ~w~n',[T]), fail)),
	memo(Src,Prem,Grd,Pnum,Stack,T,[T|Env],Rule)).

memo(Src,Prem,Grd,Pnum,Stack,Conc,Env,Rule) :-
	(Conc = answer('<http://www.w3.org/2000/10/swap/log#implies>'(_,_)) -> Q = all; Q = some), numbervars(Conc,Pnum,Pnew,[functor_name(Q)]),
	(flag(debug) -> format(user_error,'... numbervars ~w~n',[Conc]); true),
	(flag(debug) -> format(user_error,'... assert step ~w~n',[Conc]); true), astep(Src,Prem,Conc,Rule),
	(sem(Grd,Pnew,Stack,Env) -> true; end(countermodel,Env)),
	(flag(debug) -> format(user_error,'... retract step ~w~n',[Conc]); true), dstep(Src,Prem,Conc,Rule).

astep(A,B,C,Rule) :-
	C = (D,E) -> match(D,F), (\+F -> assert(F), assert(steps(A,B,F,Rule)); true), astep(A,B,E,Rule)
	; match(C,F), (\+F -> assert(F), assert(steps(A,B,F,Rule)); true).

dstep(A,B,C,Rule) :-
	C = (D,E) -> match(D,F), (\+F -> true; retract(F), retract(steps(A,B,F,Rule))), dstep(A,B,E,Rule)
	; match(C,F), (\+F -> true; retract(F), retract(steps(A,B,F,Rule))).

ancestor(A,B) :-
	steps(_,C,D,_), D \= false(_), D \= answer(_), unif(B,D), cmember(E,C), E \= true, (unif(A,E); ancestor(A,E)).

cgives(A,B) :-
	\+steps(_,_,B,_), !; steps(_,C,B,_), \+((cmember(D,C), cmember(E,A), (unif(E,D); \+cgives(E,D)))).

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
	findall(X,(false(Y), cmember(X,Y)),I), list_to_set(I,P), clist(P,Q),
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
	(steps(A,B,answer(C),R), R =.. [P,S,answer(O)], Rule =.. [P,S,O],
	\+answers(C,_), assert(answers(C,U)), write('r:component '), wi(A,B,C,Rule), write(';'), nl, ws, fail; true),
	write('r:gives {'), indent, (answers(C,U), ws, getvars(C,D), wq(D,some), wt(C), write('.'), fail; true),
	dedent, ws, write('}].'), dedent, nl, nl; true.

wi(A,true,C,_) :- !,
	write('[ a r:Extraction; r:gives {'), numbervars((A,C),0,_,[functor_name(var)]), getvars(C,D), wq(D,some), wt(C),
	write('}; r:because [ a r:Parsing; r:source '), wt(A), write(']]').
wi(A,B,C,Rule) :-
	Rule = '<http://www.w3.org/2000/10/swap/log#implies>'(Prem,_),
	unifiable(Prem,B,Bindings), term_variables(Prem,Vars), numbervars([A,B,C,Rule,Vars],0,_,[functor_name(var)]),
	write('[ a r:Inference; r:gives {'), getvars(C,D), wq(D,some), wt(C),
	write('}; r:evidence ('), indent, wr(B), write(');'), ws, reverse(Bindings,R), wb(R),
	write('r:rule [ a r:Extraction; r:gives {'), wq(Vars,all), wt(Rule),
	write('}; r:because [ a r:Parsing; r:source '), wt(A), write(']]]'), dedent, fail; true.

wr(varpred(S,P,O)) :- !,
	U =.. [P,S,O], wr(U).
wr((X,Y)) :- !,
	wr(X), wr(Y).
wr(Z) :-
	steps(X,Y,Z,Rule), !, ws, wi(X,Y,Z,Rule).
wr(Y) :-
	ws, write('[ a r:Fact; r:gives {'), numbervars(Y,0,_,[functor_name(var)]), getvars(Y,Z), wq(Z,some), wt(Y), write('}]').

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

wt0(X) :-
	flag(nope), \+flag('no-blank'), sub_atom(X,0,4,B,'var:'), !, sub_atom(X,4,B,_,Y), write('_:'), write(Y).
wt0(X) :-
	(wtcache(X,Y) -> true; (atom(X), sub_atom(X,I,1,J,'#'), sub_atom(X,0,I,_,C), atom_concat(C,'#>',D),
	pfx(E,D), K is J-1, sub_atom(X,_,K,1,F), \+sub_atom(F,_,1,_,'.') -> atom_concat(E,F,Y); Y = X),
	assert(wtcache(X,Y))), write(Y).

wt1(some(X)) :- !,
	write('_:sk'), write(X).
wt1(all(X)) :- !,
	write('?U'), write(X).
wt1(var(X)) :- !,
	write('var:x'), write(X).
wt1(X) :-
	X =.. [B|C], write('_: '), wp(B), write(' '), wt(C).

wt2(literal(X,lang(Y))) :- !,
	atom_codes(Z,X), wdq(X), write(Z), wdq(X), write('@'), wt(Y).
wt2(literal(X,type(Y))) :- !,
	atom_codes(Z,X), wdq(X), write(Z), wdq(X), write('^^'), wt(Y).
wt2(literal(X,void)) :- !,
	atom_codes(Z,X), wdq(X), write(Z), wdq(X).
wt2('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>'([X|Y],Z)) :-
	flag(tquery), !, '<http://www.w3.org/2000/10/swap/log#conjunction>'(Y,U),
	write('{'), wt(U), write('. _: e:true '), wt(Z), write('} => {'), wt(X), write('}').
wt2('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'([X|Y],Z)) :-
	flag(tquery), !, '<http://www.w3.org/2000/10/swap/log#conjunction>'(Y,U),
	write('{'), wt(U), write('. _: e:true '), wt(Z), write('} => {'), wt(X), write('}').
wt2(X =.. [P,S,O]) :- !,
	wt(X), write(' = {'), wg(S), write(' '), wp(P), write(' '), wg(O), write('}').
wt2(X) :-
	X =.. [P,S,O], wg(S), write(' '), wp(P), write(' '), wg(O).

wtn(varpred(S,P,O)) :- !,
	wg(S), write(' '), wp(P), write(' '), wg(O).
wtn(X) :-
	X =.. [B|C], write('_: '), wp(B), write(' '), wt(C).

wdq(X) :-
	memberchk(0'\r,X), !, write('"""').
wdq(X) :-
	memberchk(0'\n,X), !, write('"""').
wdq(X) :-
	memberchk(0'",X), !, write('"""').
wdq(_) :-
	write('"').

wg((X;Y)) :- !,
	wt([X,Y]), write('!'), wt('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#disjunction>').
wg(X) :-
	functor(X,F,A), ((F = varpred, !; A = 2, F \= '.', F \= literal) -> write('{'), wt(X), write('}'); wt(X)).

wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>') :- !,
	write('a').
wp('<http://www.w3.org/2000/10/swap/log#implies>') :- !,
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

wb([]) :- !.
wb([X = Y|Z]) :-
	write('r:binding [ r:variable '), wv(X), write('; r:boundTo '), wv(Y), write('];'), ws, wb(Z).

wv(var(I)) :- !,
	write('[ n3:uri "http://localhost/var#x'), write(I), write('"'), write(']').
wv(X) :-
	atom(X), sub_atom(X,0,4,B,'var:'), !, write('[ a r:Existential; n3:nodeId "http://localhost/var#'),
	sub_atom(X,4,B,_,Q), write(Q), write('"'), write(']').
wv(X) :-
	atom(X), sub_atom(X,1,_,1,U), concat_atom(['<',U,'>'],X), !, write('[ n3:uri "'), write(U), write('"'), write(']').
wv(X) :-
	wg(X).

ws:-
	nl, nb_getval(tabs,A), tab(A).

redent :-
	nb_setval(tabs,0).

indent :-
	nb_getval(tabs,A), B is A+1, nb_setval(tabs,B).

dedent :-
	nb_getval(tabs,A), B is A-1, nb_setval(tabs,B).


% --------
% builtins
% --------

'<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#pi>'([A,B],C) :-
	within_span(1), (nb_getval(fnet,done) -> true
	; (forall('<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#mu>'([X,Y],Z),((fm(X) -> true; assert(fm(X))), assert(pi(X,Y,Z)))),
	forall('<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#sigma>'([X,Y],_),((fs(X) -> true; assert(fs(X))), (fs(Y) -> true; assert(fs(Y))))),
	repeat(20), fm(X), fs(Y), findall(I,('<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#sigma>'([P,Y],W), pi(X,P,M), I is (2*M-1)*(2*W-1)),L),
	(L = [] -> true; sum(L,S), Z is 1/(1+exp(-S)), retractall(pi(X,Y,_)), assert(pi(X,Y,Z))), fail; nb_setval(fnet,done))), pi(A,B,C).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#allAncestors>'(A,B) :-
	within_span(1), A, findall(X,ancestor(X,A),L), clist(L,B).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#allDescendents>'(A,B) :-
	within_span(1), A, findall(X,ancestor(A,X),L), clist(L,B).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>'(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,B)|C],D) :-
	within_span(1), (nb_getval(bnet,done) -> true; bnet, nb_setval(bnet,done)), bvar(A), bval(B),
	bcon(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,B)],C,D).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#binaryEntropy>'(A,B) :-
	getnumber(A,C), (C =:= 0.0 -> B is 0.0; (C =:= 1.0 -> B is 0.0; B is -(C*log(C)+(1-C)*log(1-C))/log(2))).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#distinct>'(A,B) :-
	when(nonvar(A),list_to_set(A,B)).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#findall>'([Scope,Span],[A,B,C]) :-
	(var(Span) -> Span = 1; true), within_span(Span), nb_getval(scope,Scope), \+is_list(B), findall(A,B,C).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#graphDifference>'(X,Y) :-
	nonvar(X), difference(X,Y).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#graphIntersection>'(X,Y) :-
	nonvar(X), intersection(X,Y).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#graphList>'(A,B) :-
	clist(B,A).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#label>'(A,B) :-
	when(nonvar(A),(atom(A) -> sub_atom(A,0,4,_,'var:'), sub_atom(A,4,_,0,C), concat_atom(['"',C,'"'],B);
	A = some(C), atom_number(D,C), concat_atom(['"sk',D,'"'],B))).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#length>'(A,B) :-
	when(nonvar(A),length(A,B)).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#max>'(A,B) :-
	when(nonvar(A),bmax(A,B)).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#min>'(A,B) :-
	when(nonvar(A),bmin(A,B)).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#notLabel>'(A,B) :-
	when(nonvar(A),\+'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#label>'(A,B)).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#optional>'(_,A) :-
	A -> true; true.

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#pair>'(A,[B,C]) :-
	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#sublist>'(A,[B,C])
	; '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#sublist>'(A,[C,B]).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#propertyChainExtension>'([A],[B,C]) :- !,
	D =.. [A,B,C], D.
'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#propertyChainExtension>'([A|B],[C,D]) :-
	E =.. [A,C,F], E, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#propertyChainExtension>'(B,[F,D]).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#reverse>'(A,B) :-
	reverse(A,B).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#roc>'(St,[Sen,Asp]) :-
	getnumber(St,K), (getnumber(Sen,S) -> Asp is 1-(1-exp(-K*(S-1)))*(1+exp(K))/(1+exp(-K*(S-1)))/(1-exp(K))
	; getnumber(Asp,A), Sen is (1-exp(-K*A))*(1+exp(-K))/(1+exp(-K*A))/(1-exp(-K))).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#sigmoid>'(A,B) :-
	getnumber(A,C), B is 1/(1+exp(-C)).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#sort>'(A,B) :-
	when(nonvar(A),sort(A,B)).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#sublist>'(A,B) :-
	when(nonvar(A),(append(C,_,A), append(_,B,C))).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#trace>'(_,X) :-
	write('#TRACE '), copy_term(X,Y), numbervars(Y,0,_,[functor_name(var)]), wg(Y), nl.

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#true>'(_,A) :-
	when(nonvar(A),A =:= 1.0).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tuple>'(X,Y) :-
	(tuple(X,Y) -> true
	; findall(Z,tuple(Z,_),L), length(L,N), atom_number(A,N), concat_atom(['var:t',A],X), assert(tuple(X,Y))).

'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#wwwFormEncode>'(literal(X,void),literal(Y,void)) :-
	atom_codes(S,X), www_form_encode(S,T), atom_codes(T,Y).

'<http://www.w3.org/2005/xpath-functions#resolve-uri>'([literal(A,void),literal(B,void)],literal(C,void)) :-
	when(ground([A,B]),(atom_codes(U,A), atom_codes(V,B), global_url(U,V,W), atom_codes(W,C))).

'<http://www.w3.org/2005/xpath-functions#substring>'([literal(A,_),B|C],literal(D,void)) :-
	when(ground([A,B,C]),((C = [] -> length(A,E), F is E-B; C = [F]), atom_codes(U,A), sub_atom(U,B,F,_,V), atom_codes(V,D))).

'<http://www.w3.org/2005/xpath-functions#substring-after>'([literal(A,void),literal(B,void)],literal(C,void)) :-
	when(ground([A,B]),(atom_codes(U,A), atom_codes(V,B), sub_atom(U,_,_,W,V), sub_atom(U,_,W,0,X), atom_codes(X,C))).

'<http://www.w3.org/2005/xpath-functions#substring-before>'([literal(A,void),literal(B,void)],literal(C,void)) :-
	when(ground([A,B]),(atom_codes(U,A), atom_codes(V,B), sub_atom(U,W,_,_,V), sub_atom(U,0,W,_,X), atom_codes(X,C))).

'<http://www.w3.org/2000/10/swap/list#append>'(A,B) :-
	when(nonvar(A),append(A,B)).

'<http://www.w3.org/2000/10/swap/list#first>'([A|_],A).

'<http://www.w3.org/2000/10/swap/list#in>'(A,B) :-
	when(nonvar(B),member(A,B)).

'<http://www.w3.org/2000/10/swap/list#last>'(A,B) :-
	when(nonvar(A),last(A,B)).

'<http://www.w3.org/2000/10/swap/list#member>'(A,B) :-
	when(nonvar(A),member(B,A)).

'<http://www.w3.org/2000/10/swap/list#rest>'([_|B],B).

'<http://www.w3.org/2000/10/swap/log#conjunction>'(X,Y) :-
	when(nonvar(X),(conjoin(X), findall(Z,graph(Z),L), clist(L,Y), retractall(graph(_)))).

'<http://www.w3.org/2000/10/swap/log#dtlit>'([literal(A,void),B],literal(A,type(B))).

'<http://www.w3.org/2000/10/swap/log#equalTo>'(X,X).

'<http://www.w3.org/2000/10/swap/log#implies>'(X,Y) :-
	pcl(X,Y,_), X \= true, Y \= answer(_), Y \= goal.

'<http://www.w3.org/2000/10/swap/log#includes>'(X,Y) :-
	when((nonvar(X), nonvar(Y)),includes(X,Y)).

'<http://www.w3.org/2000/10/swap/log#notEqualTo>'(X,Y) :-
	X \= Y.

'<http://www.w3.org/2000/10/swap/log#notIncludes>'(X,Y) :-
	when((nonvar(X), nonvar(Y)),\+'<http://www.w3.org/2000/10/swap/log#includes>'(X,Y)).

'<http://www.w3.org/2000/10/swap/log#semantics>'(X,Y) :-
	when((nonvar(X); nonvar(Y)),(nonvar(X), (fact('<http://www.w3.org/2000/10/swap/log#semantics>'(X,Y)), !;
	sub_atom(X,1,_,1,Z), n3_pcl(Z,semantics), (fact('<http://www.w3.org/2000/10/swap/log#semantics>'(X,Y)) -> true
	; Y = fail, assert(fact('<http://www.w3.org/2000/10/swap/log#semantics>'(X,Y)))))
	; nonvar(Y), X = Y, assert(fact('<http://www.w3.org/2000/10/swap/log#semantics>'(X,Y))))).

'<http://www.w3.org/2000/10/swap/log#uri>'(X,literal(Y,void)) :-
	when((nonvar(X); nonvar(Y)),(atom(X), sub_atom(X,1,_,1,U), concat_atom(['<',U,'>'],X), !, atom_codes(U,Y)
	; nonvar(Y), atom_codes(U,Y), concat_atom(['<',U,'>'],X))).

'<http://www.w3.org/2000/10/swap/math#absoluteValue>'(X,Z) :-
	when(ground(X),(getnumber(X,U), Z is abs(U))).

'<http://www.w3.org/2000/10/swap/math#atan2>'([X,Y],Z) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), Z is atan(U/V))).

'<http://www.w3.org/2000/10/swap/math#cos>'(X,Z) :-
	when((ground(X); ground(Z)),(getnumber(X,U), Z is cos(U), !; getnumber(Z,W), X is acos(W))).

'<http://www.w3.org/2000/10/swap/math#cosh>'(X,Z) :-
	when((ground(X); ground(Z)),(getnumber(X,U), Z is cosh(U), !; getnumber(Z,W), X is acosh(W))).

'<http://www.w3.org/2000/10/swap/math#degrees>'(X,Z) :-
	when((ground(X); ground(Z)),(getnumber(X,U), Z is U*180/pi, !; getnumber(Z,W), X is W*pi/180)).

'<http://www.w3.org/2000/10/swap/math#difference>'([X,Y],Z) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), Z is U-V)).

'<http://www.w3.org/2000/10/swap/math#equalTo>'(X,Y) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), U =:= V)).

'<http://www.w3.org/2000/10/swap/math#exponentiation>'([X,Y],Z) :-
	when((ground([X,Y]); ground([X,Z])),(getnumber(X,U), (getnumber(Y,V), Z is U**V, !;
	getnumber(Z,W), W =\= 0, U =\= 0, Y is log(W)/log(U)))).

'<http://www.w3.org/2000/10/swap/math#greaterThan>'(X,Y) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), U > V)).

'<http://www.w3.org/2000/10/swap/math#integerQuotient>'([X,Y],Z) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), Z is truncate(floor(U/V)))).

'<http://www.w3.org/2000/10/swap/math#lessThan>'(X,Y) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), U < V)).

'<http://www.w3.org/2000/10/swap/math#memberCount>'(X,Y) :-
	when(nonvar(X),length(X,Y)).

'<http://www.w3.org/2000/10/swap/math#negation>'(X,Z) :-
	when((ground(X); ground(Z)),(getnumber(X,U), Z is -U, !; getnumber(Z,W), X is -W)).

'<http://www.w3.org/2000/10/swap/math#notEqualTo>'(X,Y) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), U =\= V)).

'<http://www.w3.org/2000/10/swap/math#notGreaterThan>'(X,Y) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), U =< V)).

'<http://www.w3.org/2000/10/swap/math#notLessThan>'(X,Y) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), U >= V)).

'<http://www.w3.org/2000/10/swap/math#product>'(X,Z) :-
	when(ground(X),product(X,Z)).

'<http://www.w3.org/2000/10/swap/math#quotient>'([X,Y],Z) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), Z is U/V)).

'<http://www.w3.org/2000/10/swap/math#remainder>'([X,Y],Z) :-
	when(ground([X,Y]),(getnumber(X,U), getnumber(Y,V), Z is U-V*truncate(floor(U/V)))).

'<http://www.w3.org/2000/10/swap/math#rounded>'(X,Z) :-
	when(ground(X),(getnumber(X,U), Z is truncate(round(U)))).

'<http://www.w3.org/2000/10/swap/math#sin>'(X,Z) :-
	when((ground(X); ground(Z)),(getnumber(X,U), Z is sin(U), !; getnumber(Z,W), X is asin(W))).

'<http://www.w3.org/2000/10/swap/math#sinh>'(X,Z) :-
	when((ground(X); ground(Z)),(getnumber(X,U), Z is sinh(U), !; getnumber(Z,W), X is asinh(W))).

'<http://www.w3.org/2000/10/swap/math#sum>'(X,Z) :-
	when(ground(X),sum(X,Z)).

'<http://www.w3.org/2000/10/swap/math#tan>'(X,Z) :-
	when((ground(X); ground(Z)),(getnumber(X,U), Z is tan(U), !; getnumber(Z,W), X is atan(W))).

'<http://www.w3.org/2000/10/swap/math#tanh>'(X,Z) :-
	when((ground(X); ground(Z)),(getnumber(X,U), Z is tanh(U), !; getnumber(Z,W), X is atanh(W))).

'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'([X|Y],X) :-
	'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'([X|Y],'<http://www.w3.org/1999/02/22-rdf-syntax-ns#List>').

'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'([X|Y],Y) :-
	'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'([X|Y],'<http://www.w3.org/1999/02/22-rdf-syntax-ns#List>').

'<http://www.w3.org/2000/10/swap/string#concatenation>'(X,literal(Y,void)) :-
	when(ground(X),(findall(S,member(literal(S,void),X),Z), flatten(Z,Y))).

'<http://www.w3.org/2000/10/swap/string#contains>'(literal(X,void),literal(Y,void)) :-
	when(ground([X,Y]),sublist(Y,X)).

'<http://www.w3.org/2000/10/swap/string#containsIgnoringCase>'(literal(X,void),literal(Y,void)) :-
	when(ground([X,Y]),(atom_codes(S,X), atom_codes(T,Y), downcase_atom(S,U), downcase_atom(T,V), sub_atom(U,_,_,_,V))).

'<http://www.w3.org/2000/10/swap/string#endsWith>'(literal(X,void),literal(Y,void)) :-
	when(ground([X,Y]),append(_,Y,X)).

'<http://www.w3.org/2000/10/swap/string#equalIgnoringCase>'(literal(X,void),literal(Y,void)) :-
	when(ground([X,Y]),(atom_codes(S,X), atom_codes(T,Y), downcase_atom(S,U), downcase_atom(T,V), U == V)).

'<http://www.w3.org/2000/10/swap/string#greaterThan>'(X,Y) :-
	when(ground([X,Y]),X @> Y).

'<http://www.w3.org/2000/10/swap/string#lessThan>'(X,Y) :-
	when(ground([X,Y]),X @< Y).

'<http://www.w3.org/2000/10/swap/string#matches>'(literal(X,void),literal(Y,void)) :-
	when(ground([X,Y]),tokenize(Y,X,_)), !.

'<http://www.w3.org/2000/10/swap/string#notEqualIgnoringCase>'(X,Y) :-
	when(ground([X,Y]),\+'<http://www.w3.org/2000/10/swap/string#equalIgnoringCase>'(X,Y)).

'<http://www.w3.org/2000/10/swap/string#notGreaterThan>'(X,Y) :-
	when(ground([X,Y]),X @=< Y).

'<http://www.w3.org/2000/10/swap/string#notLessThan>'(X,Y) :-
	when(ground([X,Y]),X @>= Y).

'<http://www.w3.org/2000/10/swap/string#notMatches>'(X,Y) :-
	when(ground([X,Y]),\+'<http://www.w3.org/2000/10/swap/string#matches>'(X,Y)).

'<http://www.w3.org/2000/10/swap/string#startsWith>'(literal(X,void),literal(Y,void)) :-
	when(ground([X,Y]),append(Y,_,X)).

'<http://www.w3.org/2000/10/swap/time#day>'(literal(X,_),literal(Y,void)) :-
	when(ground(X),(atom_codes(U,X), sub_atom(U,8,2,_,V), atom_codes(V,Y))).

'<http://www.w3.org/2000/10/swap/time#month>'(literal(X,_),literal(Y,void)) :-
	when(ground(X),(atom_codes(U,X), sub_atom(U,5,2,_,V), atom_codes(V,Y))).

'<http://www.w3.org/2000/10/swap/time#year>'(literal(X,_),literal(Y,void)) :-
	when(ground(X),(atom_codes(U,X), sub_atom(U,0,4,_,V), atom_codes(V,Y))).


% -------
% support
% -------

def_pfx('math:','<http://www.w3.org/2000/10/swap/math#>').
def_pfx('e:','<http://eulersharp.sourceforge.net/2003/03swap/log-rules#>').
def_pfx('list:','<http://www.w3.org/2000/10/swap/list#>').
def_pfx('xsd:','<http://www.w3.org/2001/XMLSchema#>').
def_pfx('log:','<http://www.w3.org/2000/10/swap/log#>').
def_pfx('r:','<http://www.w3.org/2000/10/swap/reason#>').
def_pfx('rdfs:','<http://www.w3.org/2000/01/rdf-schema#>').
def_pfx('fn:','<http://www.w3.org/2005/xpath-functions#>').
def_pfx('time:','<http://www.w3.org/2000/10/swap/time#>').
def_pfx('rdf:','<http://www.w3.org/1999/02/22-rdf-syntax-ns#>').
def_pfx('var:','<http://localhost/var#>').
def_pfx('str:','<http://www.w3.org/2000/10/swap/string#>').
def_pfx('owl:','<http://www.w3.org/2002/07/owl#>').
def_pfx('n3:','<http://www.w3.org/2004/06/rei#>').

put_pfx(_,URI) :-
	concat_atom(['<',URI,'>'],U), pfx(_,U), !.
put_pfx(_,URI) :-
	concat_atom(['<',URI,'>'],U), def_pfx(Pf,U), !, assert(pfx(Pf,U)).
put_pfx(Pf,URI) :-
	concat_atom(['<',URI,'>'],U), fresh_pf(Pf,Pff), assert(pfx(Pff,U)).

fresh_pf(Pf,Pfx) :-
	atom_concat(Pf,':',Pfx), \+pfx(Pfx,_), !.
fresh_pf(_,Pfx) :-
	gensym(ns,Pfn), fresh_pf(Pfn,Pfx).

resolve_uri('',A,A) :- !.
resolve_uri(A,B,A) :-
	\+sub_atom(B,_,1,_,':'), !.	
resolve_uri(A,B,C) :-
	global_url(A,B,C).

cnt(A) :-
	nb_getval(A,B), C is B+1, nb_setval(A,C).

within_span(A) :-
	(nb_getval(limit,B) -> (B < A -> nb_setval(limit,A); true); nb_setval(limit,A)), span(A).

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

dlist([],false) :- !.
dlist([A],A) :- !.
dlist([A|B],(A;C)) :-
	dlist(B,C).

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

cappend((A,B),C,(A,D)) :-
	cappend(B,C,D).
cappend(A,B,(A,B)) :-
	A \= (_,_).

clast((_,X),I) :- !,
	clast(X,I).
clast(I,I).

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
	findall(U,(cmember(U,X), \+((cmember(V,Y), unif(U,V)))),W), (W = [] -> Z = true; clist(W,G), Z = G).

intersection([X],X) :- !.
intersection([true|_],true) :- !.
intersection([X|Y],Z) :-
	intersection(Y,I), (I = true -> Z = true; findall(U,(cmember(U,X), cmember(V,I), unif(U,V)),W), clist(W,Z)).

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

inconsistent(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>')|B]) :-
	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'),B), !.
inconsistent(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>')|B]) :-
	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'),B), !.
inconsistent([_|B]) :-
	inconsistent(B).

inverse('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'),
	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>')) :- !.
inverse('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'),
	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>')).

bnet :-
	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'([A|B],_), sort(B,C),
	findall(Y,('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'([A|X],Y), sort(X,C)),L),
	sum(L,S), length(L,N), Z is S/N, \+bcnd([A|B],_), assert(bcnd([A|B],Z)),
	inverse(A,D), \+bcnd([D|B],_), E is 1-Z, assert(bcnd([D|B],E)), fail;
	bcnd(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,_)|B],_),
	(\+bvar(A), assert(bvar(A)); true),
	member('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(C,_),B),
	\+bref(C,A), assert(bref(C,A)), \+bvar(C), assert(bvar(C)), fail; true.

bval('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>').
bval('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>').

brel('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,_),'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(B,_)) :-
	bref(A,B), !.
brel(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(B,_)) :-
	bref(C,B), brel(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(C,_)).

bpar([],[]) :- !.
bpar(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,_)|B],[A|C]) :-
	bpar(B,C).

bget(A,B,1.0) :-
	memberchk(A,B), !.
bget('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'),B,0.0) :-
	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'),B), !.
bget('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'),B,C) :-
	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'),B), !,
	C is 0.0; !,
	bget('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'),B,D),
	C is 1-D.
bget(A,B,C) :-
	bgot(A,B,C) -> true; (member(X,B), brel(A,X), member(G,B),
	findall(Y,(member(Z,[A|B]), brel(G,Z)),[]), delete(B,G,H), !,
	bget(G,[A|H],U), bget(A,H,V), bget(G,H,W), (W < 1e-15 -> C is 0.5; E is U*V/W, bmin([E,1.0],C));
	findall([Z,Y],(bcnd([A|O],P), bcon(O,B,Q), Z is P*Q, bpar(O,Y)),L),
	findall(Z,(member([_,Z],L)),N), list_to_set(N,I),
	findall(Z,(member(Y,I), findall(P,(member([P,Y],L)),Q), sum(Q,R), length(Q,S), length(Y,T),
	(Q = [] -> Z is 0.0; D is 2**(T-ceiling(log(S)/log(2))), (D < 1 -> Z is R*D; Z is R))),J),
	(J = [] -> C is 0.0; bmax(J,C))), assert(bgot(A,B,C)).

bcon([],_,1.0) :- !.
bcon(_,B,0.5) :-
	inconsistent(B), !.
bcon([A|B],C,D) :-
	bget(A,C,E), bcon(B,[A|C],F), D is E*F.

repeat(_).
repeat(N) :- N > 1, N1 is N-1, repeat(N1).	

write_to_chars_swi(Term, Codes) :-
	format(codes(Codes), '~w', Term).

read_from_chars_swi("", end_of_file) :- !.
read_from_chars_swi(List, Term) :-
	atom_to_term(List, Term, _).

getvars(A,B) :-
	atomic(A), !, (atom(A), sub_atom(A,0,4,_,'var:') -> B = [A]; B = []).
getvars([],[]) :- !.
getvars([A|B],C) :-
	getvars(A,D), getvars(B,E), append(D,E,F), !, list_to_set(F,C).
getvars(A,B) :-
	A =.. C, getvars(C,B).

getnumber(A,A) :-
	ground(A), number(A), !.
getnumber(literal(A,type('<http://www.w3.org/2001/XMLSchema#dateTime>')),B) :- !,
	ground(A), phrase(datetime(B),A).
getnumber(literal(A,type('<http://www.w3.org/2001/XMLSchema#duration>')),B) :- !,
	ground(A), phrase(duration(B),A).
getnumber(literal(A,_),B) :-
	ground(A), number_codes(B,A).

datetime(A) --> int(B), "-", int(C), "-", int(D), "T", int(E), ":", int(F), ":", decimal(G), timezone(H),
	{I is -H, date_time_stamp(date(B,C,D,E,F,G,I,-,-),A)}.

timezone(A) -->
	int(B), !, ":", int(C), {A is B*3600+C*60}.
timezone(0) -->
	"Z", !.
timezone(0) -->
	[].

duration(A) -->
	"P", years(B), months(C), days(D), dtime(E), {A is B*31556952+C*2629746+D*86400.0+E}.

dtime(A) -->
	"T", !, hours(B), minutes(C), seconds(D), {A is B*3600+C*60+D}.
dtime(0) -->
	[].

years(A) -->
	int(A), "Y".
years(0) -->
	[].

months(A) -->
	int(A), "M".
months(0) -->
	[].

days(A) -->
	int(A), "D".
days(0) -->
	[].

hours(A) -->
	int(A), "H".
hours(0) -->
	[].

minutes(A) -->
	int(A), "M".
minutes(0) -->
	[].

seconds(A) -->
	decimal(A), "S".
seconds(0) -->
	[].

int(A) -->
	sgn(B), digit(C), digits(D), {number_chars(A,[B,C|D])}.

decimal(A) -->
	sgn(B), digit(C), digits(D), fraction(E), {append([B,C|D],E,F), number_chars(A,F)}.

sgn(0'+) -->
	"+".
sgn(0'-) -->
	"-".
sgn(0'+) -->
	[].

fraction([0'.,A|B]) -->
          ".", !, digit(A), digits(B).
fraction([]) -->
	[].

digits([A|B]) -->
	digit(A), digits(B).
digits([]) -->
	[].

digit(A) -->
	[A], {code_type(A,digit)}.


% ----- Regular Expressions from http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html -----
tokenize(RE,Input,Output) :-
	re(Parsed_RE,RE,[]), tokenize2(Parsed_RE,Input,Output).

tokenize2(_P_RE,[],[]).
tokenize2(P_RE,Input,Output) :-
	rematch1(P_RE,Input,Unmatched,SelStrings), names(Tokens,SelStrings),
	tokenize2(P_RE,Unmatched,MoreTokens), append(Tokens,MoreTokens,Output).

names([],[]).
names([Sym1|MoreSymbols],[Str1|MoreStrings]) :-
	name(Sym1,Str1), names(MoreSymbols,MoreStrings).

rematch1(union(RE1,_RE2),S,U,Selected) :-
	rematch1(RE1,S,U,Selected).
rematch1(union(_RE1,RE2),S,U,Selected) :-
	rematch1(RE2,S,U,Selected).
rematch1(conc(RE1,RE2),S,U,Selected) :-
	rematch1(RE1,S,U1,Sel1), rematch1(RE2,U1,U,Sel2), append(Sel1,Sel2,Selected).
rematch1(star(RE),S,U,Selected) :-
	rematch1(RE,S,U1,Sel1), rematch1(star(RE),U1,U,Sel2), append(Sel1,Sel2,Selected).
rematch1(star(_RE),S,S,[]).
rematch1(plus(RE),S,U,Selected) :-
	rematch1(RE,S,U1,Sel1), rematch1(star(RE),U1,U,Sel2), append(Sel1,Sel2,Selected).
rematch1(group(RE),S,U,Selected) :-
	rematch1(RE,S,U,Sel1), append(P,U,S), append(Sel1,[P],Selected).
rematch1(any,[_C1|U],U,[]).
rematch1(char(C),[C|U],U,[]).
rematch1(eos,[],[],[]).
rematch1(negSet(Set),[C|U],U,[]) :-
	\+charSetMember(C,Set).
rematch1(posSet(Set),[C|U],U,[]) :-
	charSetMember(C,Set).

charSetMember(C,[char(C)|_]).
charSetMember(C,[range(C1,C2)|_]) :-
	C1 =< C, C =< C2.
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


% ---------------------------------------------------------------
% N3 Parser
% according to http://www.w3.org/2000/10/swap/grammar/n3-ietf.txt
% inspired by http://code.google.com/p/km-rdf/wiki/Henry
% ---------------------------------------------------------------

barename(BareName) -->
	[name(BareName)].

barename_csl([BareName|Tail]) -->
	barename(BareName), !, barename_csl_tail(Tail).
barename_csl([]) -->
	[].

barename_csl_tail([BareName|Tail]) -->
	[','], !, barename(BareName), barename_csl_tail(Tail).
barename_csl_tail([]) -->
	[].

boolean(true) -->
	['@',name('true')], !.
boolean(true) -->
	[name('true')], !.
boolean(false) -->
	['@',name('false')].
boolean(false) -->
	[name('false')].

declaration -->
	['@',name(base)], !, explicituri(U),
	{base_uri(V), resolve_uri(U,V,URI), retractall(base_uri(_)), assert(base_uri(URI))}.
declaration -->
	['@',name(keywords)], !, barename_csl(List),
	{retractall(keywords(_)), assert(keywords(List))}.
declaration -->
	['@',name(prefix)], prefix(Prefix), explicituri(U),
	{base_uri(V), resolve_uri(U,V,URI), retractall(ns(Prefix,_)), assert(ns(Prefix,URI)), put_pfx(Prefix,URI)}.

document(Triples) -->
	statements_optional(Triples), !.
document([]) -->
	{nb_getval(past_triples,PT), throw(invalid_document(past_triples(PT)))}.

dtlang(lang(Lang)) -->
	['@'], !, langcode(Lang).
dtlang(type(Datatype)) -->
	['^','^'], !, symbol(Datatype).
dtlang(void) -->
	[].

existential -->
	['@',name(forSome)], !, symbol_csl(Symbols),
	{nb_getval(fdepth,D), forall(member(S,Symbols),(gensym(q,Q), asserta(qevar(S,Q,D))))}.

explicituri(ExplicitURI) -->
	[relative_uri(ExplicitURI)].

expression(Node,T) -->
	pathitem(N1,T1), pathtail(N1,P,N2,T2),
	{append(T1,T2,T3),
	(P = '\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#disjunction>\'' -> dlist(N1,Node), T = []; Node = N2, T = T3),
	(keywords(List), memberchk(Node,List) -> nb_getval(past_triples,PT), throw(invalid_keyword_use(Node,past_triples(PT))); true)}.

formulacontent(Formula) -->
	statementlist(List),
	{clist(List,Formula)}.

langcode(Langcode) -->
	[name(Name)],
	{concat_atom(['\'',Name,'\''],Langcode)}.

literal(Literal,DtLang) -->
	string(Literal), dtlang(DtLang).

numericliteral(Num) -->
	[numeric(_,NumC)],
	{number_codes(Num,NumC)}.

object(Node,Triples) -->
	expression(Node,Triples).

objecttail(Subject,Verb,[Triple|T]) -->
	[','], !, object(Object,Triples), objecttail(Subject,Verb,Tail),
	{append(Triples,Tail,T),
	(atom(Verb) -> true; nb_getval(past_triples,PT), throw(unexpected_atom(Verb,past_triples(PT)))),
	(Verb = isof(V) -> (\+sub_atom(V,0,1,_,'_') -> Triple =.. [V,Object,Subject]; Triple = varpred(Object,V,Subject))
	; (\+sub_atom(Verb,0,1,_,'_') -> Triple =.. [Verb,Subject,Object]; Triple = varpred(Subject,Verb,Object)))}.
objecttail(_,_,[]) -->
	[].

pathitem([],[]) -->
	symbol('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>\''), !.
pathitem(Name,[]) -->
	symbol(S), !,
	{((quvar(S,N,0), !; qevar(S,N,1)) -> (nb_getval(fdepth,0) -> concat_atom(['\'var:',N,'\''],Name); atom_concat('_',N,Name))
	; (sub_atom(S,0,1,_,'\'') -> sub_atom(S,1,_,1,B); true), (current_predicate(B/2) -> true; dynamic(B/2)), Name = S)}.
pathitem(VarID,[]) -->
	[uvar(Variable)], !,
	{atom_concat('_',Variable,VarID)}.
pathitem(Number,[]) -->
	numericliteral(Number), !.
pathitem(literal(Literal,DtLang),[]) -->
	literal(Literal,DtLang), !.
pathitem(Boolean,[]) -->
	boolean(Boolean), !.
pathitem([Head|Tail],[]) -->
	['['], pathitem('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>\'',_), pathitem(Head,_), [';'],
	pathitem('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>\'',_), pathitem(Tail,_), [']'], !.
pathitem(BNode,Triples) -->
	['['], !, {assert(nodepth), gensym(e,S), (nb_getval(fdepth,0) -> concat_atom(['\'var:',S,'\''],BNode); atom_concat('_',S,BNode))},
	propertylist(BNode,Triples), {retract(nodepth)}, [']'].
pathitem(List,Triples) -->
	['('], !, pathlist(List,Triples), [')'].
pathitem(Node,[]) -->
	['{'], {(nodepth -> true; nb_getval(fdepth,I), J is I+1, nb_setval(fdepth,J))}, formulacontent(Node),
	{(nodepth -> true; retractall(quvar(_,_,J)), retractall(qevar(_,_,J)), retractall(evar(_,_,J)), nb_setval(fdepth,I))}, ['}'].

pathlist([Node|Rest],Triples) -->
	expression(Node,T), !, pathlist(Rest,Tail),
	{append(T,Tail,Triples)}.
pathlist([],[]) -->
	[].

pathtail(Node,Verb,PNode,[Triple|Triples]) -->
	['!'], !, pathitem(Verb,Triples2),
	{gensym(e,S), (nb_getval(fdepth,0) -> concat_atom(['\'var:',S,'\''],BNode); atom_concat('_',S,BNode)),
	(atom(Verb) -> true; nb_getval(past_triples,PT), throw(unexpected_atom(Verb,past_triples(PT)))),
	(Verb = isof(V) -> (\+sub_atom(V,0,1,_,'_') -> Triple =.. [V,BNode,Node]; Triple = varpred(BNode,V,Node))
	; (\+sub_atom(Verb,0,1,_,'_') -> Triple =.. [Verb,Node,BNode]; Triple = varpred(Node,Verb,BNode)))},
	pathtail(BNode,_,PNode,Tail),
	{append(Triples2,Tail,Triples)}.
pathtail(Node,Verb,PNode,[Triple|Triples]) -->
	['^'], !, pathitem(Verb,Triples2),
	{gensym(e,S), (nb_getval(fdepth,0) -> concat_atom(['\'var:',S,'\''],BNode); atom_concat('_',S,BNode)),
	(atom(Verb) -> true; nb_getval(past_triples,PT), throw(unexpected_atom(Verb,past_triples(PT)))),
	(Verb = isof(V) -> (\+sub_atom(V,0,1,_,'_') -> Triple =.. [V,Node,BNode]; Triple = varpred(Node,V,BNode))
	; (\+sub_atom(Verb,0,1,_,'_') -> Triple =.. [Verb,BNode,Node]; Triple = varpred(BNode,Verb,Node)))},
	pathtail(BNode,_,PNode,Tail),
	{append(Triples2,Tail,Triples)}.
pathtail(Node,void,Node,[]) -->
	[].

prefix(Prefix) -->
	[Prefix:''].

propertylist(Subject,[Triple|Triples]) -->
	verb(Verb,Triples1), !, object(Object,Triples2), objecttail(Subject,Verb,Triples3), propertylisttail(Subject,Triples4),
	{append(Triples1,Triples2,Triples12), append(Triples12,Triples3,Triples123), append(Triples123,Triples4,Triples),
	(Verb = isof(V) -> (\+sub_atom(V,0,1,_,'_') -> Triple =.. [V,Object,Subject]; Triple = varpred(Object,V,Subject))
	; (\+sub_atom(Verb,0,1,_,'_') -> Triple =.. [Verb,Subject,Object]; Triple = varpred(Subject,Verb,Object)))}.
propertylist(_,[]) -->
	[].

propertylisttail(Subject,Triples) -->
	[';'], !, propertylist(Subject,Triples).
propertylisttail(_,[]) -->
	[].

qname(QName) -->
	[var:Name],
	{concat_atom(['\'var:',Name,'\''],QName)}, !.
qname(URI) -->
	[NS:Name],
	{(ns(NS,Base) -> concat_atom(['\'<',Base,Name,'>\''],URI); throw(no_prefix_directive(NS)))}, !.

simpleStatement(Triples) -->
	subject(Subject,Triples1), propertylist(Subject,Triples2),
	{append(Triples1,Triples2,Triples)}.

statement([]) -->
	declaration, !.
statement([]) -->
	universal, !.
statement([]) -->
	existential, !.
statement(Statement) -->
	simpleStatement(Statement).

statementlist(Triples) -->
	statement(Tr), !, statementtail(T),
	{append(Tr,T,Triples)}.
statementlist([]) -->
	[].

statements_optional(Triples) -->
	statement(Tr), ['.'], !, {nb_setval(past_triples,Tr)}, statements_optional(T),
	{append(Tr,T,Triples)}.
statements_optional([]) -->
	[].

statementtail(T) -->
	['.'], !, statementlist(T).
statementtail([]) -->
	[].

string(Literal) -->
	[literal(Literal)].

subject(Node,Triples) -->
	expression(Node,Triples).

symbol(Name) -->
	explicituri(U), !,
	{base_uri(V), resolve_uri(U,V,W), concat_atom(['\'<',W,'>\''],Name)}.
symbol(Name) -->
	qname(Name), !.
symbol(Name) -->
	[name(N)], !,
	{(keywords(List) -> (memberchk(N,List) -> Name = N; ns('',Base), concat_atom(['\'<',Base,N,'>\''],Name))
	; (memberchk(N,[a,is,of,true,false]) -> Name = N; nb_getval(past_triples,PT), throw(invalid_keyword(N,past_triples(PT)))))}.
symbol(Name) -->
	[bnode(N)],
	{nb_getval(fdepth,D), (evar(N,S,D) -> true; atom_concat(N,'_',M), gensym(M,S), assert(evar(N,S,D))),
	(nb_getval(fdepth,0) -> concat_atom(['\'var:',S,'\''],Name); atom_concat('_',S,Name))}.

symbol_csl([Symbol|Tail]) -->
	symbol(Symbol), !, symbol_csl_tail(Tail).
symbol_csl([]) -->
	[].

symbol_csl_tail([Symbol|T]) -->
	[','], !, symbol(Symbol), symbol_csl_tail(T).
symbol_csl_tail([]) -->
	[].

universal -->
	['@',name(forAll)], !, symbol_csl(Symbols),
	{nb_getval(fdepth,D), forall(member(S,Symbols),(gensym(q,Q), asserta(quvar(S,Q,D))))}.

verb('\'<http://www.w3.org/2000/10/swap/log#implies>\'',[]) -->
	['=','>'], !.
verb('\'<http://www.w3.org/2002/07/owl#sameAs>\'',[]) -->
	['='], !.
verb(isof('\'<http://www.w3.org/2000/10/swap/log#implies>\''),[]) -->
	['<','='], !.
verb('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>\'',[]) -->
	['@',name(a)], !.
verb('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>\'',[]) -->
	[name(a)], !.
verb(Node,Triples) -->
	['@',name(has)], !, expression(Node,Triples).
verb(isof(Node),Triples) -->
	['@',name(is)], !, expression(Node,Triples), ['@',name(of)].
verb(isof(Node),Triples) -->
	[name(is)], !, expression(Node,Triples), [name(of)].
verb(Node,Triples) -->
	expression(Node,Triples).


% tokenizer
turtle_tokens(In,List) :-
	get_code(In,C0), turtle_token(C0,In,C1,Tok1),
	(Tok1 == end_of_file -> List = []; List = [Tok1|Tokens], turtle_tokens(C1,In,Tokens)).

turtle_tokens(C0,In,List) :-
	(turtle_token(C0,In,C1,H) -> true; throw(illegal_token(C0))),
	(H == end_of_file -> List = []; List = [H|T], turtle_tokens(C1,In,T)).

turtle_token(-1,_,-1,end_of_file) :- !.
turtle_token(0'.,In,C,'.') :- !,
	get_code(In,C).
turtle_token(0'#,In,C,Token) :- !,
	get_code(In,C1), skip_line(C1,In,C2), turtle_token(C2,In,C,Token).
turtle_token(WS,In,C,Token) :-
	turtle_ws(WS), !, get_code(In,C1), turtle_token(C1,In,C,Token).
turtle_token(C0,In,C,Number) :-
	0'0 =< C0, C0 =< 0'9, !, turtle_number(C0,In,C,Number).
turtle_token(0'-,In,C,Number) :- !,
	turtle_number(0'-,In,C,Number).
turtle_token(0'+,In,C,Number) :- !,
	turtle_number(0'+,In,C,Number).
turtle_token(0'",In,C,literal(Codes)) :- !,
	(peek_code(In,0'") -> get_code(In,0'"),
	(peek_code(In,0'") -> get_code(In,0'"), get_code(In,C1), turtle_dq_string(C1,In,C,Codes) ; get_code(In,C), Codes = [])
	; get_code(In,C1), turtle_string(C1,In,C,Codes)).
turtle_token(0'?,In,C,uvar(Name)) :- !,
	get_code(In,C0), (name(C0,In,C,Name) -> true; C = C0, Name = '').
turtle_token(0'_,In,C,bnode(Name)) :-
	peek_code(In,0':), !, get_code(In,_), get_code(In,C0), (name(C0,In,C,Name) -> true; C = C0, Name = '').
turtle_token(0'<,In,C,relative_uri(URI)) :-
	peek_code(In,C1), C1 \== 0'=, !, get_code(In,C1), uri_chars(C1,In,C,Codes), atom_codes(URI,Codes).
turtle_token(0':,In,C,Token) :- !,
	get_code(In,C0), (name(C0,In,C,Name) -> Token = '':Name; Token = '':'', C = C0).
turtle_token(C0,In,C,Token) :-
	name(C0,In,C1,Name), !, (C1 == 0': -> get_code(In,C2),
	(name(C2,In,C,Name2) -> Token = (Name:Name2); Token = (Name:''), C = C2); Token = name(Name), C = C1).
turtle_token(Punct,In,C,P) :-
	punctuation(Punct,P), !, get_code(In,C).

turtle_number(0'-,In,CN,numeric(T,[0'-|Codes])) :- !,
	get_code(In,C0), turtle_number_nn(C0,In,CN, numeric(T,Codes)).
turtle_number(0'+,In,CN,numeric(T,[0'+|Codes])) :- !,
	get_code(In,C0), turtle_number_nn(C0,In,CN,numeric(T,Codes)).
turtle_number(C0,In,CN,Value) :-
	turtle_number_nn(C0,In,CN,Value).

turtle_number_nn(C,In,CN,numeric(Type,Codes)) :-
	turtle_integer_codes(C,In,CN0,Codes,T0),
	(CN0 == 0'., peek_code(In,C0), 0'0 =< C0, C0 =< 0'9 -> T0 = [CN0|T1], get_code(In,C1),turtle_integer_codes(C1,In,CN1,T1,T2),
	(exponent(CN1,In,CN,T2) -> Type = double; CN = CN1, T2 = [], Type = decimal)
	; exponent(CN0,In,CN,T0) -> Type = double ; T0 = [], CN = CN0, Type = integer).

turtle_integer_codes(C0,In,CN,[C0|T0],T) :-
	0'0 =< C0, C0 =< 0'9, !, get_code(In,C1), turtle_integer_codes(C1,In,CN,T0,T).
turtle_integer_codes(CN,_,CN,T,T).

exponent(C0,In,CN,[C0|T0]) :-
	e(C0), !, get_code(In,C1), optional_sign(C1,In,CN0,T0,T1), turtle_integer_codes(CN0,In,CN,T1,[]).

optional_sign(C0,In,CN,[C0|T],T) :-
	sign(C0), !, get_code(In,CN).
optional_sign(CN,_,CN,T,T).

e(0'e).
e(0'E).

sign(0'-).
sign(0'+).

turtle_dq_string(-1,_,_,[]) :- !,
	throw(unexpected_end_of_input).
turtle_dq_string(0'",In,C,[]) :-
	get_code(In,0'"), get_code(In,0'"), !, get_code(In,C).
turtle_dq_string(0'\\,In,C,[H|T]) :-
	get_code(In,C1), string_escape(C1,In,C2,H), !, turtle_dq_string(C2,In,C,T).
turtle_dq_string(C0,In,C,[C0|T]) :-
	get_code(In,C1), turtle_dq_string(C1,In,C,T).

turtle_string(-1,_,_,[]) :- !,
	throw(unexpected_end_of_input).
turtle_string(0'",In,C,[]) :- !,
	get_code(In,C).
turtle_string(0'\\,In,C,[H|T]) :-
	get_code(In,C1), string_escape(C1,In,C2,H), !, turtle_string(C2,In,C,T).
turtle_string(C0,In,C,[C0|T]) :-
	get_code(In,C1), turtle_string(C1,In,C,T).

string_escape(0'n,In,C,0'\n) :- !,
	get_code(In,C).
string_escape(0'",In,C,0'") :- !,
	get_code(In,C).
string_escape(0'\\,In,C,0'\\) :- !,
	get_code(In,C).
string_escape(0't,In,C,0'\t) :- !,
	get_code(In,C).
string_escape(0'r,In,C,0'\r) :- !,
	get_code(In,C).
string_escape(0'u,In,C,Code) :- !,
	get_hhhh(In,Code), get_code(In,C).
string_escape(0'U,In,C,Code) :-
	get_hhhh(In,Code0), get_hhhh(In,Code1), Code is Code0 << 16 + Code1, get_code(In,C).

get_hhhh(In,Code) :-
	get_code(In,C1), code_type(C1,xdigit(D1)),
	get_code(In,C2), code_type(C2,xdigit(D2)),
	get_code(In,C3), code_type(C3,xdigit(D3)),
	get_code(In,C4), code_type(C4,xdigit(D4)), Code is D1<<12+D2<<8+D3<<4+D4.

language(C0,In,C,[C0|Codes]) :-
	code_type(C0,lower), get_code(In,C1), lwr_word(C1,In,C2,Codes,Tail), sub_langs(C2,In,C,Tail,[]).

lwr_word(C0,In,C,[C0|T0],T) :-
	code_type(C0,lower), !, get_code(In,C1), lwr_word(C1,In,C,T0,T).
lwr_word(C,_,C,T,T).

sub_langs(0'-,In,C,[0'-,C1|Codes],T) :-
	get_code(In,C1), lwrdig(C1), !, get_code(In,C2), lwrdigs(C2,In,C3,Codes,Tail), sub_langs(C3,In,C,Tail,T).
sub_langs(C,_,C,T,T).

lwrdig(C) :-
	code_type(C,lower), !.
lwrdig(C) :-
	code_type(C,digit).

lwrdigs(C0,In,C,[C0|T0],T) :-
	lwrdig(C0), !, get_code(In,C1), lwr_word(C1,In,C,T0,T).
lwrdigs(C,_,C,T,T).

uri_chars(0'>,In,C,[]) :- !,
	get_code(In,C).
uri_chars(0'\\,In,C,[H|T]) :- !,
	get_code(In,C1), string_escape(C1,In,C2,H), uri_chars(C2,In,C,T).
uri_chars(-1,_,_,_) :- !,
	fail.
uri_chars(C0,In,C,[C0|T]) :-
	get_code(In,C1), uri_chars(C1,In,C,T).

name(C0,In,C,Atom) :-
	name_start_char(C0), get_code(In,C1), name_chars(C1,In,C,T), atom_codes(Atom,[C0|T]).

name_chars(C0,In,C,[C0|T]) :-
	name_char(C0), !, get_code(In,C1), name_chars(C1,In,C,T).
name_chars(C,_,C,[]).

name_start_char(C) :-
	code_type(C,csym), !.
name_start_char(C) :-
	0xC0 =< C, C =< 0xD6, !.
name_start_char(C) :-
	0xD8 =< C, C =< 0xF6, !.
name_start_char(C) :-
	0xF8 =< C, C =< 0x2FF, !.
name_start_char(C) :-
	0x370 =< C, C =< 0x37D, !.
name_start_char(C) :-
	0x37F =< C, C =< 0x1FFF, !.
name_start_char(C) :-
	0x200C =< C, C =< 0x200D, !.
name_start_char(C) :-
	0x2070 =< C, C =< 0x218F, !.
name_start_char(C) :-
	0x2C00 =< C, C =< 0x2FEF, !.
name_start_char(C) :-
	0x3001 =< C, C =< 0xD7FF, !.
name_start_char(C) :-
	0xF900 =< C, C =< 0xFDCF, !.
name_start_char(C) :-
	0xFDF0 =< C, C =< 0xFFFD, !.
name_start_char(C) :-
	0x10000 =< C, C =< 0xEFFFF.

name_char(C) :-
	name_start_char(C), !.
name_char(0'-) :- !.
name_char(D) :-
	code_type(D,digit), !.
name_char(0xB7) :- !.
name_char(C) :-
	0x0300 =< C, C =< 0x036F, !.
name_char(C) :-
	0x203F =< C, C =< 0x2040.

punctuation(0'(, '(').
punctuation(0'), ')').
punctuation(0'[, '[').
punctuation(0'], ']').
punctuation(0',, ',').
punctuation(0'@, '@').
punctuation(0':, ':').
punctuation(0';, ';').
punctuation(0'{, '{').
punctuation(0'}, '}').
punctuation(0'?,'?').
punctuation(0'!,'!').
punctuation(0'^,'^').
punctuation(0'=,'=').
punctuation(0'<,'<').
punctuation(0'>,'>').

skip_line(-1,_,-1) :- !.
skip_line(0xA,In,C) :- !,
	get_code(In,C).
skip_line(0xD,In,C) :- !,
	get_code(In,C).
skip_line(_,In,C) :-
	get_code(In,C1), skip_line(C1,In,C).

turtle_ws(0x9).
turtle_ws(0xA).
turtle_ws(0xD).
turtle_ws(0x20).
