% -------------------------------------------------------
% Naive Bayes Belief Network plugin for EYE -- Jos De Roo
% -------------------------------------------------------

% unplug the Full Bayes Belief Network builtin
:- abolish('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>'/2).

% plugin the Naive Bayes Belief Network builtin
'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>'(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,B)|C],D) :-
	within_scope(_),
	(	nb_getval(bnet,done)
	->	true
	;	bnet,
		nb_setval(bnet,done)
	),
	bvar(A),
	bval(B),
	nb('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,B),C,D).

nb(A,B,1.0) :-
	memberchk(A,B),
	!.
nb('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'),B,0.0) :-
	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'),B),
	!.
nb('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'),B,C) :-
	(	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'),B),
		!,
		C is 0.0
	;	!,
		nb('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A,'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'),B,D),
		C is 1-D
	).
nb(A,B,C) :-
	bcnd([A|B],C),
	!.
nb(A,B,C) :-
	(	bcnd([A,D],E),
		nb(D,B,F),
		inverse(D,G),
		bcnd([A,G],H),
		!,
		C is F*E+(1-F)*H
	;	nc(A,B,D),
		!,
		inverse(A,E),
		nc(E,B,F),
		C is 1/(1+2**(F-D))
	).

nc(_,[],0.0) :-
	!.
nc(A,[B|C],D) :-
	(	bcnd([B,A],E)
	;	inverse(B,F),
		bcnd([F,A],G),
		E is 1-G
	;	E is 1.0
	),
	!,
	(	E =:= 0
	->	I = epsilon
	;	I = E
	),
	nc(A,C,H),
	D is log(I)/log(2)+H.
