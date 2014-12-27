% --------------------------------
% FCM plugin for Eye -- Jos De Roo
% --------------------------------

'<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#pi>'([A,B],C) :-
	within_scope(_),
	(	nb_getval(fnet,done)
	->	true
	;	(	forall(
				(	'<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#mu>'([X,Y],Z)
				),
				(	(	fm(X)
					->	true
					;	assertz(fm(X))
					),
					assertz(pi(X,Y,Z))
				)
			),
			forall(
				(	'<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#sigma>'([X,Y],_)
				),
				(	(	fs(X)
					->	true
					;	assertz(fs(X))
					),
					(	fs(Y)
					->	true
					;	assertz(fs(Y))
					)
				)
			),
			repeat(20),
			fm(X),
			fs(Y),
			findall(I,
				(	'<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#sigma>'([P,Y],W),
					pi(X,P,M),
					I is (2*M-1)*(2*W-1)
				),
				L
			),
			(	L = []
			->	true
			;	sum(L,S),
				Z is 1/(1+exp(-S)),
				retractall(pi(X,Y,_)),
				assertz(pi(X,Y,Z))
			),
			fail
		;	nb_setval(fnet,done)
		)
	),
	pi(A,B,U),
	(	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#closure>'(_,'<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#pi>'([A,B],V))
	->	C = V
	;	C = U
	).

repeat(_).
repeat(N) :-
	N > 1,
	N1 is N-1,
	repeat(N1).
