% -----------------------------------------------------------------------------
% Euler Yet another proof Engine - EYE looking through N3 glasses -- Jos De Roo
% -----------------------------------------------------------------------------


% EYE [1] is a reasoning engine supporting the RGB [2] semantic web layers.
% It is a semibackward reasoner enhanced with Euler path [3] detection.
% Via N3 [4] it is interoperable with Cwm [5].
% The EYE test cases [6] and their results [7] support the development of EYE.
%
% See also README.Linux [8], README.Windows [9] and README.MacOSX [10].
%
%
% References
% ----------
%
%  [1] http://eulersharp.sourceforge.net/2006/02swap/eye-note.txt
%  [2] http://eulersharp.sourceforge.net/2006/02swap/rgb-note.txt
%  [3] http://mathworld.wolfram.com/KoenigsbergBridgeProblem.html
%  [4] http://www.w3.org/TeamSubmission/n3/
%  [5] http://www.w3.org/2000/10/swap/doc/cwm
%  [6] http://eulersharp.sourceforge.net/2006/02swap/etc.sh
%  [7] http://eulersharp.sourceforge.net/2006/02swap/etc.n3
%  [8] http://eulersharp.sourceforge.net/README.Linux
%  [9] http://eulersharp.sourceforge.net/README.Windows
% [10] http://eulersharp.sourceforge.net/README.MacOSX


% ----------
% Directives
% ----------


:- if(current_prolog_flag(dialect, swi)).
:- style_check(-atom).
:- initialization(catch(set_prolog_stack(local, limit(2^33)), _, true)).
:- initialization(catch(set_prolog_stack(global, limit(2^35)), _, true)).
:- endif.


:- use_module(library(lists)).
:- use_module(library(gensym)).
:- use_module(library(system)).
:- use_module(library(terms)).
:- use_module(library('url.pl')).
:- use_module(library(charsio)).
:- if(current_prolog_flag(dialect, swi)).
:- use_module(library(when), [when/2]).
:- use_module(library(qsave)).
:- endif.
:- if(\+current_predicate(date_time_stamp/2)).
:- load_foreign_files(['pl-tai'], [], install).
:- endif.


:- if(current_predicate(set_stream/2)).
:- initialization(catch(set_stream(user_output, encoding(utf8)), _, true)).
:- else.
:- set_prolog_flag(encoding, utf8).
:- endif.


:- dynamic(answer/8).
:- dynamic(back/0).
:- dynamic(base_uri/1).
:- dynamic(bcnd/2).
:- dynamic(bgot/3).
:- dynamic(brake/0).
:- dynamic(branch/0).
:- dynamic(branching/0).
:- dynamic(bref/2).
:- dynamic(bstep/3).
:- dynamic(bvar/1).
:- dynamic(countermodel/1).
:- dynamic(evar/2).
:- dynamic(evar/3).
:- dynamic(exopred/3).
:- dynamic(fact/1).
:- dynamic(failing/1).
:- dynamic(false/1).
:- dynamic(fd/2).
:- dynamic(flag/1).
:- dynamic(fm/1).
:- dynamic(fs/1).
:- dynamic(goal/0).
:- dynamic(got_answer/9).
:- dynamic(got_dq/0).
:- dynamic(got_labelvars/2).
:- dynamic(got_sq/0).
:- dynamic(got_wi/5).
:- dynamic(graph/2).
:- dynamic(hash_value/2).
:- dynamic(implies/3).
:- dynamic(intern/1).
:- dynamic(keywords/1).
:- dynamic(lemma/6).
:- dynamic(mtime/2).
:- dynamic(nodepth/0).
:- dynamic(ns/2).
:- dynamic(pfx/2).
:- dynamic(pi/3).
:- dynamic(possible/8).
:- dynamic(pred/1).
:- dynamic(prfstep/8).
:- dynamic(qevar/3).
:- dynamic(query/2).
:- dynamic(quvar/3).
:- dynamic(rule_conc/0).
:- dynamic(rule_uvar/1).
:- dynamic(scope/1).
:- dynamic(semantics/2).
:- dynamic(span/1).
:- dynamic(table/3).
:- dynamic(tmpfile/1).
:- dynamic(tuple/2).
:- dynamic(tuple/3).
:- dynamic(tuple/4).
:- dynamic(tuple/5).
:- dynamic(tuple/6).
:- dynamic(tuple/7).
:- dynamic(tuple/8).
:- dynamic(wcache/2).
:- dynamic(wpfx/1).
:- dynamic(wtcache/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#mu>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#pi>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#sigma>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#reflexive>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#relabel>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/list#in>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#implies>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#outputString>'/2).
:- dynamic('<http://www.w3.org/2002/07/owl#sameAs>'/2).


% -----
% Infos
% -----


version_info('$Id: euler.yap 7602 2014-12-10 20:26:19Z josd $').


license_info('EulerSharp: http://eulersharp.sourceforge.net/
	Copyright 2003 World Wide Web Consortium, (Massachusetts Institute of Technology,
	European Research Consortium for Informatics and Mathematics, Keio University).
	All Rights Reserved. This work is distributed under the W3C Software License [1]
	in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
	the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
	[1] http://www.w3.org/Consortium/Legal/2002/copyright-software-20021231').


help_info('Usage: eye <options>* <data>* <query>*
eye
	swipl -x eye.pvm --
<options>
	--nope			no proof explanation
	--no-qnames		no qnames in output
	--no-numerals		no numerals in output
	--no-distinct		no distinct answers in output
	--single-answer		give only one answer
	--step <count>		set maximimum step count
	--wcache <uri> <file>	to tell that uri is cached as file
	--ignore-syntax-error	do not halt in case of syntax error
	--n3p			output N3 P-code
	--image <file>		output PVM code
	--strings		output log:outputString objects
	--warn			output warning info
	--debug			output debug info
	--debug-cnt		output debug info about counters
	--debug-pvm		output debug info about PVM code
	--rule-histogram	output rule histogram
	--profile		output profile info
	--statistics		output statistics info
	--probe			output speedtest info
	--traditional		traditional mode
	--version		show version info
	--license		show license info
	--help			show help info
<data>
	<n3_resource>		N3 facts and rules
	--turtle <ttl_resource>	Turtle data
	--plugin <n3p_resource>	plugin N3 P-code
<query>
	--query <n3_resource>	output filtered with filter rules
	--pass			output deductive closure
	--pass-all		output deductive closure plus rules
	--pass-only-new		output only the new derived triples').



% -------------------
% Main with N3 Socket
% -------------------


main :-
	version_info(V),
	sub_atom(V, 1, _, 2, Version),
	format(user_error, '~w~n', [Version]),
	flush_output(user_error),
	prolog_flag(version, PVersion),
	format(user_error, '~w~n', [PVersion]),
	flush_output(user_error),
	current_prolog_flag(argv, Argv),
	(	append(_, ['--'|Argvp], Argv)
	->	true
	;	Argvp = Argv
	),
	(	Argvp = ['-']
	->	argp(user_input, Argvs)
	;	Argvs = Argvp
	),
	argv(Argvs, Argus),
	(	memberchk('--profile', Argus)
	->	(	current_predicate(profon/0)
		->	yap_flag(profiling, on),
			profinit,
			profon
		;	profiler(_, cputime)
		)
	;	true
	),
	catch(n3socket(Argus), Exc,
		(	Exc = halt
		->	true
		;	format(user_error, '** ERROR ** n3socket ** ~w~n', [Exc]),
			flush_output(user_error),
			nb_setval(exit_code, 1)
		)
	),
	(	memberchk('--profile', Argus)
	->	(	current_predicate(profon/0)
		->	profoff,
			showprofres,
			format(user_error, '~n', []),
			flush_output(user_error)
		;	profiler(_, false),
			tell(user_error),
			(	current_predicate(show_profile/2)
			->	show_profile(plain, 25)
			;	show_profile([])
			),
			told
		)
	;	true
	),
	(	flag(statistics)
	->	statistics,
		(	current_prolog_flag(pid, B)
		->	true
		;	pid(B)
		),
		(	\+current_prolog_flag(windows, true)
		->	atomic_list_concat(['pmap ', B, ' 1>&2'], Cmd),
			catch(exec(Cmd, _), _, true)
		;	tmp_file(Tmp),
			atomic_list_concat([Tmp, '.txt'], File),
			atomic_list_concat(['vmmap -p ', B, ' ', File], Cmd),
			(	catch(exec(Cmd, _), _, fail)
			->	open(File, read, In),
				(	between(1, 15, _),
					readln(In, Line),
					format(user_error, '~w~n', [Line]),
					flush_output(user_error),
					fail
				;	true
				),
				close(In),
				delete_file(File)
			;	true
			)
		)
	;	true
	),
	(	flag('debug-pvm')
	->	tell(user_error),
		ignore(vm_list(_))
	;	true
	),
	nb_getval(exit_code, EC),
	halt(EC).


n3socket(Argus) :-
	statistics(runtime, [T0, _]),
	statistics(walltime, [T1, _]),
	format(user_error, 'starting ~w [msec cputime] ~w [msec walltime]~n', [T0, T1]),
	flush_output(user_error),
	nb_setval(exit_code, 0),
	nb_setval(indentation, 0),
	nb_setval(possible, not_started),
	nb_setval(limit, -1),
	nb_setval(bnet, not_done),
	nb_setval(fnet, not_done),
	nb_setval(table, -1),
	nb_setval(tuple, -1),
	nb_setval(fdepth, 0),
	nb_setval(defcl, true),
	opts(Argus, Args),
	(	\+memberchk('--query', Args),
		\+memberchk('--tquery', Args),
		\+memberchk('--pass', Args),
		\+memberchk('--pass-all', Args),
		\+memberchk('--pass-only-new', Args)
	->	assertz(flag(nope))
	;	true
	),
	(	Args = []
	->	opts(['--help'], _)
	;	true
	),
	(	flag(n3p)
	->	format(':- style_check(-discontiguous).~n', []),
		format(':- style_check(-singleton).~n', []),
		format(':- multifile(exopred/3).~n', []),
		format(':- multifile(implies/3).~n', []),
		format(':- multifile(pfx/2).~n', []),
		format(':- multifile(pred/1).~n', []),
		format(':- multifile(prfstep/8).~n', []),
		format(':- multifile(scope/1).~n', []),
		format(':- multifile(\'<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#mu>\'/2).~n', []),
		format(':- multifile(\'<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#pi>\'/2).~n', []),
		format(':- multifile(\'<http://eulersharp.sourceforge.net/2003/03swap/fl-rules#sigma>\'/2).~n', []),
		format(':- multifile(\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>\'/2).~n', []),
		format(':- multifile(\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#reflexive>\'/2).~n', []),
		format(':- multifile(\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>\'/2).~n', []),
		format(':- multifile(\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>\'/2).~n', []),
		format(':- multifile(\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>\'/2).~n', []),
		format(':- multifile(\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>\'/2).~n', []),
		format(':- multifile(\'<http://www.w3.org/2000/10/swap/list#in>\'/2).~n', []),
		format(':- multifile(\'<http://www.w3.org/2000/10/swap/log#implies>\'/2).~n', []),
		format(':- multifile(\'<http://www.w3.org/2002/07/owl#sameAs>\'/2).~n', [])
	;	true
	),
	args(Args),
	findall(Sc,
		(	scope(Sc)
		),
		Scope
	),
	nb_setval(scope, Scope),
	findall([Dlen, Dsort, implies(Prem, dn(D), Src)],
		(	implies(Prem, dn(D), Src),
			nonvar(D),
			retract(implies(Prem, dn(D), Src)),
			length(D, Dlen),
			cflat([Prem|D], Df),
			labelvars(Df, 0, _),
			sort(Df, Dsort)
		),
		Dg
	),
	sort(Dg, Dh),
	forall(
		(	member([_, _, Di], Dh)
		),
		(	assertz(Di)
		)
	),
	statistics(runtime, [_, T2]),
	statistics(walltime, [_, T3]),
	format(user_error, 'networking ~w [msec cputime] ~w [msec walltime]~n', [T2, T3]),
	flush_output(user_error),
	(	flag(image(File))
	->	retractall(flag(_)),
		retractall(implies(answer(_, _, _, _, _, _, _, _), goal, '<>')),
		reset_gensym,
		(	current_predicate(qsave:qsave_program/1)
		->	qsave_program(File)
		;	save_program(File)
		),
		throw(halt)
	;	true
	),
	(	flag(n3p)
	->	throw(halt)
	;	true
	),
	(	\+implies(answer(_, _, _, _, _, _, _, _), goal, '<>'),
		\+query(_, _),
		\+flag('pass-only-new'),
		\+flag(strings)
	->	throw(halt)
	;	true
	),
	(	nb_getval(defcl, true),
		\+flag('no-branch')
	->	assertz(flag('no-branch'))
	;	true
	),
	(	flag(strings)
	->	true
	;	version_info(V),
		sub_atom(V, 1, _, 2, Version),
		format('#Processed by ~w~n', [Version]),
		(	flag(kgb)
		->	true
		;	format('#eye', []),
			wa(Argus),
			nl
		),
		nl
	),
	(	flag('no-branch')
	->	true
	;	(	pfx('e:', _)
		->	true
		;	assertz(pfx('e:', '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#>'))
		)
	),
	(	flag(nope)
	->	true
	;	(	pfx('r:', _)
		->	true
		;	assertz(pfx('r:', '<http://www.w3.org/2000/10/swap/reason#>'))
		),
		(	\+flag(traditional)
		->	true
		;	(	pfx('var:', _)
			->	true
			;	assertz(pfx('var:', '<http://localhost/var#>'))
			),
			(	pfx('n3:', _)
			->	true
			;	assertz(pfx('n3:', '<http://www.w3.org/2004/06/rei#>'))
			)
		)
	),
	(	flag('pass-only-new')
	->	wh
	;	true
	),
	nb_setval(tr, 0),
	nb_setval(br, 0),
	nb_setval(tc, 0),
	nb_setval(tp, 0),
	nb_setval(bc, 0),
	nb_setval(bp, 0),
	nb_setval(wn, 0),
	nb_setval(pm, 0),
	nb_setval(cm, 0),
	nb_setval(fm, 0),
	nb_setval(lemma_count, 0),
	nb_setval(lemma_cursor, 0),
	nb_setval(output_triples, 0),
	catch(eam(0), Exc,
		(	format(user_error, '** ERROR ** eam ** ~w~n', [Exc]),
			flush_output(user_error),
			nb_setval(exit_code, 1)
		)
	),
	nb_getval(tc, TC),
	nb_getval(tp, TP),
	nb_getval(bc, BC),
	nb_getval(bp, BP),
	nb_getval(pm, PM),
	nb_getval(cm, CM),
	nb_getval(fm, FM),
	TM is PM+CM,
	AM is PM+CM+FM,
	(	AM > 0,
		Ev is TM/AM,
		Ap is PM/AM
	->	(	flag(strings)
		->	true
		;	write('[ '),
			(	TM > 0,
				Ind is PM/TM
			->	wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#inductivity>'),
				write(' '),
				write(Ind),
				write('; ')
			;	true
			),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#evidentiality>'),
			write(' '),
			write(Ev),
			write('; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#applicability>'),
			write(' '),
			write(Ap),
			write('; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#possibleModels>'),
			write(' '),
			write(PM),
			write('; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#counterModels>'),
			write(' '),
			write(CM),
			write('; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#falseModels>'),
			write(' '),
			write(FM),
			write('; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#allModels>'),
			write(' '),
			write(AM),
			write('].'),
			nl,
			nl
		)
	;	true
	),
	format(user_error, 'TC=~w TP=~w BC=~w BP=~w PM=~w CM=~w FM=~w AM=~w~n', [TC, TP, BC, BP, PM, CM, FM, AM]),
	flush_output(user_error),
	statistics(runtime, [_, T4]),
	statistics(walltime, [_, T5]),
	format(user_error, 'reasoning ~w [msec cputime] ~w [msec walltime]~n', [T4, T5]),
	flush_output(user_error),
	(	flag(strings)
	->	findall([Key, Str],
			(	'<http://www.w3.org/2000/10/swap/log#outputString>'(Key, Str)
			;	answer(A1, A2, A3, A4, A5, A6, A7, A8),
				strela(answer('<http://www.w3.org/2000/10/swap/log#outputString>'(Key, Str)), answer(A1, A2, A3, A4, A5, A6, A7, A8))
			),
			KS
		),
		sort(KS, KT),
		forall(
			(	member([_, MT], KT),
				getcodes(MT, LT)
			),
			(	escape_string(NT, LT),
				atom_codes(ST, NT),
				wt(ST)
			)
		),
		(	query(Where, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#csvTuple>'(_, Select)),
			catch(call(Where), _, fail),
			wct(Select),
			fail
		;	true
		)
	;	TE is T1+T3+T5,
		format('#ENDS ~3d [sec] TC=~w TP=~w BC=~w BP=~w PM=~w CM=~w FM=~w AM=~w~n', [TE, TC, TP, BC, BP, PM, CM, FM, AM]),
		nl
	),
	get_time(StampN),
	datetime(StampN, StampC),
	atom_codes(StampA, StampC),
	nb_getval(output_triples, OutT),
	(	statistics(inferences, Inf)
	->	true
	;	Inf = ''
	),
	Elaps is T1+T3+T5,
	catch(Speed is round(Inf/Elaps*1000), _, Speed = ''),
	format(user_error, '[~w] outputTriples=~d inferences=~w seconds=~3d inferences/sec=~w~n~n', [StampA, OutT, Inf, Elaps, Speed]),
	flush_output(user_error),
	(	flag('rule-histogram')
	->	findall([RTC, RTP, RBC, RBP, Rule],
			(	table(ETP, tp, Rule),
				nb_getval(ETP, RTP),
				(	table(ETC, tc, Rule)
				->	nb_getval(ETC, RTC)
				;	RTC = 0
				),
				(	table(EBC, bc, Rule)
				->	nb_getval(EBC, RBC)
				;	RBC = 0
				),
				(	table(EBP, bp, Rule)
				->	nb_getval(EBP, RBP)
				;	RBP = 0
				)
			;	table(EBP, bp, Rule),
				\+table(_, tp, Rule),
				nb_getval(EBP, RBP),
				RTC = 0,
				RTP = 0,
				(	table(EBC, bc, Rule)
				->	nb_getval(EBC, RBC)
				;	RBC = 0
				)
			),
			CntRl
		),
		sort(CntRl, CntRs),
		reverse(CntRs, CntRr),
		nb_getval(tr, TR),
		nb_getval(br, BR),
		format(user_error, '>>> rule histogram TR=~w BR=~w <<<~n', [TR, BR]),
		forall(
			(	member(RCnt, CntRr)
			),
			(	(	last(RCnt, '<http://www.w3.org/2000/10/swap/log#implies>'(X, Y)),
					cn_conj(X, XC),
					c_append(XC, pstep(_), Z),
					catch(clause(Y, Z), _, fail)
				->	format(user_error, 'TC=~w TP=~w BC=~w BP=~w for backward rule ~w~n', RCnt)
				;	format(user_error, 'TC=~w TP=~w BC=~w BP=~w for rule ~w~n', RCnt)
				)
			)
		),
		format(user_error, '~n', []),
		flush_output(user_error)
	;	true
	).


argp(In, List) :-
	get_code(In, C0),
	argt(C0, In, C1, Tok1),
	(	Tok1 == end_of_file
	->	List = []
	;	List = [Tok1|Tokens],
		argp(C1, In, Tokens)
	).


argp(C0, In, List) :-
	argt(C0, In, C1, H),
	(	H == end_of_file
	->	List = []
	;	List = [H|T],
		argp(C1, In, T)
	).


argt(-1, _, -1, end_of_file) :-
	!.
argt(C0, In, C, Token) :-
	white_space(C0),
	!,
	get_code(In, C1),
	argt(C1, In, C, Token).
argt(C0, In, C, Token) :-
	get_code(In, C1),
	argn(C1, In, C, T),
	atom_codes(Token, [C0|T]).


argn(C0, In, C, [C0|T]) :-
	\+white_space(C0),
	C0 \= -1,
	!,
	get_code(In, C1),
	argn(C1, In, C, T).
argn(C, _, C, []).


argv([], []) :-
	!.
argv([Arg|Argvs], [U, V|Argus]) :-
	sub_atom(Arg, B, 1, E, '='),
	sub_atom(Arg, 0, B, _, U),
	memberchk(U, ['--tmp-file', '--wget-path', '--image', '--yabc', '--plugin', '--turtle', '--trules', '--query', '--tquery', '--step']),
	!,
	sub_atom(Arg, _, E, 0, V),
	argv(Argvs, Argus).
argv([Arg|Argvs], [Arg|Argus]) :-
	argv(Argvs, Argus).


opts([], []) :-
	!.
% DEPRECATED
opts(['--quick-answer'|Argus], Args) :-
	!,
	assertz(flag('single-answer')),
	opts(Argus, Args).
opts(['--wcache', Argument, File|Argus], Args) :-
	!,
	absolute_uri(Argument, Arg),
	(	wcache(Arg, _)
	->	true
	;	assertz(wcache(Arg, File))
	),
	opts(Argus, Args).
% DEPRECATED
opts(['--tmp-file', File|Argus], Args) :-
	!,
	assertz(flag('tmp-file'(File))),
	opts(Argus, Args).
% DEPRECATED
opts(['--wget-path', Path|Argus], Args) :-
	!,
	assertz(flag('wget-path'(Path))),
	opts(Argus, Args).
% DEPRECATED
opts(['--pcl'|Argus], Args) :-
	!,
	assertz(flag(n3p)),
	opts(Argus, Args).
opts(['--image', File|Argus], Args) :-
	!,
	assertz(flag(image(File))),
	opts(Argus, Args).
% DEPRECATED
opts(['--yabc', File|Argus], Args) :-
	!,
	assertz(flag(image(File))),
	opts(Argus, Args).
opts(['--step', Lim|Argus], Args) :-
	!,
	(	number(Lim)
	->	Limit = Lim
	;	catch(atom_number(Lim, Limit), Exc,
			(	format(user_error, '** ERROR ** step ** ~w~n', [Exc]),
				flush_output(user_error),
				halt(1)
			)
		)
	),
	assertz(flag(step(Limit))),
	opts(Argus, Args).
opts(['--version'|_], _) :-
	!,
	throw(halt).
opts(['--license'|_], _) :-
	!,
	license_info(License),
	format(user_error, '~w~n~n', [License]),
	flush_output(user_error),
	throw(halt).
opts(['--help'|_], _) :-
	\+flag(image(_)),
	\+flag('debug-pvm'),
	!,
	help_info(Help),
	format(user_error, '~w~n~n', [Help]),
	flush_output(user_error),
	throw(halt).
opts(['--probe'|_], _) :-
	tmp_file(File),
	(	atomic_list_concat(['wget --header="Cache-Control: max-age=3600" --timeout=60 --tries=1 -q http://www.agfa.com/w3c/temp/graph-100000.n3p -O ', File], Cmd),
		catch(exec(Cmd, _), _, fail)
	->	statistics(walltime, [_, T1]),
		S1 is 100000000/T1,
		format(user_error, 'probing web ~0f [triples/sec]~n', [S1]),
		flush_output(user_error)
	;	open(File, write, Out),
		tell(Out),
		format(':- style_check(-discontiguous).~n', []),
		format(':- style_check(-singleton).~n', []),
		(	between(0, 99, I),
			format(':- dynamic(\'<http://eulersharp.sourceforge.net/2007/07test/graph#i~d>\'/2).~n', [I]),
			format('pred(\'<http://eulersharp.sourceforge.net/2007/07test/graph#i~d>\').~n', [I]),
			fail
		;	true
		),
		(	between(1, 100000, _),
			S is random(10000),
			P is random(100),
			O is random(10000),
			format('\'<http://eulersharp.sourceforge.net/2007/07test/graph#i~d>\'(\'<http://eulersharp.sourceforge.net/2007/07test/graph#i~d>\',\'<http://eulersharp.sourceforge.net/2007/07test/graph#i~d>\').~n', [P, S, O]),
			fail
		;	true
		),
		told,
		statistics(walltime, [_, _]),
		format(user_error, 'probing web is not possible~n', []),
		flush_output(user_error)
	),
	open(File, read, In, [encoding(utf8)]),
	repeat,
	read_term(In, Rt, []),
	(	Rt = end_of_file
	->	true
	;	(	Rt = ':-'(Rg)
		->	call(Rg)
		;	(	call(Rt)
			->	true
			;	strelas(Rt)
			)
		),
		fail
	),
	statistics(walltime, [_, T2]),
	S2 is 100000000/T2,
	format(user_error, 'probing file ~0f [triples/sec]~n', [S2]),
	flush_output(user_error),
	(	between(1, 100, _),
		forall(
			(	pred(P)
			),
			(	forall(
					(	call(P, _, _)
					),
					(	true
					)
				)
			)
		),
		fail
	;	true
	),
	statistics(walltime, [_, T3]),
	S3 is 10000000000/T3,
	format(user_error, 'probing memory ~0f [triples/sec]~n', [S3]),
	flush_output(user_error),
	close(In),
	delete_file(File),
	throw(halt).
opts([Arg|Argus], Args) :-
	\+memberchk(Arg, ['--plugin', '--turtle', '--trules', '--query', '--pass', '--pass-all', '--tquery']),
	sub_atom(Arg, 0, 2, _, '--'),
	!,
	sub_atom(Arg, 2, _, 0, Opt),
	assertz(flag(Opt)),
	opts(Argus, Args).
opts([Arg|Argus], [Arg|Args]) :-
	opts(Argus, Args).


args([]) :-
	!.
args(['--plugin', Arg|Args]) :-
	sub_atom(Arg, _, 14, 0, 'rif-plugin.yap'),
	!,
	args(Args).
args(['--plugin', Argument|Args]) :-
	!,
	absolute_uri(Argument, Arg),
	(	wcache(Arg, File)
	->	true
	;	(	(	sub_atom(Arg, 0, 5, _, 'http:')
			->	true
			;	sub_atom(Arg, 0, 6, _, 'https:')
			)
		->	(	flag('tmp-file'(File))
			->	true
			;	tmp_file(File),
				assertz(tmpfile(File))
			),
			(	flag('wget-path'(Path))
			->	true
			;	Path = ''
			),
			atomic_list_concat([Path, 'wget --header="Cache-Control: max-age=3600" -q "', Arg, '" -O ', File], Cmd),
			catch(exec(Cmd, _), Exc,
				(	format(user_error, '** ERROR ** ~w ** ~w~n', [Arg, Exc]),
					flush_output(user_error),
					(	retract(tmpfile(File))
					->	delete_file(File)
					;	true
					),
					halt(1)
				)
			)
		;	(	sub_atom(Arg, 0, 5, _, 'file:')
			->	parse_url(Arg, Parts),
				memberchk(path(File), Parts)
			;	File = Arg
			)
		)
	),
	(	File = '-'
	->	In = user_input
	;	open(File, read, In, [encoding(utf8)])
	),
	nb_setval(sc, 0),
	repeat,
	read_term(In, Rt, []),
	(	Rt = end_of_file
	->	true
	;	(	Rt = ':-'(Rg)
		->	call(Rg)
		;	(	predicate_property(Rt, dynamic)
			->	true
			;	(	File = '-'
				->	true
				;	close(In)
				),
				(	retract(tmpfile(File))
				->	delete_file(File)
				;	true
				),
				throw(builtin_redefinition(Rt))
			),
			(	Rt = scope(Scope)
			->	nb_setval(current_scope, Scope)
			;	true
			),
			(	call(Rt)
			->	true
			;	strelas(Rt),
				(	Rt \= scope(_),
					Rt \= pfx(_, _),
					Rt \= pred(_)
				->	(	flag(nope),
						\+flag(ances)
					->	true
					;	nb_getval(current_scope, Src),
						assertz(prfstep(Rt, _, true, _, Rt, _, forward, Src))
					),
					cnt(sc)
				;	true
				)
			)
		),
		fail
	),
	(	File = '-'
	->	true
	;	close(In)
	),
	(	retract(tmpfile(File))
	->	delete_file(File)
	;	true
	),
	nb_getval(sc, SC),
	(	wcache(Arg, File)
	->	format(user_error, 'GET ~w FROM ~w SC=~w~n', [Arg, File, SC])
	;	format(user_error, 'GET ~w SC=~w~n', [Arg, SC])
	),
	flush_output(user_error),
	args(Args).
args(['--turtle', Arg|Args]) :-
	absolute_uri(Arg, A),
	atomic_list_concat(['<', A, '>'], R),
	assertz(scope(R)),
	(	flag(n3p)
	->	format('~q.~n', [scope(R)])
	;	true
	),
	assertz(flag(turtle)),
	n3_n3p(Arg, data),
	retract(flag(turtle)),
	args(Args).
% DEPRECATED
args(['--trules', Arg|Args]) :-
	!,
	absolute_uri(Arg, A),
	atomic_list_concat(['<', A, '>'], R),
	assertz(scope(R)),
	(	flag(n3p)
	->	format('~q.~n', [scope(R)])
	;	true
	),
	n3_n3p(Arg, trules),
	args(Args).
args(['--query', Arg|Args]) :-
	!,
	n3_n3p(Arg, query),
	assertz(implies(answer(_, _, _, _, _, _, _, _), goal, '<>')),
	args(Args).
args(['--pass'|Args]) :-
	!,
	(	flag(nope),
		\+flag('single-answer')
	->	assertz(query(exopred(P, S, O), exopred(P, S, O)))
	;	assertz(implies(exopred(P, S, O), answer(P, S, O, exopred, epsilon, epsilon, epsilon, epsilon), '<http://eulersharp.sourceforge.net/2003/03swap/pass>')),
		assertz(implies(answer(_, _, _, _, _, _, _, _), goal, '<>')),
		(	flag(n3p)
		->	format('~q.~n', [implies(exopred(P, S, O), answer(P, S, O, exopred, epsilon, epsilon, epsilon, epsilon), '<http://eulersharp.sourceforge.net/2003/03swap/pass>')])
		;	true
		)
	),
	args(Args).
args(['--pass-all'|Args]) :-
	!,
	(	flag(nope),
		\+flag('single-answer')
	->	assertz(query(exopred(P, S, O), exopred(P, S, O))),
		assertz(query('<http://www.w3.org/2000/10/swap/log#implies>'(A, C), '<http://www.w3.org/2000/10/swap/log#implies>'(A, C)))
	;	assertz(implies(exopred(P, S, O), answer(P, S, O, exopred, epsilon, epsilon, epsilon, epsilon), '<http://eulersharp.sourceforge.net/2003/03swap/pass>')),
		assertz(implies('<http://www.w3.org/2000/10/swap/log#implies>'(A, C),
				answer('<http://www.w3.org/2000/10/swap/log#implies>', A, C, gamma, gamma, gamma, gamma, gamma), '<http://eulersharp.sourceforge.net/2003/03swap/pass-all>')),
		assertz(implies(answer(_, _, _, _, _, _, _, _), goal, '<>')),
		(	flag(n3p)
		->	format('~q.~n', [implies(exopred(P, S, O), answer(P, S, O, exopred, epsilon, epsilon, epsilon, epsilon), '<http://eulersharp.sourceforge.net/2003/03swap/pass>')]),
			format('~q.~n', [implies('<http://www.w3.org/2000/10/swap/log#implies>'(A, C),
				answer('<http://www.w3.org/2000/10/swap/log#implies>', A, C, gamma, gamma, gamma, gamma, gamma), '<http://eulersharp.sourceforge.net/2003/03swap/pass-all>')])
		;	true
		)
	),
	args(Args).
% DEPRECATED
args(['--tquery', Arg|Args]) :-
	!,
	assertz(flag(tquery)),
	n3_n3p(Arg, tquery),
	assertz(implies(answer(_, _, _, _, _, _, _, _), goal, '<>')),
	args(Args).
args([Arg|Args]) :-
	absolute_uri(Arg, A),
	atomic_list_concat(['<', A, '>'], R),
	assertz(scope(R)),
	(	flag(n3p)
	->	format('~q.~n', [scope(R)])
	;	true
	),
	n3_n3p(Arg, data),
	args(Args).



% ------------------
% N3 to N3P compiler
% ------------------


n3_n3p(Argument, Mode) :-
	absolute_uri(Argument, Arg),
	(	flag('tmp-file'(Tmp))
	->	true
	;	tmp_file(Tmp)
	),
	(	flag('ignore-syntax-error')
	->	Ise = 'IGNORED'
	;	Ise = 'ERROR'
	),
	(	wcache(Arg, File)
	->	true
	;	(	(	sub_atom(Arg, 0, 5, _, 'http:')
			->	true
			;	sub_atom(Arg, 0, 6, _, 'https:')
			)
		->	File = Tmp,
			(	flag('tmp-file'(_))
			->	true
			;	assertz(tmpfile(File))
			),
			(	flag('wget-path'(Path))
			->	true
			;	Path = ''
			),
			atomic_list_concat([Path, 'wget --header="Accept: text/*" --header="Cache-Control: max-age=3600" -q "', Arg, '" -O ', File], Cmd),
			catch(exec(Cmd, _), Exc,
				(	format(user_error, '** ERROR ** ~w ** ~w~n', [Arg, Exc]),
					flush_output(user_error),
					(	retract(tmpfile(File))
					->	delete_file(File)
					;	true
					),
					halt(1)
				)
			)
		;	(	sub_atom(Arg, 0, 5, _, 'file:')
			->	(	parse_url(Arg, Parts)
				->	memberchk(path(File), Parts)
				;	sub_atom(Arg, 7, _, 0, File)
				)
			;	File = Arg
			)
		)
	),
	(	File = '-'
	->	In = user_input
	;	open(File, read, In, [encoding(utf8)])
	),
	retractall(base_uri(_)),
	(	Arg = '-'
	->	absolute_uri('', Abu),
		(	sub_atom(Abu, _, 1, _, '#')
		->	throw(base_may_not_contain_hash(Abu))
		;	true
		),
		assertz(base_uri(Abu))
	;	(	\+sub_atom(Arg, 0, 4, _, 'urn:'),
			sub_atom(Arg, _, 1, _, '#')
		->	throw(base_may_not_contain_hash(Arg))
		;	true
		),
		assertz(base_uri(Arg))
	),
	retractall(ns(_, _)),
	atomic_list_concat([Arg, '#'], D),
	(	flag(turtle)
	->	true
	;	assertz(ns('', D))
	),
	retractall(keywords(_)),
	retractall(quvar(_, _, _)),
	retractall(qevar(_, _, _)),
	retractall(evar(_, _)),
	retractall(evar(_, _, _)),
	nb_setval(line_number, 1),
	nb_setval(sc, 0),
	nb_setval(semantics, []),
	atomic_list_concat(['\'<', Arg, '>\''], Src),
	atomic_list_concat([Tmp, '_p'], Tmp_p),
	assertz(tmpfile(Tmp_p)),
	open(Tmp_p, write, Ws, [encoding(utf8)]),
	tell(Ws),
	catch(
		(	repeat,
			tokens(In, Tokens),
			phrase(document(Triples), Tokens, Rest),
			(	Rest = []
			->	true
			;	nb_getval(line_number, Ln),
				throw(invalid_document(after_line(Ln)))
			),
			(	Mode = semantics
			->	nb_getval(semantics, TriplesPrev),
				append(TriplesPrev, Triples, TriplesNext),
				nb_setval(semantics, TriplesNext)
			;	tr_n3p(Triples, Src, Mode)
			),
			Tokens = []
		),
		Exc,
		(	(	wcache(Arg, File)
			->	format(user_error, '** ~w ** ~w FROM ~w ** ~w~n', [Ise, Arg, File, Exc])
			;	format(user_error, '** ~w ** ~w ** ~w~n', [Ise, Arg, Exc])
			),
			flush_output(user_error),
			ignore(Parsed = fail)
		)
	),
	ignore(Parsed = true),
	(	Mode = semantics
	->	nb_getval(semantics, TriplesFinal),
		clist(TriplesFinal, Graph),
		write(semantics(Src, Graph)),
		writeln('.')
	;	true
	),
	told,
	(	File = '-'
	->	true
	;	close(In)
	),
	(	retract(tmpfile(Tmp))
	->	delete_file(Tmp)
	;	true
	),
	(	call(Parsed)
	->	(	flag(n3p)
		->	forall(
				(	pfx(Pp, Pu),
					\+wpfx(Pp)
				),
				(	format('~q.~n', [pfx(Pp, Pu)]),
					assertz(wpfx(Pp))
				)
			)
		;	true
		),
		open(Tmp_p, read, Rs, [encoding(utf8)]),
		(	Mode = semantics
		->	repeat,
			read(Rs, Rt),
			(	Rt = end_of_file
			->	true
			;	strelas(Rt),
				(	Rt = semantics(_, cn(L))
				->	length(L, N),
					nb_setval(sc, N)
				;	Rt \= semantics(_, true),
					nb_setval(sc, 1)
				),
				fail
			)
		;	repeat,
			read(Rs, Rt),
			(	Rt = end_of_file
			->	true
			;	(	predicate_property(Rt, dynamic),
					functor(Rt, P, 2)
				->	(	\+pred(P),
						P \= '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#relabel>',
						P \= query
					->	assertz(pred(P)),
						(	flag(n3p)
						->	format(':- dynamic(~q).~n', [P/2]),
							format(':- multifile(~q).~n', [P/2]),
							format('~q.~n', [pred(P)])
						;	true
						)
					;	true
					)
				;	true
				),
				(	ground(Rt),
					Rt \= ':-'(_, _)
				->	(	predicate_property(Rt, dynamic)
					->	true
					;	close(Rs),
						(	retract(tmpfile(Tmp_p))
						->	delete_file(Tmp_p)
						;	true
						),
						throw(builtin_redefinition(Rt))
					),
					(	call(Rt)
					->	true
					;	strelas(Rt),
						cnt(sc),
						(	flag(n3p)
						->	format('~q.~n', [Rt])
						;	true
						)
					)
				;	(	Rt = prfstep(Ct, _, Pt, _, Qt, It, Mt, St)
					->	term_index(Ct, Cnd),
						term_index(Pt, Pnd),
						(	nonvar(It)
						->	copy_term(It, Ic)
						;	Ic = It
						),
						assertz(prfstep(Ct, Cnd, Pt, Pnd, Qt, Ic, Mt, St)),
						(	flag(n3p)
						->	format('~q.~n', [prfstep(Ct, Cnd, Pt, Pnd, Qt, Ic, Mt, St)])
						;	true
						)
					;	(	Rt = ':-'(Ci, Pi)
						->	(	Ci = true
							->	call(Pi)
							;	atomic_list_concat(['<', Arg, '>'], Si),
								copy_term('<http://www.w3.org/2000/10/swap/log#implies>'(Pi, Ci), Ri),
								cn_conj(Pi, Pn),
								(	flag(nope)
								->	Ph = Pn
								;	(	Pi = when(Ai, Bi)
									->	c_append(Bi, istep(Si, Pi, Ci, Ri), Bh),
										Ph = when(Ai, Bh)
									;	c_append(Pn, istep(Si, Pi, Ci, Ri), Ph)
									)
								),
								(	flag('rule-histogram')
								->	(	Ph = when(Ak, Bk)
									->	c_append(Bk, pstep(Ri), Bj),
										Pj = when(Ak, Bj)
									;	c_append(Ph, pstep(Ri), Pj)
									)
								;	Pj = Ph
								),
								cnt(sc),
								(	flag(n3p)
								->	format('~q.~n', [':-'(Ci, Pj)])
								;	assertz(':-'(Ci, Pj))
								)
							)
						;	strelas(Rt),
							cnt(sc),
							(	flag(n3p)
							->	format('~q.~n', [Rt])
							;	true
							)
						)
					)
				),
				fail
			)
		),
		close(Rs),
		(	retract(tmpfile(Tmp_p))
		->	delete_file(Tmp_p)
		;	true
		),
		nb_getval(sc, SC),
		(	wcache(Arg, File)
		->	format(user_error, 'GET ~w FROM ~w SC=~w~n', [Arg, File, SC])
		;	format(user_error, 'GET ~w SC=~w~n', [Arg, SC])
		),
		flush_output(user_error)
	;	(	retract(tmpfile(Tmp_p))
		->	catch(delete_file(Tmp_p), _, true)
		;	true
		),
		(	flag('ignore-syntax-error')
		->	true
		;	nl,
			halt(1)
		)
	),
	!.


tr_n3p([], _, _) :-
	!.
tr_n3p(['\'<http://www.w3.org/2000/10/swap/log#implies>\''(X, Y)|Z], Src, trules) :-
	!,
	(	clast(X, '\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#true>\''(_, T))
	->	true
	;	T = 1.0
	),
	clist(L, X),
	tr_split(L, K, M),
	clist(K, N),
	write(implies(N, '\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>\''([Y|M], T), Src)),
	writeln('.'),
	tr_n3p(Z, Src, trules).
tr_n3p(['\'<http://www.w3.org/2000/10/swap/log#implies>\''(X, Y)|Z], Src, tquery) :-
	!,
	clist(L, X),
	tr_split(L, K, M),
	append(K, ['\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>\''([Y|M], T)], J),
	clist(J, N),
	write(implies(N, answer('\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>\'', [Y|M], T, gamma, gamma, gamma, gamma, gamma), Src)),
	writeln('.'),
	tr_n3p(Z, Src, tquery).
tr_n3p(['\'<http://www.w3.org/2000/10/swap/log#implies>\''(X, Y)|Z], Src, query) :-
	!,
	(	flag(nope),
		\+flag('single-answer'),
		(	flag('no-distinct')
		;	Y = '\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#csvTuple>\''(_, _)
		)
	->	write(query(X, Y)),
		writeln('.')
	;	strela(answer(Y), V),
		write(implies(X, V, Src)),
		writeln('.')
	),
	tr_n3p(Z, Src, query).
tr_n3p([':-'(Y, X)|Z], Src, query) :-
	!,
	(	flag(nope),
		\+flag('single-answer')
	->	write(query(X, Y)),
		writeln('.')
	;	strela(answer(Y), V),
		write(implies(X, V, Src)),
		writeln('.')
	),
	tr_n3p(Z, Src, query).
tr_n3p([X|Z], Src, query) :-
	!,
	(	flag(nope),
		\+flag('single-answer'),
		(	flag('no-distinct')
		;	X = '\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#csvTuple>\''(_, _)
		)
	->	write(query(true, X)),
		writeln('.')
	;	strela(answer(X), U),
		write(implies(true, U, Src)),
		writeln('.')
	),
	tr_n3p(Z, Src, query).
tr_n3p(['\'<http://www.w3.org/2000/10/swap/log#implies>\''(X, Y)|Z], Src, Mode) :-
	!,
	(	Y \= dn(_),
		(	\+flag(ances),
			\+flag(quiet)
		->	true
		;	Y \= false
		)
	->	true
	;	nb_setval(defcl, false)
	),
	write(implies(X, Y, Src)),
	writeln('.'),
	tr_n3p(Z, Src, Mode).
tr_n3p([':-'(Conc, Prem)|Z], Src, Mode) :-
	!,
	write(':-'(Conc, Prem)),
	writeln('.'),
	tr_n3p(Z, Src, Mode).
tr_n3p(['\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>\''(X, Y)|Z], Src, Mode) :-
	!,
	write('\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>\''(X, Y)),
	writeln('.'),
	tr_n3p(Z, Src, Mode).
tr_n3p([X|Z], Src, Mode) :-
	tr_tr(X, Y),
	write(Y),
	writeln('.'),
	(	flag(nope),
		\+flag(ances)
	->	true
	;	write(prfstep(Y, _, true, _, Y, _, forward, Src)),
		writeln('.')
	),
	tr_n3p(Z, Src, Mode).


tr_tr([], []) :-
	!.
tr_tr([A|B], [C|D]) :-
	!,
	tr_tr(A, C),
	tr_tr(B, D).
tr_tr(A, B) :-
	atom(A),
	!,
	(	atom_concat('_', C, A)
	->	atomic_list_concat(['\'<http://localhost/var#', C, '>\''], B)
	;	B = A
	).
tr_tr(A, A) :-
	number(A),
	!.
tr_tr(A, B) :-
	A =.. [C|D],
	tr_tr(D, E),
	B =.. [C|E].


tr_split([], [], []) :-
	!.
tr_split([A|B], C, [A|D]) :-
	functor(A, '\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>\'', _),
	!,
	tr_split(B, C, D).
tr_split([A|B], C, D) :-
	functor(A, '\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#true>\'', _),
	!,
	tr_split(B, C, D).
tr_split([A|B], [A|C], D) :-
	tr_split(B, C, D).



% ----------------------
% strela (stretch relax)
% ----------------------


strela(answer(A), answer(P1, S2, S3, P2, O2, P3, O3, alpha)) :-
	A =.. [P1, S1, O1],
	\+is_list(S1),
	S1 =.. [P2, S2, O2],
	\+is_list(O1),
	O1 =.. [P3, S3, O3],
	!.
strela(answer(A), answer(P1, S1, S2, P2, O2, beta, beta, beta)) :-
	A =.. [P1, S1, O1],
	\+is_list(O1),
	O1 =.. [P2, S2, O2],
	!.
strela(answer(A), answer(P1, S1, O1, gamma, gamma, gamma, gamma, gamma)) :-
	A =.. [P1, S1, O1],
	!.
strela(answer(A), answer(P1, S1, S2, P2, O2, P, delta, delta)) :-
	A =.. [P, P1, S1, O1],
	\+is_list(O1),
	O1 =.. [P2, S2, O2],
	!.
strela(answer(A), answer(P1, S1, O1, P, epsilon, epsilon, epsilon, epsilon)) :-
	A =.. [P, P1, S1, O1],
	!.
strela(answer(cn(A)), cn(B)) :-
	!,
	strelax(A, B).
strela(answer(A), answer(A, zeta, zeta, zeta, zeta, zeta, zeta, zeta)).


strelax([], []) :-
	!.
strelax([A|B], [C|D]) :-
	strela(answer(A), C),
	strelax(B, D).


strelan(answer(cn(A), zeta, zeta, zeta, zeta, zeta, zeta, zeta), cn(B)) :-
	!,
	strelaz(A, B).
strelan(A, A).


strelaz([], []) :-
	!.
strelaz([A|B], [answer(A, zeta, zeta, zeta, zeta, zeta, zeta, zeta)|C]) :-
	strelaz(B, C).


strelar(answer(P1, S1, O1, gamma, gamma, gamma, gamma, gamma), answer(P1, S1, S2, P2, O2, exopred, delta, delta)) :-
	P1 \= '<http://www.w3.org/2000/10/swap/log#implies>',
	P1 \= '<http://www.w3.org/2000/10/swap/log#outputString>',
	O1 =.. [P2, S2, O2],
	!.
strelar(answer(P1, S1, O1, P, epsilon, epsilon, epsilon, epsilon), answer(P1, S1, S2, P2, O2, P, delta, delta)) :-
	O1 =.. [P2, S2, O2],
	!.
strelar(A, A).


strelas(answer(A1, A2, A3, A4, A5, A6, A7, A8)) :-
	atomic(A1),
	!,
	(	\+pred(A1)
	->	assertz(pred(A1))
	;	true
	),
	B =.. [A1, A2, A3, A4, A5, A6, A7, A8],
	assertz(B).
strelas(A) :-
	ground(A),
	A =.. [P, [S1, S2|S3], O],
	!,
	(	current_predicate(P/4)
	->	true
	;	dynamic(P/4),
		X =.. [P, [U1, U2|U3], V],
		assertz(':-'(X,
				(	Y =.. [P, U1, U2, U3, V],
					call(Y)
				)
			)
		)
	),
	B =.. [P, S1, S2, S3, O],
	assertz(B).
strelas(A) :-
	ground(A),
	A =.. [P, S, literal(O1, O2)],
	!,
	(	current_predicate(P/3)
	->	true
	;	dynamic(P/3),
		X =.. [P, U, literal(V1, V2)],
		assertz(':-'(X,
				(	Y =.. [P, U, V1, V2],
					call(Y)
				)
			)
		)
	),
	B =.. [P, S, O1, O2],
	assertz(B).
strelas(A) :-
	assertz(A).


answer(A1, A2, A3, A4, A5, A6, A7, A8) :-
	pred(A1),
	(	current_predicate(A1/7)
	->	true
	;	dynamic(A1/7)
	),
	B =.. [A1, A2, A3, A4, A5, A6, A7, A8],
	call(B).



% ----------------------------
% EAM (Euler Abstract Machine)
% ----------------------------


eam(Span) :-
	(	cnt(tr),
		(	flag(debug)
		->	format(user_error, 'eam/1 enter trunk span ~w~n', [Span]),
			flush_output(user_error)
		;	true
		),
		implies(Prem, Conc, Src),
		ignore(Prem = exopred(_, _, _)),
		(	var(Conc)
		->	true
		;	Conc \= dn(_),
			Conc \= goal,
			(	\+flag(ances),
				\+flag(quiet)
			->	true
			;	Conc \= false
			)
		),
		(	flag(nope),
			\+flag('rule-histogram')
		->	true
		;	copy_term('<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc), Rule)
		),
		(	nb_getval(defcl, true)
		->	true
		;	\+'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'('<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc), _)
		),
		(	flag(debug)
		->	format(user_error, '. eam/1 selecting rule ~w~n', [implies(Prem, Conc, Src)]),
			flush_output(user_error)
		;	true
		),
		catch(call(Prem), _, fail),
		(	Conc = false
		->	throw(inference_fuse(Prem))
		;	true
		),
		(	flag('rule-histogram'),
			copy_term(Rule, RuleL)
		->	lookup(RTP, tp, RuleL),
			cnt(RTP)
		;	true
		),
		cnt(tp),
		(	flag(step(StepLim)),
			nb_getval(tp, Step),
			Step >= StepLim
		->	(	flag(strings)
			->	true
			;	w3(trunk)
			),
			throw(maximimum_step_count(Step))
		;	true
		),
		strelan(Conc, Concdt),
		strelar(Concdt, Concdv),
		(	ground(Prem)
		->	Concd = Concdv
		;	clist(Lv, Concdv),
			partconc(Prem, Lv, Lw),
			clist(Lw, Concd)
		),
		term_index(Prem, Pnd),
		(	flag(think),
			\+flag(nope),
			\+prfstep(_, _, Prem, Pnd, _, _, _, _)
		->	true
		;	(	\+call(Concd)
			->	true
			;	(	flag(debug),
					flag(warn)
				->	format(user_error, '.. eam/1 euler path so do not step in your own step ~w~n', [Concd]),
					flush_output(user_error),
					fail
				)
			)
		),
		(	flag('rule-histogram')
		->	lookup(RTC, tc, RuleL),
			cnt(RTC)
		;	true
		),
		cnt(tc),
		copy_term(Concd, Cc),
		nb_getval(wn, W),
		labelvars(Concd, W, N),
		nb_setval(wn, N),
		(	flag(debug)
		->	format(user_error, '... eam/1 assert step ~w~n', [Concd]),
			flush_output(user_error)
		;	true
		),
		clist(La, Concd),
		clist(Lb, Cc),
		couple(La, La, Lb, Lc),
		findall([D, F, E],
			(	member([D, D, E], Lc),
				unify(D, F),
				(	flag(think),
					\+flag(nope)
				->	true
				;	catch(\+call(F), _, true)
				)
			),
			Ld
		),
		couple(Ls, Le, Lf, Ld),
		clist(Ls, Concs),
		clist(Le, Conce),
		clist(Lf, Clc),
		astep(Src, Prem, Concs, Conce, Clc, Rule),
		(	flag('single-answer'),
			answer(_, _, _, _, _, _, _, _)
		->	(	flag(strings)
			->	true
			;	w3(trunk)
			)
		;	retract(brake),
			fail
		)
	;	brake,
		(	S is Span+1,
			assertz(span(S)),
			nb_getval(limit, Limit),
			Span < Limit,
			eam(S)
		;	(	flag(strings)
			->	true
			;	w3(trunk)
			),
			\+flag('no-branch'),
			assertz(branch),
			eam([], 0, []),
			(	nb_getval(cm, CM),
				CM > 0
			->	true
			;	forall(
					(	possible(A1, A2, A3, A4, A5, A6, A7, A8)
					),
					(	strela(answer(Ans), answer(A1, A2, A3, A4, A5, A6, A7, A8)),
						wt(Ans),
						write('.'),
						nl
					)
				),
				(	possible(_, _, _, _, _, _, _, _)
				->	nl
				;	true
				)
			)
		;	true
		),
		!
	;	assertz(brake),
		eam(Span)
	).


astep(A, B, C, Cn, Cc, Rule) :-
	term_index(B, Pnd),
	(	Cn = cn([Dn|En]),
		Cc = cn([Dc|Ec])
	->	(	flag(think),
			Dc = '<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc)
		->	asserta(implies(Prem, Conc, A))
		;	true
		),
		functor(Dn, P, N),
		(	\+pred(P),
			P \= '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#relabel>',
			P \= '<http://www.w3.org/2000/10/swap/log#implies>',
			N = 2
		->	assertz(pred(P))
		;	true
		),
		(	\+branch,
			catch(call(Dn), _, fail)
		->	true
		;	strelas(Dn),
			(	flag('pass-only-new'),
				Dn \= answer(_, _, _, _, _, _, _, _)
			->	indent,
				relabel(Dn, Dr),
				wt(Dr),
				ws(Dr),
				write('.'),
				nl
			;	true
			)
		),
		(	flag(nope),
			\+flag(ances)
		->	true
		;	term_index(Dn, Cnd),
			assertz(prfstep(Dn, Cnd, B, Pnd, C, Rule, forward, A))
		),
		(	En = [Fn],
			Ec = [Fc]
		->	true
		;	Fn = cn(En),
			Fc = cn(Ec)
		),
		astep(A, B, C, Fn, Fc, Rule)
	;	(	Cn = true
		->	true
		;	(	flag(think),
				Cc = '<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc)
			->	asserta(implies(Prem, Conc, A))
			;	true
			),
			functor(Cn, P, N),
			(	\+pred(P),
				P \= '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#relabel>',
				P \= '<http://www.w3.org/2000/10/swap/log#implies>',
				N = 2
			->	assertz(pred(P))
			;	true
			),
			(	\+branch,
				catch(call(Cn), _, fail)
			->	true
			;	strelas(Cn),
				(	flag('pass-only-new'),
					Cn \= answer(_, _, _, _, _, _, _, _)
				->	indent,
					relabel(Cn, Cr),
					wt(Cr),
					ws(Cr),
					write('.'),
					nl
				;	true
				)
			),
			(	flag(nope),
				\+flag(ances)
			->	true
			;	term_index(Cn, Cnd),
				assertz(prfstep(Cn, Cnd, B, Pnd, C, Rule, forward, A))
			)
		)
	).


istep(Src, Prem, Conc, Rule) :-
	term_index(Conc, Cnd),
	term_index(Prem, Pnd),
	assertz(prfstep(Conc, Cnd, Prem, Pnd, Conc, Rule, backward, Src)),
	(	branch
	->	(	\+bstep(Src, Prem, Conc)
		->	assertz(bstep(Src, Prem, Conc))
		;	true
		)
	;	true
	).


pstep(Rule) :-
	copy_term(Rule, RuleL),
	lookup(RTC, tc, RuleL),
	cnt(RTC),
	lookup(RTP, tp, RuleL),
	cnt(RTP).


% Coherent Logic inspired by http://www.cs.vu.nl/~diem/research/ht/CL.pl

% DEPRECATED
eam(Grd, Pnum, Env) :-
	cnt(br),
	(	flag(debug)
	->	format(user_error, 'eam/3 enter branch ~w~n', [Env]),
		flush_output(user_error)
	;	true
	),
	implies(Prem, Conc, Src),
	ignore(Prem = exopred(_, _, _)),
	(	flag(nope),
		\+flag('rule-histogram')
	->	true
	;	copy_term('<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc), Rule)
	),
	(	\+'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'('<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc), _)
	->	Gnew = Grd
	;	(	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'('<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc), [Grd, Gnew])
		->	true
		;	(	flag(debug)
			->	format(user_error, '. eam/3 mismatch with current guard ~w for rule ~w~n', [Grd, implies(Prem, Conc, Src)]),
				flush_output(user_error),
				fail
			)
		)
	),
	(	flag(debug)
	->	format(user_error, '. eam/3 selecting rule ~w~n', [implies(Prem, Conc, Src)]),
		flush_output(user_error)
	;	true
	),
	catch(call(Prem), _, fail),
	(	Conc = answer(_, _, _, _, _, _, _, _)
	->	true
	;	commonvars(Prem, Conc, [])
	),
	(	flag('rule-histogram'),
		copy_term(Rule, RuleL)
	->	lookup(RBP, bp, RuleL),
		cnt(RBP)
	;	true
	),
	cnt(bp),
	(	flag(step(StepLim)),
		nb_getval(tp, TP),
		nb_getval(bp, BP),
		Step is TP+BP,
		Step >= StepLim
	->	throw(maximimum_step_count(Step))
	;	true
	),
	strelan(Conc, Concd),
	(	flag('single-answer')
	->	term_index(Prem, Pnd),
		(	flag(think),
			\+flag(nope),
			\+prfstep(_, _, Prem, Pnd, _, _, _, _)
		->	true
		;	(	\+call(Concd)
			->	true
			;	(	flag(debug),
					flag(warn)
				->	format(user_error, '.. eam/3 euler path so do not step in your own step ~w~n', [Concd]),
					flush_output(user_error),
					fail
				)
			)
		),
		(	flag('rule-histogram')
		->	lookup(RBC, bc, RuleL),
			cnt(RBC)
		;	true
		),
		cnt(bc)
	;	true
	),
	(	Concd = false
	->	\+false(Prem),
		C = false(Prem)
	;	C = Concd
	),
	(	(	C = goal
		->	true
		;	C = false(_),
			flag('quick-false')
		)
	->	ances(Env),
		(	flag(strings)
		->	true
		;	end(C, Env)
		)
	;	(	C = dn(D)
		->	(	branching
			->	true
			;	assertz(branching)
			),
			(	flag('quick-possible')
			->	retract(implies(Prem, Concd, Src))
			;	true
			),
			forall(
				(	member(E, D)
				),
				(	(	memo(Src, Prem, Gnew, Pnum, E, [E|Env], Rule, RuleL)
					->	true
					;	(	\+failing(Env)
						->	assertz(failing(Env))
						;	true
						)
					)
				)
			),
			(	flag('quick-possible')
			->	assertz(implies(Prem, Concd, Src))
			;	true
			),
			(	retract(failing(Env))
			->	Env = [],
				\+flag(strings),
				\+answer(_, _, _, _, _, _, _, _),
				\+countermodel([]),
				assertz(countermodel([])),
				end(countermodel, []),
				fail
			;	true
			)
		;	memo(Src, Prem, Gnew, Pnum, C, Env, Rule, RuleL)
		)
	).


% DEPRECATED
memo(Src, Prem, Grd, Pnum, Conc, Env, Rule, RuleL) :-
	term_index(Prem, Pnd),
	(	flag(think),
		\+flag(nope),
		\+prfstep(_, _, Prem, Pnd, _, _, _, _)
	->	true
	;	(	\+call(Conc)
		->	true
		;	(	flag(debug),
				flag(warn)
			->	format(user_error, '.. memo/8 euler path so do not step in your own step ~w~n', [Conc]),
				flush_output(user_error),
				fail
			)
		)
	),
	(	flag('rule-histogram')
	->	lookup(RBC, bc, RuleL),
		cnt(RBC)
	;	true
	),
	cnt(bc),
	copy_term(Conc, Cc),
	(	Conc = false(_)
	->	Pnew = Pnum
	;	labelvars(Conc, Pnum, Pnew)
	),
	(	flag(debug)
	->	format(user_error, '... memo/8 assert step ~w~n', [Conc]),
		flush_output(user_error)
	;	true
	),
	clist(La, Conc),
	clist(Lb, Cc),
	couple(La, La, Lb, Lc),
	findall([D, F, E],
		(	member([D, D, E], Lc),
			unify(D, F),
			(	flag(think),
				\+flag(nope)
			->	true
			;	catch(\+call(F), _, true)
			)
		),
		Ld
	),
	couple(Ls, Le, Lf, Ld),
	clist(Ls, Concs),
	clist(Le, Concl),
	clist(Lf, Clc),
	astep(Src, Prem, Concs, Concl, Clc, Rule),
	(	eam(Grd, Pnew, Env)
	->	true
	;	ances(Env),
		(	flag(strings)
		->	true
		;	end(countermodel, Env)
		)
	),
	(	flag(debug)
	->	format(user_error, '... memo/8 retract step ~w~n', [Conc]),
		flush_output(user_error)
	;	true
	),
	dstep(Src, Prem, Concl, Rule).


% DEPRECATED
dstep(A, B, C, Rule) :-
	term_index(B, Ind),
	(	C = cn([D|E])
	->	(	flag(think),
			D = '<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc)
		->	retract(implies(Prem, Conc, A))
		;	true
		),
		retract(D),
		(	flag(nope),
			\+flag(ances)
		->	true
		;	term_index(D, Cnd),
			retract(prfstep(D, Cnd, B, Ind, _, _, _, A))
		),
		(	E = [F]
		->	true
		;	F = cn(E)
		),
		dstep(A, B, F, Rule)
	;	(	C = true
		->	true
		;	(	flag(think),
				C = '<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc)
			->	retract(implies(Prem, Conc, A))
			;	true
			),
			retract(C),
			(	flag(nope),
				\+flag(ances)
			->	true
			;	term_index(C, Cnd),
				retract(prfstep(C, Cnd, B, Ind, _, _, _, A))
			)
		)
	),
	forall(
		(	retract(bstep(Sr, Pr, Cn))
		),
		(	retract(prfstep(Cn, _, Pr, _, _, _, _, Sr))
		)
	).


% DEPRECATED
hstep(A, B) :-
	(	nonvar(A),
		A = exopred(P, S, O)
	->	pred(P),
		U =.. [P, S, O],
		qstep(U, B)
	;	qstep(A, B)
	).


% DEPRECATED
qstep(A, B) :-
	prfstep(A, _, B, _, _, _, _, _).
% DEPRECATED
qstep(A, true) :-
	(	nonvar(A)
	->	(	A =.. [P, [S1, S2|S3], O]
		->	B =.. [P, S1, S2, S3, O]
		;	(	A =.. [P, S, literal(O1, O2)]
			->	B =.. [P, S, O1, O2]
			;	B = A
			)
		)
	;	pred(P),
		A =.. [P, _, _],
		B = A
	),
	catch(clause(B, true), _, fail),
	\+prfstep(A, _, _, _, _, _, _, _).


% DEPRECATED
ancestor(A, B) :-
	hstep(D, C),
	C \= true,
	D \= false(_),
	D \= answer(_, _, _, _, _, _, _, _),
	unify(B, D),
	cmember(E, C),
	(	unify(A, E)
	;	ancestor(A, E)
	).


% DEPRECATED
cgives(A, B) :-
	(	\+hstep(B, _),
		!
	;	hstep(B, C),
		\+(	(	cmember(D, C),
				cmember(E, A),
				(	unify(E, D)
				;	\+cgives(E, D)
				)
			)
		)
	).


% DEPRECATED
ances(Env) :-
	(	flag(ances),
		\+flag(quiet)
	->	write('[ '),
		wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#ancestorModel>'),
		write(' '),
		clist(Env, G),
		wg(G),
		nl,
		(	hstep(answer(_, _, _, _, _, _, _, _), D),
			findall(X,
				(	ancestor(X, D)
				),
				T
			),
			distinct(T, U),
			clist(U, V),
			write('; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#selected>'),
			write(' [ '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#triple>'),
			write(' '),
			wg(D),
			nl,
			write('  ; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#ancestors>'),
			write(' '),
			wg(V),
			nl,
			write('  ]'),
			nl,
			fail
		;	write('].'),
			nl
		)
	;	true
	).


% DEPRECATED
end(goal, Env) :-
	\+false(_),
	!,
	retractall(got_answer(_, _, _, _, _, _, _, _, branch)),
	(	\+branching
	->	true
	;	write('[ '),
		wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#possibleModel>'),
		write(' '),
		clist(Env, G),
		indentation(2),
		wg(G),
		nl,
		write('; '),
		wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#gives>'),
		write(' {'),
		nl,
		indentation(2),
		(	flag(strings)
		->	true
		;	retractall(lemma(_, _, _, _, _, _)),
			w3(branch)
		),
		indentation(-2),
		indent,
		write('}'),
		nl,
		write('].'),
		indentation(-2),
		nl,
		nl,
		cnt(pm)
	),
	(	flag('quick-possible')
	->	true
	;	(	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'(_, _)
		->	true
		;	(	nb_getval(possible, started)
			->	forall(
					(	possible(A1, A2, A3, A4, A5, A6, A7, A8)
					),
					(	(	got_answer(A1, A2, A3, A4, A5, A6, A7, A8, branch)
						->	true
						;	retract(possible(A1, A2, A3, A4, A5, A6, A7, A8))
						)
					)
				)
			;	nb_setval(possible, started),
				forall(
					(	got_answer(A1, A2, A3, A4, A5, A6, A7, A8, branch)
					),
					(	assertz(possible(A1, A2, A3, A4, A5, A6, A7, A8))
					)
				)
			)
		)
	).
% DEPRECATED
end(countermodel, Env) :-
	\+false(_),
	!,
	write('[ '),
	wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#counterModel>'),
	write(' '),
	clist(Env, G),
	indentation(2),
	wg(G),
	indentation(-2),
	nl,
	write('].'),
	nl,
	nl,
	cnt(cm).
% DEPRECATED
end(End, Env) :-
	write('[ '),
	wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#falseModel>'),
	write(' '),
	clist(Env, G),
	indentation(2),
	wg(G),
	nl,
	retractall(fd(_, _)),
	(	(	End = false(F)
		;	false(F)
		),
		write('; '),
		wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#because>'),
		write(' [ '),
		wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#integrityConstraint>'),
		write(' {'),
		labelvars(F, 0, _, allv),
		wg(F),
		write(' '),
		wp('<http://www.w3.org/2000/10/swap/log#implies>'),
		write(' false}'),
		(	cmember(A, F),
			nl,
			write('  ; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#selected>'),
			write(' [ '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#triple>'),
			write(' '),
			wg(A),
			nl,
			(	flag(quiet)
			->	true
			;	(	flag(nope),
					\+flag(ances)
				->	throw(no_ances_flag)
				;	true
				),
				findall(X,
					(	ancestor(X, A)
					),
					L
				),
				distinct(L, Ls),
				clist(Ls, U),
				findall(X,
					(	ancestor(A, X)
					),
					M
				),
				distinct(M, Ms),
				clist(Ms, V),
				findall(X,
					(	false(Y),
						cmember(X, Y)
					),
					I
				),
				distinct(I, Is),
				clist(Is, Q),
				findall(X,
					(	cmember(X, U),
						cmember(Y, Q),
						unify(X, Y)
					),
					J
				),
				distinct(J, Js),
				clist(Js, R),
				write('    ; '),
				wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#falseAncestors>'),
				write(' '),
				indentation(2),
				wg(R),
				nl,
				findall(X,
					(	cmember(X, V),
						cmember(Y, Q),
						unify(X, Y)
					),
					K
				),
				distinct(K, Ks),
				clist(Ks, S),
				write('    ; '),
				wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#falseDescendents>'),
				write(' '),
				wg(S),
				nl,
				findall(X,
					(	ancestor(X, A),
						hstep(Y, true),
						unify(X, Y)
					),
					T
				),
				distinct(T, Ts),
				clist(Ts, Ua),
				write('    ; '),
				wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#assertedAncestors>'),
				write(' [ '),
				wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#triples>'),
				write(' '),
				wg(Ua),
				nl,
				(	cmember(Ca, Ua),
					write('      ; '),
					wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#selected>'),
					write(' [ '),
					wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#ancestor>'),
					write(' '),
					wg(Ca),
					nl,
					findall(X,
						(	ancestor(Ca, X)
						),
						D
					),
					distinct(D, Ds),
					clist(Ds, Va),
					write('        ; '),
					wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#inferredDescendents>'),
					write(' '),
					indentation(2),
					wg(Va),
					indentation(-2),
					nl,
					write('        ]'),
					nl,
					fail
				;	true
				),
				write('      ]'),
				nl,
				(	fd(A, _)
				->	true
				;	'<http://www.w3.org/2000/10/swap/log#conjunction>'([R, S], Fd),
					assertz(fd(A, Fd))
				),
				(	flag(think)
				->	findall(X,
						(	hstep(X, _),
							X \= false(_),
							X \= answer(_, _, _, _, _, _, _, _),
							\+unify(X, A),
							cgives(A, X)
						),
						N
					),
					distinct(N, Ns),
					clist(Ns, W),
					write('    ; '),
					wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#consistentGives>'),
					write(' '),
					wg(W),
					nl
				;	true
				),
				indentation(-2)
			),
			write('    ]'),
			fail
		;	true
		),
		nl,
		write('  ]'),
		nl,
		fail
	;	true
	),
	(	flag(think),
		\+flag(quiet)
	->	findall(X,
			(	hstep(X, _),
				X \= false(_),
				X \= answer(_, _, _, _, _, _, _, _),
				forall(
					(	false(H),
						cmember(B, H)
					),
					(	\+unify(X, B),
						cgives(B, X)
					)
				)
			),
			O
		),
		distinct(O, Os),
		clist(Os, Z),
		write('; '),
		wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#consistentGives>'),
		write(' '),
		wg(Z),
		nl
	;	true
	),
	(	flag('quick-false')
	->	true
	;	findall(X,
			(	false(X)
			),
			Ir
		),
		'<http://www.w3.org/2000/10/swap/log#conjunction>'(Ir, It),
		findall([X, Y],
			(	cmember(Y, It),
				findall(1,
					(	false(Ig),
						cmember(Y, Ig)
					),
					Im
				),
				length(Im, X)
			),
			In
		),
		sort(In, Io),
		findall(X,
			(	member([_, X], Io)
			),
			Ip
		),
		reverse(Ip, Iq),
		write('; '),
		wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#inconsistentTriplesOrdering>'),
		write(' '),
		wt(Iq),
		nl,
		(	flag(quiet)
		->	true
		;	findall([X, Y],
				(	cmember(Y, It),
					fd(Y, Ig),
					clist(Ih, Ig),
					length(Ih, X)
				),
				Iu
			),
			sort(Iu, Iv),
			findall(X,
				(	member([_, X], Iv)
				),
				Iw
			),
			write('; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#closureInconsistentTriplesOrdering>'),
			write(' '),
			wt(Iw),
			nl,
			findall([X, Y],
				(	cmember(Y, It),
					findall(1,
						(	false(Ig),
							cmember(Y, Ig)
						),
						Im
					),
					length(Im, Xa),
					fd(Y, Jg),
					clist(Jh, Jg),
					length(Jh, Xb),
					X is Xb-Xa
				),
				Ju
			),
			sort(Ju, Jv),
			findall(X,
				(	member([_, X], Jv)
				),
				Jw
			),
			write('; '),
			wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#maxResolveMinRemoveOrdering>'),
			write(' '),
			wt(Jw),
			nl
		),
		write('; '),
		wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#gives>'),
		write(' {'),
		nl,
		retractall(got_answer(_, _, _, _, _, _, _, _, branch)),
		indentation(2),
		(	flag(strings)
		->	true
		;	retractall(lemma(_, _, _, _, _, _)),
			w3(branch)
		),
		indentation(-2),
		indent,
		write('}')
	),
	write('].'),
	indentation(-2),
	nl,
	nl,
	(	Env = []
	->	throw(empty_false_model)
	;	true
	),
	cnt(fm).



% ------------
% proof output
% ------------


wa([]) :-
	!.
wa(['--wget-path', _|A]) :-
	!,
	wa(A).
wa([A|B]) :-
	format(' ~w', [A]),
	wa(B).


wh :-
	(	flag('no-qnames')
	->	true
	;	nb_setval(wpfx, false),
		forall(
			(	pfx(A, B),
				\+wpfx(A)
			),
			(	(	\+flag(traditional)
				->	format('PREFIX ~w ~w~n', [A, B])
				;	format('@prefix ~w ~w.~n', [A, B])
				),
				assertz(wpfx(A)),
				nb_setval(wpfx, true)
			)
		),
		(	nb_getval(wpfx, true)
		->	nl
		;	true
		)
	).


w3(U) :-
	wh,
	flag(nope),
	!,
	(	query(Q, A),
		catch(call(Q), _, fail),
		nb_getval(wn, W),
		labelvars(A, W, N),
		nb_setval(wn, N),
		relabel(A, B),
		indent,
		wt(B),
		ws(B),
		write('.'),
		nl,
		cnt(output_triples),
		fail
	;	true
	),
	(	answer(B1, B2, B3, B4, B5, B6, B7, B8),
		(	B4 = exopred,
			answer(B1, B2, B3, gamma, gamma, gamma, gamma, gamma)
		->	fail
		;	true
		),
		(	\+flag('no-branch'),
			\+got_answer(B1, B2, B3, B4, B5, B6, B7, B8, _)
		->	assertz(got_answer(B1, B2, B3, B4, B5, B6, B7, B8, U))
		;	true
		),
		relabel([B1, B2, B3, B4, B5, B6, B7, B8], [C1, C2, C3, C4, C5, C6, C7, C8]),
		strela(answer(C), answer(C1, C2, C3, C4, C5, C6, C7, C8)),
		indent,
		wt(C),
		ws(C),
		write('.'),
		nl,
		cnt(output_triples),
		fail
	;	(	U = branch
		->	true
		;	nl
		)
	).
w3(U) :-
	(	prfstep(answer(Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8), _, _, _, _, _, _, _),
		\+got_answer(Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, _),
		!,
		indent,
		write('[ '),
		wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'),
		write(' '),
		wp('<http://www.w3.org/2000/10/swap/reason#Proof>'),
		write(', '),
		wp('<http://www.w3.org/2000/10/swap/reason#Conjunction>'),
		write(';'),
		indentation(2),
		nl,
		indent,
		(	prfstep(answer(B1, B2, B3, B4, B5, B6, B7, B8), _, B, Pnd, Cn, R, _, A),
			(	B4 = exopred,
				prfstep(answer(B1, B2, B3, gamma, gamma, gamma, gamma, gamma),_, _, _, _, _, _, _)
			->	fail
			;	true
			),
			R =.. [P, S, O1],
			strela(answer(O), O1),
			Rule =.. [P, S, O],
			(	flag(think)
			->	true
			;	\+got_answer(B1, B2, B3, B4, B5, B6, B7, B8, _)
			),
			assertz(got_answer(B1, B2, B3, B4, B5, B6, B7, B8, U)),
			relabel([B1, B2, B3, B4, B5, B6, B7, B8], [C1, C2, C3, C4, C5, C6, C7, C8]),
			strela(answer(C), Cn),
			\+got_wi(A, B, Pnd, C, Rule),
			assertz(got_wi(A, B, Pnd, C, Rule)),
			wp('<http://www.w3.org/2000/10/swap/reason#component>'),
			write(' '),
			wi(A, B, C, Rule),
			write(';'),
			nl,
			indent,
			fail
		;	retractall(got_wi(_, _, _, _, _))
		),
		wp('<http://www.w3.org/2000/10/swap/reason#gives>'),
		write(' {'),
		indentation(2),
		(	got_answer(B1, B2, B3, B4, B5, B6, B7, B8, U),
			relabel([B1, B2, B3, B4, B5, B6, B7, B8], [C1, C2, C3, C4, C5, C6, C7, C8]),
			strela(answer(C), answer(C1, C2, C3, C4, C5, C6, C7, C8)),
			nl,
			indent,
			getvars(C, D),
			(	C = '<http://www.w3.org/2000/10/swap/log#implies>'(_, _)
			->	Q = allv
			;	Q = some
			),
			(	\+flag(traditional)
			->	true
			;	wq(D, Q)
			),
			wt(C),
			ws(C),
			write('.'),
			cnt(output_triples),
			fail
		;	true
		),
		indentation(-2),
		nl,
		indent,
		write('}].'),
		indentation(-2),
		nl,
		nl
	;	true
	),
	(	nb_getval(lemma_count, Lco),
		nb_getval(lemma_cursor, Lcu),
		Lcu < Lco
	->	repeat,
		cnt(lemma_cursor),
		nb_getval(lemma_cursor, Cursor),
		lemma(Cursor, Ai, Bi, _, Ci, Di),
		indent,
		wj(Cursor, Ai, Bi, Ci, Di),
		nl,
		nl,
		nb_getval(lemma_count, Cnt),
		Cursor = Cnt,
		!
	;	true
	).


wi(A, B, C, Rule) :-
	term_index(B, Pnd),
	(	lemma(Cnt, A, B, Pnd, C, Rule)
	->	true
	;	cnt(lemma_count),
		nb_getval(lemma_count, Cnt),
		assertz(lemma(Cnt, A, B, Pnd, C, Rule))
	),
	write('<#lemma'),
	write(Cnt),
	write('>').


wj(Cnt, A, true, C, _) :-
	!,
	write('<#lemma'),
	write(Cnt),
	write('> '),
	wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'),
	write(' '),
	wp('<http://www.w3.org/2000/10/swap/reason#Extraction>'),
	write('; '),
	wp('<http://www.w3.org/2000/10/swap/reason#gives>'),
	write(' {'),
	(	C = rule(PVars, EVars, Rule)
	->	(	\+flag(traditional)
		->	true
		;	wq(PVars, allv),
			wq(EVars, some)
		),
		wt(Rule)
	;	labelvars([A, C], 0, _, avar),
		getvars(C, D),
		(	C = '<http://www.w3.org/2000/10/swap/log#implies>'(_, _)
		->	Q = allv
		;	Q = some
		),
		(	\+flag(traditional)
		->	true
		;	wq(D, Q)
		),
		wt(C)
	),
	write('};'),
	nl,
	indentation(2),
	indent,
	wp('<http://www.w3.org/2000/10/swap/reason#because>'),
	write(' [ '),
	wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'),
	write(' '),
	wp('<http://www.w3.org/2000/10/swap/reason#Parsing>'),
	write('; '),
	wp('<http://www.w3.org/2000/10/swap/reason#source>'),
	write(' '),
	wt(A),
	write('].'),
	indentation(-2).
wj(Cnt, A, B, C, Rule) :-
	write('<#lemma'),
	write(Cnt),
	write('> '),
	wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'),
	write(' '),
	wp('<http://www.w3.org/2000/10/swap/reason#Inference>'),
	write('; '),
	wp('<http://www.w3.org/2000/10/swap/reason#gives>'),
	write(' {'),
	Rule = '<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc),
	unifiable(Prem, B, Bs),
	(	unifiable(Conc, C, Cs)
	->	true
	;	(	Conc = dn(G),
			member(H, G),
			unifiable(H, C, Cs)
		->	true
		;	Cs = []
		)
	),
	append(Bs, Cs, Ds),
	sort(Ds, Bindings),
	term_variables(Prem, PVars),
	term_variables(Conc, CVars),
	nb_getval(wn, W),
	labelvars([A, B, C], W, N, some),
	nb_setval(wn, N),
	labelvars([Rule, PVars, CVars], 0, _, avar),
	findall(V,
		(	member(V, CVars),
			\+member(V, PVars)
		),
		EVars
	),
	getvars(C, D),
	(	C = '<http://www.w3.org/2000/10/swap/log#implies>'(_, _)
	->	Q = allv
	;	Q = some
	),
	indentation(2),
	(	\+flag(traditional)
	->	true
	;	wq(D, Q)
	),
	wt(C),
	indentation(-2),
	write('}; '),
	wp('<http://www.w3.org/2000/10/swap/reason#evidence>'),
	write(' ('),
	indentation(2),
	wr(B),
	write(');'),
	retractall(got_wi(_, _, _, _, _)),
	nl,
	indent,
	(	\+flag(traditional)
	->	true
	;	wb(Bindings)
	),
	wp('<http://www.w3.org/2000/10/swap/reason#rule>'),
	write(' '),
	wi(A, true, rule(PVars, EVars, Rule), _),
	write('.'),
	indentation(-2).


wr(exopred(P, S, O)) :-
	!,
	U =.. [P, S, O],
	wr(U).
wr(cn([X])) :-
	!,
	wr(X).
wr(cn([X|Y])) :-
	!,
	wr(X),
	(	Y = [Z]
	->	true
	;	Z = cn(Y)
	),
	wr(Z).
wr(Z) :-
	term_index(Z, Cnd),
	prfstep(Z, Cnd, Y, Pnd, Q, Rule, _, X),
	!,
	(	\+got_wi(X, Y, Pnd, Q, Rule)
	->	assertz(got_wi(X, Y, Pnd, Q, Rule)),
		nl,
		indent,
		wi(X, Y, Q, Rule)
	;	true
	).
wr(Y) :-
	nl,
	indent,
	write('[ '),
	wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'),
	write(' '),
	wp('<http://www.w3.org/2000/10/swap/reason#Fact>'),
	write('; '),
	wp('<http://www.w3.org/2000/10/swap/reason#gives>'),
	write(' {'),
	labelvars(Y, 0, _, avar),
	getvars(Y, Z),
	(	\+flag(traditional)
	->	true
	;	wq(Z, some)
	),
	wt(Y),
	write('}]').


wt(rdiv(X, Y)) :-
	number_codes(Y, [0'1|Z]),
	lzero(Z, Z),
	!,
	(	Z = []
	->	F = '~d.0'
	;	length(Z, N),
		number_codes(X, U),
		(	length(U, N)
		->	F = '0.~d'
		;	atomic_list_concat(['~', N, 'd'], F)
		)
	),
	format(F, [X]).
wt(rdiv(X, Y)) :-
	!,
	format('~g', [rdiv(X, Y)]).
wt(X) :-
	number(X),
	!,
	(	flag('no-numerals')
	->	dtlit([U, V], X),
		dtlit([U, V], W),
		wt(W)
	;	write(X)
	).
wt(cn([X])) :-
	!,
	wt(X).
wt(cn([X|Y])) :-
	!,
	wt(X),
	ws(X),
	write('.'),
	nl,
	indent,
	(	Y = [Z]
	->	true
	;	Z = cn(Y)
	),
	wt(Z).
% DEPRECATED
wt(dn(X)) :-
	!,
	wt(X),
	write('!'),
	wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#disjunction>').
wt(set(X)) :-
	!,
	write('($'),
	wl(X),
	write(' $)').
wt([]) :-
	!,
	write('()').
wt([X|Y]) :-
	!,
	(	\+last_tail([X|Y], [])
	->	write('[ '),
		wt('<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'),
		write(' '),
		wg(X),
		write('; '),
		wt('<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'),
		write(' '),
		wt(Y),
		write(']')
	;	write('('),
		wg(X),
		wl(Y),
		write(')')
	).
wt(X) :-
	functor(X, _, A),
	(	A = 0,
		!,
		wt0(X)
	;	A = 1,
		!,
		wt1(X)
	;	A = 2,
		!,
		wt2(X)
	;	wtn(X)
	).


wt0(!) :-
	!,
	write('() '),
	wp(!),
	write(' true').
wt0(X) :-
	atom(X),
	atom_concat(some, Y, X),
	!,
	(	\+flag('no-qvars'),
		\+flag('no-blank')
	->	(	rule_uvar(L),
			(	rule_conc
			->	memberchk(Y, L)
			;	(	memberchk(Y, L)
				->	true
				;	retract(rule_uvar(L)),
					assertz(rule_uvar([Y|L]))
				)
			)
		->	write('?U')
		;	write('_:sk')
		),
		write(Y)
	;	atomic_list_concat(['<http://localhost/var#sk', Y, '>'], Z),
		wt(Z)
	).
wt0(X) :-
	atom(X),
	atom_concat(allv, Y, X),
	!,
	(	\+flag('no-qvars')
	->	(	rule_uvar(L),
			(	rule_conc
			->	memberchk(Y, L)
			;	(	memberchk(Y, L)
				->	true
				;	retract(rule_uvar(L)),
					assertz(rule_uvar([Y|L]))
				)
			)
		->	write('?U')
		;	write('_:sk')
		),
		write(Y)
	;	atomic_list_concat(['<http://localhost/var#U', Y, '>'], Z),
		wt(Z)
	).
wt0(X) :-
	atom(X),
	atom_concat(avar, Y, X),
	!,
	atomic_list_concat(['<http://localhost/var#x', Y, '>'], Z),
	wt(Z).
wt0(X) :-
	(	\+flag(traditional)
	->	true
	;	flag(nope)
	),
	\+flag('no-qvars'),
	\+flag('no-blank'),
	sub_atom(X, 0, 22, _, '<http://localhost/var#'),
	!,
	sub_atom(X, 22, _, 1, Y),
	(	\+sub_atom(Y, 0, 2, _, 'qe'),
		rule_uvar(L),
		(	rule_conc
		->	memberchk(Y, L)
		;	(	memberchk(Y, L)
			->	true
			;	retract(rule_uvar(L)),
				assertz(rule_uvar([Y|L]))
			)
		)
	->	write('?')
	;	write('_:')
	),
	write(Y).
wt0(X) :-
	(	wtcache(X, W)
	->	true
	;	(	\+flag('no-qnames'),
			atom(X),
			sub_atom(X, I, 1, J, '#'),
			J > 1,
			sub_atom(X, 0, I, _, C),
			atom_concat(C, '#>', D),
			pfx(E, D),
			K is J-1,
			sub_atom(X, _, K, 1, F),
			atom_codes(F, G),
			atom_codes('^[A-Z_a-z][\\\\-0-9A-Z_a-z]*$', H),
			regex(H, G, _)
		->	atom_concat(E, F, W)
		;	(	\+flag(strings),
				atom(X),
				\+ (sub_atom(X, 0, 1, _, '<'), sub_atom(X, _, 1, 0, '>')),
				X \= true,
				X \= false
			->	W = literal(X, type('<http://eulersharp.sourceforge.net/2003/03swap/prolog#atom>'))
			;	W = X
			)
		),
		assertz(wtcache(X, W))
	),
	(	W = literal(X, type('<http://eulersharp.sourceforge.net/2003/03swap/prolog#atom>'))
	->	wt2(W)
	;	write(W)
	).


wt1(X) :-
	X =.. [B|C],
	wt(C),
	write(' '),
	wp(B),
	write(' true').


wt2(literal(X, lang(Y))) :-
	!,
	write('"'),
	atom_codes(X, U),
	escape_unicode(U, V),
	atom_codes(Z, V),
	write(Z),
	write('"@'),
	write(Y).
wt2(literal(X, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	!,
	write('"'),
	atom_codes(X, U),
	escape_unicode(U, V),
	atom_codes(Z, V),
	write(Z),
	write('"').
wt2(literal(X, type(Y))) :-
	!,
	write('"'),
	atom_codes(X, U),
	escape_unicode(U, V),
	atom_codes(Z, V),
	write(Z),
	write('"^^'),
	wt(Y).
wt2('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>'([X|Y], Z)) :-
	flag(nope),
	flag(tquery),
	!,
	'<http://www.w3.org/2000/10/swap/log#conjunction>'(Y, U),
	write('{'),
	wt(U),
	write('. _: '),
	wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#true>'),
	write(' '),
	wt(Z),
	write('} '),
	wp('<http://www.w3.org/2000/10/swap/log#implies>'),
	write(' {'),
	wt(X),
	write('}').
wt2('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'([X|Y], Z)) :-
	flag(nope),
	flag(tquery),
	!,
	'<http://www.w3.org/2000/10/swap/log#conjunction>'(Y, U),
	write('{'),
	wt(U),
	write('. _: '),
	wp('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#true>'),
	write(' '),
	wt(Z),
	write('} '),
	wp('<http://www.w3.org/2000/10/swap/log#implies>'),
	write(' {'),
	wt(X),
	write('}').
wt2('<http://www.w3.org/2000/10/swap/log#implies>'(X, Y)) :-
	(	flag(nope)
	->	U = X
	;	(	X = when(A, B)
		->	c_append(B, istep(_, _, _, _), C),
			U = when(A, C)
		;	cn_conj(X, V),
			c_append(V, istep(_, _, _, _), U)
		)
	),
	(	flag('rule-histogram')
	->	(	U = when(D, E)
		->	c_append(E, pstep(_), F),
			Z = when(D, F)
		;	cn_conj(U, W),
			c_append(W, pstep(_), Z)
		)
	;	Z = U
	),
	(	rule_uvar(R)
	->	true
	;	R = []
	),
	assertz(rule_uvar(R)),
	(	catch(clause(Y, Z), _, fail)
	->	wg(Y),
		write(' <= '),
		wg(X)
	;	wg(X),
		write(' => '),
		assertz(rule_conc),
		wg(Y),
		retract(rule_conc)
	),
	retract(rule_uvar(_)),
	!.
wt2(':-'(X, Y)) :-
	(	rule_uvar(R)
	->	true
	;	R = []
	),
	assertz(rule_uvar(R)),
	wg(X),
	write(' <= '),
	wg(Y),
	retract(rule_uvar(_)),
	!.
wt2(is(O, T)) :-
	!,
	(	number(T),
		T < 0
	->	P = -,
		Q is -T,
		S = [Q]
	;	T =.. [P|S]
	),
	wg(S),
	write(' '),
	wp(P),
	write(' '),
	wg(O).
wt2(prolog:X) :-
	!,
	(	X = '\'C\''
	->	Y = 'C'
	;	(	X = '\';\''
		->	Y = disjunction
		;	prolog_sym(Y, X, _)
		)
	),
	atomic_list_concat(['<http://eulersharp.sourceforge.net/2003/03swap/prolog#', Y, '>'], Z),
	wt0(Z).
wt2(X) :-
	X =.. [P, S, O],
	(	prolog_sym(_, P, _)
	->	wt([S, O]),
		write(' '),
		wp(P),
		write(' true')
	;	wg(S),
		write(' '),
		wp(P),
		write(' '),
		wg(O)
	).


wtn(exopred(P, S, O)) :-
	!,
	X =.. [P, S, O],
	wt2(X).
wtn(X) :-
	X =.. [B|C],
	(	atom(B),
		\+sub_atom(B, 0, 1, _, '<'),
		\+prolog_sym(_, B, _),
		X \= true,
		X \= false
	->	wt([B|C]),
		write('^'),
		wp('<http://eulersharp.sourceforge.net/2003/03swap/prolog#univ>')
	;	wt(C),
		write(' '),
		wp(B),
		write(' true')
	).


wg(X) :-
	functor(X, F, A),
	(	(	F = exopred,
			!
		;	F = cn,
			!
		;	prolog_sym(_, F, _),
			F \= true,
			F \= false,
			F \= '-',
			!
		;	A >= 2,
			F \= '.',
			F \= '[|]',
			F \= ':',
			F \= literal,
			F \= rdiv
		)
	->	write('{'),
		indentation(2),
		nb_getval(fdepth, D),
		E is D+1,
		nb_setval(fdepth, E),
		wt(X),
		nb_setval(fdepth, D),
		indentation(-2),
		write('}')
	;	wt(X)
	).


wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>') :-
	\+flag('no-qnames'),
	!,
	write('a').
wp('<http://www.w3.org/2000/10/swap/log#implies>') :-
	\+flag('no-qnames'),
	!,
	write('=>').
wp(':-') :-
	\+flag('no-qnames'),
	!,
	write('<=').
wp(X) :-
	(	prolog_sym(Y, X, _),
		X \= true,
		X \= false
	->	atomic_list_concat(['<http://eulersharp.sourceforge.net/2003/03swap/prolog#', Y, '>'], Z),
		wt(Z)
	;	wg(X)
	).


wk([]) :-
	!.
wk([X|Y]) :-
	write(', '),
	wt(X),
	wk(Y).


wl([]) :-
	!.
wl([X|Y]) :-
	write(' '),
	wg(X),
	wl(Y).


wq([], _) :-
	!.
wq([X|Y], allv) :-
	!,
	write('@forAll '),
	wt(X),
	wk(Y),
	write('. ').
wq([X|Y], some) :-
	(	\+flag('no-qvars'),
		\+flag('no-blank')
	->	write('@forSome '),
		wt(X),
		wk(Y),
		write('. ')
	;	true
	).


wb([]) :-
	!.
wb([X = Y|Z]) :-
	wp('<http://www.w3.org/2000/10/swap/reason#binding>'),
	write(' [ '),
	wp('<http://www.w3.org/2000/10/swap/reason#variable>'),
	write(' '),
	wv(X),
	write('; '),
	wp('<http://www.w3.org/2000/10/swap/reason#boundTo>'),
	write(' '),
	wv(Y),
	write('];'),
	nl,
	indent,
	wb(Z).


wv(X) :-
	atom(X),
	atom_concat(avar, Y, X),
	!,
	write('[ '),
	wp('<http://www.w3.org/2004/06/rei#uri>'),
	write(' "http://localhost/var#x'),
	write(Y),
	write('"]').
wv(X) :-
	atom(X),
	atom_concat(some, Y, X),
	!,
	write('[ '),
	wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'),
	write(' '),
	wp('<http://www.w3.org/2000/10/swap/reason#Existential>'),
	write('; '),
	wp('<http://www.w3.org/2004/06/rei#nodeId>'),
	write(' "_:sk'),
	write(Y),
	write('"]').
wv(X) :-
	atom(X),
	sub_atom(X, 0, 22, _, '<http://localhost/var#'),
	!,
	write('[ '),
	wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'),
	write(' '),
	wp('<http://www.w3.org/2000/10/swap/reason#Existential>'),
	write('; '),
	wp('<http://www.w3.org/2004/06/rei#nodeId>'),
	write(' "http://localhost/var#'),
	sub_atom(X, 22, _, 1, Q),
	write(Q),
	write('"]').
wv(X) :-
	atom(X),
	sub_atom(X, 1, _, 1, U),
	atomic_list_concat(['<', U, '>'], X),
	!,
	write('[ '),
	wp('<http://www.w3.org/2004/06/rei#uri>'),
	write(' "'),
	write(U),
	write('"]').
wv(X) :-
	wg(X).


ws(cn(X)) :-
	!,
	last(X, Y),
	ws(Y).
ws(X) :-
	X =.. Y,
	(	flag(tquery)
	->	true
	;	last(Y, Z),
		(	\+number(Z),
			Z \= rdiv(_, _)
		->	true
		;	write(' ')
		)
	).


wct([]) :-
	!,
	nl.
wct([A]) :-
	!,
	wcf(A),
	nl.
wct([A|B]) :-
	wcf(A),
	write(','),
	wct(B).


wcf(A) :-
	var(A),
	!.
wcf(literal(A, _)) :-
	!,
	write('"'),
	atom_codes(A, B),
	escape_string(C, B),
	subst([[[0'"], [0'", 0'"]]], C, D),
	atom_codes(E, D),
	write(E),
	write('"').
wcf(A) :-
	atom(A),
	sub_atom(A, 0, 22, _, '<http://localhost/var#'),
	!,
	sub_atom(A, 22, _, 1, B),
	write('_:'),
	write(B).
wcf(A) :-
	atom(A),
	sub_atom(A, 0, 1, _, '<'),
	!,
	sub_atom(A, 1, _, 1, B),
	write(B).
wcf(A) :-
	write(A).


indent:-
	nb_getval(indentation, A),
	tab(A).


indentation(C) :-
	nb_getval(indentation, A),
	B is A+C,
	nb_setval(indentation, B).



% --------
% builtins
% --------


% DEPRECATED
'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#allAncestors>'(A, B) :-
	(	flag(nope),
		\+flag(ances)
	->	throw(no_ances_flag)
	;	true
	),
	within_scope(_),
	hstep(C, D),
	unify(A, C),
	(	D = true
	->	B = true
	;	findall(X,
			(	ancestor(X, A)
			),
			L
		),
		clist(L, M),
		unify(M, B)
	).


% DEPRECATED
'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#allAssertedAncestors>'(A, B) :-
	(	flag(nope),
		\+flag(ances)
	->	throw(no_ances_flag)
	;	true
	),
	within_scope(_),
	hstep(C, D),
	unify(A, C),
	(	D = true
	->	B = true
	;	findall(X,
			(	ancestor(X, A),
				hstep(Y, true),
				unify(X, Y)
			),
			L
		),
		clist(L, M),
		unify(M, B)
	).


% DEPRECATED
'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#allDescendents>'(A, B) :-
	(	flag(nope),
		\+flag(ances)
	->	throw(no_ances_flag)
	;	true
	),
	within_scope(_),
	hstep(C, _),
	unify(A, C),
	findall(X,
		(	ancestor(A, X)
		),
		L
	),
	clist(L, M),
	unify(M, B).


% DEPRECATED
'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#assertedTriple>'(A, B) :-
	(	flag(nope),
		\+flag(ances)
	->	throw(no_ances_flag)
	;	true
	),
	hstep(B, true),
	unify(A, B).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>'(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, B)|C], D) :-
	within_scope(_),
	(	nb_getval(bnet, done)
	->	true
	;	bnet,
		nb_setval(bnet, done)
	),
	bvar(A),
	bval(B),
	bcon(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, B)], C, D).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#binaryEntropy>'(A, B) :-
	getnumber(A, C),
	(	C =:= 0.0
	->	B is 0.0
	;	(	C =:= 1.0
		->	B is 0.0
		;	B is -(C*log(C)+(1-C)*log(1-C))/log(2)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#call>'(Sc, A) :-
	within_scope(Sc),
	call(A).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#cartesianProduct>'(A, B) :-
	findall(C,
		(	cartesian(A, C)
		),
		B
	).


% DEPRECATED
'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#closure>'(Sc, A) :-
	within_scope(Sc),
	hstep(A, _).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#distinct>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	distinct(A, B)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#findall>'(Sc, [A, B, C|D]) :-
	within_scope(Sc),
	\+is_list(B),
	(	D = [F]
	->	findall(A,
			B,
			E,
			F
		)
	;	findall(A,
			B,
			E
		)
	),
	(	flag(warn)
	->	copy_term([A, B, E|D], [Ac, Bc, Ec|Dc]),
		labelvars([Ac, Bc, Ec|Dc], 0, _),
		(	fact('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#findall>'(Sc, [Ac, Bc, G|H]))
		->	(	E \= G
			->	format(user_error, '** WARNING ** conflicting_findall_answers ~w VERSUS ~w~n', [[A, B, G|H], [A, B, E|D]])
			;	true
			)
		;	assertz(fact('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#findall>'(Sc, [Ac, Bc, Ec|Dc])))
		)
	;	true
	),
	E = C.


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#format>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))|B], literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, D),
			preformat(B, E),
			format_to_chars(D, E, F),
			atom_codes(C, F)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#graphCopy>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	copy_term_nat(A, C),
			labelvars(C, 0, _, some),
			clist(L, C),
			sort(L, M),
			clist(M, K),
			unify(K, B)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#graphDifference>'(A, B) :-
	nonvar(A),
	difference(A, M),
	unify(M, B).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#graphIntersection>'(A, B) :-
	nonvar(A),
	intersection(A, M),
	unify(M, B).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#graphList>'(A, B) :-
	clistflat(B, A).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#label>'(A, literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	nonvar(A)
		),
		(	atom(A),
			(	sub_atom(A, 0, 22, _, '<http://localhost/var#')
			->	sub_atom(A, 22, _, 1, B)
			;	atom_concat(some, C, A),
				atomic_list_concat(['sk', C], B)
			)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#labelvars>'(A, B) :-
	(	got_labelvars(A, B)
	->	true
	;	copy_term_nat(A, B),
		nb_getval(wn, W),
		labelvars(B, W, N),
		nb_setval(wn, N),
		assertz(got_labelvars(A, B))
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#length>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	(	getlist(A, C)
			->	true
			;	clistflat(C, A)
			),
			length(C, B)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#max>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	bmax(A, B)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#min>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	bmin(A, B)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#notLabel>'(A, B) :-
	\+'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#label>'(A, B).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#numeral>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	getnumber(A, B)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#optional>'(Sc, A) :-
	within_scope(Sc),
	(	\+call(A)
	->	true
	;	call(A)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#propertyChainExtension>'([A], [B, C]) :-
	!,
	D =.. [A, B, C],
	catch(call(D), _, fail).
'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#propertyChainExtension>'([A|B], [C, D]) :-
	E =.. [A, C, F],
	catch(call(E), _, fail),
	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#propertyChainExtension>'(B, [F, D]).


% DEPRECATED
'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#reason>'(literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), B) :-
	when(
		(	ground(A)
		),
		(	sub_atom(A, 0, 4, _, 'eye '),
			sub_atom(A, 4, _, 0, C),
			(	current_prolog_flag(windows, true)
			->	A1 = ['cmd.exe', '/C']
			;	A1 = []
			),
			(	current_prolog_flag(argv, Argv),
				append(Argu, ['--'|_], Argv)
			->	append(Argu, ['--'], A2)
			;	A2 = ['eye']
			),
			append([A1, A2, [C]], A4),
			findall([G, ' '],
				(	member(G, A4)
				),
				H
			),
			flatten(H, I),
			atomic_list_concat(I, J),
			exec(J, B)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#reverse>'(A, B) :-
	reverse(A, B).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#roc>'(St, [Sen, Asp]) :-
	getnumber(St, K),
	(	getnumber(Sen, S)
	->	Asp is 1-(1-exp(-K*(S-1)))*(1+exp(K))/(1+exp(-K*(S-1)))/(1-exp(K))
	;	getnumber(Asp, A),
		Sen is (1-exp(-K*A))*(1+exp(-K))/(1+exp(-K*A))/(1-exp(-K))
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#sigmoid>'(A, B) :-
	getnumber(A, C),
	B is 1/(1+exp(-C)).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#sort>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	quicksort(A, B)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#stringEscape>'(literal(X, Y), literal(Z, Y)) :-
	when(
		(	ground(X)
		),
		(	atom_codes(X, U),
			escape_string(U, V),
			atom_codes(Z, V)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#sublist>'(A, B) :-
	when(
		(	nonvar(A),
			nonvar(B)
		),
		(	forall(
				(	member(C, B)
				),
				(	member(C, A)
				)
			)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#trace>'(X, Y) :-
	ignore(get_time(X)),
	(	flag(strings)
	->	true
	;	write('#TRACE '),
		copy_term_nat(Y, Z),
		labelvars(Z, 0, _, avar),
		wg(Z),
		nl
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tripleList>'(A, [B, C, D]) :-
	A =.. [C, B, D].


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#true>'(_, A) :-
	when(
		(	nonvar(A)
		),
		(	A =:= 1.0
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tuple>'(X, Y) :-
	when(
		(	nonvar(X)
		;	ground(Y)
		),
		(	(	is_list(Y),
				length(Y, I),
				I < 8
			->	Z =.. [tuple, X|Y]
			;	Z =.. [tuple, X, Y]
			),
			(	call(Z)
			->	true
			;	var(X),
				nb_getval(tuple, M),
				N is M+1,
				nb_setval(tuple, N),
				atom_number(A, N),
				atomic_list_concat(['<http://localhost/var#t', A, '>'], X),
				assertz(Z)
			)
		)
	).


'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#wwwFormEncode>'(literal(X, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground(X)
		;	ground(Y)
		),
		(	(	ground(X)
			->	www_form_encode(X, Z),
				atom_codes(Z, U),
				subst([[[0'%, 0'2, 0'0], [0'+]]], U, V),
				atom_codes(Y, V)
			;	www_form_encode(X, Y)
			)
		)
	).


% DEPRECATED
'<http://www.w3.org/2005/xpath-functions#resolve-uri>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))],
	literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([A, B])
		),
		(	resolve_uri(A, B, C)
		)
	).


% DEPRECATED
'<http://www.w3.org/2005/xpath-functions#substring>'([literal(A, _), B|C], literal(D, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([A, B, C])
		),
		(	atom_codes(A, U),
			(	C = []
			->	length(U, E),
				F is E-B
			;	C = [F]
			),
			sub_atom(A, B, F, _, D)
		)
	).


% DEPRECATED
'<http://www.w3.org/2005/xpath-functions#substring-after>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))],
	literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([A, B])
		),
		(	sub_atom(A, _, _, W, B),
			sub_atom(A, _, W, 0, C)
		)
	).


% DEPRECATED
'<http://www.w3.org/2005/xpath-functions#substring-before>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))],
	literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([A, B])
		),
		(	sub_atom(A, W, _, _, B),
			sub_atom(A, 0, W, _, C)
		)
	).


'<http://www.w3.org/2000/10/swap/list#append>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	getlist(A, C),
			append(C, B)
		)
	).


'<http://www.w3.org/2000/10/swap/list#first>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	getlist(A, C),
			C = [B|D],
			nonvar(D)
		)
	).


'<http://www.w3.org/2000/10/swap/list#in>'(A, B) :-
	when(
		(	nonvar(B)
		),
		(	getlist(B, C),
			member(A, C)
		)
	).


'<http://www.w3.org/2000/10/swap/list#last>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	getlist(A, C),
			last(C, B)
		)
	).


'<http://www.w3.org/2000/10/swap/list#member>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	getlist(A, C),
			member(B, C)
		)
	).


'<http://www.w3.org/2000/10/swap/list#rest>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	getlist(A, C),
			C = [_|B]
		)
	).


'<http://www.w3.org/2000/10/swap/log#conclusion>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	reset_gensym,
			tmp_file(Tmp1),
			open(Tmp1, write, Ws1, [encoding(utf8)]),
			tell(Ws1),
			(	flag('no-qnames')
			->	true
			;	forall(
					(	pfx(C, D)
					),
					(	(	\+flag(traditional)
						->	format('PREFIX ~w ~w~n', [C, D])
						;	format('@prefix ~w ~w.~n', [C, D])
						)
					)
				),
				nl
			),
			labelvars(A, 0, _),
			wt(A),
			write('.'),
			nl,
			told,
			tmp_file(Tmp2),
			!,
			(	current_prolog_flag(windows, true)
			->	A1 = ['cmd.exe', '/C']
			;	A1 = []
			),
			(	current_prolog_flag(argv, Argv),
				append(Argu, ['--'|_], Argv)
			->	append(Argu, ['--'], A2)
			;	A2 = ['eye']
			),
			append([A1, A2, ['--nope', Tmp1, '--pass-all', '>', Tmp2]], A4),
			findall([G, ' '],
				(	member(G, A4)
				),
				H
			),
			flatten(H, I),
			atomic_list_concat(I, J),
			(	catch(exec(J, _), _, fail)
			->	n3_n3p(Tmp2, semantics),
				absolute_uri(Tmp2, Tmp),
				atomic_list_concat(['<', Tmp, '>'], Res),
				semantics(Res, B),
				labelvars(B, 0, _),
				delete_file(Tmp1),
				delete_file(Tmp2)
			;	delete_file(Tmp1),
				delete_file(Tmp2),
				fail
			)
		)
	).


'<http://www.w3.org/2000/10/swap/log#conjunction>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	cnt(graph),
			nb_getval(graph, N),
			conjoin(N, A, 0, _),
			findall(C,
				(	graph(N, C)
				),
				L
			),
			retractall(graph(N, _)),
			sort(L, M),
			clist(M, K),
			unify(K, B)
		)
	).


:- if(current_prolog_flag(dialect, swi)).
'<http://www.w3.org/2000/10/swap/log#dtlit>'(A, B) :-
	nonvar(B),
	!,
	dtlit(A, B).
:- endif.
'<http://www.w3.org/2000/10/swap/log#dtlit>'(A, B) :-
	when(
		(	ground(A)
		;	nonvar(B)
		),
		(	(	nonvar(B)
			->	dtlit(A, B)
			;	A = [E, D],
				getcodes(E, F),
				atom_codes(C, F),
				B = literal(C, type(D))
			)
		)
	).


'<http://www.w3.org/2000/10/swap/log#equalTo>'(X, Y) :-
	unify(X, Y).


'<http://www.w3.org/2000/10/swap/log#implies>'(X, Y) :-
	implies(X, Y, _),
	\+prfstep('<http://www.w3.org/2000/10/swap/log#implies>'(X, Y), _, _, _, _, _, _, _),
	Y \= answer(_, _, _, _, _, _, _, _),
	Y \= goal.


'<http://www.w3.org/2000/10/swap/log#includes>'(X, Y) :-
	when(
		(	nonvar(X),
			nonvar(Y)
		),
		(	cnt(graph),
			nb_getval(graph, N),
			copy_term(X, Z),
			labelvars(Z, 0, _),
			agraph(N, Z),
			qgraph(N, Y)
		)
	).


'<http://www.w3.org/2000/10/swap/log#rawType>'(A, B) :-
	raw_type(A, C),
	C = B.


'<http://www.w3.org/2000/10/swap/log#notEqualTo>'(X, Y) :-
	\+'<http://www.w3.org/2000/10/swap/log#equalTo>'(X, Y).


'<http://www.w3.org/2000/10/swap/log#notIncludes>'(X, Y) :-
	\+'<http://www.w3.org/2000/10/swap/log#includes>'(X, Y).


'<http://www.w3.org/2000/10/swap/log#semantics>'(X, Y) :-
	when(
		(	nonvar(X)
		),
		(	nonvar(X),
			(	semantics(X, Q)
			->	Y = Q
			;	sub_atom(X, 1, _, 1, Z),
				n3_n3p(Z, semantics),
				semantics(X, Y)
			)
		)
	).


'<http://www.w3.org/2000/10/swap/log#uri>'(X, literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	nonvar(X)
		;	nonvar(Y)
		),
		(	atom(X),
			\+sub_atom(X, 0, 22, _, '<http://localhost/var#'),
			sub_atom(X, 1, _, 1, Y),
			atomic_list_concat(['<', Y, '>'], X),
			!
		;	nonvar(Y),
			atomic_list_concat(['<', Y, '>'], X)
		)
	).


'<http://www.w3.org/2000/10/swap/math#absoluteValue>'(X, Z) :-
	when(
		(	ground(X)
		),
		(	getnumber(X, U),
			Z is abs(U)
		)
	).


'<http://www.w3.org/2000/10/swap/math#atan2>'([X, Y], Z) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			Z is atan(U/V)
		)
	).


'<http://www.w3.org/2000/10/swap/math#cos>'(X, Z) :-
	when(
		(	ground(X)
		;	ground(Z)
		),
		(	getnumber(X, U),
			Z is cos(U),
			!
		;	getnumber(Z, W),
			X is acos(W)
		)
	).


'<http://www.w3.org/2000/10/swap/math#cosh>'(X, Z) :-
	when(
		(	ground(X)
		;	ground(Z)
		),
		(	getnumber(X, U),
			Z is cosh(U),
			!
		;	getnumber(Z, W),
			X is acosh(W)
		)
	).


'<http://www.w3.org/2000/10/swap/math#degrees>'(X, Z) :-
	when(
		(	ground(X)
		;	ground(Z)
		),
		(	getnumber(X, U),
			Z is U*180/pi,
			!
		;	getnumber(Z, W),
			X is W*pi/180
		)
	).


'<http://www.w3.org/2000/10/swap/math#difference>'([X, Y], Z) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			Z is U-V)
	).


'<http://www.w3.org/2000/10/swap/math#equalTo>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			U =:= V
		)
	).


'<http://www.w3.org/2000/10/swap/math#exponentiation>'([X, Y], Z) :-
	when(
		(	ground([X, Y])
		;	ground([X, Z])
		),
		(	getnumber(X, U),
			(	getnumber(Y, V),
				Z is U**V,
				!
			;	getnumber(Z, W),
				W =\= 0,
				U =\= 0,
				Y is log(W)/log(U)
			)
		)
	).


'<http://www.w3.org/2000/10/swap/math#greaterThan>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			U > V
		)
	).


'<http://www.w3.org/2000/10/swap/math#integerQuotient>'([X, Y], Z) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			(	V =\= 0
			->	Z is round(floor(U/V))
			;	throw(zero_division('<http://www.w3.org/2000/10/swap/math#integerQuotient>'([X, Y], Z)))
			)
		)
	).


'<http://www.w3.org/2000/10/swap/math#lessThan>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			U < V
		)
	).


'<http://www.w3.org/2000/10/swap/math#memberCount>'(X, Y) :-
	when(
		(	nonvar(X)
		),
		(	(	getlist(X, Z)
			->	true
			;	clistflat(Z, X)
			),
			length(Z, Y)
		)
	).


'<http://www.w3.org/2000/10/swap/math#negation>'(X, Z) :-
	when(
		(	ground(X)
		;	ground(Z)
		),
		(	getnumber(X, U),
			Z is -U,
			!
		;	getnumber(Z, W),
			X is -W
		)
	).


'<http://www.w3.org/2000/10/swap/math#notEqualTo>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			U =\= V
		)
	).


'<http://www.w3.org/2000/10/swap/math#notGreaterThan>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			U =< V
		)
	).


'<http://www.w3.org/2000/10/swap/math#notLessThan>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			U >= V
		)
	).


'<http://www.w3.org/2000/10/swap/math#product>'(X, Z) :-
	when(
		(	ground(X)
		),
		(	product(X, Z)
		)
	).


'<http://www.w3.org/2000/10/swap/math#quotient>'([X, Y], Z) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			(	V =\= 0
			->	Z is U/V
			;	throw(zero_division('<http://www.w3.org/2000/10/swap/math#quotient>'([X, Y], Z)))
			)
		)
	).


'<http://www.w3.org/2000/10/swap/math#remainder>'([X, Y], Z) :-
	when(
		(	ground([X, Y])
		),
		(	getnumber(X, U),
			getnumber(Y, V),
			(	V =\= 0
			->	Z is U-V*round(floor(U/V))
			;	throw(zero_division('<http://www.w3.org/2000/10/swap/math#remainder>'([X, Y], Z)))
			)
		)
	).


'<http://www.w3.org/2000/10/swap/math#rounded>'(X, Z) :-
	when(
		(	ground(X)
		),
		(	getnumber(X, U),
			Z is round(round(U))
		)
	).


'<http://www.w3.org/2000/10/swap/math#sin>'(X, Z) :-
	when(
		(	ground(X)
		;	ground(Z)
		),
		(	getnumber(X, U),
			Z is sin(U),
			!
		;	getnumber(Z, W),
			X is asin(W)
		)
	).


'<http://www.w3.org/2000/10/swap/math#sinh>'(X, Z) :-
	when(
		(	ground(X)
		;	ground(Z)
		),
		(	getnumber(X, U),
			Z is sinh(U),
			!
		;	getnumber(Z, W),
			X is asinh(W)
		)
	).


'<http://www.w3.org/2000/10/swap/math#sum>'(X, Z) :-
	when(
		(	ground(X)
		),
		(	sum(X, Z)
		)
	).


'<http://www.w3.org/2000/10/swap/math#tan>'(X, Z) :-
	when(
		(	ground(X)
		;	ground(Z)
		),
		(	getnumber(X, U),
			Z is tan(U),
			!
		;	getnumber(Z, W),
			X is atan(W)
		)
	).


'<http://www.w3.org/2000/10/swap/math#tanh>'(X, Z) :-
	when(
		(	ground(X)
		;	ground(Z)
		),
		(	getnumber(X, U),
			Z is tanh(U),
			!
		;	getnumber(Z, W),
			X is atanh(W)
		)
	).


'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(X, Y) :-
	when(
		(	nonvar(X)
		),
		(	X = [Y|Z],
			nonvar(Z)
		)
	).


'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(X, Y) :-
	when(
		(	nonvar(X)
		),
		(	X = [_|Y]
		)
	).


'<http://www.w3.org/2000/10/swap/string#concatenation>'(X, literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground(X)
		),
		(	findall(S,
				(	member(A, X),
					getcodes(A, S)
				),
				Z
			),
			flatten(Z, C),
			atom_codes(Y, C)
		)
	).


'<http://www.w3.org/2000/10/swap/string#contains>'(literal(X, _), literal(Y, _)) :-
	when(
		(	ground([X, Y])
		),
		(	sub_atom(X, _, _, _, Y)
		)
	).


'<http://www.w3.org/2000/10/swap/string#containsIgnoringCase>'(literal(X, _), literal(Y, _)) :-
	when(
		(	ground([X, Y])
		),
		(	downcase_atom(X, U),
			downcase_atom(Y, V),
			sub_atom(U, _, _, _, V)
		)
	).


'<http://www.w3.org/2000/10/swap/string#endsWith>'(literal(X, _), literal(Y, _)) :-
	when(
		(	ground([X, Y])
		),
		(	sub_atom(X, _, _, 0, Y)
		)
	).


'<http://www.w3.org/2000/10/swap/string#equalIgnoringCase>'(literal(X, _), literal(Y, _)) :-
	when(
		(	ground([X, Y])
		),
		(	downcase_atom(X, U),
			downcase_atom(Y, U)
		)
	).


'<http://www.w3.org/2000/10/swap/string#greaterThan>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	X @> Y
		)
	).


'<http://www.w3.org/2000/10/swap/string#lessThan>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	X @< Y
		)
	).


'<http://www.w3.org/2000/10/swap/string#matches>'(literal(X, _), literal(Y, _)) :-
	when(
		(	ground([X, Y])
		),
		(	atom_codes(X, U),
			atom_codes(Y, V),
			regex(V, U, _)
		)
	).


'<http://www.w3.org/2000/10/swap/string#notEqualIgnoringCase>'(X, Y) :-
	\+'<http://www.w3.org/2000/10/swap/string#equalIgnoringCase>'(X, Y).


'<http://www.w3.org/2000/10/swap/string#notGreaterThan>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	X @=< Y
		)
	).


'<http://www.w3.org/2000/10/swap/string#notLessThan>'(X, Y) :-
	when(
		(	ground([X, Y])
		),
		(	X @>= Y
		)
	).


'<http://www.w3.org/2000/10/swap/string#notMatches>'(X, Y) :-
	\+'<http://www.w3.org/2000/10/swap/string#matches>'(X, Y).


'<http://www.w3.org/2000/10/swap/string#replace>'([literal(X, _), literal(Search, _), literal(Replace, _)], literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([X, Search, Replace])
		),
		(	atomic_list_concat(Split, Search, X),
			atomic_list_concat(Split, Replace, Y)
		)
	).


'<http://www.w3.org/2000/10/swap/string#scrape>'([literal(X, _), literal(Y, _)], literal(Z, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([X, Y])
		),
		(	atom_codes(X, U),
			atom_codes(Y, V),
			regex(V, U, [W|_]),
			atom_codes(Z, W)
		)
	).


'<http://www.w3.org/2000/10/swap/string#search>'([literal(X, _), literal(Y, _)], Z) :-
	when(
		(	ground([X, Y])
		),
		(	atom_codes(X, U),
			atom_codes(Y, V),
			regex(V, U, L),
			findall(literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')),
				(	member(M, L),
					atom_codes(A, M)
				),
				Z
			)
		)
	).


'<http://www.w3.org/2000/10/swap/string#startsWith>'(literal(X, _), literal(Y, _)) :-
	when(
		(	ground([X, Y])
		),
		(	sub_atom(X, 0, _, _, Y)
		)
	).


'<http://www.w3.org/2000/10/swap/time#day>'(literal(X, _), literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground(X)
		),
		(	sub_atom(X, 8, 2, _, Y)
		)
	).


'<http://www.w3.org/2000/10/swap/time#month>'(literal(X, _), literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground(X)
		),
		(	sub_atom(X, 5, 2, _, Y)
		)
	).


'<http://www.w3.org/2000/10/swap/time#year>'(literal(X, _), literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground(X)
		),
		(	sub_atom(X, 0, 4, _, Y)
		)
	).



% -----------------------------------------------------------------------------
% RIF builtins
% according to RIF Datatypes and Built-Ins 1.0 -- http://www.w3.org/TR/rif-dtb/
% -----------------------------------------------------------------------------


% 4.1.1.1 pred:literal-not-identical

'<http://www.w3.org/2007/rif-builtin-predicate#literal-not-identical>'([literal(A, B), literal(C, B)], D) :-
	when(
		(	ground([A, B, C])
		),
		(	A \== C
		->	D = true
		;	D = false
		)
	).


% 4.4.4 pred:iri-string

'<http://www.w3.org/2007/rif-builtin-predicate#iri-string>'([A, literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))], C) :-
	when(
		(	nonvar(A)
		;	nonvar(B)
		),
		(	atom(A),
			sub_atom(A, 1, _, 1, U),
			atomic_list_concat(['<', U, '>'], A),
			!,
			(	U = B
			->	C = true
			;	C = false
			)
		;	nonvar(B),
			(	atomic_list_concat(['<', B, '>'], A)
			->	C = true
			;	C = false
			)
		)
	).


% 4.5.1 Numeric Functions

'<http://www.w3.org/2007/rif-builtin-function#numeric-add>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	sum([A, B], C)
		)
	).


'<http://www.w3.org/2007/rif-builtin-function#numeric-subtract>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			C is U-V
		)
	).


'<http://www.w3.org/2007/rif-builtin-function#numeric-multiply>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			C is U*V
		)
	).


'<http://www.w3.org/2007/rif-builtin-function#numeric-divide>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			(	V =\= 0
			->	C is U/V
			;	throw(zero_division('<http://www.w3.org/2007/rif-builtin-function#numeric-divide>'([A, B], C)))
			)
		)
	).


'<http://www.w3.org/2007/rif-builtin-function#numeric-integer-divide>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			(	V =\= 0
			->	C is integer(floor(U/V))
			;	throw(zero_division('<http://www.w3.org/2007/rif-builtin-function#numeric-integer-divide>'([A, B], C)))
			)
		)
	).


'<http://www.w3.org/2007/rif-builtin-function#numeric-mod>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			(	V =\= 0
			->	C is U-V*integer(floor(U/V))
			;	throw(zero_division('<http://www.w3.org/2007/rif-builtin-function#numeric-mod>'([A, B], C)))
			)
		)
	).


% 4.5.2.1 pred:numeric-equal

'<http://www.w3.org/2007/rif-builtin-predicate#numeric-equal>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			(	U =:= V
			->	C = true
			;	C = false
			)
		)
	).


% 4.5.2.2 pred:numeric-less-than

'<http://www.w3.org/2007/rif-builtin-predicate#numeric-less-than>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			(	U < V
			->	C = true
			;	C = false
			)
		)
	).


% 4.5.2.3 pred:numeric-greater-than

'<http://www.w3.org/2007/rif-builtin-predicate#numeric-greater-than>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			(	U > V
			->	C = true
			;	C = false
			)
		)
	).


% 4.5.2.4 pred:numeric-not-equal

'<http://www.w3.org/2007/rif-builtin-predicate#numeric-not-equal>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			(	U =\= V
			->	C = true
			;	C = false
			)
		)
	).


% 4.5.2.5 pred:numeric-less-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#numeric-less-than-or-equal>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			(	U =< V
			->	C = true
			;	C = false
			)
		)
	).


% 4.5.2.6 pred:numeric-greater-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#numeric-greater-than-or-equal>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getnumber(A, U),
			getnumber(B, V),
			(	U >= V
			->	C = true
			;	C = false
			)
		)
	).


% 4.6.1.1 func:not

'<http://www.w3.org/2007/rif-builtin-function#not>'([A], B) :-
	when(
		(	ground(A)
		),
		(	getbool(A, U),
			(	ground(B)
			->	getbool(B, V)
			;	V = B
			),
			inv(U, V)
		)
	).


% 4.6.2.1 pred:boolean-equal

'<http://www.w3.org/2007/rif-builtin-predicate#boolean-equal>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getbool(A, U),
			getbool(B, U)
		->	C = true
		;	C = false
		)
	).


% 4.6.2.2 pred:boolean-less-than

'<http://www.w3.org/2007/rif-builtin-predicate#boolean-less-than>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getbool(A, false),
			getbool(B, true)
		->	C = true
		;	C = false
		)
	).


% 4.6.2.3 pred:boolean-greater-than

'<http://www.w3.org/2007/rif-builtin-predicate#boolean-greater-than>'([A, B], C) :-
	when(
		(	ground([A, B])
		),
		(	getbool(A, true),
			getbool(B, false)
		->	C = true
		;	C = false
		)
	).


% 4.7.1.1 func:compare @@partial implementation: no collation

'<http://www.w3.org/2007/rif-builtin-function#compare>'([literal(A, B), literal(C, B)], D) :-
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

'<http://www.w3.org/2007/rif-builtin-function#concat>'(A, literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground(A)
		),
		(	findall(F,
				(	member(literal(S, type('<http://www.w3.org/2001/XMLSchema#string>')), A),
					atom_codes(S, F)
				),
				C
			),
			flatten(C, D),
			atom_codes(B, D)
		)
	 ).


% 4.7.1.3 func:string-join

'<http://www.w3.org/2007/rif-builtin-function#string-join>'([A, literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(B, D),
			findall([D, E],
				(	member(literal(F, type('<http://www.w3.org/2001/XMLSchema#string>')), A),
					atom_codes(F, E)
				),
				G
			),
			(	G = [[_, H]|I]
			->	flatten([H|I], J),
				atom_codes(C, J)
			;	C = ''
			)
		)
	).


% 4.7.1.4 func:substring

'<http://www.w3.org/2007/rif-builtin-function#substring>'([literal(A, _), B, C], literal(D, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	!,
	when(
		(	ground([A, B, C])
		),
		(	getint(B, I),
			getint(C, J),
			(	I < 1
			->	G is 0,
				H is J+I-1
			;	G is I-1,
				H is J
			),
			(	H < 0
			->	D = ''
			;	sub_atom(A, G, H, _, D)
			)
		)
	).
'<http://www.w3.org/2007/rif-builtin-function#substring>'([literal(A, _), B], literal(D, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([A, B])
		),
		(	getint(B, I),
			sub_atom(A, 0, E, 0, _),
			J is E-I+1,
			(	I < 1
			->	G is 0,
				H is J+I-1
			;	G is I-1,
				H is J
			),
			(	H < 0
			->	D = []
			;	sub_atom(A, G, H, _, D)
			)
		)
	).


% 4.7.1.5 func:string-length

'<http://www.w3.org/2007/rif-builtin-function#string-length>'([literal(A, _)], B) :-
	when(
		(	ground(A)
		),
		(	sub_atom(A, 0, B, 0, _)
		)
	).


% 4.7.1.6 func:upper-case

'<http://www.w3.org/2007/rif-builtin-function#upper-case>'([literal(A, B)], literal(C, B)) :-
	when(
		(	ground([A, B])
		),
		(	upcase_atom(A, C)
		)
	).


% 4.7.1.7 func:lower-case

'<http://www.w3.org/2007/rif-builtin-function#lower-case>'([literal(A, B)], literal(C, B)) :-
	when(
		(	ground([A, B])
		),
		(	downcase_atom(A, C)
		)
	).


% 4.7.1.8 func:encode-for-uri

'<http://www.w3.org/2007/rif-builtin-function#encode-for-uri>'([literal(A, B)], literal(C, B)) :-
	when(
		(	ground([A, B])
		),
		(	www_form_encode(A, C)
		)
	).


% 4.7.1.11 func:substring-before @@partial implementation: no collation

'<http://www.w3.org/2007/rif-builtin-function#substring-before>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))],
	literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([A, B])
		),
		(	sub_atom(A, W, _, _, B),
			sub_atom(A, 0, W, _, C)
		)
	).


% 4.7.1.12 func:substring-after @@partial implementation: no collation

'<http://www.w3.org/2007/rif-builtin-function#substring-after>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))],
	literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	when(
		(	ground([A, B])
		),
		(	sub_atom(A, _, _, W, B),
			sub_atom(A, _, W, 0, C)
		)
	).


% 4.7.2.1 pred:contains @@partial implementation: no collation

'<http://www.w3.org/2007/rif-builtin-predicate#contains>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	sub_atom(B, 0, D, 0, _),
			sub_atom(A, _, D, _, B)
		->	C = true
		;	C = false
		)
	).


% 4.7.2.2 pred:starts-with @@partial implementation: no collation

'<http://www.w3.org/2007/rif-builtin-predicate#starts-with>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	sub_atom(A, 0, _, _, B)
		->	C = true
		;	C = false
		)
	).


% 4.7.2.3 pred:ends-with @@partial implementation: no collation

'<http://www.w3.org/2007/rif-builtin-predicate#ends-with>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	sub_atom(A, _, _, 0, B)
		->	C = true
		;	C = false
		)
	).


% 4.7.2.4 pred:matches @@partial implementation: no flags

'<http://www.w3.org/2007/rif-builtin-predicate#matches>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			regex(V, U, _)
		->	C = true
		;	C = false
		)
	).


% 4.8.1.1 func:year-from-dateTime

'<http://www.w3.org/2007/rif-builtin-function#year-from-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(datetime(C, _, _, _, _, _, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.2 func:month-from-dateTime

'<http://www.w3.org/2007/rif-builtin-function#month-from-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(datetime(_, C, _, _, _, _, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.3 func:day-from-dateTime

'<http://www.w3.org/2007/rif-builtin-function#day-from-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(datetime(_, _, C, _, _, _, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.4 func:hours-from-dateTime

'<http://www.w3.org/2007/rif-builtin-function#hours-from-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(datetime(_, _, _, C, _, _, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.5 func:minutes-from-dateTime

'<http://www.w3.org/2007/rif-builtin-function#minutes-from-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(datetime(_, _, _, _, C, _, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.6 func:seconds-from-dateTime

'<http://www.w3.org/2007/rif-builtin-function#seconds-from-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(datetime(_, _, _, _, _, C, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.7 func:year-from-date

'<http://www.w3.org/2007/rif-builtin-function#year-from-date>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(date(C, _, _, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.8 func:month-from-date

'<http://www.w3.org/2007/rif-builtin-function#month-from-date>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(date(_, C, _, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.9 func:day-from-date

'<http://www.w3.org/2007/rif-builtin-function#day-from-date>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(date(_, _, C, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.10 func:hours-from-time

'<http://www.w3.org/2007/rif-builtin-function#hours-from-time>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(time(C, _, _, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.11 func:minutes-from-time

'<http://www.w3.org/2007/rif-builtin-function#minutes-from-time>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(time(_, C, _, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.12 func:seconds-from-time

'<http://www.w3.org/2007/rif-builtin-function#seconds-from-time>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(time(_, _, C, _), U),
			(	nonvar(B)
			->	C =:= B
			;	C = B
			)
		)
	).


% 4.8.1.13 func:years-from-duration

'<http://www.w3.org/2007/rif-builtin-function#years-from-duration>'([literal(_, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], 0) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#years-from-duration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(yearmonthduration(C), U),
			D is C//12,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).


% 4.8.1.14 func:months-from-duration

'<http://www.w3.org/2007/rif-builtin-function#months-from-duration>'([literal(_, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], 0) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#months-from-duration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(yearmonthduration(C), U),
			D is C-(C//12)*12,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).


% 4.8.1.15 func:days-from-duration

'<http://www.w3.org/2007/rif-builtin-function#days-from-duration>'([literal(_, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], _) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#days-from-duration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(daytimeduration(C), U),
			D is integer(C)//86400,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).


% 4.8.1.16 func:hours-from-duration

'<http://www.w3.org/2007/rif-builtin-function#hours-from-duration>'([literal(_, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], _) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#hours-from-duration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(daytimeduration(C), U),
			D is (integer(C)-(integer(C)//86400)*86400)//3600,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).


% 4.8.1.17 func:minutes-from-duration

'<http://www.w3.org/2007/rif-builtin-function#minutes-from-duration>'([literal(_, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], _) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#minutes-from-duration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(daytimeduration(C), U),
			D is (integer(C)-(integer(C)//3600)*3600)//60,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).


% 4.8.1.18 func:seconds-from-duration

'<http://www.w3.org/2007/rif-builtin-function#seconds-from-duration>'([literal(_, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], _) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#seconds-from-duration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], B) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(daytimeduration(C), U),
			D is C-(integer(C)//60)*60,
			(	nonvar(B)
			->	D =:= B
			;	D = B
			)
		)
	).


% 4.8.1.19 func:timezone-from-dateTime

'<http://www.w3.org/2007/rif-builtin-function#timezone-from-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))],
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(datetime(_, _, _, _, _, _, C), U),
			(	ground(B)
			->	atom_codes(B, V),
				phrase(daytimeduration(D), V),
				D =:= C
			;	daytimeduration(C, E),
				atom_codes(B, E)
			)
		)
	).


% 4.8.1.20 func:timezone-from-date

'<http://www.w3.org/2007/rif-builtin-function#timezone-from-date>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>'))],
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(date(_, _, _, C), U),
			(	ground(B)
			->	atom_codes(B, V),
				phrase(daytimeduration(D), V),
				D =:= C
			;	daytimeduration(C, E),
				atom_codes(B, E)
			)
		)
	).


% 4.8.1.21 func:timezone-from-time

'<http://www.w3.org/2007/rif-builtin-function#timezone-from-time>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>'))],
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground(A)
		),
		(	atom_codes(A, U),
			phrase(time(_, _, _, C), U),
			(	ground(B)
			->	atom_codes(B, V),
				phrase(daytimeduration(D), V),
				D =:= C
			;	daytimeduration(C, E),
				atom_codes(B, E)
			)
		)
	).


% 4.8.1.22 func:subtract-dateTimes

'<http://www.w3.org/2007/rif-builtin-function#subtract-dateTimes>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D), U),
			phrase(datetime(E), V),
			F is D-E,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(daytimeduration(G), W),
				G =:= F
			;	daytimeduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.23 func:subtract-dates

'<http://www.w3.org/2007/rif-builtin-function#subtract-dates>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#date>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D), U),
			phrase(date(E), V),
			F is D-E,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(daytimeduration(G), W),
				G =:= F
			;	daytimeduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.24 func:subtract-times

'<http://www.w3.org/2007/rif-builtin-function#subtract-times>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#time>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(time(D), U),
			phrase(time(E), V),
			F is D-E,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(daytimeduration(G), W),
				G =:= F
			;	daytimeduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.25 func:add-yearMonthDurations

'<http://www.w3.org/2007/rif-builtin-function#add-yearMonthDurations>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(yearmonthduration(D), U),
			phrase(yearmonthduration(E), V),
			F is D+E,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(yearmonthduration(G), W),
				G =:= F
			;	yearmonthduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.26 func:subtract-yearMonthDurations

'<http://www.w3.org/2007/rif-builtin-function#subtract-yearMonthDurations>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(yearmonthduration(D), U),
			phrase(yearmonthduration(E), V),
			F is D-E,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(yearmonthduration(G), W),
				G =:= F
			;	yearmonthduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.27 func:multiply-yearMonthDuration

'<http://www.w3.org/2007/rif-builtin-function#multiply-yearMonthDuration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')), B],
	literal(C, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			phrase(yearmonthduration(D), U),
			getnumber(B, E),
			F is integer(round(D*E-1)+1),
			(	ground(C)
			->	atom_codes(C, W),
				phrase(yearmonthduration(G), W),
				G =:= F
			;	yearmonthduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.28 func:divide-yearMonthDuration

'<http://www.w3.org/2007/rif-builtin-function#divide-yearMonthDuration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')), B],
	literal(C, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			phrase(yearmonthduration(D), U),
			getnumber(B, E),
			F is integer(round(D/E-1)+1),
			(	ground(C)
			->	atom_codes(C, W),
				phrase(yearmonthduration(G), W),
				G =:= F
			;	yearmonthduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.29 func:divide-yearMonthDuration-by-yearMonthDuration

'<http://www.w3.org/2007/rif-builtin-function#divide-yearMonthDuration-by-yearMonthDuration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(yearmonthduration(D), U),
			phrase(yearmonthduration(E), V),
			F is D/E,
			(	ground(C)
			->	C =:= F
			;	C = F
			)
		)
	).


% 4.8.1.30 func:add-dayTimeDurations

'<http://www.w3.org/2007/rif-builtin-function#add-dayTimeDurations>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(daytimeduration(D), U),
			phrase(daytimeduration(E), V),
			F is D+E,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(daytimeduration(G), W),
				G =:= F
			;	daytimeduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.31 func:subtract-dayTimeDurations

'<http://www.w3.org/2007/rif-builtin-function#subtract-dayTimeDurations>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(daytimeduration(D), U),
			phrase(daytimeduration(E), V),
			F is D-E,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(daytimeduration(G), W),
				G =:= F
			;	daytimeduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.32 func:multiply-dayTimeDuration

'<http://www.w3.org/2007/rif-builtin-function#multiply-dayTimeDuration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')), B],
	literal(C, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			phrase(daytimeduration(D), U),
			getnumber(B, E),
			F is integer(round(D*E-1)+1),
			(	ground(C)
			->	atom_codes(C, W),
				phrase(daytimeduration(G), W),
				G =:= F
			;	daytimeduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.33 func:divide-dayTimeDuration

'<http://www.w3.org/2007/rif-builtin-function#divide-dayTimeDuration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')), B],
	literal(C, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			phrase(daytimeduration(D), U),
			getnumber(B, E),
			F is integer(round(D/E-1)+1),
			(	ground(C)
			->	atom_codes(C, W),
				phrase(daytimeduration(G), W),
				G =:= F
			;	daytimeduration(F, H),
				atom_codes(C, H)
			)
		)
	).


% 4.8.1.34 func:divide-dayTimeDuration-by-dayTimeDuration

'<http://www.w3.org/2007/rif-builtin-function#divide-dayTimeDuration-by-dayTimeDuration>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(daytimeduration(D), U),
			phrase(daytimeduration(E), V),
			F is D/E,
			(	ground(C)
			->	C =:= F
			;	C = F
			)
		)
	).


% 4.8.1.35 func:add-yearMonthDuration-to-dateTime

'<http://www.w3.org/2007/rif-builtin-function#add-yearMonthDuration-to-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D, E, F, G, H, I, J), U),
			phrase(yearmonthduration(K), V),
			L is E+K-1,
			Q is D+integer(floor(L/12)),
			R is L-integer(floor(L/12))*12+1,
			memotime(datime(Q, R, F, G, H, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			O is M+I+31536000-N-J,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(datetime(P), W),
				O =:= P
			;	Offset is -J,
				stamp_date_time(O, date(Year, Month, Day, Hour, Minute, Second, _, _, _), Offset),
				datetime(Year, Month, Day, Hour, Minute, Second, Offset, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.1.36 func:add-yearMonthDuration-to-date

'<http://www.w3.org/2007/rif-builtin-function#add-yearMonthDuration-to-date>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#date>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D, E, F, G), U),
			phrase(yearmonthduration(K), V),
			L is E+K-1,
			Q is D+integer(floor(L/12)),
			R is L-integer(floor(L/12))*12+1,
			memotime(datime(Q, R, F, 0, 0, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			O is (integer(floor(M+31536000-N-G))//60)*60,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(date(P), W),
				O =:= P
			;	date(O, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.1.37 func:add-dayTimeDuration-to-dateTime

'<http://www.w3.org/2007/rif-builtin-function#add-dayTimeDuration-to-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D, E, F, G, H, I, J), U),
			phrase(daytimeduration(K), V),
			L is I+K,
			memotime(datime(D, E, F, G, H, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			O is M+L+31536000-N-J,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(datetime(P), W),
				O =:= P
			;	Offset is -J,
				stamp_date_time(O, date(Year, Month, Day, Hour, Minute, Second, _, _, _), Offset),
				datetime(Year, Month, Day, Hour, Minute, Second, Offset, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.1.38 func:add-dayTimeDuration-to-date

'<http://www.w3.org/2007/rif-builtin-function#add-dayTimeDuration-to-date>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#date>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D, E, F, G), U),
			phrase(daytimeduration(K), V),
			L is integer(K),
			memotime(datime(D, E, F, 0, 0, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			O is (integer(floor(M+L+31536000-N))//86400)*86400-G,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(date(P), W),
				O =:= P
			;	date(O, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.1.39 func:add-dayTimeDuration-to-time

'<http://www.w3.org/2007/rif-builtin-function#add-dayTimeDuration-to-time>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#time>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(time(D, E, F, G), U),
			phrase(daytimeduration(K), V),
			L is F+K,
			memotime(datime(1972, 12, 31, D, E, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			Z is M+L+31536000-N-G,
			O is Z-86400*integer(floor(Z/86400)),
			(	ground(C)
			->	atom_codes(C, W),
				phrase(time(P), W),
				O =:= P-86400*integer(floor(P/86400))
			;	Offset is -G,
				stamp_date_time(O, date(_, _, _, Hour, Minute, Second, _, _, _), Offset),
				time(Hour, Minute, Second, Offset, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.1.40 func:subtract-yearMonthDuration-from-dateTime

'<http://www.w3.org/2007/rif-builtin-function#subtract-yearMonthDuration-from-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D, E, F, G, H, I, J), U),
			phrase(yearmonthduration(K), V),
			L is E-K-1,
			Q is D+integer(floor(L/12)),
			R is L-integer(floor(L/12))*12+1,
			memotime(datime(Q, R, F, G, H, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			O is M+I+31536000-N-J,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(datetime(P), W),
				O =:= P
			;	Offset is -J,
				stamp_date_time(O, date(Year, Month, Day, Hour, Minute, Second, _, _, _), Offset),
				datetime(Year, Month, Day, Hour, Minute, Second, Offset, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.1.41 func:subtract-yearMonthDuration-from-date

'<http://www.w3.org/2007/rif-builtin-function#subtract-yearMonthDuration-from-date>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#date>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D, E, F, G), U),
			phrase(yearmonthduration(K), V),
			L is E-K-1,
			Q is D+integer(floor(L/12)),
			R is L-integer(floor(L/12))*12+1,
			memotime(datime(Q, R, F, 0, 0, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			O is (integer(floor(M+31536000-N-G))//60)*60,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(date(P), W),
				O =:= P
			;	date(O, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.1.42 func:subtract-dayTimeDuration-from-dateTime

'<http://www.w3.org/2007/rif-builtin-function#subtract-dayTimeDuration-from-dateTime>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D, E, F, G, H, I, J), U),
			phrase(daytimeduration(K), V),
			L is I-integer(K),
			memotime(datime(D, E, F, G, H, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			O is M+L+31536000-N-J,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(datetime(P), W),
				O =:= P
			;	Offset is -J,
				stamp_date_time(O, date(Year, Month, Day, Hour, Minute, Second, _, _, _), Offset),
				datetime(Year, Month, Day, Hour, Minute, Second, Offset, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.1.43 func:subtract-dayTimeDuration-from-date

'<http://www.w3.org/2007/rif-builtin-function#subtract-dayTimeDuration-from-date>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#date>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D, E, F, G), U),
			phrase(daytimeduration(K), V),
			L is -integer(K),
			memotime(datime(D, E, F, 0, 0, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			O is (integer(floor(M+L+31536000-N))//86400)*86400-G,
			(	ground(C)
			->	atom_codes(C, W),
				phrase(date(P), W),
				O =:= P
			;	date(O, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.1.44 func:subtract-dayTimeDuration-from-time

'<http://www.w3.org/2007/rif-builtin-function#subtract-dayTimeDuration-from-time>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], literal(C, type('<http://www.w3.org/2001/XMLSchema#time>'))) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(time(D, E, F, G), U),
			phrase(daytimeduration(K), V),
			L is F-K,
			memotime(datime(1972, 12, 31, D, E, 0), M),
			memotime(datime(1971, 1, 1, 0, 0, 0), N),
			Z is M+L+31536000-N-G,
			O is Z-86400*integer(floor(Z/86400)),
			(	ground(C)
			->	atom_codes(C, W),
				phrase(time(P), W),
				O =:= P-86400*integer(floor(P/86400))
			;	Offset is -G,
				stamp_date_time(O, date(_, _, _, Hour, Minute, Second, _, _, _), Offset),
				time(Hour, Minute, Second, Offset, S),
				atom_codes(C, S)
			)
		)
	).


% 4.8.2.1 pred:dateTime-equal

'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D), U),
			phrase(datetime(E), V),
			(	D =:= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.2 pred:dateTime-less-than

'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-less-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D), U),
			phrase(datetime(E), V),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.3 pred:dateTime-greater-than

'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-greater-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D), U),
			phrase(datetime(E), V),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.4 pred:date-equal

'<http://www.w3.org/2007/rif-builtin-predicate#date-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#date>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D), U),
			phrase(date(E), V),
			(	D =:= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.5 pred:date-less-than

'<http://www.w3.org/2007/rif-builtin-predicate#date-less-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#date>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D), U),
			phrase(date(E), V),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.6 pred:date-greater-than

'<http://www.w3.org/2007/rif-builtin-predicate#date-greater-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#date>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D), U),
			phrase(date(E), V),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.7 pred:time-equal

'<http://www.w3.org/2007/rif-builtin-predicate#time-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#time>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(time(D), U),
			phrase(time(E), V),
			(	D =:= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.8 pred:time-less-than

'<http://www.w3.org/2007/rif-builtin-predicate#time-less-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#time>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(time(D), U),
			phrase(time(E), V),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.9 pred:time-greater-than

'<http://www.w3.org/2007/rif-builtin-predicate#time-greater-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#time>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(time(D), U),
			phrase(time(E), V),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.10 pred:duration-equal

'<http://www.w3.org/2007/rif-builtin-predicate#duration-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#duration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#duration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(duration(D), U),
			phrase(duration(E), V),
			(	D =:= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.11 pred:dayTimeDuration-less-than

'<http://www.w3.org/2007/rif-builtin-predicate#dayTimeDuration-less-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(daytimeduration(D), U),
			phrase(daytimeduration(E), V),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.12 pred:dayTimeDuration-greater-than

'<http://www.w3.org/2007/rif-builtin-predicate#dayTimeDuration-greater-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(daytimeduration(D), U),
			phrase(daytimeduration(E), V),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.13 pred:yearMonthDuration-less-than

'<http://www.w3.org/2007/rif-builtin-predicate#yearMonthDuration-less-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(yearmonthduration(D), U),
			phrase(yearmonthduration(E), V),
			(	D < E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.14 pred:yearMonthDuration-greater-than

'<http://www.w3.org/2007/rif-builtin-predicate#yearMonthDuration-greater-than>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(yearmonthduration(D), U),
			phrase(yearmonthduration(E), V),
			(	D > E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.15 pred:dateTime-not-equal

'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-not-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D), U),
			phrase(datetime(E), V),
			(	D =\= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.16 pred:dateTime-less-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-less-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D), U),
			phrase(datetime(E), V),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.17 pred:dateTime-greater-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#dateTime-greater-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(datetime(D), U),
			phrase(datetime(E), V),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.18 pred:date-not-equal

'<http://www.w3.org/2007/rif-builtin-predicate#date-not-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#date>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D), U),
			phrase(date(E), V),
			(	D =\= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.19 pred:date-less-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#date-less-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#date>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D), U),
			phrase(date(E), V),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.20 pred:date-greater-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#date-greater-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#date>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(date(D), U),
			phrase(date(E), V),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.21 pred:time-not-equal

'<http://www.w3.org/2007/rif-builtin-predicate#time-not-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#time>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(time(D), U),
			phrase(time(E), V),
			(	D =\= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.22 pred:time-less-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#time-less-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#time>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(time(D), U),
			phrase(time(E), V),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	 ).


% 4.8.2.23 pred:time-greater-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#time-greater-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#time>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(time(D), U),
			phrase(time(E), V),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.24 pred:duration-not-equal

'<http://www.w3.org/2007/rif-builtin-predicate#duration-not-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#duration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#duration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(duration(D), U),
			phrase(duration(E), V),
			(	D =\= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.25 pred:dayTimeDuration-less-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#dayTimeDuration-less-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(daytimeduration(D), U),
			phrase(daytimeduration(E), V),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.26 pred:dayTimeDuration-greater-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#dayTimeDuration-greater-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(daytimeduration(D), U),
			phrase(daytimeduration(E), V),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.27 pred:yearMonthDuration-less-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#yearMonthDuration-less-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(yearmonthduration(D), U),
			phrase(yearmonthduration(E), V),
			(	D =< E
			->	C = true
			;	C = false
			)
		)
	).


% 4.8.2.28 pred:yearMonthDuration-greater-than-or-equal

'<http://www.w3.org/2007/rif-builtin-predicate#yearMonthDuration-greater-than-or-equal>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'))], C) :-
	when(
		(	ground([A, B])
		),
		(	atom_codes(A, U),
			atom_codes(B, V),
			phrase(yearmonthduration(D), U),
			phrase(yearmonthduration(E), V),
			(	D >= E
			->	C = true
			;	C = false
			)
		)
	).


% 4.10.1.1 func:PlainLiteral-from-string-lang

'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-from-string-lang>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))],
	literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-from-string-lang>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')),
	literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))], literal(A, lang(C))) :-
	downcase_atom(B, C).


% 4.10.1.2 func:string-from-PlainLiteral

'<http://www.w3.org/2007/rif-builtin-function#string-from-PlainLiteral>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))], literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#string-from-PlainLiteral>'([literal(A, lang(_))], literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))).


% 4.10.1.3 func:lang-from-PlainLiteral

'<http://www.w3.org/2007/rif-builtin-function#lang-from-PlainLiteral>'([literal(_, type('<http://www.w3.org/2001/XMLSchema#string>'))], literal('', type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
	!.
'<http://www.w3.org/2007/rif-builtin-function#lang-from-PlainLiteral>'([literal(_, lang(A))], literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))).


% 4.10.1.4 func:PlainLiteral-compare @@partial implementation: no collation

'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-compare>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))], D) :-
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
'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-compare>'([literal(A, lang(B)), literal(C, lang(B))], D) :-
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

'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-length>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))], C) :-
	!,
	sub_atom(A, 0, C, 0, _).
'<http://www.w3.org/2007/rif-builtin-function#PlainLiteral-length>'([literal(A, lang(_))], C) :-
	sub_atom(A, 0, C, 0, _).


% 4.10.2.1 pred:matches-language-range @@partial implementation: no false results

'<http://www.w3.org/2007/rif-builtin-predicate#matches-language-range>'([literal(A, lang(B)), literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))], true) :-
	A \= '',
	atom_codes(C, U),
	regexp_wildcard(U, V),
	atom_codes(E, V),
	atomic_list_concat(['^', E], F),
	downcase_atom(F, G),
	downcase_atom(B, H),
	atom_codes(G, I),
	atom_codes(H, J),
	regex(I, J, _).


% 4.11.3.1 pred:is-list

'<http://www.w3.org/2007/rif-builtin-predicate#is-list>'([A], B) :-
	(	is_list(A)
	->	B = true
	;	B = false
	).


% 4.11.3.2 pred:list-contains

'<http://www.w3.org/2007/rif-builtin-predicate#list-contains>'([A, B], C) :-
	when(
		(	nonvar(A)
		),
		(	member(B, A)
		->	C = true
		;	C = false
		)
	).


% 4.11.4.1 func:make-list

'<http://www.w3.org/2007/rif-builtin-function#make-list>'(A, A).


% 4.11.4.2 func:count

'<http://www.w3.org/2007/rif-builtin-function#count>'([A], B) :-
	when(
		(	nonvar(A)
		),
		(	length(A, B)
		)
	).


% 4.11.4.3 func:get

'<http://www.w3.org/2007/rif-builtin-function#get>'([A, B], C) :-
	when(
		(	nonvar(A),
			ground(B)
		),
		(	getnumber(B, U),
			nth0(U, A, C)
		)
	).


% 4.11.4.4 func:sublist

'<http://www.w3.org/2007/rif-builtin-function#sublist>'([A, B, C], D) :-
	!,
	when(
		(	nonvar(A),
			ground([B, C])
		),
		(	getint(B, U),
			getint(C, V),
			length(A, W),
			(	U < 0
			->	I is W+U
			;	I is U
			),
			(	V < 0
			->	J is W+V
			;	J is V
			),
			append(E, F, A),
			length(E, I),
			append(D, G, F),
			K is J-I,
			(	length(D, K)
			->	true
			;	G = []
			),
			!
		)
	).
'<http://www.w3.org/2007/rif-builtin-function#sublist>'([A, B], C) :-
	when(
		(	nonvar(A),
			ground(B)
		),
		(	getint(B, U),
			length(A, W),
			(	U < 0
			->	I is W+U
			;	I is U
			),
			append(E, C, A),
			length(E, I),
			!
		)
	).


% 4.11.4.5 func:append

'<http://www.w3.org/2007/rif-builtin-function#append>'([A|B], C) :-
	append(A, B, C).


% 4.11.4.6 func:concatenate

'<http://www.w3.org/2007/rif-builtin-function#concatenate>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	append(A, B)
		)
	).


% 4.11.4.7 func:insert-before

'<http://www.w3.org/2007/rif-builtin-function#insert-before>'([A, B, C], D) :-
	when(
		(	nonvar(A),
			ground([B, C])
		),
		(	getint(B, U),
			length(A, W),
			(	U < 0
			->	I is W+U
			;	I is U
			),
			append(G, H, A),
			length(G, I),
			append([G, [C], H], D)
		)
	).


% 4.11.4.8 func:remove

'<http://www.w3.org/2007/rif-builtin-function#remove>'([A, B], C) :-
	when(
		(	nonvar(A),
			ground(B)
		),
		(	getint(B, U),
			length(A, W),
			(	U < 0
			->	I is W+U
			;	I is U
			),
			append(G, [_|T], A),
			length(G, I),
			append(G, T, C)
		)
	).


% 4.11.4.9 func:reverse

'<http://www.w3.org/2007/rif-builtin-function#reverse>'([A], B) :-
	reverse(A, B).


% 4.11.4.10 func:index-of

'<http://www.w3.org/2007/rif-builtin-function#index-of>'([A, B], C) :-
	when(
		(	nonvar(A),
			ground(B)
		),
		(	findall(I,
				(	nth0(I, A, B)
				),
				C
			)
		)
	).


% 4.11.4.11 func:union

'<http://www.w3.org/2007/rif-builtin-function#union>'(A, B) :-
	when(
		(	nonvar(A)
		),
		(	append(A, C),
			distinct(C, B)
		)
	).


% 4.11.4.12 func:distinct-values

'<http://www.w3.org/2007/rif-builtin-function#distinct-values>'([A], B) :-
	when(
		(	nonvar(A)
		),
		(	distinct(A, B)
		)
	).


% 4.11.4.13 func:intersect

'<http://www.w3.org/2007/rif-builtin-function#intersect>'([A, B], C) :-
	when(
		(	ground(A),
			ground(B)
		),
		(	findall(I,
				(	member(I, A),
					member(I, B)
				),
				C
			)
		)
	).


% 4.11.4.14 func:except

'<http://www.w3.org/2007/rif-builtin-function#except>'([A, B], C) :-
	when(
		(	ground(A),
			ground(B)
		),
		(	findall(I,
				(	member(I, A),
					\+member(I, B)
				),
				C
			)
		)
	).



% -------
% support
% -------


def_pfx('math:', '<http://www.w3.org/2000/10/swap/math#>').
def_pfx('e:', '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#>').
def_pfx('list:', '<http://www.w3.org/2000/10/swap/list#>').
def_pfx('xsd:', '<http://www.w3.org/2001/XMLSchema#>').
def_pfx('log:', '<http://www.w3.org/2000/10/swap/log#>').
def_pfx('r:', '<http://www.w3.org/2000/10/swap/reason#>').
def_pfx('rdfs:', '<http://www.w3.org/2000/01/rdf-schema#>').
def_pfx('time:', '<http://www.w3.org/2000/10/swap/time#>').
def_pfx('rdf:', '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>').
def_pfx('var:', '<http://localhost/var#>').
def_pfx('string:', '<http://www.w3.org/2000/10/swap/string#>').
def_pfx('owl:', '<http://www.w3.org/2002/07/owl#>').
def_pfx('n3:', '<http://www.w3.org/2004/06/rei#>').


put_pfx(_, URI) :-
	atomic_list_concat(['<', URI, '>'], U),
	pfx(_, U),
	!.
put_pfx(_, URI) :-
	atomic_list_concat(['<', URI, '>'], U),
	def_pfx(Pf, U),
	\+pfx(Pf, _),
	!,
	assertz(pfx(Pf, U)).
put_pfx(Pf, URI) :-
	atomic_list_concat(['<', URI, '>'], U),
	fresh_pf(Pf, Pff),
	assertz(pfx(Pff, U)).


fresh_pf(Pf, Pfx) :-
	atom_concat(Pf, ':', Pfx),
	\+pfx(Pfx, _),
	!.
fresh_pf(_, Pfx) :-
	gensym(ns, Pfn),
	fresh_pf(Pfn, Pfx).


cnt(A) :-
	catch(nb_getval(A, B), _, B = 0),
	C is B+1,
	nb_setval(A, C),
	(	flag('debug-cnt'),
		C mod 1000 =:= 0
	->	format(user_error, '~w = ~w~n', [A, C])
	;	true
	).


within_scope([A, B]) :-
	(	var(B)
	->	B = 1
	;	true
	),
	(	(	flag('no-span')
		;	B = 0
		)
	->	brake
	;	nb_getval(limit, C),
		(	C < B
		->	nb_setval(limit, B)
		;	true
		),
		span(B)
	),
	nb_getval(scope, A).


exopred(P, S, O) :-
	pred(P),
	call(P, S, O).


unify(A, B) :-
	nonvar(A),
	A = exopred(P, S, O),
	tpred(P, Q),
	unify(S, T),
	unify(O, R),
	(	(	nonvar(B)
		;	nonvar(Q)
		)
	->	atom(Q),
		B =.. [Q, T, R]
	),
	!.
unify(A, B) :-
	nonvar(B),
	B = exopred(P, S, O),
	tpred(P, Q),
	unify(S, T),
	unify(O, R),
	(	(	nonvar(A)
		;	nonvar(Q)
		)
	->	A =.. [Q, T, R]
	),
	!.
unify(A, B) :-
	nonvar(A),
	nonvar(B),
	A = cn(_),
	B = cn(_),
	!,
	(	ground(A)
	->	'<http://www.w3.org/2000/10/swap/log#includes>'(A, B),
		'<http://www.w3.org/2000/10/swap/log#includes>'(B, A)
	;	'<http://www.w3.org/2000/10/swap/log#includes>'(B, A),
		'<http://www.w3.org/2000/10/swap/log#includes>'(A, B)
	).
unify(A, B) :-
	nonvar(A),
	nonvar(B),
	A =.. [P, S, O],
	B =.. [P, T, R],
	!,
	unify(S, T),
	unify(O, R).
unify(A, A).


tpred(A, A) :-
	var(A),
	!.
tpred(A, B) :-
	atom(A),
	atom_concat(some, I, A),
	!,
	(	\+flag('no-qvars'),
		\+flag('no-blank')
	->	atom_concat('_:sk', I, B)
	;	atomic_list_concat(['<http://localhost/var#sk', I, '>'], B)
	).
tpred(A, A).


dn([A|B]) :-
	(	call(A)
	;	(	B = [C]
		->	true
		;	C = dn(B)
		),
		call(C)
	).


dlist([], false) :-
	!.
dlist([A], A) :-
	A \= dn(_),
	!.
dlist(A, dn(A)).


cn([A|B]) :-
	call(A),
	(	B = [C]
	->	true
	;	C = cn(B)
	),
	call(C).


clist([], true) :-
	!.
clist([A], A) :-
	A \= cn(_),
	!.
clist(A, cn(A)).


clistflat([], true) :-
	!.
clistflat([A], A) :-
	A \= cn(_),
	!.
clistflat(A, cn(B)) :-
	(	nonvar(A)
	->	cflat(A, C),
		distinct(C, B)
	;	distinct(B, A)
	).


cflat([], []) :-
	!.
cflat([A|B], C) :-
	cflat(B, D),
	copy_term(A, E),
	(	E = cn(F)
	->	append(F, D, C)
	;	(	E = true
		->	C = D
		;	C = [E|D]
		)
	).


cmember(A, cn(B)) :-
	member(A, B).
cmember(A, A) :-
	A \= cn(_).


clast(cn(A), B) :-
	!,
	last(A, B).
clast(A, A).


cn_conj(A, B) :-
	clist(C, A),
	c_d(C, D),
	c_list(D, B).


c_d([], []) :-
	!.
c_d([(A;B)|C], [(D;E)|F]) :-
	!,
	cn_conj(A, D),
	cn_conj(B, E),
	c_d(C, F).
c_d([A|B], [A|C]) :-
	c_d(B, C).


c_list([], true) :-
	!.
c_list([A], A) :-
	!.
c_list([A|B], (A, C)) :-
	c_list(B, C).


c_append((A, B), C, (A, D)) :-
	c_append(B, C, D),
	!.
c_append(A, B, (A, B)).


couple([], [], [], []).
couple([A|B], [C|D], [E|F], [[A, C, E]|G]) :-
	couple(B, D, F, G).


conjoin(_, [], _, _) :-
	!.
conjoin(N, [true|Y], I, J) :-
	!,
	conjoin(N, Y, I, J).
conjoin(N, [X|Y], I, K) :-
	copy_term_nat(X, Z),
	labelvars(Z, I, J, some),
	agraph(N, Z),
	conjoin(N, Y, J, K).


agraph(N, cn([X|Y])) :-
	!,
	unify(X, U),
	(	\+graph(N, U)
	->	assertz(graph(N, U))
	;	true
	),
	(	Y = [Z]
	->	true
	;	Z = cn(Y)
	),
	agraph(N, Z).
agraph(N, X) :-
	unify(X, U),
	(	\+graph(N, U)
	->	assertz(graph(N, U))
	;	true
	).


qgraph(N, cn([X|Y])) :-
	!,
	(	X = exopred(_, _, _)
	->	graph(N, T),
		unify(T, X)
	;	graph(N, X)
	),
	(	Y = [Z]
	->	true
	;	Z = cn(Y)
	),
	qgraph(N, Z).
qgraph(N, X) :-
	(	X = exopred(_, _, _)
	->	graph(N, T),
		unify(T, X)
	;	graph(N, X)
	).


difference([true, _], true) :-
	!.
difference([X, true], X) :-
	!.
difference([X, Y], Z) :-
	findall(U,
		(	cmember(U, X),
			\+(	(	cmember(V, Y),
					unify(U, V)
				)
			)
		),
		W
	),
	(	W = []
	->	Z = true
	;	clist(W, G),
		Z = G
	).


intersection([X], X) :-
	!.
intersection([true|_], true) :-
	!.
intersection([X|Y], Z) :-
	intersection(Y, I),
	(	I = true
	->	Z = true
	;	findall(U,
			(	cmember(U, X),
				cmember(V, I),
				unify(U, V)
			),
			W
		),
		clist(W, Z)
	).


cartesian([], []).
cartesian([A|B], [C|D]) :-
	member(C, A),
	cartesian(B, D).


distinct(A, B) :-
	(	ground(A)
	->	distinct_hash(A, B)
	;	distinct_value(A, B)
	).


distinct_hash([], []) :-
	retractall(hash_value(_, _)).
distinct_hash([A|B], C) :-
	term_index(A, D),
	(	hash_value(D, E)
	->	(	unify(A, E)
		->	C = F
		;	C = [A|F]
		)
	;	assertz(hash_value(D, A)),
		C = [A|F]
	),
	distinct_hash(B, F).


distinct_value([], []).
distinct_value([A|B], [A|D]) :-
	del(B, A, E),
	distinct_value(E, D).


del([], _, []).
del([A|B], C, D) :-
	copy_term(A, Ac),
	copy_term(C, Cc),
	unify(Ac, Cc),
	!,
	del(B, C, D).
del([A|B], C, [A|D]) :-
	del(B, C, D).


subst(_, [], []).
subst(A, B, C) :-
	member([D, E], A),
	append(D, F, B),
	!,
	append(E, G, C),
	subst(A, F, G).
subst(A, [B|C], [B|D]) :-
	subst(A, C, D).


quicksort([], []).
quicksort([A|B], C) :-
	split(A, B, D, E),
	quicksort(D, F),
	quicksort(E, G),
	append(F, [A|G], C).


split(_, [], [], []).
split(A, [B|C], [B|D], E) :-
	sort([A, B], [B, A]),
	!,
	split(A, C, D, E).
split(A, [B|C], D, [B|E]) :-
	split(A, C, D, E).


last_tail([], []) :-
	!.
last_tail([_|B], B) :-
	\+is_list(B),
	!.
last_tail([_|B], C) :-
	last_tail(B, C).


sum([], 0) :-
	!.
sum([A|B], C) :-
	getnumber(A, X),
	sum(B, D),
	C is X+D.


product([], 1) :-
	!.
product([A|B], C) :-
	getnumber(A, X),
	product(B, D),
	C is X*D.


rms(A, B) :-
	findall(C,
		(	member(D, A),
			getnumber(D, E),
			C is E*E
		),
		F
	),
	sum(F, G),
	length(F, H),
	B is sqrt(G/H).


bmax([A|B], C) :-
	bmax(B, A, C).


bmax([], A, A).
bmax([A|B], C, D) :-
	getnumber(A, X),
	getnumber(C, Y),
	(	X > Y
	->	bmax(B, A, D)
	;	bmax(B, C, D)
	).


bmin([A|B], C) :-
	bmin(B, A, C).


bmin([], A, A).
bmin([A|B], C, D) :-
	getnumber(A, X),
	getnumber(C, Y),
	(	X < Y
	->	bmin(B, A, D)
	;	bmin(B, C, D)
	).


inconsistent(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>')|B]) :-
	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'), B),
	!.
inconsistent(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>')|B]) :-
	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'), B),
	!.
inconsistent([_|B]) :-
	inconsistent(B).


inverse('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'),
	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>')) :-
	!.
inverse('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'),
	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>')).


bnet :-
	(	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'([A|B], _),
		sort(B, C),
		findall(Y,
			(	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'([A|X], Y),
				sort(X, C)
			),
			L
		),
		sum(L, S),
		length(L, N),
		Z is S/N,
		\+bcnd([A|B], _),
		assertz(bcnd([A|B], Z)),
		inverse(A, D),
		\+bcnd([D|B], _),
		E is 1-Z,
		assertz(bcnd([D|B], E)),
		fail
	;	bcnd(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, _)|B], _),
		(	\+bvar(A),
			assertz(bvar(A))
		;	true
		),
		member('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(C, _), B),
		\+bref(C, A),
		assertz(bref(C, A)),
		\+bvar(C),
		assertz(bvar(C)),
		fail
	;	true
	).


bval('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>').
bval('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>').


brel('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, _), '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(B, _)) :-
	bref(A, B),
	!.
brel(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(B, _)) :-
	bref(C, B),
	brel(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(C, _)).


bpar([], []) :-
	!.
bpar(['<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, _)|B], [A|C]) :-
	bpar(B, C).


bget(A, B, 1.0) :-
	memberchk(A, B),
	!.
bget('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'), B, 0.0) :-
	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'), B),
	!.
bget('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#F>'), B, C) :-
	(	memberchk('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'), B),
		!,
		C is 0.0
	;
		!,
		bget('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#boolean>'(A, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#T>'), B, D),
		C is 1-D
	).
bget(A, B, C) :-
	(	bgot(A, B, C)
	->	true
	;	(	member(X, B),
			brel(A, X),
			member(G, B),
			findall(_,
				(	member(Z, [A|B]),
					brel(G, Z)
				),
				[]
			),
			del(B, G, H),
			!,
			bget(G, [A|H], U),
			bget(A, H, V),
			bget(G, H, W),
			(	W < 1e-15
			->	C is 0.5
			;	E is U*V/W,
				bmin([E, 1.0], C)
			)
		;	findall([Z, Y],
				(	bcnd([A|O], P),
					bcon(O, B, Q),
					Z is P*Q,
					bpar(O, Y)
				),
				L
			),
			findall(Z,
				(	member([_, Z], L)
				),
				N
			),
			distinct(N, I),
			findall(Z,
				(	member(Y, I),
					findall(P,
						(	member([P, Y], L)
						),
						Q
					),
					sum(Q, R),
					length(Q, S),
					length(Y, T),
					(	Q = []
					->	Z is 0.0
					;	D is 2**(T-ceiling(log(S)/log(2))),
						(	D < 1
						->	Z is R*D
						;	Z is R
						)
					)
				),
				J
			),
			(	J = []
			->	C is 0.0
			;	bmax(J, C)
			)
		),
		assertz(bgot(A, B, C))
	).


bcon([], _, 1.0) :-
	!.
bcon(_, B, 0.5) :-
	inconsistent(B),
	!.
bcon([A|B], C, D) :-
	bget(A, C, E),
	bcon(B, [A|C], F),
	D is E*F.


tmp_file(A) :-
	(	current_prolog_flag(dialect, swi),
		current_prolog_flag(windows, true),
		current_prolog_flag(pid, B)
	->	atomic_list_concat(['pl_eye_', B, '_'], C)
	;	C = 'eye'
	),
	tmp_file(C, A).


readln(In, Line):-
	get_code(In, Code),
	readln(Code, Codes, In),
	atom_codes(Line, Codes).


readln(10, [], _) :-
	!.
readln(-1, [], _) :-
	!.
readln(Code, [Code|Codes], In) :-
	get_code(In, Next),
	readln(Next, Codes, In).


:- if(current_predicate(operating_system_support:system/2)).
exec(A, B) :-
	(	system(A, B)
	->	true
	;	B = 1
	),
	(	B =:= 0
	->	true
	;	throw(exec_error(A))
	).
:- else.
exec(A, B) :-
	shell(A, B),
	(	B =:= 0
	->	true
	;	throw(exec_error(A))
	).
:- endif.


:- if(current_prolog_flag(dialect, swi)).
term_index(A, B) :-
	term_hash(A, B).
:- else.
term_index(A, B) :-
	(	ground(A)
	->	term_hash(A, B)
	;	true
	).
:- endif.


if_then_else(A, B, C) :-
	(	call(A)
	->	call(B)
	;	call(C)
	).


soft_cut(A, B, C) :-
	(	call(A)
	*->	call(B)
	;	call(C)
	).


inv(false, true).
inv(true, false).


+(A, B, C) :-
	plus(A, B, C).


lookup(A, B, C) :-
	table(A, B, C),
	!.
lookup(A, B, C) :-
	var(A),
	nb_getval(table, M),
	N is M+1,
	nb_setval(table, N),
	atom_number(I, N),
	atomic_list_concat([B, '_table_entry_', I], A),
	assertz(table(A, B, C)).


escape_string([], []) :-
	!.
escape_string([0'\t|A], [0'\\, 0't|B]) :-
	!,
	escape_string(A, B).
escape_string([0'\b|A], [0'\\, 0'b|B]) :-
	!,
	escape_string(A, B).
escape_string([0'\n|A], [0'\\, 0'n|B]) :-
	!,
	escape_string(A, B).
escape_string([0'\r|A], [0'\\, 0'r|B]) :-
	!,
	escape_string(A, B).
escape_string([0'\f|A], [0'\\, 0'f|B]) :-
	!,
	escape_string(A, B).
escape_string([0'"|A], [0'\\, 0'"|B]) :-
	!,
	escape_string(A, B).
escape_string([0'\\|A], [0'\\, 0'\\|B]) :-
	!,
	escape_string(A, B).
escape_string([A|B], [A|C]) :-
	escape_string(B, C).


escape_squote([], []) :-
	!.
escape_squote([0''|A], [0'\\, 0''|B]) :-
	!,
	escape_squote(A, B).
escape_squote([A|B], [A|C]) :-
	escape_squote(B, C).


escape_unicode([], []) :-
	!.
escape_unicode([A, B|C], D) :-
	0xD800 =< A,
	A =< 0xDBFF,
	0xDC00 =< B,
	B =< 0xDFFF,
	E is 0x10000+(A-0xD800)*0x400+(B-0xDC00),
	(	0x100000 =< E
	->	with_output_to(codes(F), format('\\U00~16R', [E]))
	;	with_output_to(codes(F), format('\\U000~16R', [E]))
	),
	append(F, G, D),
	!,
	escape_unicode(C, G).
escape_unicode([A|B], C) :-
	0x10000 =< A,
	(	0x100000 =< A
	->	with_output_to(codes(D), format('\\U00~16R', [A]))
	;	with_output_to(codes(D), format('\\U000~16R', [A]))
	),
	append(D, E, C),
	!,
	escape_unicode(B, E).
escape_unicode([A|B], [A|C]) :-
	escape_unicode(B, C).


quant('<http://www.w3.org/2000/10/swap/log#implies>'(_, _), allv) :-
	!.
quant(':-'(_, _), allv) :-
	!.
quant(answer('<http://www.w3.org/2000/10/swap/log#implies>', _, _, _, _, _, _, _), allv) :-
	!.
quant(answer(_, _, _, _, _, _, '<http://www.w3.org/2000/10/swap/log#implies>', _), allv) :-
	!.
quant(answer(':-', _, _, _, _, _, _, _), allv) :-
	!.
quant(answer(_, _, _, _, _, _, ':-', _), allv) :-
	!.
quant(answer('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>', _, _, _, _, _, _, _), allv) :-
	!.
quant(answer(_, _, _, _, _, _, '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>', _), allv) :-
	!.
quant(answer(exopred, P, _, _, _, _, _, _), allv) :-
	P == '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>',
	!.
quant(_, some).


labelvars(cn([A|B]), C, D) :-
	!,
	quant(A, Q),
	labelvars(A, C, E, Q),
	(	B = [F]
	->	true
	;	F = cn(B)
	),
	labelvars(F, E, D).
labelvars(A, B, C) :-
	quant(A, Q),
	labelvars(A, B, C, Q).


labelvars(A, B, C, D) :-
	var(A),
	!,
	atom_number(E, B),
	atomic_list_concat([D, E], A),
	C is B+1.
labelvars(A, B, B, _) :-
	atomic(A),
	!.
labelvars([A|B], C, D, Q) :-
	!,
	labelvars(A, C, E, Q),
	labelvars(B, E, D, Q).
labelvars(A, B, C, Q) :-
	nonvar(A),
	functor(A, _, D),
	labelvars(0, D, A, B, C, Q).


labelvars(A, A, _, B, B, _) :-
	!.
labelvars(A, B, C, D, E, Q) :-
	F is A+1,
	arg(F, C, G),
	labelvars(G, D, H, Q),
	labelvars(F, B, C, H, E, Q).


relabel([], []) :-
	!.
relabel([A|B], [C|D]) :-
	!,
	relabel(A, C),
	relabel(B, D).
relabel(A, B) :-
	atom(A),
	!,
	(	'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#relabel>'(A, B)
	->	labelvars(B, 0, _)
	;	B = A
	).
relabel(A, A) :-
	number(A),
	!.
relabel(A, B) :-
	A =.. [C|D],
	relabel(C, E),
	relabel(D, F),
	B =.. [E|F].


partconc(_, [], []).
partconc(A, [B|C], [B|D]) :-
	B = answer('<http://www.w3.org/2000/10/swap/log#implies>', _, _, _, _, _, _, _),
	!,
	partconc(A, C, D).
partconc(A, [B|C], [B|D]) :-
	commonvars(A, B, []),
	!,
	partconc(A, C, D).
partconc(A, [_|C], D) :-
	partconc(A, C, D).


commonvars(A, B, C) :-
	term_variables(A, D),
	term_variables(B, E),
	copy_term_nat([D, E], [F, G]),
	labelvars([F, G], 0, _),
	findall(H,
		(	member(H, F),
			member(H, G)
		),
		C
	).


getvars(A, B) :-
	findvars(A, C),
	distinct(C, B).


findvars(A, B) :-
	atomic(A),
	!,
	(	atom(A),
		sub_atom(A, 0, 22, _, '<http://localhost/var#')
	->	B = [A]
	;	B = []
	).
findvars([], []) :-
	!.
findvars([A|B], C) :-
	findvars(A, D),
	findvars(B, E),
	append(D, E, C),
	!.
findvars(A, B) :-
	A =.. C,
	findvars(C, B).


raw_type(A, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#List>') :-
	is_list(A),
	!.
raw_type(A, '<http://www.w3.org/2000/01/rdf-schema#Literal>') :-
	number(A),
	!.
raw_type(A, '<http://www.w3.org/2000/01/rdf-schema#Literal>') :-
	atom(A),
	\+ (sub_atom(A, 0, 1, _, '<'), sub_atom(A, _, 1, 0, '>')),
	!.
raw_type(literal(_, _), '<http://www.w3.org/2000/01/rdf-schema#Literal>') :-
	!.
raw_type(rdiv(_, _), '<http://www.w3.org/2000/01/rdf-schema#Literal>') :-
	!.
raw_type('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#epsilon>', '<http://www.w3.org/2000/01/rdf-schema#Literal>') :-
	!.
raw_type(cn(_), '<http://www.w3.org/2000/10/swap/log#Formula>') :-
	!.
raw_type(dn(_), '<http://www.w3.org/2000/10/swap/log#Formula>') :-
	!.
raw_type(set(_), '<http://www.w3.org/2000/10/swap/log#Set>') :-
	!.
raw_type(A, '<http://www.w3.org/2000/10/swap/log#Formula>') :-
	functor(A, B, C),
	B \= ':',
	C >= 2,
	!.
raw_type(_, '<http://www.w3.org/2000/10/swap/log#Other>').


getnumber(rdiv(A, B), C) :-
	nonvar(A),
	!,
	C is A/B.
getnumber(A, A) :-
	number(A),
	!.
getnumber(A, epsilon) :-
	nonvar(A),
	A = '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#epsilon>',
	!.
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')), B) :-
	!,
	ground(A),
	atom_codes(A, C),
	phrase(datetime(B), C).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')), B) :-
	!,
	ground(A),
	atom_codes(A, C),
	phrase(date(B), C).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')), B) :-
	!,
	ground(A),
	atom_codes(A, C),
	phrase(time(B), C).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#duration>')), B) :-
	!,
	ground(A),
	atom_codes(A, C),
	phrase(duration(B), C).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')), B) :-
	!,
	ground(A),
	atom_codes(A, C),
	phrase(yearmonthduration(B), C).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')), B) :-
	!,
	ground(A),
	atom_codes(A, C),
	phrase(daytimeduration(B), C).
getnumber(literal(A, _), B) :-
	ground(A),
	atom_codes(A, C),
	numeral(C, D),
	number_codes(B, D).


getint(A, B) :-
	getnumber(A, C),
	B is integer(round(C)).


getbool(literal(false, type('<http://www.w3.org/2001/XMLSchema#boolean>')), false).
getbool(literal(true, type('<http://www.w3.org/2001/XMLSchema#boolean>')), true).
getbool(literal('0', type('<http://www.w3.org/2001/XMLSchema#boolean>')), false).
getbool(literal('1', type('<http://www.w3.org/2001/XMLSchema#boolean>')), true).
getbool(false, false).
getbool(true, true).


getlist(A, A) :-
	var(A),
	!.
getlist(set(A), A) :-
	!.
getlist('<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>', []) :-
	!.
getlist([], []) :-
	!.
getlist([A|B], [C|D]) :-
	getlist(A, C),
	!,
	getlist(B, D).
getlist([A|B], [A|D]) :-
	!,
	getlist(B, D).
getlist(A, [B|C]) :-
	'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(A, B),
	'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(A, D),
	getlist(D, C).


getcodes(literal(A, _), B) :-
	!,
	atom_codes(A, B).
getcodes(A, B) :-
	with_output_to_chars(wg(A), B).


preformat([], []) :-
	!.
preformat([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))|B], [A|D]) :-
	!,
	preformat(B, D).
preformat([A|B], [A|D]) :-
	preformat(B, D).


numeral([0'-, 0'.|A], [0'-, 0'0, 0'.|A]) :-
	!.
numeral([0'+, 0'.|A], [0'+, 0'0, 0'.|A]) :-
	!.
numeral([0'.|A], [0'0, 0'.|A]) :-
	!.
numeral(A, B) :-
	append([C, [0'., 0'e], D], A),
	append([C, [0'., 0'0, 0'e], D], B),
	!.
numeral(A, B) :-
	append([C, [0'., 0'E], D], A),
	append([C, [0'., 0'0, 0'E], D], B),
	!.
numeral(A, B) :-
	last(A, 0'.),
	append(A, [0'0], B),
	!.
numeral(A, A).


rdiv_codes(rdiv(A, B), C) :-
	append(D, [0'.|E], C),
	append(D, E, F),
	number_codes(A, F),
	lzero(E, G),
	number_codes(B, [0'1|G]),
	!.
rdiv_codes(rdiv(A, 1), C) :-
	number_codes(A, C).


lzero([], []) :-
	!.
lzero([_|A], [0'0|B]) :-
	lzero(A, B).


dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#integer>'], B) :-
	integer(B),
	!,
	atom_number(A, B).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#double>'], B) :-
	float(B),
	!,
	atom_number(A, B).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#dateTime>'], B) :-
	(	number(B)
	->	datetime(B, C)
	;	B = date(Year, Month, Day, Hour, Minute, Second, Offset, _, _),
		datetime(Year, Month, Day, Hour, Minute, Second, Offset, C)
	),
	!,
	atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#date>'], B) :-
	(	number(B)
	->	date(B, C)
	;	B = date(Year, Month, Day, _, _, _, Offset, _, _),
		date(Year, Month, Day, Offset, C)
	),
	!,
	atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#time>'], B) :-
	(	number(B)
	->	time(B, C)
	;	B = date(_, _, _, Hour, Minute, Second, Offset, _, _),
		time(Hour, Minute, Second, Offset, C)
	),
	!,
	atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#duration>'], B) :-
	number(B),
	!,
	daytimeduration(B, C),
	atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'], B) :-
	number(B),
	!,
	yearmonthduration(B, C),
	atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'], B) :-
	number(B),
	!,
	daytimeduration(B, C),
	atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#boolean>'], A) :-
	atomic(A),
	getbool(A, A),
	!.
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://eulersharp.sourceforge.net/2003/03swap/prolog#atom>'], A) :-
	atomic(A),
	\+sub_atom(A, 0, 1, _, '<'),
	!.
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#string>'], literal(A, lang(_))) :-
	!.
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), B], literal(A, type(B))).


:- if(\+current_predicate(get_time/1)).
get_time(A) :-
	datime(B),
	mktime(B, C),
	A is C*1.0.
:- endif.


memotime(datime(A, B, C, D, E, F), G) :-
	(	mtime(datime(A, B, C, D, E, F), G)
	->	true
	;	date_time_stamp(date(A, B, C, D, E, F, 0, -, -), G),
		assertz(mtime(datime(A, B, C, D, E, F), G))
	).


datetime(A) -->
	int(B),
	[0'-],
	int(C),
	[0'-],
	int(D),
	[0'T],
	int(E),
	[0':],
	int(F),
	[0':],
	decimal(G),
	timezone(H),
	{	I is -H,
		date_time_stamp(date(B, C, D, E, F, G, I, -, -), A)
	}.


datetime(A, B, C, D, E, F, G) -->
	int(A),
	[0'-],
	int(B),
	[0'-],
	int(C),
	[0'T],
	int(D),
	[0':],
	int(E),
	[0':],
	decimal(F),
	timezone(G).


date(A) -->
	int(B),
	[0'-],
	int(C),
	[0'-],
	int(D),
	timezone(H),
	{	I is -H,
		date_time_stamp(date(B, C, D, 0, 0, 0, I, -, -), A)
	}.


date(A, B, C, D) -->
	int(A),
	[0'-],
	int(B),
	[0'-],
	int(C),
	timezone(D).


time(A) -->
	int(B),
	[0':],
	int(C),
	[0':],
	decimal(D),
	timezone(E),
	{	B = 24
	->	A is C*60+D-E
	;	A is B*3600+C*60+D-E
	}.


time(A, B, C, D) -->
	int(A),
	[0':],
	int(B),
	[0':],
	decimal(C),
	timezone(D).


duration(A) -->
	dsign(B),
	[0'P],
	years(C),
	months(D),
	days(E),
	dtime(F),
	{	A is B*(C*31556952+D*2629746+E*86400.0+F)
	}.


yearmonthduration(A) -->
	dsign(B),
	[0'P],
	years(C),
	months(D),
	{	A is B*(C*12+D)
	}.


daytimeduration(A) -->
	dsign(B),
	[0'P],
	days(C),
	dtime(D),
	{	A is B*(C*86400.0+D)
	}.


timezone(A) -->
	int(B),
	!,
	[0':],
	int(C),
	{	A is B*3600+C*60
	}.
timezone(0) -->
	[0'Z],
	!.
timezone(0) -->
	[].


dsign(1) -->
	[0'+].
dsign(-1) -->
	[0'-].
dsign(1) -->
	[].


dtime(A) -->
	[0'T],
	!,
	hours(B),
	minutes(C),
	seconds(D),
	{	A is B*3600+C*60+D
	}.
dtime(0) -->
	[].


years(A) -->
	int(A),
	[0'Y].
years(0) -->
	[].


months(A) -->
	int(A),
	[0'M].
months(0) -->
	[].


days(A) -->
	int(A),
	[0'D].
days(0) -->
	[].


hours(A) -->
	int(A),
	[0'H].
hours(0) -->
	[].


minutes(A) -->
	int(A),
	[0'M].
minutes(0) -->
	[].


seconds(A) -->
	decimal(A),
	[0'S].
seconds(0) -->
	[].


int(A) -->
	sgn(B),
	digit(C),
	digits(D),
	{	number_codes(A, [B, C|D])
	}.


decimal(A) -->
	sgn(B),
	digit(C),
	digits(D),
	fraction(E),
	{	append([B, C|D], E, F),
		number_codes(G, F),
		A is G*1.0
	}.


sgn(0'+) -->
	[0'+].
sgn(0'-) -->
	[0'-].
sgn(0'+) -->
	[].


fraction([0'., A|B]) -->
	[0'.],
	!,
	digit(A),
	digits(B).
fraction([]) -->
	[].


digits([A|B]) -->
	digit(A),
	digits(B).
digits([]) -->
	[].


digit(A) -->
	[A],
	{	code_type(A, digit)
	}.


datetime(A, B) :-
	stamp_date_time(A, date(Year, Month, Day, Hour, Minute, Second, _, _, _), 0),
	ycodes(Year, C),
	ncodes(Month, D),
	ncodes(Day, E),
	ncodes(Hour, F),
	ncodes(Minute, G),
	ncodes(Second, H),
	append([C, [0'-], D, [0'-], E, [0'T], F, [0':], G, [0':], H, [0'Z]], B).


datetime(Year, Month, Day, Hour, Minute, Second, Offset, B) :-
	ycodes(Year, C),
	ncodes(Month, D),
	ncodes(Day, E),
	ncodes(Hour, F),
	ncodes(Minute, G),
	ncodes(Second, H),
	(	Offset =:= 0
	->	append([C, [0'-], D, [0'-], E, [0'T], F, [0':], G, [0':], H, [0'Z]], B)
	;	(	Offset > 0
		->	I = [0'-],
			OHour is Offset//3600
		;	I = [0'+],
			OHour is -Offset//3600
		),
		ncodes(OHour, J),
		OMinute is (Offset mod 3600)//60,
		ncodes(OMinute, K),
		append([C, [0'-], D, [0'-], E, [0'T], F, [0':], G, [0':], H, I, J, [0':], K], B)
	).


date(A, B) :-
	N is A+3600*12,
	stamp_date_time(N, date(Year, Month, Day, _, _, _, _, _, _), 0),
	ycodes(Year, C),
	ncodes(Month, D),
	ncodes(Day, E),
	Offset is (round(floor(N)) mod 86400) - 3600*12,
	(	Offset =:= 0
	->	append([C, [0'-], D, [0'-], E, [0'Z]], B)
	;	(	Offset > 0
		->	I = [0'-],
			OHour is Offset//3600
		;	I = [0'+],
			OHour is -Offset//3600
		),
		ncodes(OHour, J),
		OMinute is (Offset mod 3600)//60,
		ncodes(OMinute, K),
		append([C, [0'-], D, [0'-], E, I, J, [0':], K], B)
	).


date(Year, Month, Day, Offset, B) :-
	ycodes(Year, C),
	ncodes(Month, D),
	ncodes(Day, E),
	(	Offset =:= 0
	->	append([C, [0'-], D, [0'-], E, [0'Z]], B)
	;	(	Offset > 0
		->	I = [0'-],
			OHour is Offset//3600
		;	I = [0'+],
			OHour is -Offset//3600
		),
		ncodes(OHour, J),
		OMinute is (Offset mod 3600)//60,
		ncodes(OMinute, K),
		append([C, [0'-], D, [0'-], E, I, J, [0':], K], B)
	).


time(A, B) :-
	stamp_date_time(A, date(_, _, _, Hour, Minute, Second, _, _, _), 0),
	ncodes(Hour, F),
	ncodes(Minute, G),
	ncodes(Second, H),
	append([F, [0':], G, [0':], H, [0'Z]], B).


time(Hour, Minute, Second, Offset, B) :-
	ncodes(Hour, F),
	ncodes(Minute, G),
	ncodes(Second, H),
	(	Offset =:= 0
	->	append([F, [0':], G, [0':], H, [0'Z]], B)
	;	(	Offset > 0
		->	I = [0'-],
			OHour is Offset//3600
		;	I = [0'+],
			OHour is -Offset//3600
		),
		ncodes(OHour, J),
		OMinute is (Offset mod 3600)//60,
		ncodes(OMinute, K),
		append([F, [0':], G, [0':], H, I, J, [0':], K], B)
	).


yearmonthduration(A, B) :-
	(	A < 0
	->	C = [0'-]
	;	C = []
	),
	D is abs(A),
	E is D//12,
	number_codes(E, Years),
	F is D-(D//12)*12,
	number_codes(F, Months),
	append([C, [0'P], Years, [0'Y], Months, [0'M]], B).


daytimeduration(A, B) :-
	AInt is round(floor(A)),
	AFrac is A-AInt,
	(	AInt < 0
	->	C = [0'-]
	;	C = []
	),
	D is abs(AInt),
	E is D//86400,
	number_codes(E, Days),
	F is (D-(D//86400)*86400)//3600,
	number_codes(F, Hours),
	G is (D-(D//3600)*3600)//60,
	number_codes(G, Minutes),
	H is D-(D//60)*60+AFrac,
	number_codes(H, Seconds),
	append([C, [0'P], Days, [0'D, 0'T], Hours, [0'H], Minutes, [0'M], Seconds, [0'S]], B).


ncodes(A, B) :-
	number_codes(A, D),
	(	A < 10
	->	append([[0'0], D], B)
	;	B = D
	).


ycodes(A, B) :-
	C is abs(A),
	number_codes(C, D),
	(	C < 10
	->	append([[0'0, 0'0, 0'0], D], E)
	;	(	C < 100
		->	append([[0'0, 0'0], D], E)
		;	(	C < 1000
			->	append([[0'0], D], E)
			;	E = D
			)
		)
	),
	(	A >= 0
	->	B = E
	;	B = [0'-|E]
	).


absolute_uri('-', '-') :-
	!.
absolute_uri(A, B) :-
	(	is_absolute_url(A)
	->	B = A
	;	absolute_file_name(A, C),
		prolog_to_os_filename(D, C),
		atom_codes(D, E),
		subst([[[0x20], [0'+]]], E, F),
		atom_codes(G, F),
		atomic_list_concat(['file://', G], B)
	).


resolve_uri(A, _, A) :-
	sub_atom(A, _, 1, _, ':'),
	!.
resolve_uri('', A, A) :-
	!.
resolve_uri('#', A, B) :-
	!,
	atomic_list_concat([A, '#'], B).
resolve_uri(A, B, A) :-
	\+sub_atom(B, _, 1, _, ':'),
	!.
resolve_uri(A, B, C) :-
	so_uri(U),
	atom_length(U, V),
	sub_atom(A, 0, 1, _, '#'),
	sub_atom(B, 0, V, _, U),
	!,
	atomic_list_concat([B, A], C).
resolve_uri(A, B, C) :-
	sub_atom(A, 0, 2, _, './'),
	!,
	sub_atom(A, 2, _, 0, R),
	resolve_uri(R, B, C).
resolve_uri(A, B, C) :-
	sub_atom(A, 0, 3, _, '../'),
	!,
	sub_atom(A, 3, _, 0, R),
	so_uri(U),
	atom_length(U, V),
	sub_atom(B, 0, V, D, U),
	sub_atom(B, V, D, _, E),
	sub_atom(E, F, 1, G, '/'),
	sub_atom(E, _, G, 0, H),
	\+sub_atom(H, _, _, _, '/'),
	K is V+F,
	sub_atom(B, 0, K, _, S),
	resolve_uri(R, S, C).
resolve_uri(A, B, C) :-
	so_uri(U),
	atom_length(U, V),
	sub_atom(A, 0, 1, _, '/'),
	sub_atom(B, 0, V, D, U),
	sub_atom(B, V, D, _, E),
	(	sub_atom(E, F, 1, _, '/')
	->	sub_atom(E, 0, F, _, G)
	;	G = E
	),
	!,
	atomic_list_concat([U, G, A], C).
resolve_uri(A, B, C) :-
	so_uri(U),
	atom_length(U, V),
	sub_atom(B, 0, V, D, U),
	sub_atom(B, V, D, _, E),
	(	sub_atom(E, F, 1, G, '/'),
		sub_atom(E, _, G, 0, H),
		\+sub_atom(H, _, _, _, '/')
	->	sub_atom(E, 0, F, _, I)
	;	I = E
	),
	!,
	atomic_list_concat([U, I, '/', A], C).
resolve_uri(A, _, _) :-
	nb_getval(line_number, Ln),
	throw(unresolvable_relative_uri(A, after_line(Ln))).


so_uri('http://').
so_uri('https://').
so_uri('ftp://').
so_uri('file://').


prolog_sym(abolish, abolish, rel).
prolog_sym(abort, abort, rel).
prolog_sym(abs, abs, func).
prolog_sym(absolute_file_name, absolute_file_name, rel).
prolog_sym(acos, acos, func).
prolog_sym(acosh, acosh, func).
prolog_sym(acyclic_term, acyclic_term, rel).
prolog_sym(alarm, alarm, rel).
prolog_sym(append, append, rel).
prolog_sym(arg, arg, rel).
prolog_sym(arithmetic_equal, =:=, rel).
prolog_sym(arithmetic_greater_than, >, rel).
prolog_sym(arithmetic_greater_than_or_equal, >=, rel).
prolog_sym(arithmetic_less_than, <, rel).
prolog_sym(arithmetic_less_than_or_equal, =<, rel).
prolog_sym(arithmetic_not_equal, =\=, rel).
prolog_sym(asin, asin, func).
prolog_sym(asinh, asinh, func).
prolog_sym(assert, assert, rel).
prolog_sym(asserta, asserta, rel).
prolog_sym(assertz, assertz, rel).
prolog_sym(at_end_of_stream, at_end_of_stream, rel).
prolog_sym(atan, atan, func).
prolog_sym(atan2, atan2, func).
prolog_sym(atanh, atanh, func).
prolog_sym(atom, atom, rel).
prolog_sym(atom_chars, atom_chars, rel).
prolog_sym(atom_codes, atom_codes, rel).
prolog_sym(atom_concat, atom_concat, rel).
prolog_sym(atom_length, atom_length, rel).
prolog_sym(atom_number, atom_number, rel).
prolog_sym(atomic, atomic, rel).
prolog_sym(atomic_concat, atomic_concat, rel).
prolog_sym(atomic_list_concat, atomic_list_concat, rel).
prolog_sym(b_getval, b_getval, rel).
prolog_sym(b_setval, b_setval, rel).
prolog_sym(bagof, bagof, rel).
prolog_sym(between, between, rel).
prolog_sym(break, break, rel).
prolog_sym('C', 'C', rel).
prolog_sym(call, call, rel).
prolog_sym(call_residue_vars, call_residue_vars, rel).
prolog_sym(callable, callable, rel).
prolog_sym(catch, catch, rel).
prolog_sym(ceiling, ceiling, func).
prolog_sym(char_code, char_code, rel).
prolog_sym(char_conversion, char_conversion, rel).
prolog_sym(char_type, char_type, rel).
prolog_sym(character_count, character_count, rel).
prolog_sym(clause, clause, rel).
prolog_sym(close, close, rel).
prolog_sym(code_type, code_type, rel).
prolog_sym(compare, compare, rel).
prolog_sym(compound, compound, rel).
prolog_sym(conjunction, ',', rel).
prolog_sym(copy_term, copy_term, rel).
prolog_sym(cos, cos, func).
prolog_sym(cosh, cosh, func).
prolog_sym(cputime, cputime, func).
prolog_sym(create_mutable, create_mutable, rel).
prolog_sym(create_prolog_flag, create_prolog_flag, rel).
prolog_sym(current_atom, current_atom, rel).
prolog_sym(current_char_conversion, current_char_conversion, rel).
prolog_sym(current_input, current_input, rel).
prolog_sym(current_key, current_key, rel).
prolog_sym(current_module, current_module, rel).
prolog_sym(current_op, current_op, rel).
prolog_sym(current_output, current_output, rel).
prolog_sym(current_predicate, current_predicate, rel).
prolog_sym(current_prolog_flag, current_prolog_flag, rel).
prolog_sym(cut, !, rel).
prolog_sym(cyclic_term, cyclic_term, rel).
prolog_sym(date_time_stamp, date_time_stamp, rel).
prolog_sym(date_time_value, date_time_value, rel).
prolog_sym(day_of_the_week, day_of_the_week, rel).
prolog_sym(delete, delete, rel).
prolog_sym(dif, dif, rel).
prolog_sym(discontiguous, discontiguous, rel).
prolog_sym(disjunction, ;, rel).
prolog_sym(display, display, rel).
prolog_sym(div, div, func).
prolog_sym(duplicate_term, duplicate_term, rel).
prolog_sym(dynamic, dynamic, rel).
prolog_sym(e, e, func).
prolog_sym(ensure_loaded, ensure_loaded, rel).
prolog_sym(environ, environ, rel).
prolog_sym(epsilon, epsilon, func).
prolog_sym(erase, erase, rel).
prolog_sym(erf, erf, func).
prolog_sym(erfc, erfc, func).
prolog_sym(exception, exception, rel).
prolog_sym(exists, exists, rel).
prolog_sym(exp, exp, func).
prolog_sym(fail, fail, rel).
prolog_sym(false, false, rel).
prolog_sym(file_base_name, file_base_name, rel).
prolog_sym(file_name_extension, file_name_extension, rel).
prolog_sym(findall, findall, rel).
prolog_sym(flatten, flatten, rel).
prolog_sym(float, float, rel).
prolog_sym(float_fractional_part, float_fractional_part, func).
prolog_sym(float_function, float, func).
prolog_sym(float_integer_part, float_integer_part, func).
prolog_sym(floor, floor, func).
prolog_sym(flush_output, flush_output, rel).
prolog_sym(forall, forall, rel).
prolog_sym(format, format, rel).
prolog_sym(format_time, format_time, rel).
prolog_sym(freeze, freeze, rel).
prolog_sym(frozen, frozen, rel).
prolog_sym(functor, functor, rel).
prolog_sym(garbage_collect, garbage_collect, rel).
prolog_sym(garbage_collect_atoms, garbage_collect_atoms, rel).
prolog_sym(gc, gc, rel).
prolog_sym(gcd, gcd, func).
prolog_sym(get, get, rel).
prolog_sym(get_byte, get_byte, rel).
prolog_sym(get_char, get_char, rel).
prolog_sym(get_code, get_code, rel).
prolog_sym(get_mutable, get_mutable, rel).
prolog_sym(get_time, get_time, rel).
prolog_sym(get0, get0, rel).
prolog_sym(getcwd, getcwd, rel).
prolog_sym(getrand, getrand, rel).
prolog_sym(ground, ground, rel).
prolog_sym(halt, halt, rel).
prolog_sym(if, soft_cut, rel).
prolog_sym(if_then, ->, rel).
prolog_sym(if_then_else, if_then_else, rel).
prolog_sym(ignore, ignore, rel).
prolog_sym(include, include, rel).
prolog_sym(initialization, initialization, rel).
prolog_sym(instance, instance, rel).
prolog_sym(integer, integer, rel).
prolog_sym(integer_conjunction, /\, func).
prolog_sym(integer_disjunction, \/, func).
prolog_sym(integer_exclusive_disjunction, xor, func).
prolog_sym(integer_function, integer, func).
prolog_sym(integer_left_logical_shift, <<, func).
prolog_sym(integer_negation, \, func).
prolog_sym(integer_power, ^, func).
prolog_sym(integer_quotient, //, func).
prolog_sym(integer_right_logical_shift, >>, func).
prolog_sym(is, is, rel).
prolog_sym(is_list, is_list, rel).
prolog_sym(is_stream, is_stream, rel).
prolog_sym(keysort, keysort, rel).
prolog_sym(last, last, rel).
prolog_sym(length, length, rel).
prolog_sym(lgamma, lgamma, func).
prolog_sym(line_count, line_count, rel).
prolog_sym(line_position, line_position, rel).
prolog_sym(listing, listing, rel).
prolog_sym(log, log, func).
prolog_sym(log10, log10, func).
prolog_sym(lsb, lsb, func).
prolog_sym(max, max, func).
prolog_sym(max_list, max_list, rel).
prolog_sym(member, member, rel).
prolog_sym(memberchk, memberchk, rel).
prolog_sym(message_to_string, message_to_string, rel).
prolog_sym(min, min, func).
prolog_sym(min_list, min_list, rel).
prolog_sym(minus, -, func).
prolog_sym(mod, mod, func).
prolog_sym(msb, msb, func).
prolog_sym(multifile, multifile, rel).
prolog_sym(name, name, rel).
prolog_sym(nb_current, nb_current, rel).
prolog_sym(nb_delete, nb_delete, rel).
prolog_sym(nb_getval, nb_getval, rel).
prolog_sym(nb_linkarg, nb_linkarg, rel).
prolog_sym(nb_linkval, nb_linkval, rel).
prolog_sym(nb_setarg, nb_setarg, rel).
prolog_sym(nb_setval, nb_setval, rel).
prolog_sym(nl, nl, rel).
prolog_sym(nonvar, nonvar, rel).
prolog_sym(not_provable, \+, rel).
prolog_sym(not_unifiable, \=, rel).
prolog_sym(nth, nth, rel).
prolog_sym(nth_clause, nth_clause, rel).
prolog_sym(nth0, nth0, rel).
prolog_sym(nth1, nth1, rel).
prolog_sym(number, number, rel).
prolog_sym(number_chars, number_chars, rel).
prolog_sym(number_codes, number_codes, rel).
prolog_sym(numbervars, numbervars, rel).
prolog_sym(numlist, numlist, rel).
prolog_sym(on_signal, on_signal, rel).
prolog_sym(once, once, rel).
prolog_sym(op, op, rel).
prolog_sym(open, open, rel).
prolog_sym(parse_time, parse_time, rel).
prolog_sym(peek_byte, peek_byte, rel).
prolog_sym(peek_char, peek_char, rel).
prolog_sym(peek_code, peek_code, rel).
prolog_sym(permutation, permutation, rel).
prolog_sym(phrase, phrase, rel).
prolog_sym(pi, pi, func).
prolog_sym(plus, +, rel).
prolog_sym(plus_function, +, func).
prolog_sym(popcount, popcount, func).
prolog_sym(portray_clause, portray_clause, rel).
prolog_sym(power, **, func).
prolog_sym(predicate_property, predicate_property, rel).
prolog_sym(predsort, predsort, rel).
prolog_sym(print, print, rel).
prolog_sym(print_message, print_message, rel).
prolog_sym(print_message_lines, print_message_lines, rel).
prolog_sym(product, *, func).
prolog_sym(prolog_flag, prolog_flag, rel).
prolog_sym(prolog_load_context, prolog_load_context, rel).
prolog_sym(prompt, prompt, rel).
prolog_sym(put, put, rel).
prolog_sym(put_byte, put_byte, rel).
prolog_sym(put_char, put_char, rel).
prolog_sym(put_code, put_code, rel).
prolog_sym(quotient, /, func).
prolog_sym(random, random, func).
prolog_sym(rational, rational, rel).
prolog_sym(rational_function, rational, func).
prolog_sym(rationalize, rationalize, func).
prolog_sym(read, read, rel).
prolog_sym(read_term, read_term, rel).
prolog_sym(recorda, recorda, rel).
prolog_sym(recorded, recorded, rel).
prolog_sym(recordz, recordz, rel).
prolog_sym(rem, rem, func).
prolog_sym(rename_file, rename_file, rel).
prolog_sym(repeat, repeat, rel).
prolog_sym(retract, retract, rel).
prolog_sym(retractall, retractall, rel).
prolog_sym(reverse, reverse, rel).
prolog_sym(round, round, func).
prolog_sym(same_length, same_length, rel).
prolog_sym(see, see, rel).
prolog_sym(seeing, seeing, rel).
prolog_sym(seen, seen, rel).
prolog_sym(select, select, rel).
prolog_sym(selectchk, selectchk, rel).
prolog_sym(set_input, set_input, rel).
prolog_sym(set_output, set_output, rel).
prolog_sym(set_prolog_flag, set_prolog_flag, rel).
prolog_sym(set_stream_position, set_stream_position, rel).
prolog_sym(setarg, setarg, rel).
prolog_sym(setof, setof, rel).
prolog_sym(setrand, setrand, rel).
prolog_sym(sign, sign, func).
prolog_sym(simple, simple, rel).
prolog_sym(sin, sin, func).
prolog_sym(sinh, sinh, func).
prolog_sym(skip, skip, rel).
prolog_sym(sort, sort, rel).
prolog_sym(source_file, source_file, rel).
prolog_sym(source_location, source_location, rel).
prolog_sym(sqrt, sqrt, func).
prolog_sym(stamp_date_time, stamp_date_time, rel).
prolog_sym(statistics, statistics, rel).
prolog_sym(stream_position, stream_position, rel).
prolog_sym(stream_position_data, stream_position_data, rel).
prolog_sym(stream_property, stream_property, rel).
prolog_sym(sub_atom, sub_atom, rel).
prolog_sym(sublist, sublist, rel).
prolog_sym(subsumes_term, subsumes_term, rel).
prolog_sym(succ, succ, rel).
prolog_sym(sum_list, sum_list, rel).
prolog_sym(tab, tab, rel).
prolog_sym(tan, tan, func).
prolog_sym(tanh, tanh, func).
prolog_sym(tell, tell, rel).
prolog_sym(telling, telling, rel).
prolog_sym(term_greater_than, @>, rel).
prolog_sym(term_greater_than_or_equal, @>=, rel).
prolog_sym(term_hash, term_index, rel).
prolog_sym(term_identical, ==, rel).
prolog_sym(term_less_than, @<, rel).
prolog_sym(term_less_than_or_equal, @=<, rel).
prolog_sym(term_not_identical, \==, rel).
prolog_sym(term_to_atom, term_to_atom, rel).
prolog_sym(term_variables, term_variables, rel).
prolog_sym(throw, throw, rel).
prolog_sym(time, time, rel).
prolog_sym(time_file, time_file, rel).
prolog_sym(told, told, rel).
prolog_sym(true, true, rel).
prolog_sym(truncate, truncate, func).
prolog_sym(unifiable, unifiable, rel).
prolog_sym(unify, =, rel).
prolog_sym(unify_with_occurs_check, unify_with_occurs_check, rel).
prolog_sym(univ, =.., rel).
prolog_sym(unknown, unknown, rel).
prolog_sym(update_mutable, update_mutable, rel).
prolog_sym(var, var, rel).
prolog_sym(variant, variant, rel).
prolog_sym(version, version, rel).
prolog_sym(when, when, rel).
prolog_sym(with_output_to, with_output_to, rel).
prolog_sym(write, write, rel).
prolog_sym(write_canonical, write_canonical, rel).
prolog_sym(write_term, write_term, rel).
prolog_sym(writeln, writeln, rel).
prolog_sym(writeq, writeq, rel).


dynamic_verb(Verb) :-
	(	(	atom(Verb)
		->	V = Verb
		;	Verb = isof(V)
		),
		\+sub_atom(V, 0, 1, _, '_')
	->	(	intern(V)
		->	true
		;	assertz(intern(V)),
			(	sub_atom(V, 0, 1, _, '\'')
			->	sub_atom(V, 1, _, 1, B)
			;	B = V
			),
			(	current_predicate(B/2)
			->	true
			;	dynamic(B/2)
			)
		)
	;	true
	).


% Regular Expressions inspired by http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html

regex(RE_esc, Input_esc, Outputs_esc) :-
	escape_string(RE, RE_esc),
	re(Parsed_RE, RE, []),
	(	RE = [0'^|_]
	->	Bos = true
	;	Bos = false
	),
	escape_string(Input, Input_esc),
	tokenize2(Parsed_RE, Input, Outputs, Bos),
	findall(Output_esc,
		(	member(Output, Outputs),
			escape_string(Output, Output_esc)
		),
		Outputs_esc
	),
	!.


tokenize2(_P_RE, [], [], true).
tokenize2(P_RE, Input, Output, Bos) :-
	(	rematch1(P_RE, Input, _, Output)
	->	true
	;	Bos = false,
		Input = [_|Inp],
		tokenize2(P_RE, Inp, Output, Bos)
	).


rematch1(union(RE1, _RE2), S, U, Selected) :-
	rematch1(RE1, S, U, Selected).
rematch1(union(_RE1, RE2), S, U, Selected) :-
	rematch1(RE2, S, U, Selected).
rematch1(conc(RE1, RE2), S, U, Selected) :-
	rematch1(RE1, S, U1, Sel1),
	rematch1(RE2, U1, U, Sel2),
	append(Sel1, Sel2, Selected).
rematch1(star(RE), S, U, Selected) :-
	rematch1(RE, S, U1, Sel1),
	rematch1(star(RE), U1, U, Sel2),
	append(Sel1, Sel2, Selected).
rematch1(star(_RE), S, S, []).
rematch1(qm(RE), S, U, Selected) :-
	rematch1(RE, S, U, Selected).
rematch1(qm(_RE), S, S, []).
rematch1(plus(RE), S, U, Selected) :-
	rematch1(RE, S, U1, Sel1),
	rematch1(star(RE), U1, U, Sel2),
	append(Sel1, Sel2, Selected).
rematch1(group(RE), S, U, Selected) :-
	rematch1(RE, S, U, Sel1),
	append(P, U, S),
	append(Sel1, [P], Selected).
rematch1(any, [_C1|U], U, []).
rematch1(char(C), [C|U], U, []).
rematch1(bos, S, S, []).
rematch1(eos, [], [], []).
rematch1(negSet(Set), [C|U], U, []) :-
	\+charSetMember(C, Set).
rematch1(posSet(Set), [C|U], U, []) :-
	charSetMember(C, Set).


charSetMember(C, [char(C)|_]).
charSetMember(C, [range(C1, C2)|_]) :-
	C1 =< C,
	C =< C2.
charSetMember(C, [negSet(Set)|_]) :-
	\+charSetMember(C, Set).
charSetMember(C, [posSet(Set)|_]) :-
	charSetMember(C, Set).
charSetMember(C, [_|T]) :-
	charSetMember(C, T).


re(Z) -->
	basicRE(W),
	reTail(W, Z).


reTail(W, Z) -->
	[0'|],
	basicRE(X),
	reTail(union(W, X), Z).
reTail(W, W) -->
	[].


basicRE(Z) -->
	simpleRE(W),
	basicREtail(W, Z).


basicREtail(W, Z) -->
	simpleRE(X),
	basicREtail(conc(W, X), Z).
basicREtail(W, W) -->
	[].


simpleRE(Z) -->
	elementalRE(W),
	simpleREtail(W, Z).


simpleREtail(W, star(W)) -->
	[0'*].
simpleREtail(W, qm(W)) -->
	[0'?].
simpleREtail(W, plus(W)) -->
	[0'+].
simpleREtail(W, W) -->
	[].


elementalRE(any) -->
	[0'.].
elementalRE(group(X)) -->
	[0'(],
	re(X),
	[0')].
elementalRE(bos) -->
	[0'^].
elementalRE(eos) -->
	[0'$].
elementalRE(posSet([range(0'A, 0'Z), range(0'a, 0'z), range(0'0, 0'9), char(0'_)])) -->
	[0'\\, 0'w].
elementalRE(negSet([range(0'A, 0'Z), range(0'a, 0'z), range(0'0, 0'9), char(0'_)])) -->
	[0'\\, 0'W].
elementalRE(posSet([range(0'0, 0'9)])) -->
	[0'\\, 0'd].
elementalRE(negSet([range(0'0, 0'9)])) -->
	[0'\\, 0'D].
elementalRE(posSet([char(0x20), char(0'\t), char(0'\r), char(0'\n), char(0'\v), char(0'\f)])) -->
	[0'\\, 0's].
elementalRE(negSet([char(0x20), char(0'\t), char(0'\r), char(0'\n), char(0'\v), char(0'\f)])) -->
	[0'\\, 0'S].
elementalRE(char(C)) -->
	[0'\\, C],
	{	re_metachar([C])
	}.
elementalRE(char(C)) -->
	[C],
	{	\+re_metachar([C])
	}.
elementalRE(negSet(X)) -->
	[0'[, 0'^],
	!,
	setItems(X),
	[0']].
elementalRE(posSet(X)) -->
	[0'[],
	setItems(X),
	[0']].


re_metachar([0'\\]).
re_metachar([0'|]).
re_metachar([0'*]).
re_metachar([0'?]).
re_metachar([0'+]).
re_metachar([0'.]).
re_metachar([0'[]).
re_metachar([0'$]).
re_metachar([0'(]).
re_metachar([0')]).


setItems([Item1|MoreItems]) -->
	setItem(Item1),
	setItems(MoreItems).
setItems([Item1]) -->
	setItem(Item1).


setItem(posSet([range(0'A, 0'Z), range(0'a, 0'z), range(0'0, 0'9), char(0'_)])) -->
	[0'\\, 0'w].
setItem(negSet([range(0'A, 0'Z), range(0'a, 0'z), range(0'0, 0'9), char(0'_)])) -->
	[0'\\, 0'W].
setItem(posSet([range(0'0, 0'9)])) -->
	[0'\\, 0'd].
setItem(negSet([range(0'0, 0'9)])) -->
	[0'\\, 0'D].
setItem(posSet([char(0x20), char(0'\t), char(0'\r), char(0'\n), char(0'\v), char(0'\f)])) -->
	[0'\\, 0's].
setItem(negSet([char(0x20), char(0'\t), char(0'\r), char(0'\n), char(0'\v), char(0'\f)])) -->
	[0'\\, 0'S].
setItem(char(C)) -->
	[0'\\, C],
	{	set_metachar([C])
	}.
setItem(char(C)) -->
	[C],
	{	\+set_metachar([C])
	}.
setItem(range(A, B)) -->
	setItem(char(A)),
	[0'-],
	setItem(char(B)).


set_metachar([0'\\]).
set_metachar([0']]).
set_metachar([0'-]).


regexp_wildcard([], []) :-
	!.
regexp_wildcard([0'*|A], [0'., 0'*|B]) :-
	!,
	regexp_wildcard(A, B).
regexp_wildcard([A|B], [A|C]) :-
	regexp_wildcard(B, C).



% ---------------------------------------------------------------
% N3 Parser
% according to http://www.w3.org/2000/10/swap/grammar/n3-ietf.txt
% inspired by http://code.google.com/p/km-rdf/wiki/Henry
% ---------------------------------------------------------------


barename(BareName) -->
	[name(BareName)].


barename_csl([BareName|Tail]) -->
	barename(BareName),
	!,
	barename_csl_tail(Tail).
barename_csl([]) -->
	[].


barename_csl_tail([BareName|Tail]) -->
	[','],
	!,
	barename(BareName),
	barename_csl_tail(Tail).
barename_csl_tail([]) -->
	[].


boolean(true) -->
	['@', name('true')],
	!.
boolean(true) -->
	[name('true')],
	!.
boolean(false) -->
	['@', name('false')],
	!.
boolean(false) -->
	[name('false')],
	!.
boolean(Boolean) -->
	literal(Atom, type(T)),
	{	T = '\'<http://www.w3.org/2001/XMLSchema#boolean>\'',
		memberchk([Boolean, Atom], [[true, '\'true\''], [true, true], [true, '\'1\''], [false, '\'false\''], [false, false], [false, '\'0\'']])
	}.


declaration -->
	['@', name(base)],
	!,
	explicituri(U),
	{	base_uri(V),
		resolve_uri(U, V, URI),
		retractall(base_uri(_)),
		(	sub_atom(URI, _, 1, _, '#')
		->	throw(base_may_not_contain_hash(URI))
		;	true
		),
		assertz(base_uri(URI))
	}.
declaration -->
	[name(Name)],
	{	downcase_atom(Name, 'base')
	},
	!,
	explicituri(U),
	{	base_uri(V),
		resolve_uri(U, V, URI),
		retractall(base_uri(_)),
		(	sub_atom(URI, _, 1, _, '#')
		->	throw(base_may_not_contain_hash(URI))
		;	true
		),
		assertz(base_uri(URI))
	},
	withoutdot.
declaration -->
	['@', name(keywords)],
	!,
	barename_csl(List),
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('@keywords', after_line(Ln)))
		;	true
		),
		retractall(keywords(_)),
		assertz(keywords(List))
	}.
declaration -->
	['@', name(prefix)],
	!,
	prefix(Prefix),
	explicituri(U),
	{	base_uri(V),
		resolve_uri(U, V, URI),
		retractall(ns(Prefix, _)),
		assertz(ns(Prefix, URI)),
		put_pfx(Prefix, URI)
	}.
declaration -->
	[name(Name)],
	{	downcase_atom(Name, 'prefix')
	},
	prefix(Prefix),
	explicituri(U),
	{	base_uri(V),
		resolve_uri(U, V, URI),
		retractall(ns(Prefix, _)),
		assertz(ns(Prefix, URI)),
		put_pfx(Prefix, URI)
	},
	withoutdot.


document(Triples) -->
	statements_optional(Triples).


dtlang(lang(Lang)) -->
	['@'],
	!,
	langcode(Lang).
dtlang(type(Datatype)) -->
	['^', '^'],
	!,
	uri(Datatype).
dtlang(type(T)) -->
	{	T = '\'<http://www.w3.org/2001/XMLSchema#string>\''
	},
	[].


existential -->
	['@', name(forSome)],
	!,
	symbol_csl(Symbols),
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('@forSome', after_line(Ln)))
		;	true
		),
		nb_getval(fdepth, D),
		forall(
			(	member(S, Symbols)
			),
			(	(	\+qevar(S, _, D)
				->	gensym(qe, Q),
					asserta(qevar(S, Q, D))
				;	true
				)
			)
		)
	}.


explicituri(ExplicitURI) -->
	[relative_uri(ExplicitURI)].


expression(Node, T) -->
	pathitem(N1, T1),
	pathtail(N1, P, N2, T2),
	{	append(T1, T2, T3),
		(	P = '\'<http://eulersharp.sourceforge.net/2003/03swap/log-rules#disjunction>\''
		->	(	distinct(N1, Distinct)
			->	true
			;	Distinct = N1
			),
			dlist(Distinct, Node),
			T = []
		;	Node = N2,
			T = T3
		),
		(	keywords(List),
			memberchk(Node, List)
		->	nb_getval(line_number, Ln),
			throw(invalid_keyword_use(Node, after_line(Ln)))
		;	true
		)
	}.


formulacontent(Formula) -->
	statementlist(List),
	{	(	nb_getval(fdepth, 1),
			retract(back)
		->	L = List
		;	(	nb_getval(smod, true)
			->	sort(List, L)
			;	distinct(List, L)
			)
		),
		clist(L, Formula)
	}.


langcode(Langcode) -->
	[name(Name)],
	{	downcase_atom(Name, Nm),
		atomic_list_concat(['\'', Nm, '\''], Langcode)
	}.


literal(Atom, DtLang) -->
	string(Codes),
	dtlang(DtLang),
	{	escape_string(Codes, B),
		escape_string(B, C),
		escape_squote(C, D),
		atom_codes(E, D),
		atomic_list_concat(['\'', E, '\''], Atom)
	}.


numericliteral(Num) -->
	[numeric(Type, NumB)],
	{	(	flag(turtle)
		->	atomic_list_concat(['\'<http://www.w3.org/2001/XMLSchema#', Type, '>\''], T),
			atom_codes(A, NumB),
			atomic_list_concat(['\'', A, '\''], B),
			Num = literal(B, type(T))
		;	numeral(NumB, NumC),
			(	length(NumC, LenC),
				LenC > 16,
				Type = decimal
			->	rdiv_codes(Num, NumC)
			;	number_codes(Num, NumC)
			)
		)
	}.


object(Node, Triples) -->
	expression(Node, Triples).


objecttail(Subject, Verb, [Triple|T]) -->
	[','],
	!,
	object(Object, Triples),
	objecttail(Subject, Verb, Tail),
	{	append(Triples, Tail, T),
		(	Verb = isof(V)
		->	(	atom(V),
				\+sub_atom(V, 0, 1, _, '_')
			->	Triple =.. [V, Object, Subject]
			;	Triple = exopred(V, Object, Subject)
			)
		;	(	atom(Verb),
				\+sub_atom(Verb, 0, 1, _, '_')
			->	Triple =.. [Verb, Subject, Object]
			;	Triple = exopred(Verb, Subject, Object)
			)
		)
	}.
objecttail(_, _, []) -->
	[].


pathitem([], []) -->
	symbol(S),
	{	S = '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>\''
	},
	!.
pathitem(Name, []) -->
	symbol(S),
	!,
	{	(	qevar(S, N, D)
		->	(	D = 1,
				nb_getval(fdepth, FD),
				FD >= 1
			->	atom_concat('_', N, Name),
				nb_setval(smod, false)
			;	atomic_list_concat(['\'<http://localhost/var#', N, '>\''], Name)
			)
		;	(	quvar(S, N, D)
			->	(	D = 1,
					nb_getval(fdepth, FD),
					FD >= 1
				->	atomic_list_concat(['\'<http://localhost/var#', N, '>\''], Name)
				;	atom_concat('_', N, Name),
					nb_setval(smod, false)
				)
			;	(	atom(S),
					atom_concat('\'<http://eulersharp.sourceforge.net/2003/03swap/prolog#', A, S),
					atom_concat(B, '>\'', A)
				->	(	B = 'C'
					->	Pred = '\'C\''
					;	(	B = conjunction
						->	Pred = '\',\''
						;	(	B = disjunction
							->	Pred = '\';\''
							;	(	prolog_sym(B, Pred, _)
								->	true
								;	nb_getval(line_number, Ln),
									throw(invalid_prolog_builtin(B, after_line(Ln)))
								)
							)
						)
					),
					Name = prolog:Pred
				;	Name = S
				)
			)
		),
		(	quvar(S, _, _)
		->	nb_setval(smod, false)
		;	true
		)
	}.
pathitem(VarID, []) -->
	[uvar(Var)],
	!,
	{	atom_codes(Var, VarCodes),
		subst([[[0'-], [0'_, 0'M, 0'I, 0'N, 0'U, 0'S, 0'_]], [[0'.], [0'_, 0'D, 0'O, 0'T, 0'_]]], VarCodes, VarTidy),
		atom_codes(VarID, [0'_|VarTidy]),
		nb_setval(smod, false)
	}.
pathitem(Number, []) -->
	numericliteral(Number),
	!.
pathitem(Boolean, []) -->
	boolean(Boolean),
	!.
pathitem(Atom, []) -->
	literal(A, type(T)),
	{	T = '\'<http://eulersharp.sourceforge.net/2003/03swap/prolog#atom>\''
	},
	!,
	{	atom_codes(A, B),
		escape_string(C, B),
		atom_codes(Atom, C)
	}.
pathitem(Number, []) -->
	literal(Atom, type(Type)),
	{	\+flag(turtle),
		memberchk(Type, ['\'<http://www.w3.org/2001/XMLSchema#integer>\'', '\'<http://www.w3.org/2001/XMLSchema#decimal>\'', '\'<http://www.w3.org/2001/XMLSchema#double>\'']),
		sub_atom(Atom, 1, _, 1, A),
		atom_codes(A, NumB),
		numeral(NumB, NumC),
		(	length(NumC, LenC),
			LenC > 16,
			Type = '\'<http://www.w3.org/2001/XMLSchema#decimal>\''
		->	rdiv_codes(Number, NumC)
		;	number_codes(Number, NumC)
		)
	},
	!.
pathitem(literal(Atom, DtLang), []) -->
	literal(Atom, DtLang),
	!.
pathitem(BNode, Triples) -->
	['['],
	!,
	{	gensym(e, S),
		(	nb_getval(fdepth, 0)
		->	atomic_list_concat(['\'<http://localhost/var#', S, '>\''], BN)
		;	atom_concat('_', S, BN),
			nb_setval(smod, false)
		)
	},
	propertylist(BN, T),
	{	(	memberchk('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>\''(X, Head), T),
			memberchk('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>\''(X, Tail), T),
			del(T, '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>\''(X, '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#List>\''), U),
			del(U, '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>\''(X, Head), V),
			del(V, '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>\''(X, Tail), W)
		->	BNode = [Head|Tail],
			Triples = W
		;	BNode = BN,
			Triples = T
		)
	},
	[']'].
pathitem(set(Distinct), Triples) -->
	['(', '$'],
	!,
	pathlist(List, Triples),
	{	(	nb_getval(smod, true)
		->	sort(List, Distinct)
		;	distinct(List, Distinct)
		)
	},
	['$', ')'].
pathitem(List, Triples) -->
	['('],
	!,
	pathlist(List, Triples),
	[')'].
pathitem(Node, []) -->
	['{'],
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('{}', after_line(Ln)))
		;	true
		),
		nb_getval(fdepth, I),
		J is I+1,
		nb_setval(fdepth, J),
		nb_setval(smod, true)
	},
	formulacontent(Node),
	{	retractall(quvar(_, _, J)),
		retractall(qevar(_, _, J)),
		retractall(evar(_, _, J)),
		nb_setval(fdepth, I),
		nb_setval(smod, false)
	},
	['}'].


pathlist([Node|Rest], Triples) -->
	expression(Node, T),
	!,
	pathlist(Rest, Tail),
	{	append(T, Tail, Triples)
	}.
pathlist([], []) -->
	[].


pathtail(Node, Verb, PNode, [Triple|Triples]) -->
	['!'],
	!,
	pathitem(Verb, Triples2),
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('forward_path', after_line(Ln)))
		;	true
		),
		dynamic_verb(Verb),
		gensym(e, S),
		(	nb_getval(fdepth, 0)
		->	atomic_list_concat(['\'<http://localhost/var#', S, '>\''], BNode)
		;	atom_concat('_', S, BNode),
			nb_setval(smod, false)
		),
		(	Verb = isof(V)
		->	(	atom(V),
				\+sub_atom(V, 0, 1, _, '_')
			->	Triple =.. [V, BNode, Node]
			;	Triple = exopred(V, BNode, Node)
			)
		;	(	Verb = prolog:Pred
			->	(	BNode = true
				->	Triple =.. [Pred|Node]
				;	(	BNode = false
					->	T =.. [Pred|Node],
						Triple = \+(T)
					;	(	prolog_sym(_, Pred, func)
						->	T =.. [Pred|Node],
							Triple = is(BNode, T)
						;	Triple =.. [Pred, Node, BNode]
						)
					)
				)
			;	(	atom(Verb),
					\+sub_atom(Verb, 0, 1, _, '_')
				->	Triple =.. [Verb, Node, BNode]
				;	Triple = exopred(Verb, Node, BNode)
				)
			)
		)
	},
	pathtail(BNode, _, PNode, Tail),
	{	append(Triples2, Tail, Triples)
	}.
pathtail(Node, Verb, PNode, [Triple|Triples]) -->
	['^'],
	!,
	pathitem(Verb, Triples2),
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('backward_path', after_line(Ln)))
		;	true
		),
		dynamic_verb(Verb),
		gensym(e, S),
		(	nb_getval(fdepth, 0)
		->	atomic_list_concat(['\'<http://localhost/var#', S, '>\''], BNode)
		;	atom_concat('_', S, BNode),
			nb_setval(smod, false)
		),
		(	Verb = isof(V)
		->	(	atom(V),
				\+sub_atom(V, 0, 1, _, '_')
			->	Triple =.. [V, Node, BNode]
			;	Triple = exopred(V, Node, BNode)
			)
		;	(	Verb = prolog:Pred
			->	(	Node = true
				->	Triple =.. [Pred|BNode]
				;	(	Node = false
					->	T =.. [Pred|BNode],
						Triple = \+(T)
					;	(	prolog_sym(_, Pred, func)
						->	T =.. [Pred|BNode],
							Triple = is(Node, T)
						;	Triple =.. [Pred, BNode, Node]
						)
					)
				)
			;	(	atom(Verb),
					\+sub_atom(Verb, 0, 1, _, '_')
				->	Triple =.. [Verb, BNode, Node]
				;	Triple = exopred(Verb, BNode, Node)
				)
			)
		)
	},
	pathtail(BNode, _, PNode, Tail),
	{	append(Triples2, Tail, Triples)
	}.
pathtail(Node, void, Node, []) -->
	[].


prefix(Prefix) -->
	[Prefix:''].


propertylist(Subject, [Triple|Triples]) -->
	verb(Verb, Triples1),
	{	dynamic_verb(Verb)
	},
	!,
	object(Object, Triples2),
	objecttail(Subject, Verb, Triples3),
	propertylisttail(Subject, Triples4),
	{	append(Triples1, Triples2, Triples12),
		append(Triples12, Triples3, Triples123),
		append(Triples123, Triples4, Triples),
		(	Verb = isof(V)
		->	(	atom(V),
				\+sub_atom(V, 0, 1, _, '_')
			->	Triple =.. [V, Object, Subject]
			;	Triple = exopred(V, Object, Subject)
			)
		;	(	Verb = prolog:Pred
			->	(	Object = true
				->	Triple =.. [Pred|Subject]
				;	(	Object = false
					->	T =.. [Pred|Subject],
						Triple = \+(T)
					;	(	prolog_sym(_, Pred, func)
						->	T =.. [Pred|Subject],
							Triple = is(Object, T)
						;	Triple =.. [Pred, Subject, Object]
						)
					)
				)
			;	(	atom(Verb),
					\+sub_atom(Verb, 0, 1, _, '_')
				->	Triple =.. [Verb, Subject, Object]
				;	Triple = exopred(Verb, Subject, Object)
				)
			)
		)
	}.
propertylist(_, []) -->
	[].


propertylisttail(Subject, Triples) -->
	[';'],
	!,
	propertylisttailsemis,
	propertylist(Subject, Triples).
propertylisttail(_, []) -->
	[].


propertylisttailsemis -->
	[';'],
	!,
	propertylisttailsemis.
propertylisttailsemis -->
	[].


qname(URI) -->
	[NS:Name],
	{	(	ns(NS, Base)
		->	atom_codes(Name, Codes),
			escape_squote(Codes, Codes2),
			atom_codes(Name2, Codes2),
			atomic_list_concat(['\'<', Base, Name2, '>\''], URI)
		;	nb_getval(line_number, Ln),
			throw(no_prefix_directive(NS, after_line(Ln)))
		)
	},
	!.


simpleStatement(Triples) -->
	subject(Subject, Triples1),
	(	{	Subject = (D1;D2)
		}
	->	{	Triples = [(D1;D2)]
		}
	;	propertylist(Subject, Triples2),
		{	append(Triples1, Triples2, Triples),
			(	flag(turtle),
				Triples = []
			->	nb_getval(line_number, Ln),
				throw(not_in_turtle('empty_pred_obj', after_line(Ln)))
			;	true
			)
		}
	).


statement([]) -->
	declaration,
	!.
statement([]) -->
	universal,
	!.
statement([]) -->
	existential,
	!.
statement(Statement) -->
	simpleStatement(Statement).


statementlist(Triples) -->
	statement(Tr),
	!,
	statementtail(T),
	{	append(Tr, T, Triples)
	}.
statementlist([]) -->
	[].


statements_optional(Triples) -->
	statement(Tr),
	[dot(Ln)],
	!,
	{	nb_setval(line_number, Ln)
	},
	statements_optional(T),
	{	append(Tr, T, Triples)
	}.
statements_optional([]) -->
	[].


statementtail(T) -->
	[dot(Ln)],
	!,
	{	nb_setval(line_number, Ln)
	},
	statementlist(T).
statementtail([]) -->
	[].


string(Codes) -->
	[literal(Codes)].


subject(Node, Triples) -->
	expression(Node, Triples),
	{	(	flag(turtle),
			(	Node = literal(_, _)
			->	true
			;	(	Node = true
				->	true
				;	(	Node = false
					->	true
					)
				)
			)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('no_lit_subj', after_line(Ln)))
		;	true
		)
	}.


symbol(Name) -->
	uri(Name),
	!.
symbol(Name) -->
	[name(N)],
	!,
	{	(	keywords(List)
		->	(	memberchk(N, List)
			->	Name = N
			;	ns('', Base),
				atomic_list_concat(['\'<', Base, N, '>\''], Name)
			)
		;	(	memberchk(N, [true, false])
			->	Name = N
			;	nb_getval(line_number, Ln),
				throw(invalid_keyword(N, after_line(Ln)))
			)
		)
	}.
symbol(Name) -->
	[bnode(Label)],
	{	nb_getval(fdepth, D),
		(	D =:= 0
		->	N = Label
		;	atom_codes(Label, LabelCodes),
			subst([[[0'-], [0'_, 0'M, 0'I, 0'N, 0'U, 0'S, 0'_]], [[0'.], [0'_, 0'D, 0'O, 0'T, 0'_]]], LabelCodes, LabelTidy),
			atom_codes(N, LabelTidy)
		),
		(	(	D =:= 0
			->	evar(N, S)
			;	evar(N, S, D)
			)
		->	true
		;	atom_concat(N, '_', M),
			gensym(M, S),
			(	D =:= 0
			->	assertz(evar(N, S))
			;	assertz(evar(N, S, D))
			)
		),
		(	nb_getval(fdepth, 0)
		->	atomic_list_concat(['\'<http://localhost/var#', S, '>\''], Name)
		;	atom_concat('_', S, Name),
			nb_setval(smod, false)
		)
	}.


symbol_csl([Symbol|Tail]) -->
	symbol(Symbol),
	!,
	symbol_csl_tail(Tail).
symbol_csl([]) -->
	[].


symbol_csl_tail([Symbol|T]) -->
	[','],
	!,
	symbol(Symbol),
	symbol_csl_tail(T).
symbol_csl_tail([]) -->
	[].


universal -->
	['@', name(forAll)],
	!,
	symbol_csl(Symbols),
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('@forAll', after_line(Ln)))
		;	true
		),
		nb_getval(fdepth, D),
		forall(
			(	member(S, Symbols)
			),
			(	(	\+quvar(S, _, D)
				->	gensym(qu, Q),
					asserta(quvar(S, Q, D))
				;	true
				)
			)
		)
	}.


uri(Name) -->
	explicituri(U),
	!,
	{	base_uri(V),
		resolve_uri(U, V, W),
		atomic_list_concat(['\'<', W, '>\''], Name)
	}.
uri(Name) -->
	qname(Name).


verb(V, []) -->
	['=', '>'],
	!,
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('=>', after_line(Ln)))
		;	true
		),
		V = '\'<http://www.w3.org/2000/10/swap/log#implies>\''
	}.
verb(V, []) -->
	['='],
	!,
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('=', after_line(Ln)))
		;	true
		),
		V = '\'<http://www.w3.org/2002/07/owl#sameAs>\''
	}.
verb(':-', []) -->
	['<', '='],
	!,
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('<=', after_line(Ln)))
		;	true
		),
		assertz(back)
	}.
verb(V, []) -->
	['@', name(a)],
	!,
	{	V = '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>\''
	}.
verb(V, []) -->
	[name(a)],
	!,
	{	V = '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>\''
	}.
verb(Node, Triples) -->
	['@', name(has)],
	!,
	expression(Node, Triples).
verb(Node, Triples) -->
	[name(has)],
	!,
	expression(Node, Triples).
verb(isof(Node), Triples) -->
	['@', name(is)],
	!,
	expression(Node, Triples),
	['@', name(of)],
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('no_is_of', after_line(Ln)))
		;	true
		)
	}.
verb(isof(Node), Triples) -->
	[name(is)],
	!,
	expression(Node, Triples),
	[name(of)],
	{	(	flag(turtle)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('no_is_of', after_line(Ln)))
		;	true
		)
	}.
verb(Node, Triples) -->
	expression(Node, Triples),
	{	(	flag(turtle),
			(	Node = literal(_, _)
			->	true
			;	(	Node = true
				->	true
				;	(	Node = false
					->	true
					;	(	sub_atom(Node, 1, 22, _, '<http://localhost/var#')
						->	true
						)
					)
				)
			)
		->	nb_getval(line_number, Ln),
			throw(not_in_turtle('non_iri_pred', after_line(Ln)))
		;	true
		)
	}.


withoutdot, [dot(Ln)] -->
	[dot(Ln)],
	!,
	{	throw(unexpected_dot(after_line(Ln)))
	}.
withoutdot, [dot(Ln)] -->
	[],
	{	nb_getval(line_number, Ln)
	}.



% tokenizer

tokens(In, List) :-
	get_code(In, C0),
	(	token(C0, In, C1, Tok1)
	->	true
	;	nb_getval(line_number, Ln),
		char_code(Char, C0),
		throw(illegal_token(char_code(Char, C0), line(Ln)))
	),
	(	Tok1 == end_of_file
	->	List = []
	;	(	flag(turtle),
			Tok1 = dot(Ln)
		->	List = [Tok1]
		;	List = [Tok1|Tokens],
			tokens(C1, In, Tokens)
		)
	).


tokens(C0, In, List) :-
	(	token(C0, In, C1, H)
	->	true
	;	nb_getval(line_number, Ln),
		char_code(Char, C0),
		throw(illegal_token(char_code(Char, C0), line(Ln)))
	),
	(	H == end_of_file
	->	List = []
	;	(	flag(turtle),
			H = dot(Ln)
		->	List = [H]
		;	List = [H|T],
			tokens(C1, In, T)
		)
	).


token(-1, _, -1, end_of_file) :-
	!.
token(0'., In, C, Token) :-
	(	peek_code(In, C0),
		(	e(C0)
		->	T1 = [0'0|T2],
			get_code(In, CN1)
		;	0'0 =< C0,
			C0 =< 0'9,
			get_code(In, C1),
			integer_codes(C1, In, CN1, T1, T2)
		)
	->	(	exponent(CN1, In, C, T2)
		->	Type = double
		;	C = CN1,
			T2 = [],
			Type = decimal
		),
		Token = numeric(Type, [0'0, 0'.|T1])
	;	nb_getval(line_number, Ln),
		get_code(In, C),
		!,
		Token = dot(Ln)
	).
token(0'#, In, C, Token) :-
	!,
	get_code(In, C1),
	skip_line(C1, In, C2),
	token(C2, In, C, Token).
token(C0, In, C, Token) :-
	white_space(C0),
	!,
	get_code(In, C1),
	token(C1, In, C, Token).
token(C0, In, C, Number) :-
	0'0 =< C0,
	C0 =< 0'9,
	!,
	number_n(C0, In, C, Number).
token(0'-, In, C, Number) :-
	!,
	number_n(0'-, In, C, Number).
token(0'+, In, C, Number) :-
	!,
	number_n(0'+, In, C, Number).
token(0'", In, C, literal(Codes)) :-
	!,
	(	peek_code(In, 0'")
	->	get_code(In, 0'"),
		(	peek_code(In, 0'")
		->	get_code(In, 0'"),
			get_code(In, C1),
			dq_string(C1, In, C, Codes)
		;	get_code(In, C),
			Codes = []
		)
	;	get_code(In, C1),
		string_dq(C1, In, C, Codes)
	).
token(0'', In, C, literal(Codes)) :-
	!,
	(	peek_code(In, 0'')
	->	get_code(In, 0''),
		(	peek_code(In, 0'')
		->	get_code(In, 0''),
			get_code(In, C1),
			sq_string(C1, In, C, Codes)
		;	get_code(In, C),
			Codes = []
		)
	;	get_code(In, C1),
		string_sq(C1, In, C, Codes)
	).
token(0'?, In, C, uvar(Name)) :-
	!,
	get_code(In, C0),
	(	name(C0, In, C, Name)
	->	true
	;	C = C0,
		nb_getval(line_number, Ln),
		throw(empty_quickvar_name(line(Ln)))
	).
token(0'_, In, C, bnode(Name)) :-
	peek_code(In, 0':),
	!,
	get_code(In, _),
	get_code(In, C0),
	(	name(C0, In, C, Name)
	->	true
	;	C = C0,
		Name = ''
	).
token(0'<, In, C, relative_uri(URI)) :-
	peek_code(In, C1),
	C1 \== 0'=,
	!,
	get_code(In, C1),
	iri_chars(C1, In, C, Codes),
	D = Codes,
	atom_codes(URI, D).
token(0':, In, C, Token) :-
	!,
	get_code(In, C0),
	(	local_name(C0, In, C, Name)
	->	Token = '':Name
	;	Token = '':'',
		C = C0
	).
token(C0, In, C, Token) :-
	name(C0, In, C1, Name),
	!,
	(	C1 == 0':
	->	get_code(In, C2),
		(	local_name(C2, In, C, Name2)
		->	Token = (Name:Name2)
		;	Token = (Name:''),
			C = C2
		)
	;	Token = name(Name),
		C = C1
	).
token(C0, In, C, P) :-
	punctuation(C0, P),
	!,
	get_code(In, C).


number_n(0'-, In, CN, numeric(T, [0'-|Codes])) :-
	!,
	get_code(In, C0),
	number_nn(C0, In, CN, numeric(T, Codes)).
number_n(0'+, In, CN, numeric(T, [0'+|Codes])) :-
	!,
	get_code(In, C0),
	number_nn(C0, In, CN, numeric(T, Codes)).
number_n(C0, In, CN, Value) :-
	number_nn(C0, In, CN, Value).


number_nn(C, In, CN, numeric(Type, Codes)) :-
	integer_codes(C, In, CN0, Codes, T0),
	(	CN0 == 0'.,
		peek_code(In, C0),
		(	e(C0)
		->	T1 = [0'0|T2],
			get_code(In, CN1)
		;	0'0 =< C0,
			C0 =< 0'9,
			get_code(In, C1),
			integer_codes(C1, In, CN1, T1, T2)
		),
		T0 = [0'.|T1]
	->	(	exponent(CN1, In, CN, T2)
		->	Type = double
		;	CN = CN1,
			T2 = [],
			Type = decimal
		)
	;	(	exponent(CN0, In, CN, T0)
		->	Type = double
		;	T0 = [],
			CN = CN0,
			Type = integer
		)
	).


integer_codes(C0, In, CN, [C0|T0], T) :-
	0'0 =< C0,
	C0 =< 0'9,
	!,
	get_code(In, C1),
	integer_codes(C1, In, CN, T0, T).
integer_codes(CN, _, CN, T, T).


exponent(C0, In, CN, [C0|T0]) :-
	e(C0),
	!,
	get_code(In, C1),
	optional_sign(C1, In, CN0, T0, T1),
	integer_codes(CN0, In, CN, T1, []),
	(	T1 = []
	->	nb_getval(line_number, Ln),
		throw(invalid_exponent(line(Ln)))
	;	true
	).


optional_sign(C0, In, CN, [C0|T], T) :-
	sign(C0),
	!,
	get_code(In, CN).
optional_sign(CN, _, CN, T, T).


e(0'e).
e(0'E).


sign(0'-).
sign(0'+).


dq_string(-1, _, _, []) :-
	!,
	nb_getval(line_number, Ln),
	throw(unexpected_end_of_input(line(Ln))).
dq_string(0'", In, C, []) :-
	(	retract(got_dq)
	->	true
	;	peek_code(In, 0'"),
		get_code(In, _)
	),
	(	retract(got_dq)
	->	assertz(got_dq)
	;	assertz(got_dq),
		peek_code(In, 0'"),
		get_code(In, _),
		assertz(got_dq)
	),
	!,
	(	peek_code(In, 0'")
	->	nb_getval(line_number, Ln),
		throw(unexpected_double_quote(line(Ln)))
	;	true
	),
	retractall(got_dq),
	get_code(In, C).
dq_string(0'", In, C, [0'"|T]) :-
	!,
	(	retract(got_dq)
	->	C1 = 0'"
	;	get_code(In, C1)
	),
	dq_string(C1, In, C, T).
dq_string(0'\\, In, C, [H|T]) :-
	(	retract(got_dq)
	->	C1 = 0'"
	;	get_code(In, C1)
	),
	!,
	string_escape(C1, In, C2, H),
	dq_string(C2, In, C, T).
dq_string(C0, In, C, [C0|T]) :-
	(	retract(got_dq)
	->	C1 = 0'"
	;	get_code(In, C1)
	),
	dq_string(C1, In, C, T).


sq_string(-1, _, _, []) :-
	!,
	nb_getval(line_number, Ln),
	throw(unexpected_end_of_input(line(Ln))).
sq_string(0'', In, C, []) :-
	(	retract(got_sq)
	->	true
	;	peek_code(In, 0''),
		get_code(In, _)
	),
	(	retract(got_sq)
	->	assertz(got_sq)
	;	assertz(got_sq),
		peek_code(In, 0''),
		get_code(In, _),
		assertz(got_sq)
	),
	!,
	(	peek_code(In, 0'')
	->	nb_getval(line_number, Ln),
		throw(unexpected_single_quote(line(Ln)))
	;	true
	),
	retractall(got_sq),
	get_code(In, C).
sq_string(0'', In, C, [0''|T]) :-
	!,
	(	retract(got_sq)
	->	C1 = 0''
	;	get_code(In, C1)
	),
	sq_string(C1, In, C, T).
sq_string(0'\\, In, C, [H|T]) :-
	(	retract(got_sq)
	->	C1 = 0''
	;	get_code(In, C1)
	),
	!,
	string_escape(C1, In, C2, H),
	sq_string(C2, In, C, T).
sq_string(C0, In, C, [C0|T]) :-
	(	retract(got_sq)
	->	C1 = 0''
	;	get_code(In, C1)
	),
	sq_string(C1, In, C, T).


string_dq(-1, _, _, []) :-
	!,
	nb_getval(line_number, Ln),
	throw(unexpected_end_of_input(line(Ln))).
string_dq(0'\n, _, _, []) :-
	!,
	nb_getval(line_number, Ln),
	throw(unexpected_end_of_line(line(Ln))).
string_dq(0'", In, C, []) :-
	!,
	get_code(In, C).
string_dq(0'\\, In, C, D) :-
	get_code(In, C1),
	!,
	string_escape(C1, In, C2, H),
	(	H =< 0xFFFF
	->	D = [H|T]
	;	E is (H-0x10000)>>10+0xD800,
		F is (H-0x10000) mod 0x400+0xDC00,
		D = [E, F|T]
	),
	string_dq(C2, In, C, T).
string_dq(C0, In, C, D) :-
	(	C0 =< 0xFFFF
	->	D = [C0|T]
	;	E is (C0-0x10000)>>10+0xD800,
		F is (C0-0x10000) mod 0x400+0xDC00,
		D = [E, F|T]
	),
	get_code(In, C1),
	string_dq(C1, In, C, T).


string_sq(-1, _, _, []) :-
	!,
	nb_getval(line_number, Ln),
	throw(unexpected_end_of_input(line(Ln))).
string_sq(0'', In, C, []) :-
	!,
	get_code(In, C).
string_sq(0'\\, In, C, D) :-
	get_code(In, C1),
	!,
	string_escape(C1, In, C2, H),
	(	H =< 0xFFFF
	->	D = [H|T]
	;	E is (H-0x10000)>>10+0xD800,
		F is (H-0x10000) mod 0x400+0xDC00,
		D = [E, F|T]
	),
	string_sq(C2, In, C, T).
string_sq(C0, In, C, D) :-
	(	C0 =< 0xFFFF
	->	D = [C0|T]
	;	E is (C0-0x10000)>>10+0xD800,
		F is (C0-0x10000) mod 0x400+0xDC00,
		D = [E, F|T]
	),
	get_code(In, C1),
	string_sq(C1, In, C, T).


string_escape(0't, In, C, 0'\t) :-
	!,
	get_code(In, C).
string_escape(0'b, In, C, 0'\b) :-
	!,
	get_code(In, C).
string_escape(0'n, In, C, 0'\n) :-
	!,
	get_code(In, C).
string_escape(0'r, In, C, 0'\r) :-
	!,
	get_code(In, C).
string_escape(0'f, In, C, 0'\f) :-
	!,
	get_code(In, C).
string_escape(0'", In, C, 0'") :-
	!,
	get_code(In, C).
string_escape(0'', In, C, 0'') :-
	!,
	get_code(In, C).
string_escape(0'\\, In, C, 0'\\) :-
	!,
	get_code(In, C).
string_escape(0'u, In, C, Code) :-
	!,
	get_hhhh(In, Code),
	get_code(In, C).
string_escape(0'U, In, C, Code) :-
	!,
	get_hhhh(In, Code0),
	get_hhhh(In, Code1),
	Code is Code0 << 16 + Code1,
	get_code(In, C).
string_escape(C, _, _, _) :-
	nb_getval(line_number, Ln),
	atom_codes(A, [0'\\, C]),
	throw(illegal_string_escape_sequence(A, line(Ln))).


get_hhhh(In, Code) :-
	get_code(In, C1),
	code_type(C1, xdigit(D1)),
	get_code(In, C2),
	code_type(C2, xdigit(D2)),
	get_code(In, C3),
	code_type(C3, xdigit(D3)),
	get_code(In, C4),
	code_type(C4, xdigit(D4)),
	Code is D1<<12+D2<<8+D3<<4+D4.


language(C0, In, C, [C0|Codes]) :-
	code_type(C0, lower),
	get_code(In, C1),
	lwr_word(C1, In, C2, Codes, Tail),
	sub_langs(C2, In, C, Tail, []).


lwr_word(C0, In, C, [C0|T0], T) :-
	code_type(C0, lower),
	!,
	get_code(In, C1),
	lwr_word(C1, In, C, T0, T).
lwr_word(C, _, C, T, T).


sub_langs(0'-, In, C, [0'-, C1|Codes], T) :-
	get_code(In, C1),
	lwrdig(C1),
	!,
	get_code(In, C2),
	lwrdigs(C2, In, C3, Codes, Tail),
	sub_langs(C3, In, C, Tail, T).
sub_langs(C, _, C, T, T).


lwrdig(C) :-
	code_type(C, lower),
	!.
lwrdig(C) :-
	code_type(C, digit).


lwrdigs(C0, In, C, [C0|T0], T) :-
	lwrdig(C0),
	!,
	get_code(In, C1),
	lwr_word(C1, In, C, T0, T).
lwrdigs(C, _, C, T, T).


iri_chars(0'>, In, C, []) :-
	!,
	get_code(In, C).
iri_chars(0'\\, In, C, [H|T]) :-
	!,
	get_code(In, C1),
	iri_escape(C1, In, C2, H),
	\+non_iri_char(H),
	iri_chars(C2, In, C, T).
iri_chars(0'%, In, C, [0'%, C1, C2|T]) :-
	!,
	get_code(In, C1),
	code_type(C1, xdigit(_)),
	get_code(In, C2),
	code_type(C2, xdigit(_)),
	get_code(In, C3),
	iri_chars(C3, In, C, T).
iri_chars(0'', In, C, [0'', 0''|T]) :-
	!,
	get_code(In, C1),
	iri_chars(C1, In, C, T).
iri_chars(-1, _, _, _) :-
	!,
	fail.
iri_chars(C0, In, C, [C0|T]) :-
	\+non_iri_char(C0),
	get_code(In, C1),
	iri_chars(C1, In, C, T).


iri_escape(0'u, In, C, Code) :-
	!,
	get_hhhh(In, Code),
	get_code(In, C).
iri_escape(0'U, In, C, Code) :-
	!,
	get_hhhh(In, Code0),
	get_hhhh(In, Code1),
	Code is Code0 << 16 + Code1,
	get_code(In, C).
iri_escape(C, _, _, _) :-
	nb_getval(line_number, Ln),
	atom_codes(A, [0'\\, C]),
	throw(illegal_iri_escape_sequence(A, line(Ln))).


non_iri_char(0x20).
non_iri_char(0'<).
non_iri_char(0'>).
non_iri_char(0'").
non_iri_char(0'{).
non_iri_char(0'}).
non_iri_char(0'|).
non_iri_char(0'^).
non_iri_char(0'`).
non_iri_char(0'\\).


name(C0, In, C, Atom) :-
	name_start_char(C0),
	get_code(In, C1),
	name_chars(C1, In, C, T),
	atom_codes(Atom, [C0|T]).


name_start_char(C) :-
	pn_chars_base(C),
	!.
name_start_char(0'_).
name_start_char(C) :-
	code_type(C, digit).


name_chars(0'., In, C, [0'.|T]) :-
	peek_code(In, C1),
	pn_chars(C1),
	!,
	get_code(In, C1),
	name_chars(C1, In, C, T).
name_chars(C0, In, C, [C0|T]) :-
	pn_chars(C0),
	!,
	get_code(In, C1),
	name_chars(C1, In, C, T).
name_chars(C, _, C, []).


pn_chars_base(C) :-
	code_type(C, alpha),
	!.
pn_chars_base(C) :-
	0xC0 =< C,
	C =< 0xD6,
	!.
pn_chars_base(C) :-
	0xD8 =< C,
	C =< 0xF6,
	!.
pn_chars_base(C) :-
	0xF8 =< C,
	C =< 0x2FF,
	!.
pn_chars_base(C) :-
	0x370 =< C,
	C =< 0x37D,
	!.
pn_chars_base(C) :-
	0x37F =< C,
	C =< 0x1FFF,
	!.
pn_chars_base(C) :-
	0x200C =< C,
	C =< 0x200D,
	!.
pn_chars_base(C) :-
	0x2070 =< C,
	C =< 0x218F,
	!.
pn_chars_base(C) :-
	0x2C00 =< C,
	C =< 0x2FEF,
	!.
pn_chars_base(C) :-
	0x3001 =< C,
	C =< 0xD7FF,
	!.
pn_chars_base(C) :-
	0xF900 =< C,
	C =< 0xFDCF,
	!.
pn_chars_base(C) :-
	0xFDF0 =< C,
	C =< 0xFFFD,
	!.
pn_chars_base(C) :-
	0x10000 =< C,
	C =< 0xEFFFF.


pn_chars(C) :-
	code_type(C, csym),
	!.
pn_chars(C) :-
	pn_chars_base(C),
	!.
pn_chars(0'-) :-
	!.
pn_chars(0xB7) :-
	!.
pn_chars(C) :-
	0x0300 =< C,
	C =< 0x036F,
	!.
pn_chars(C) :-
	0x203F =< C,
	C =< 0x2040.


local_name(0'\\, In, C, Atom) :-
	!,
	get_code(In, C0),
	reserved_char_escapes(C0),
	get_code(In, C1),
	local_name_chars(C1, In, C, T),
	atom_codes(Atom, [C0|T]).
local_name(0'%, In, C, Atom) :-
	!,
	get_code(In, C0),
	code_type(C0, xdigit(_)),
	get_code(In, C1),
	code_type(C1, xdigit(_)),
	get_code(In, C2),
	local_name_chars(C2, In, C, T),
	atom_codes(Atom, [0'%, C0, C1|T]).
local_name(C0, In, C, Atom) :-
	local_name_start_char(C0),
	get_code(In, C1),
	local_name_chars(C1, In, C, T),
	atom_codes(Atom, [C0|T]).


local_name_chars(0'\\, In, C, [C0|T]) :-
	!,
	get_code(In, C0),
	reserved_char_escapes(C0),
	get_code(In, C1),
	local_name_chars(C1, In, C, T).
local_name_chars(0'%, In, C, [0'%, C0, C1|T]) :-
	!,
	get_code(In, C0),
	code_type(C0, xdigit(_)),
	get_code(In, C1),
	code_type(C1, xdigit(_)),
	get_code(In, C2),
	local_name_chars(C2, In, C, T).
local_name_chars(0'., In, C, [0'.|T]) :-
	peek_code(In, C1),
	(	local_name_char(C1)
	;	C1 = 0'.
	),
	!,
	get_code(In, C1),
	local_name_chars(C1, In, C, T).
local_name_chars(C0, In, C, [C0|T]) :-
	local_name_char(C0),
	!,
	get_code(In, C1),
	local_name_chars(C1, In, C, T).
local_name_chars(C, _, C, []).


local_name_start_char(C) :-
	name_start_char(C),
	!.
local_name_start_char(0':).
local_name_start_char(0'%).
local_name_start_char(0'\\).


local_name_char(C) :-
	pn_chars(C),
	!.
local_name_char(0':).
local_name_char(0'%).
local_name_char(0'\\).


reserved_char_escapes(0'~).
reserved_char_escapes(0'.).
reserved_char_escapes(0'-).
reserved_char_escapes(0'!).
reserved_char_escapes(0'$).
reserved_char_escapes(0'&).
reserved_char_escapes(0'').
reserved_char_escapes(0'().
reserved_char_escapes(0')).
reserved_char_escapes(0'*).
reserved_char_escapes(0'+).
reserved_char_escapes(0',).
reserved_char_escapes(0';).
reserved_char_escapes(0'=).
reserved_char_escapes(0'/).
reserved_char_escapes(0'?).
reserved_char_escapes(0'#).
reserved_char_escapes(0'@).
reserved_char_escapes(0'%).
reserved_char_escapes(0'_).


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
punctuation(0'?, '?').
punctuation(0'!, '!').
punctuation(0'^, '^').
punctuation(0'=, '=').
punctuation(0'<, '<').
punctuation(0'>, '>').
punctuation(0'$, '$').


skip_line(-1, _, -1) :-
	!.
skip_line(0xA, In, C) :-
	!,
	cnt(line_number),
	get_code(In, C).
skip_line(0xD, In, C) :-
	!,
	get_code(In, C).
skip_line(_, In, C) :-
	get_code(In, C1),
	skip_line(C1, In, C).


white_space(0x9).
white_space(0xA) :-
	cnt(line_number).
white_space(0xD).
white_space(0x20).

