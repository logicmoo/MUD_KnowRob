
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/turtle)).	% Turtle and TRiG
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(owl_parser)).
:- use_module(library(owl)).

% :- use_module(library(rdfs_computable)).
% :- use_module(library(knowrob_owl)).

:- rdf_register_prefix(e, 'http://eulersharp.sourceforge.net/2003/03swap/log-rules#').
:- rdf_register_prefix( fn, 'http://www.w3.org/2006/xpath-functions#').
:- rdf_register_prefix(list, 'http://www.w3.org/2000/10/swap/list#').
:- rdf_register_prefix(log, 'http://www.w3.org/2000/10/swap/log#').
:- rdf_register_prefix(prolog, 'http://eulersharp.sourceforge.net/2003/03swap/prolog#').
:- rdf_register_prefix(math, 'http://www.w3.org/2000/10/swap/math#').
:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(string, 'http://www.w3.org/2000/10/swap/string#').
:- rdf_register_prefix(time, 'http://www.w3.org/2000/10/swap/time#').
:- rdf_register_prefix(func, 'http://www.w3.org/2007/rif-builtin-function#').
:- rdf_register_prefix(pred, 'http://www.w3.org/2007/rif-builtin-predicate#').

reload_euler:-forall(sw_file(euler,'./autoload',File),rdf_reload_file(File)).

:- initialization(reload_euler). 

