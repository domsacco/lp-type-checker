:- module(signatures,
		  [ signature/3  % signature(Name, ArgTypes, ReturnType)
		  ]).

% Simple type-language used in the signatures below:
%  - num, sym, str, bool, any
%  - list(T)            a list whose elements have type T
%  - fn(ArgTypes,Ret)   function type
%  - varargs(T)         zero-or-more args of type T

% The actual signature facts are split into category files for clarity.
:- ensure_loaded('signatures/arithmetic.pl').
:- ensure_loaded('signatures/comparison.pl').
:- ensure_loaded('signatures/logical.pl').
:- ensure_loaded('signatures/equality.pl').
:- ensure_loaded('signatures/io.pl').
:- ensure_loaded('signatures/lists.pl').
:- ensure_loaded('signatures/higher_order.pl').
:- ensure_loaded('signatures/conversions.pl').
:- ensure_loaded('signatures/typepreds.pl').
:- ensure_loaded('signatures/strings.pl').
:- ensure_loaded('signatures/sets.pl').
:- ensure_loaded('signatures/assoc.pl').
:- ensure_loaded('signatures/vectors.pl').
:- ensure_loaded('signatures/misc.pl').

