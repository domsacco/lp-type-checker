:- module(typemap,
		  [ type/2  % type(Name, TypeTerm) where TypeTerm is fn(ArgTypes, ReturnType)
		  ]).

% Simple type-language used in the types below:
%  - num, sym, str, bool, any
%  - list(T)            a list whose elements have type T
%  - fn(ArgTypes,Ret)   function type
%  - varargs(T)         zero-or-more args of type T

% The actual type facts are split into category files for clarity.
:- ensure_loaded('types/arithmetic.pl').
:- ensure_loaded('types/comparison.pl').
:- ensure_loaded('types/logical.pl').
% :- ensure_loaded('types/equality.pl').
% :- ensure_loaded('types/io.pl').
% :- ensure_loaded('types/lists.pl').
% :- ensure_loaded('types/higher_order.pl').
% :- ensure_loaded('types/conversions.pl').
% :- ensure_loaded('types/typepreds.pl').
% :- ensure_loaded('types/strings.pl').
% :- ensure_loaded('types/sets.pl').
% :- ensure_loaded('types/assoc.pl').
% :- ensure_loaded('types/vectors.pl').
% :- ensure_loaded('types/misc.pl').

