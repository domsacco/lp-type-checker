:- module(parser,
          [ parse/2        % parse(+Tokens, -ASTs)
          ]).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(dcg/basics)).  % for DCG helpers

% The parser works on the token list produced by `lexer:tokenize/2`.
% A token list contains terms like `lpar`, `rpar`, `symbol(Name)`, etc.
% Define a simple DCG `token//1` that matches one token from that list.
% (We intentionally do not import `lexer:token//1` here, because that
% nonterminal is for turning characters into tokens; the parser expects
% already-tokenized input.)
token(T) --> [T].

% Public API: parse/2
parse(Tokens, ASTs) :-
    phrase(sexprs(ASTs), Tokens).

% Top-level DCG: many s-expressions
sexprs([]) --> [].
sexprs([S|Ss]) --> sexpr(S), !, sexprs(Ss).

% Individual s-expression rules
sexpr(S) --> token(lpar), sexprs(Ss), token(rpar), { S = list(Ss) }.
sexpr(quote(S)) --> token(quote), sexpr(S).
sexpr(quasiquote(S)) --> token(quasiquote), sexpr(S).
sexpr(unquote(S)) --> token(unquote), sexpr(S).
sexpr(unquote_splicing(S)) --> token(unquote_splicing), sexpr(S).
sexpr(dot) --> token(dot).
sexpr(vector(Ss)) --> token(vector_start), sexprs(Ss), token(rpar).
sexpr(S) --> atomic_s(S).

% Atomic s-expressions
atomic_s(symbol(S)) --> token(symbol(S)).
atomic_s(number(N)) --> token(number(N)).
atomic_s(string(S)) --> token(string(S)).
atomic_s(boolean(B)) --> token(boolean(B)).
atomic_s(char(C)) --> token(char(C)).