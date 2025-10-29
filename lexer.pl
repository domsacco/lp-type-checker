:- module(lexer,
          [ tokenize/2,        % tokenize(+String, -Tokens)
            skip_ws//0,        % exported for external debug helpers
            token//1           % exported for external debug helpers
          ]).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(dcg/basics)).  % for blank, digit, etc.

% Public API: tokenize/2
% String -> list of token(...) terms
tokenize(String, Tokens) :-
    string_chars(String, Cs),
    phrase(tokens(Tokens), Cs).

% Top-level DCG: many tokens with whitespace/comments skipped
tokens([T|Ts]) --> skip_ws, token(T), !, tokens(Ts).
tokens([])      --> skip_ws, [].

% Individual token rules (order matters: numbers before dot, etc.)
token(lpar)        --> "(".
token(rpar)        --> ")".
token(quote)       --> "'".
token(quasiquote)  --> "`".
token(unquote_splicing) --> [',','@'].
token(unquote)     --> ",".
token(dot)         --> ".".
token(boolean(true))  --> "#", "t".
token(boolean(false)) --> "#", "f".
token(vector_start) --> "#", "(".
token(char(Cname)) --> "#", "\\", char_name(Cname).
token(number(N))   --> number_token(N).
token(string(S))   --> string_token(S).
token(symbol(S))   --> symbol_token(S).

% Skip whitespace and comments (line comments starting with ;)
skip_ws --> ( blank ; comment_line ), !, skip_ws.
skip_ws --> [].

comment_line --> [';'], comment_chars.
comment_chars --> [C], { C \= '\n' }, !, comment_chars.
comment_chars --> [].

% Character name after #\  (e.g. #\a , #\Space)
char_name(Name) --> symbol_chars(Cs), { Cs \= [], atom_chars(Name, Cs) }.

% Number: integer or float with optional sign and exponent
number_token(N) -->
    sign_opt(Sign),
    ( l_digits(Ds), fraction_opt(Fs), exponent_opt(Es)
    ; ['.'], l_digits(Fs), { Ds = [] }, exponent_opt(Es)
    ),
    { append(Ds, Fs, Body1),
      append(Body1, Es, AllDigits),
      (Sign = '-' -> All = ['-'|AllDigits] ; All = AllDigits),
      number_chars(N, All)
    }.

sign_opt('-') --> ['-'], !.
sign_opt('+') --> ['+'], !.
sign_opt('+') --> [].

% renamed digit helpers to avoid collisions with std predicates
l_digits([D|Ds]) --> [D], { char_type(D, digit) }, l_digits_tail(Ds).
l_digits_tail([D|Ds]) --> [D], { char_type(D, digit) }, !, l_digits_tail(Ds).
l_digits_tail([]) --> [].

fraction_opt(['.'|Fs]) --> ['.'], l_digits(Fs), !.
fraction_opt([]) --> [].

exponent_opt(Exp) --> ( ['e'] ; ['E'] ), sign_opt(Esign), l_digits(Ds),
    { (Esign = '-' -> Exp = ['e','-'|Ds] ; Exp = ['e'|Ds]) }.
exponent_opt([]) --> [].

% String with backslash escapes
string_token(S) --> ['"'], string_chars(Cs), ['"'], { atom_chars(S, Cs) }.
string_chars([C|Cs]) --> ['\\', Esc], { escape_char(Esc, C) }, !, string_chars(Cs).
string_chars([C|Cs]) --> [C], { C \= '"' , C \= '\\' }, !, string_chars(Cs).
string_chars([]) --> [].

escape_char('n','\n').
escape_char('t','\t').
escape_char('r','\r').
escape_char('"','"').
escape_char('\\','\\').
escape_char(X,X).

% Symbol token: run of non-delimiter chars
symbol_token(Atom) -->
    symbol_chars(Cs),
    { Cs \= [], atom_chars(Atom, Cs) }.

symbol_chars([C|Cs]) --> [C], { \+ symbol_delim(C) }, symbol_chars_tail(Cs).
symbol_chars_tail([C|Cs]) --> [C], { \+ symbol_delim(C) }, !, symbol_chars_tail(Cs).
symbol_chars_tail([]) --> [].

% characters considered delimiters for symbols
symbol_delim(C) :-
    memberchk(C, ['(',')','"',';',' ', '\t','\n','\r',',','@','`','\'','.']).
