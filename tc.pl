:- module(tc,
          [ file_tokens/2,
            file_parse/2,
            file_type_check/2,
            tc/1
          ]).

:- use_module(lexer).
:- use_module(parser).
:- use_module(library(readutil)).  % read_file_to_string/3

% file_tokens(+FilePath, -Tokens)
% Reads the file (UTF-8) and returns the token list produced by tokenize/2.
file_tokens(File, Tokens) :-
    read_file_to_string(File, String, [encoding(utf8)]),
    lexer:tokenize(String, Tokens).

% file_parse(+FilePath, -ASTs)
% Reads the file, tokenizes it, and parses the tokens into ASTs.
file_parse(File, ASTs) :-
    file_tokens(File, Tokens),
    parser:parse(Tokens, ASTs).

% file_type_check(+FilePath, -TypeCheckedASTs)
% Reads the file, tokenizes, parses, and type-checks the ASTs.
file_type_check(File, TypeCheckedASTs) :-
    file_parse(File, ASTs),
    type_check(ASTs, TypeCheckedASTs).

tc(File) :-
    file_type_check(File, _TypeCheckedASTs).

% trivial type checker: always succeeds
type_check(_ASTs, _TypeChecked) :-
    true.