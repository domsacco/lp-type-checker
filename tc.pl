:- module(tc).

:- use_module(lisp_lexer_utils).
:- use_module(library(readutil)).  % read_file_to_string/3

% file_tokens(+FilePath, -Tokens)
% Reads the file (UTF-8) and returns the token list produced by tokenize/2.
file_tokens(File, Tokens) :-
    read_file_to_string(File, String, [encoding(utf8)]),
    tokenize(String, Tokens).

% file_parse(+FilePath, -ASTs)
% Reads the file (UTF-8) and returns the AST produced by parse_tokens/2.
file_parse(File, ASTs):-
    file_tokens(File, Tokens),
    parse_tokens(Tokens, ASTs).

% file_type_check(+FilePath, -TypeCheckedAST)
% Reads the file (UTF-8), parses it and type-checks the resulting AST.
file_type_check(File, TypeCheckedASTs):-
    file_parse(File, ASTs)
    type_check(ASTs, TypeCheckedASTs).

tc(File):-
    file_type_check(File, TypeCheckedASTs).