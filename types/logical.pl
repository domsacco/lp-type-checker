% logical.pl

:- multifile type/2.

% Logical operations
type(not, fn([bool], bool)).
type(and, fn([varargs(bool)], bool)).
type(or, fn([varargs(bool)], bool)).

% Control-flow / logical forms
% `if`: (if cond then else) returns the type of the branches; require both branches to have the same type
type(if, fn([bool, any, any], any)).

% `when` / `unless`: conditional execution of zero-or-more expressions.
% If the body has expressions of type any, the overall form returns `maybe(any)` because the
% body may not be executed (i.e. nothing returned).
type(when, fn([bool, varargs(any)], maybe(any))).
type(unless, fn([bool, varargs(any)], maybe(any))).

% `while`: simple loop that evaluates a boolean condition and optional body; returns maybe(any)
% because the body may never execute. For more precise typing the condition could be a function
% type (e.g. fn([], bool)) but we keep the simple form here.
type(while, fn([bool, varargs(any)], maybe(any))).

% `loop`: generic looping construct with body expressions; return type is `any` as it depends on body
% semantics (could be infinite, or return last expression). Keep permissive for now.
type(loop, fn([varargs(any)], any)).

% `cond`: n-ary conditional; very permissive signature here (could be refined to require
% pairs of (bool, any) and a common result type), so we keep `any` as the return type for now.
type(cond, fn([varargs(any)], any)).
