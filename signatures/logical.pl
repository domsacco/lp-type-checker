% logical.pl
% Logical operations

signature(not, [bool], bool).
signature(and, [varargs(bool)], bool).
signature(or, [varargs(bool)], bool).
