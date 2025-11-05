% arithmetic.pl
% Arithmetic and numeric helper signatures

:- multifile type/2.

type(plus, fn([varargs(num)], num)).
type(minus, fn([varargs(num)], num)).
type(times, fn([varargs(num)], num)).
type(divide, fn([varargs(num)], num)).

type(abs, fn([num], num)).
type(max, fn([varargs(num)], num)).
type(min, fn([varargs(num)], num)).
type(round, fn([num], num)).
type(floor, fn([num], num)).
type(ceiling, fn([num], num)).

type(pow, fn([num, num], num)).
type(sqrt, fn([num], num)).
type(mod, fn([num, num], num)).
type(exp, fn([num], num)).
type(log, fn([num], num)).
type(sin, fn([num], num)).
type(cos, fn([num], num)).
type(tan, fn([num], num)).
type(evenp, fn([num], bool)).
type(oddp, fn([num], bool)).
type(zero, fn([num], bool)).
