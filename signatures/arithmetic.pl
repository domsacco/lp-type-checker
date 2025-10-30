% arithmetic.pl
% Arithmetic and numeric helper signatures

signature(plus, [varargs(num)], num).
signature(minus, [varargs(num)], num).
signature(times, [varargs(num)], num).
signature(divide, [varargs(num)], num).

signature(abs, [num], num).
signature(max, [varargs(num)], num).
signature(min, [varargs(num)], num).
signature(round, [num], num).
signature(floor, [num], num).
signature(ceiling, [num], num).

signature(pow, [num, num], num).
signature(sqrt, [num], num).
signature(mod, [num, num], num).
signature(exp, [num], num).
signature(log, [num], num).
signature(sin, [num], num).
signature(cos, [num], num).
signature(tan, [num], num).
signature(evenp, [num], bool).
signature(oddp, [num], bool).
signature(zero, [num], bool).
