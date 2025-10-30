% comparison.pl
% Numeric and general comparison signatures

signature(lt, [num, num], bool).
signature(gt, [num, num], bool).

signature(eq, [X, X], bool).
signature(equal, [X, X], bool).
signature(atom, [X], bool).
signature(null, [list(X)], bool).
signature(identity, [X], X).
