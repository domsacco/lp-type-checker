% comparison.pl
% Numeric and general comparison signatures

:- multifile type/2.

type(lt, fn([num, num], bool)).
type(gt, fn([num, num], bool)).

type(eq, fn([any, any], bool)).
type(equal, fn([any, any], bool)).
type(atom, fn([any], bool)).
type(null, fn([list(any)], bool)).
type(identity, fn([any], any)).
