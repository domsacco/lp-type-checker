
:- module(typechecker,
          [ type_check/2  % type_check(+ASTs, -TypeCheckedASTs)
          ]).

% TYPE INFERENCE
% CONTEXT MANAGEMENT
% TYPE MAPPING FOR COMMON SYMB

% Public API: type_check/2
% type_check(+ASTs, -TypeCheckedASTs)
% Given a list of ASTs, produces a list of type-annotated ASTs.
type_check(ASTs, TypeCheckedASTs) :-
    type_infer(ASTs, TypeAnnotedASTs, _Context),
    TypeCheckedASTs = TypeAnnotedASTs.

% type_infer(+ASTs, -TypeCheckedASTs, -FinalContext)
% Infers types for a list of ASTs, producing type-annotated ASTs and a final context.
type_infer([], [], []).
type_infer([AST|Rest], [TypeAnnotedAST|TypedRest], FinalContext) :-
    type_infer(AST, TypedAnnotedAST, Context1),
    type_infer(Rest, TypedRest, Context2),
    merge(Context1, Context2, FinalContext).

% type_infer(+AST, -TypeCheckedAST, -Context)
% Infers the type of a single AST node, producing a type-annotated AST and a context.
type_infer(number(N), number(N-num), C) :- [number(N-num)|C].
type_infer(string(S), string(S-string), []).
type_infer(boolean(B), boolean(B-boolean), []).
type_infer(list(Elements), list(TypedElements-list), Context) :-
    type_infer_list(Elements, TypedElements, Context).
type_infer(fn(Args, Body), fn(TypedArgs, TypedBody-fn), Context) :-
    type_infer_list(Args, TypedArgs, ContextArgs),
    type_infer(Body, TypedBody, ContextBody),
    append(ContextArgs, ContextBody, Context).



% -------------------- All good from here -----------------
% merge(+Context1, +Context2, -MergedContext)
% Merges two contexts, generalizing types for overlapping keys.
merge([], C, C).
merge([K-V|Rest], C2, [K-Vg|C]) :-
    ( (member(K-V1, C2) -> 
        generalization(V, V1, Vg), !) ;
    V = Vg ),
    merge(Rest, C2, C).

% compatibile(+Symbol-Type, +Context)
% Checks if the given Symbol-Type is compatible with the types in the Context.
compatibile(Symbol-Type, Context) :-
    member(Symbol-Type1, Context),
    generalize(Type1, Type).
compatibile(_, _).  % If symbol not in context, it's compatible by default.

% generalization(Type1, Type2, GeneralType)
% Succeeds if GeneralType is the most general type that covers both Type1 and Type2.
% Base case: if both types are identical, the generalization is that type itself.
generalization(Type, Type, Type) :- !.
generalization(Type1, Type2, Type3) :-
    generalize(Type1, Type2),
    Type3 = Type1, !.
generalization(Type1, Type2, Type3) :-
    generalize(Type2, Type1),
    Type3 = Type2.

% generalize(Type1, Type2)
% Succeeds if Type1 is a generalization of Type2.
generalize(any, _).
generalize(num, integer).
generalize(num, float).
generalize(num, complex).
generalize(list([T1|T1s]), list([T2|T2s])) :-
    length(T1, N),
    length(T2, N),
    generalize(T1, T2),
    generalize(T1s, T2s).
generalize(varargs(T1), varargs(T2)) :-
    generalize(T1, T2).
generalize(fn(Args1, Ret1), fn(Args2, Ret2)) :-
    length(Args1, N),
    length(Args2, N),
    maplist(generalize, Args1, Args2),
    generalize(Ret1, Ret2).