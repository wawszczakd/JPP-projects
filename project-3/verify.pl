% Dominik Wawszczak

verify(N, _) :-
    (\+ integer(N); \+ N > 0),
    format("Error: parametr 0 powinien być liczbą > 0~n").

verify(N, File) :-
    set_prolog_flag(fileerrors, off),
    see(File),
    read(variables(VarsIds)),
    read(arrays(ArrsIds)),
    read(program(Stmts)),
    ( Stmts = [] ->
        exitSafe
    ; 
        initState(prog(VarsIds, ArrsIds, Stmts), N, State),
        check(N, prog(VarsIds, ArrsIds, Stmts), State, [], [], _, Res),
        format("Check done~n")
    ),
    seen.

verify(_, File) :-
    format("Error: brak pliku o nazwie - ~w~n", [File]).

exitSafe :-
    format("Program jest poprawny (bezpieczny).~n").

initState(prog(VarsIds, ArrsIds, _), N, state(Vars, Arrs, Insts)) :-
    createVars(VarsIds, Vars),
    createArrs(ArrsIds, N, Arrs),
    createArr(N, Insts).

createVars([], []).
createVars([VarId | Tail1], [(VarId, 0) | Tail2]) :-
    createVars(Tail1, Tail2).

createArrs([], _, []).
createArrs([ArrId | Tail1], N, [(ArrId, Arr) | Tail2]) :-
    createArr(N, Arr),
    createArrs(Tail1, N, Tail2).

createArr(0, []) :- !.
createArr(N, [0 | Tail]) :-
    N1 is N - 1,
    createArr(N1, Tail).

% check(+NProc, +Prog, +State, +Stack, +Vis, -NewVis, -Res)
check(_, _, State, _, Vis, Vis, _) :-
    member(State, Vis),
    !.

check(_, Prog, State, Stack, Vis, Vis, bad(Inter, ProcsIds, StateId)) :-
    collision(Prog, State, ProcsIds),
    !,
    reverse(Stack, Inter),
    length(Vis, Len),
    StateId is Len + 1.

check(N, Prog, State, Stack, Vis, NewVis, Res) :-
    iterateNeighbours(N, Prog, State, Stack, Vis, NewVis, Res, 0).
    
% iterateNeighbours(N, Prog, State, Stack, Vis, NewVis, Res, NeiId)
iterateNeighbours(N, _, _, _, Vis, Vis, _, N) :- !.

iterateNeighbours(N, Prog, State, Stack, Vis, NewVis, Res, NeiId).

collision(prog(_, _, Stmts), state(_, _, Insts), ProcsIds) :-
    procsInSection(Stmts, Insts, 0, ProcsIds),
    length(ProcsIds, Len),
    Len > 1.

procsInSection(_, [], _, []).
procsInSection(Stmts, [Inst | Insts], ProcId, Res) :-
    ( atPosition(Stmts, Inst, sekcja) ->
        Res = [ProcId | ProcsIds]
    ;
        Res = ProcsIds
    ),
    ProcId1 is ProcId + 1,
    procsInSection(Stmts, Insts, ProcId1, ProcsIds).

% atPosition(I, List, Val)
atPosition([Head | _], 0, Head) :- !.
atPosition([_ | Tail], I, Res) :-
    I > 0,
    I1 is I - 1,
    atPosition(Tail, I1, Res).
