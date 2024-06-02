% Dominik Wawszczak

% The state is represented as state(Vars, Arrs, Insts), where:
%  * Vars is a list of pairs (VarName, Value), representing the variable names
%    and their corresponding values.
%  * Arrs is a list of pairs (ArrName, Array), where ArrName is the name of the
%    array and Array is the list of its elements.
%  * Insts is a list of the current instruction numbers for each process,
%    indexed by process ID.

:- op(700, xfx, <>).

verify(N, _) :-
    (\+ integer(N); \+ N > 0),
    write('Error: parametr 0 powinien byc liczba > 0'), nl.

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
        ( var(Res) ->
            exitSafe
        ;
            exitInterleaving(Res)
        )
    ),
    seen.

verify(_, File) :-
    write('Error: brak pliku o nazwie - '), write(File), nl.

exitSafe :-
    write('Program jest poprawny (bezpieczny).'), nl.

exitInterleaving(bad(Stack, ProcsIds)) :-
    write('Program jest niepoprawny.'), nl,
    write('Niepoprawny przeplot:'), nl,
    printInterleaving(Stack),
    write('Procesy w sekcji: '),
    printProcsIds(ProcsIds).

printInterleaving([]).
printInterleaving([(ProcId, InstNo) | Tail]) :-
    printInterleaving(Tail),
    InstNo1 is InstNo + 1,
    write('   Proces '), write(ProcId), write(': '), write(InstNo1), nl.

printProcsIds([ProcId]) :-
    write(ProcId), write('.'), nl.
printProcsIds([ProcId | Tail]) :-
    write(ProcId), write(', '),
    printProcsIds(Tail).

% initState(+Prog, +N, -State)
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

createArr(0, []).
createArr(N, [0 | Tail]) :-
    N1 is N - 1,
    createArr(N1, Tail).

% check(+N, +Prog, +State, +Stack, +Vis, -NewVis, -Res)
check(_, _, State, _, Vis, Vis, _) :-
    member(State, Vis).

check(_, Prog, State, Stack, Vis, Vis, bad(Stack, ProcsIds)) :-
    collision(Prog, State, ProcsIds).

check(N, Prog, State, Stack, Vis, NewVis, Res) :-
    iterateNeighbours(N, Prog, State, Stack, [State | Vis], NewVis, Res, 0).

% iterateNeighbours(N, Prog, State, Stack, Vis, NewVis, Res, ProcId)
iterateNeighbours(N, _, _, _, Vis, Vis, _, N).
iterateNeighbours(N, Prog, state(Vars, Arrs, Insts), Stack, Vis, NewVis, Res,
                  ProcId) :-
    step(Prog, state(Vars, Arrs, Insts), ProcId, NewState),
    atPosition(Insts, ProcId, InstNo),
    check(N, Prog, NewState, [(ProcId, InstNo) | Stack], Vis, Vis2, Res),
    ProcId1 is ProcId + 1,
    ( var(Res) ->
        iterateNeighbours(N, Prog, state(Vars, Arrs, Insts), Stack, Vis2,
                          NewVis, Res, ProcId1)
    ;
        true
    ).

% collision(+Prog, +State, -ProcsIds)
collision(prog(_, _, Stmts), state(_, _, Insts), ProcsIds) :-
    procsInSection(Stmts, Insts, 0, ProcsIds),
    length(ProcsIds, Len),
    Len > 1.

% procsInSection(+Stmts, +Insts, +ProcId, -Res)
% Res is a list of processes in section.
procsInSection(_, [], _, []).
procsInSection(Stmts, [Inst | Insts], ProcId, Res) :-
    ( atPosition(Stmts, Inst, sekcja) ->
        Res = [ProcId | ProcsIds]
    ;
        Res = ProcsIds
    ),
    ProcId1 is ProcId + 1,
    procsInSection(Stmts, Insts, ProcId1, ProcsIds).

% atPosition(+List, +I, -Val)
atPosition([Head | _], 0, Head).
atPosition([_ | Tail], I, Res) :-
    I > 0,
    I1 is I - 1,
    atPosition(Tail, I1, Res).

% setPosition(+List, +I, +Val, -NewList)
setPosition([_ | Tail], 0, Val, [Val | Tail]).
setPosition([Head | Tail1], I, Val, [Head | Tail2]) :-
    I > 0,
    I1 is I - 1,
    setPosition(Tail1, I1, Val, Tail2).

% step(+Program, +State, +ProcId, -NewState)
step(prog(_, _, Stmts), state(Vars1, Arrs1, Insts1), ProcId,
     state(Vars2, Arrs2, Insts2)) :-
    atPosition(Insts1, ProcId, InstNo),
    atPosition(Stmts, InstNo, Stmt),
    stepInst(Stmt, state(Vars1, Arrs1, Insts1), ProcId, InstNo,
             state(Vars2, Arrs2, Insts2)).

% stepInst(+Stmt, +StateIn, +ProcId, -StateOut)
stepInst(assign(array(Name, Expr1), Expr2), state(Vars, Arrs1, Insts1), ProcId,
         InstNo, state(Vars, Arrs2, Insts2)) :-
    evalExpr(Expr1, Vars, Arrs1, ProcId, I),
    evalExpr(Expr2, Vars, Arrs1, ProcId, Val),
    setArr(Arrs1, Name, I, Val, Arrs2),
    InstNo1 is InstNo + 1,
    setPosition(Insts1, ProcId, InstNo1, Insts2).

stepInst(assign(Var, Expr), state(Vars1, Arrs, Insts1), ProcId, InstNo,
         state(Vars2, Arrs, Insts2)) :-
    evalExpr(Expr, Vars1, Arrs, ProcId, Val),
    setVar(Vars1, Var, Val, Vars2),
    InstNo1 is InstNo + 1,
    setPosition(Insts1, ProcId, InstNo1, Insts2).

stepInst(goto(NewInstNo), state(Vars, Arrs, Insts1), ProcId, _,
         state(Vars, Arrs, Insts2)) :-
    NewInstNo1 is NewInstNo - 1,
    setPosition(Insts1, ProcId, NewInstNo1, Insts2).

stepInst(condGoto(Expr, NewInstNo), state(Vars, Arrs, Insts1), ProcId, InstNo,
         state(Vars, Arrs, Insts2)) :-
    ( evalBool(Expr, Vars, Arrs, ProcId) ->
        NewInstNo1 is NewInstNo - 1,
        setPosition(Insts1, ProcId, NewInstNo1, Insts2)
    ;
        InstNo1 is InstNo + 1,
        setPosition(Insts1, ProcId, InstNo1, Insts2)
    ).

stepInst(sekcja, state(Vars, Arrs, Insts1), ProcId, InstNo,
         state(Vars, Arrs, Insts2)) :-
    InstNo1 is InstNo + 1,
    setPosition(Insts1, ProcId, InstNo1, Insts2).

% evalExpr(+Expr, +Vars, +Arrs, +ProcId, -Val)
evalExpr(pid, _, _, ProcId, ProcId).

evalExpr(N, _, _, _, N) :- integer(N).

evalExpr(array(Name, Expr), Vars, Arrs, ProcId, Val) :-
    member((Name, Arr), Arrs),
    evalExpr(Expr, Vars, Arrs, ProcId, I),
    atPosition(Arr, I, Val).

evalExpr(Var, Vars, _, _, Val) :-
    member((Var, Val), Vars).

evalExpr(E1 + E2, Vars, Arrs, ProcId, Val) :-
    evalExpr(E1, Vars, Arrs, ProcId, Val1),
    evalExpr(E2, Vars, Arrs, ProcId, Val2),
    Val is Val1 + Val2.

evalExpr(E1 - E2, Vars, Arrs, ProcId, Val) :-
    evalExpr(E1, Vars, Arrs, ProcId, Val1),
    evalExpr(E2, Vars, Arrs, ProcId, Val2),
    Val is Val1 - Val2.

evalExpr(E1 * E2, Vars, Arrs, ProcId, Val) :-
    evalExpr(E1, Vars, Arrs, ProcId, Val1),
    evalExpr(E2, Vars, Arrs, ProcId, Val2),
    Val is Val1 * Val2.

evalExpr(E1 / E2, Vars, Arrs, ProcId, Val) :-
    evalExpr(E1, Vars, Arrs, ProcId, Val1),
    evalExpr(E2, Vars, Arrs, ProcId, Val2),
    Val is Val1 // Val2.

% evalBool(+Expr, +Vars, +Arrs, +ProcId)
evalBool(E1 < E2, Vars, Arrs, ProcId) :-
    evalExpr(E1, Vars, Arrs, ProcId, Val1),
    evalExpr(E2, Vars, Arrs, ProcId, Val2),
    Val1 < Val2.

evalBool(E1 = E2, Vars, Arrs, ProcId) :-
    evalExpr(E1, Vars, Arrs, ProcId, Val1),
    evalExpr(E2, Vars, Arrs, ProcId, Val2),
    Val1 =:= Val2.

evalBool(E1 <> E2, Vars, Arrs, ProcId) :-
    evalExpr(E1, Vars, Arrs, ProcId, Val1),
    evalExpr(E2, Vars, Arrs, ProcId, Val2),
    Val1 =\= Val2.

% setVar(+Vars, +Var, +Val, -NewVars)
setVar([(Var, _) | Tail], Var, Val, [(Var, Val) | Tail]).
setVar([Head | Tail1], Var, Val, [Head | Tail2]) :-
    setVar(Tail1, Var, Val, Tail2).

% setArr(+Arrs, +Name, +I, +Val, -NewArrs)
setArr([(Name, Arr) | Tail], Name, I, Val, [(Name, NewArr) | Tail]) :-
    setPosition(Arr, I, Val, NewArr).
setArr([Head | Tail1], Name, I, Val, [Head | Tail2]) :-
    setArr(Tail1, Name, I, Val, Tail2).
