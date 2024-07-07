% Solution for the third task in the course JPP MIM UW 2023/2024.
% Author: Krzysztof Żyndul
% Indeks: kz438842
% State is represented as a tuple state(ICL, VL, AL), where:
% - ICL is a list of instruction counters for each process
% - VL is a list of pairs (variableName-value)
% - AL is a list of pairs (arrayName-values)

% When progrma finds incorrect interleaving it prints the list of pairs
% (ProcessPid-InstructionCounter) that represent the incorrect interleaving.
% InstructionCounter is the instruction number that process with pid ProcessPid
% will execute next.

:- op(500, xfx, <>).
:- ensure_loaded(library(lists)).
:- set_prolog_flag(fileerrors, off).

% verify(+N, +Program)
verify(N, Program) :-
    checkFile(Program),
    checkProcessNumber(N),
    readProgram(VL, AL, Instructions),
    initState(program(VL, AL), N, InitialState),
    generateInitialPath(N, Path),
    (isStateSafe(Instructions, InitialState, Path) -> 
        checkProgram(Instructions, InitialState, N, Path) ;
        true
    ),
    seen.

checkFile(Program) :-
    (see(Program) ->
        true ;
        format('Error: there is no file named "~p".~n', [Program]), seen, false
    ).

checkProcessNumber(N) :-
    (N > 0 ->
        true ;
        format('Error: invalid number of processes: ~p.~n', [N]), seen, false
    ).

readProgram(VL, AL, Instructions) :-
    read(variables(VL)),
    read(arrays(AL)),
    read(program(Instructions)).

% checkProgram(+Program, +InitialState, +N, +InitialPath)
% Check if the program is safe by performing a depth-first search on all
% possible interleavings.
checkProgram(Program, InitialState, N, InitialPath) :-
    (dfs(Program, InitialState, N, [InitialState], _, InitialPath) ->
        format('Program is safe.~n', []) ;
        true
    ).

% dfs(+Program, +State, +N, +Visited, -FinalVisited, -Path)
% Perform a depth-first search on all possible interleavings of the program.
dfs(Program, State, N, Visited, FinalVisited, Path) :-
    dfsIterate(Program, State, N, 0, Visited, FinalVisited, Path).

% dfsIterate(+Program, +State, +N, +I, +Visited, -FinalVisited, +Path)
% In a loop iterate over all processes and try to execute the next instruction.
% If the new state is safe then continue the search. If the new state is not safe
% then print the incorrect interleaving and the list of processes in the critical
% section and fails.
dfsIterate(Program, State, N, I, Visited, FinalVisited, Path) :-
    N > I,
    step(Program, State, I, NewState),
    \+ member(NewState, Visited), !,
    getProcessIC(NewState, I, CurrentIC),
    isStateSafe(Program, NewState, [I-CurrentIC | Path]),
    NextI is I + 1,
    dfs(Program, NewState, N, [NewState | Visited], NewVisited, [I-CurrentIC | Path]),
    dfsIterate(Program, State, N, NextI, NewVisited, FinalVisited, Path).

% If the new state is already visited then skip it and continue the search.
dfsIterate(Program, State, N, I, Visited, FinalVisited, Path) :-
    N > I,
    step(Program, State, I, NewState),
    member(NewState, Visited), !,
    NextI is I + 1,
    dfsIterate(Program, State, N, NextI, Visited, FinalVisited, Path).

% If iteration is finished then return the list of visited states.
dfsIterate(_, _, N, I, Visited, Visited, _) :-
    N =< I.




% isStateSafe(+Program, +State, +Path)
% Check if the state is safe by verifying if there is more than one process in
% the critical section. Path is a list of (ProcessPid, Value) pairs that
% represent the current interleaving. If there is more then one process in the
% process in critical section then the state is not safe.
isStateSafe(Program, state(ICL, _, _), Path) :-
    getProcessInSekcja(Program, ICL, 0, InSection),
    length(InSection, Len),
    (Len =< 1 ->
        true ;
        printNotSafe(Path, InSection), false
    ).

% getProcessInSekcja(+Program, +ICL, +PrId, -InSection)
% Get the list of processes that are in the critical section.
getProcessInSekcja(Program, [IC | ICL], PrId, [PrId | InSection]) :-
    nth1(IC, Program, sekcja), 
    NextPrId is PrId + 1,
    getProcessInSekcja(Program, ICL, NextPrId, InSection).

getProcessInSekcja(Program, [IC | ICL], PrId, InSection) :-
    \+ nth1(IC, Program, sekcja),
    NextPrId is PrId + 1,
    getProcessInSekcja(Program, ICL, NextPrId, InSection).

getProcessInSekcja(_, [], _, []).



% initState(+Program, +N, -StanPoczątkowy)
% Initialize the state of the program. The instruction counter is set to 1 for
% each process. Variables are initialized to 0 and arrays are initialized to
% a list of zeros.
initState(program(VL, AL), N, state(ICL, Variables, Arrays)) :-
    createList(N, 1, ICL),    
    maplist(initVariable(0), VL, Variables),
    createList(N, 0, ZerosList),
    maplist(initVariable(ZerosList), AL, Arrays).

% createList(+N, +Value, -List)
% Create a list of N elements with all elements set to Value.
createList(N, Value, List) :-
    length(List, N),
    maplist(=(Value), List).

initVariable(Value, Name, Name-Value).


% step(+Program, +StanWe, ?PrId, -StanWy)
step(Program, state(ICL, VL, AL), PrId, NewState) :-
    nth0(PrId, ICL, CurrentIC),
    nth1(CurrentIC, Program, Instruction),
    executeInstruction(Instruction, state(ICL, VL, AL), PrId, NewState).



% executeInstruction(+Instruction, +State, +PrId, -NewState)
% Execute the instruction and returns new state.
executeInstruction(assign(Name, Expression),
                    state(ICL, VL, AL),
                    PrId,
                    state(NewICL, NewVL, AL)) :-
    atom(Name), !,
    evaluate(Expression, state(ICL, VL, AL), PrId, Value),
    update(Name, Value, VL, NewVL),
    nth0(PrId, ICL, CurrentIC),
    NewIC is CurrentIC + 1,
    replaceNth0(ICL, PrId, NewIC, NewICL).

executeInstruction(assign(array(Name, Index), Expression),
                    state(ICL, VL, AL),
                    PrId,
                    state(NewICL, VL, NewAL)) :-
    evaluate(Expression, state(ICL, VL, AL), PrId, Value),
    evaluate(Index, state(ICL, VL, AL), PrId, IndexValue),
    update(array(Name, IndexValue), Value, AL, NewAL),
    nth0(PrId, ICL, CurrentIC),
    NewIC is CurrentIC + 1,
    replaceNth0(ICL, PrId, NewIC, NewICL).

executeInstruction(goto(Number),
                    state(ICL, VL, AL),
                    PrId,
                    state(NewICL, VL, AL)) :-
    number(Number),
    replaceNth0(ICL, PrId, Number, NewICL).

executeInstruction(condGoto(Condition, Number),
                    state(ICL, VL, AL),
                    PrId,
                    state(NewICL, VL, AL)) :-
    evaluate(Condition, state(ICL, VL, AL), PrId, Value),
    (Value =:= 1 ->
        replaceNth0(ICL, PrId, Number, NewICL) ;
        replaceNth0(ICL, PrId, NewIC, NewICL),
        nth0(PrId, ICL, CurrentIC),
        NewIC is CurrentIC + 1).

executeInstruction(sekcja,
                    state(ICL, VL, AL),
                    PrId,
                    state(NewICL, VL, AL)) :-
    nth0(PrId, ICL, CurrentIC),
    NewIC is CurrentIC + 1,
    replaceNth0(ICL, PrId, NewIC, NewICL).



% evaluate(+Expression, +State, +PrId, -Value)
% Evaluate the expression and return the value.
evaluate(Value, _, _, Value) :- number(Value), !.

evaluate(pid, _, PrId, PrId) :- !.

evaluate(Variable, state(_, VL, _), _, Value) :-
    atom(Variable),
    Variable \= pid,
    member(Variable-Value, VL).

evaluate(array(Variable, Index), state(_, VL, AL), PrId, Value) :-
    evaluate(Index, state(_, VL, AL), PrId, IndexValue),
    member(Variable-Array, AL),
    nth0(IndexValue, Array, Value). 

evaluate(Expression, state(_, VL, AL), PrId, Value) :-
    Expression =.. [Operator, Left, Right],
    evaluate(Left, state(_, VL, AL), PrId, LeftValue),
    evaluate(Right, state(_, VL, AL), PrId, RightValue),
    apply(Operator, LeftValue, RightValue, Value).



apply(+, Left, Right, Result) :- Result is Left + Right.
apply(-, Left, Right, Result) :- Result is Left - Right.
apply(*, Left, Right, Result) :- Result is Left * Right.
apply(/, Left, Right, Result) :- Result is Left / Right.
apply(<, Left, Right, Result) :- (Left < Right -> Result = 1 ; Result = 0).
apply(=, Left, Right, Result) :- (Left =:= Right -> Result = 1 ; Result = 0).
apply(<>, Left, Right, Result) :- (Left =\= Right -> Result = 1 ; Result = 0).


% getProcessIC(+State, +PrId, -CurrentIC)
% Get the current instruction counter for the process with pid PrId.
getProcessIC(state(ICL, _, _), PrId, CurrentIC) :-
    nth0(PrId, ICL, CurrentIC).          

% replaceNth0(+List, +Index, +Value, -NewList)
% Replace the Nth element in a list with a new value start counting from 0.
replaceNth0([_|T], 0, X, [X|T]) :- !.
replaceNth0([H|T], I, X, [H|R]) :-
    I > 0,
    NI is I - 1,
    replaceNth0(T, NI, X, R).

% update(+Name, +Value, +Dict, -NewDict)
% Update the value of the variable in the dictionary.
update(Name, Value, Dict, [Name-Value | Rest]) :-
    atom(Name), !,                                  % we are updating variable.
    select(Name-_, Dict, Rest).

update(array(Name, Index), Value, Dict, [Name-NewArray | Rest]) :-
    select(Name-Array, Dict, Rest),                 % we are updating array.
    replaceNth0(Array, Index, Value, NewArray).


% generateInitialPath(+N, -Path)
% Generate the initial path with all instruction counters set to 1.
generateInitialPath(N, [N1-1 | Path]) :-
    N > 0, !,
    N1 is N - 1,
    generateInitialPath(N1, Path).

generateInitialPath(0, []).


% printNotSafe(+Path, +InSection)
% Print the incorrect interleaving and the list of processes in the critical
% section.
printNotSafe(Path, InSection) :-
    format('Program is incorrect.~n', []),
    format('Incorrect interleaving:~n', []),
    printPath(Path),
    format('Processes in section: ~p.~n', [InSection]).

printPath(ReversedPath) :-
    reverse(ReversedPath, Path),
    maplist(printElement, Path).

printElement(ProcessPid-Value) :-
    format('\tProcess ~p: ~p\n', [ProcessPid, Value]).
