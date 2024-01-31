% Represent state as `board(CL,ML,BS,CR,MR)`

start(board(3,3,left,0,0)).

goal(board(0,0,right,3,3)).

% this function will verify the rules in place
legal(CL,ML,CR,MR) :-
    % We avoid negative results in the operations
	ML>=0, CL>=0, MR>=0, CR>=0,
    % Cannibals can't outnumber missionaries, unless there aren't any.
	(ML>=CL ; ML=0),
	(MR>=CR ; MR=0).

% Possible moves:
move(board(CL,ML,right,CR,MR),board(CL,ML2,left,CR,MR2)):-
	% Two missionaries cross right to left.
    % We subtract the outgoing passengers.
	MR2 is MR-2, 
	% We add the arriving passengers.
	ML2 is ML+2, 
    % We verify that cannibals won't get a snack!
	legal(CL,ML2,CR,MR2). 

move(board(CL,ML,right,CR,MR),board(CL2,ML,left,CR2,MR)):-
	% Two cannibals cross right to left.
	CR2 is CR-2,
	CL2 is CL+2,
	legal(CL2,ML,CR2,MR).

move(board(CL,ML,left,CR,MR),board(CL,ML2,right,CR,MR2)):-
	% Two missionaries cross left to right.
	MR2 is MR+2, 
	ML2 is ML-2,
	legal(CL,ML2,CR,MR2).

move(board(CL,ML,left,CR,MR),board(CL2,ML,right,CR2,MR)):-
	% Two cannibals cross left to right.
	CR2 is CR+2,
	CL2 is CL-2,
	legal(CL2,ML,CR2,MR).

move(board(CL,ML,right,CR,MR),board(CL,ML2,left,CR,MR2)):-
	% One missionary crosses right to left.
	MR2 is MR-1,
	ML2 is ML+1,
	legal(CL,ML2,CR,MR2).

move(board(CL,ML,left,CR,MR),board(CL,ML2,right,CR,MR2)):-
	% One missionary crosses left to right.
	MR2 is MR+1,
	ML2 is ML-1,
	legal(CL,ML2,CR,MR2).

move(board(CL,ML,left,CR,MR),board(CL2,ML,right,CR2,MR)):-
	% One cannibal crosses left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	legal(CL2,ML,CR2,MR).

move(board(CL,ML,right,CR,MR),board(CL2,ML,left,CR2,MR)):-
	% One cannibal crosses right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	legal(CL2,ML,CR2,MR).

move(board(CL,ML,right,CR,MR),board(CL2,ML2,left,CR2,MR2)):-
	%  One missionary and one cannibal cross right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	MR2 is MR-1,
	ML2 is ML+1,
	legal(CL2,ML2,CR2,MR2).

move(board(CL,ML,left,CR,MR),board(CL2,ML2,right,CR2,MR2)):-
	%  One missionary and one cannibal cross left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	MR2 is MR+1,
	ML2 is ML-1,
	legal(CL2,ML2,CR2,MR2).

% Recursive call to solve the problem
path(board(CL1,ML1,B1,CR1,MR1),board(CL2,ML2,B2,CR2,MR2),Explored,MovesList) :-
   % Move to new state.
   move(board(CL1,ML1,B1,CR1,MR1),board(CL3,ML3,B3,CR3,MR3)), 
   % Verify it hasn't been there yet.
   not(member(board(CL3,ML3,B3,CR3,MR3),Explored)),
   % Reboot.
   path(board(CL3,ML3,B3,CR3,MR3),board(CL2,ML2,B2,CR2,MR2),[board(CL3,ML3,B3,CR3,MR3)|Explored],
    [ [ board(CL3,ML3,B3,CR3,MR3), board(CL1,ML1,B1,CR1,MR1)] | MovesList ]).

% Solution found
path(board(CL,ML,B,CR,MR),board(CL,ML,B,CR,MR),_,MovesList):- 
    % We call printing function.
	output(MovesList).

% Printing
output([]) :- nl. % First call.
output([[A,B]|MovesList]) :-
    % We choose current state and previous state.
	output(MovesList), 
    % We write the previous state first, then the current state.
   	write(B), write(' -> '), write(A), nl.

% Find the solution for the missionaries and cannibals problem
do_test :- 
   path(board(0,0,right,3,3),board(3,3,left,0,0),[board(0,0,right,3,3)],_).
