% Tic tac toe
:- dynamic
	board/1.

% Setup of the game board.
play_tic_tac_toe():-
	asserta(board([-,-,-,-,-,-,-,-,-])),
	play_turn().

% Main game loop
play_turn():-
	board(Pos),
	min_to_move(Pos),
	write('Your turn: '),
	read(Turn),
	X is Turn-1,
	ins(o, Pos, X, NewPos),
	retract(board(Pos)),
	asserta(board(NewPos)),
	max_to_move(NewPos),
	minimax(NewPos, NewPos1, _),
	retract(board(NewPos)),
	asserta(board(NewPos1)),
	play_turn().


play_turn():-
	board(Pos),
	\+ moves(Pos, _),
	staticval(Pos, Val),
	outcome(Val).


outcome(Val):-
	Val = +1,
	write('You lose.').

outcome(Val):-
	Val = -1,
	write('You win.').

outcome(Val):-
	Val = 0,
	write('Its a draw.').









% The move/2 predicate which takes a board
% configuration for the tic-tac-toe game and
% returns a list of all possible follow-up
% board configurations.
% In other words, the possible follow-up moves.
% This is done by invoking the predicate get_moves/3
moves(Pos, PosList):-
	member(-, Pos),
	player_turn(Pos, Symbol),
	get_moves(Pos, PosList, 0, Symbol).

% Makes a list of all possible follow up moves
get_moves(Pos, [List], 8, Symbol):-
    nth0(8, Pos, -),
	ins(Symbol, Pos, 8, List).

get_moves(_, [], 8, _).

get_moves(Pos, [Head| Tail], Number, Symbol):-
	nth0(Number, Pos, -),
	ins(Symbol, Pos, Number, Head),
	NewNumber is Number+1,
	get_moves(Pos, Tail, NewNumber, Symbol).

get_moves(Pos, PosList, Number, Symbol):-
	NewNumber is Number+1,
	get_moves(Pos, PosList, NewNumber, Symbol).

% Replaces an element in the list with a different one.
ins(Element, List, Nth, Result):-
	N is Nth +1,
	length([_| T], N),
	append(T, [_| R], List),
	append(T, [Element| R], Result).

% Checks whose turn it is for the current player.
player_turn(Pos, Symbol):-
	max_to_move(Pos),
	Symbol = x.

player_turn(Pos, Symbol):-
	min_to_move(Pos),
	Symbol = o.

min_to_move(Pos):-
	countO(Pos, O),
	countX(Pos, X),
	O =< X.

max_to_move(Pos):-
	countO(Pos, O),
	countX(Pos, X),
	X < O.

% Counts the amount of o's currently on the field
countO( [], 0).

countO([o | Tail], O):-
	countO(Tail, NewO),
	O is NewO + 1.

countO([Head | Tail], O):-
	Head \= 'o',
	countO(Tail, O).

%counts the amount of x's currently on the field
countX( [], 0).

countX([x | Tail], X):-
	countX(Tail, NewX),
	X is NewX + 1.

countX([Head | Tail], X):-
	Head \= 'x',
	countX(Tail, X).

% Gives a value for each ending position.
staticval(Pos, +1):-
	Pos = [x,x,x,_,_,_,_,_,_];
	Pos = [_,_,_,x,x,x,_,_,_];
	Pos = [_,_,_,_,_,_,x,x,x];
	Pos = [x,_,_,x,_,_,x,_,_];
	Pos = [_,x,_,_,x,_,_,x,_];
	Pos = [_,_,x,_,_,x,_,_,x];
	Pos = [x,_,_,_,x,_,_,_,x];
	Pos = [_,_,x,_,x,_,x,_,_].

staticval(Pos, -1):-
	Pos = [o,o,o,_,_,_,_,_,_];
	Pos = [_,_,_,o,o,o,_,_,_];
	Pos = [_,_,_,_,_,_,o,o,o];
	Pos = [o,_,_,o,_,_,o,_,_];
	Pos = [_,o,_,_,o,_,_,o,_];
	Pos = [_,_,o,_,_,o,_,_,o];
	Pos = [o,_,_,_,o,_,_,_,o];
	Pos = [_,_,o,_,o,_,o,_,_].

staticval(_, 0).
