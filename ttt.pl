% Search, Navigate & Actuate
% Assignment Tic-Tac-Toe
% 30-5-2016
% Jim Badrie 10465553 & Fabian Reinshagen 10689214
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%		    Tic-Tac-Toe		                           %
%		Computer VS.Human		                       %
%		                                               %
% This is an implementation of the Tic-Tac-Toe game,   %
% using the AlphaBeta algorithm.	         	       %
% To start the game, use the query: play_tic_tac_toe/0.%
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The dynamic/1 predicate makes sure that prolog will
% not give an error, because our board/1 will be made
% while playing the game.

:- dynamic board/1.


% The predicate play_tic_tac_toe/0 initiates the
% Tic-Tac-Toe game. First it makes an empty board,
% which will keep track of the game. Our board will be
% a list with 9 elements representing, 'empty' spaces.
% After we have made our initial board, we will print
% basic rules in the terminal and then predicate human_turn/0
% is invoked, giving the first turn to the human player.

play_tic_tac_toe:-
	asserta(board([-,-,-,-,-,-,-,-,-])), nl,
	write('Lets play Tic-Tac-Toe!'), nl, nl,
	write('Rules:'), nl,
	write('You will be player O.'), nl,
	write('Give an integer between 1 and 9 for your move'), nl,
	write('
	       1 = upper left
	       2 = upper middle
	       3 = upper right
	       4 = center left
	       5 = center middle
	       6 = center right
	       7 = bottom left
	       8 = bottom middle
	       9 = bottom right
	       '), nl,
	human_turn.


% The predicate human_turn/0, represent a turn for the human.
% We will show the human player the current board
% by printing the board in the terminal. This is done by the
% predicate print_board/0.
% An input move from the user is necessary, this is handled by
% predicate user_input/1. This give us an integer, representing
% the move of the player. ins/4 predicate will take the input
% from the user and transforms the current board, into a new one
% by inserting an o into the designated place at our board. This
% 'new' board is put into our memory by asserta/1 and the 'old'
% board is deleted, by using retract/1.
% After each turn, we will check if somebody has won the game.

human_turn:-
	board(Pos),
	print_board,
	nl,
	user_input(Turn),
	ins(o, Pos, Turn, NewPos),
	retract(board(Pos)),
	asserta(board(NewPos)),
	check_winner.


% check_winner/0 can check if somebody has won, by calling upon the
% current board and checking whether the staticvalue of the current
% board, gives us a +1, -1 or a 0. If this is the case, outcome/1 will
% be invoked, marking the end of our game. The board will also be
% deleted, when the game has ended.
% If there is not yet a winner, then either the human or the computer is
% at turn.

check_winner:-
	board(Pos),
	staticval(Pos, Val),
	outcome(Val),
	retract(board(_)).

check_winner:-
	board(Pos),
	max_to_move(Pos),
	computer_turn.

check_winner:-
	board(Pos),
	min_to_move(Pos),
	human_turn.


% computer_turn/0 is almost identical to human_turn, except we dont need
% input from the user. We invoke the alphabeta/5 predicate and this will
% give us a follow-up turn.

computer_turn:-
	board(Pos),
	alphabeta(Pos, -50, 50, NewPos, _),
	retract(board(Pos)),
	asserta(board(NewPos)),
	check_winner.


% outcome/1 predicate is invoked when it is the end of the game. This
% means there is either a winner/loser or a draw. The argument it takes
% in determines the result and prints the endgame in the terminal.

outcome(Val):-
	Val = +1,
	print_board,
	nl,
	write('You lose.').

outcome(Val):-
	Val = -1,
	print_board,
	nl,
	write('You win.').

outcome(Val):-
	Val = 0,
	print_board,
	nl,
	write('Its a draw.').


% user_input/1 checks if the input from the user is an integer ranging
% from 1 and 9. It will continue to ask an input from the user, until
% the asked input is given.

user_input(X):-
	write('Your Turn: '),
	read(X),
	integer(X),
	X > 0,
	X < 10.

user_input(X):-
	write('Not a valid Turn.'),
	nl,
	user_input(X).


% move/2 Predicate takes a board configuration for the tic-tac-toe game
% and returns a list of all possible follow-up board configurations. In
% other words, the possible follow-up moves. A possible follow-up move
% is only possible when there are empty spaces in our board, so we check
% this with the member/2 predicate.
% plyer_turn is invoked by checking which player is at turn, this
% will give back a symbol (x or o), which we in turn will use to
% make follow-up moves. Now the predicate get_moves/4 is invoked, with
% as argument the current grid, the symbol of the player at turn and a
% number zero.

moves(Pos, PosList):-
	member(-, Pos),
	player_turn(Pos, Symbol),
	get_moves(Pos, PosList, 1, Symbol).


% get_moves/3 is a recursive predicate which iterates through our
% current board. The predicate has 2 base cases.
% 1 in which case the last element of our board is non-empty and another
% in which it is an empty space.
% Do we come across a space that is empty in our, then we can make a new
% possible follow-up grid, by invoking the predicate ins/4.
% Don't we come across an empty space, then we will check for the next
% element in our grid.

get_moves(Pos, [List], 9, Symbol):-
        nth0(8, Pos, -),
	ins(Symbol, Pos, 9, List).

get_moves(_, [], 9, _).

get_moves(Pos, [Head| Tail], Number, Symbol):-
	nth1(Number, Pos, -),
	ins(Symbol, Pos, Number, Head),
	NewNumber is Number+1,
	get_moves(Pos, Tail, NewNumber, Symbol).

get_moves(Pos, PosList, Number, Symbol):-
	NewNumber is Number+1,
	get_moves(Pos, PosList, NewNumber, Symbol).


% ins/4 will give back a list as output with the nth element of the list
% that has been given as input replaced by the symbol.

ins(Symbol, List, N, Result):-
	length([_| T], N),
	append(T, [_| R], List),
	append(T, [Symbol| R], Result).


% player_turn/2 checks which player is at turn and give back its
% corresponding symbol.

player_turn(Pos, Symbol):-
	max_to_move(Pos),
	Symbol = x.

player_turn(Pos, Symbol):-
	min_to_move(Pos),
	Symbol = o.


% min_to_move/1 checks if the human is at turn.

min_to_move(Pos):-
	countO(Pos, O),
	countX(Pos, X),
	O =< X.

% max_to_move/1 checks if the computer is at turn.

max_to_move(Pos):-
	countO(Pos, O),
	countX(Pos, X),
	X < O.


% countO/2 checks the occurrences of the amount of o in the current
% board.

countO( [], 0).

countO([o | Tail], O):-
	countO(Tail, NewO),
	O is NewO + 1.

countO([Head | Tail], O):-
	Head \= 'o',
	countO(Tail, O).


% countX/2 checks the occurrences of the amount of x in the current
% board.

countX( [], 0).

countX([x | Tail], X):-
	countX(Tail, NewX),
	X is NewX + 1.

countX([Head | Tail], X):-
	Head \= 'x',
	countX(Tail, X).


% Value of each terminal node.

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

staticval(Pos, 0):-
	\+ moves(Pos, _).


% This prints out the board in a nice readable format.

print_board:-
	write('The current board:'),
	nl,
	board([A,B,C,D,E,F,G,H,I]),
	write('_____'), nl,
	write('|' ), write(A), write(B), write(C), write('|'), nl,
	write('|' ), write(D), write(E), write(F), write('|'), nl,
	write('|' ), write(G), write(H), write(I), write('|'), nl,
	write('-----'), nl.


% alphabeta/5 predicate, this is the alphabeta algorithm.

alphabeta( Pos, Alpha, Beta, GoodPos, Val)  :-
	moves( Pos, PosList), !,
	boundedbest( PosList, Alpha, Beta, GoodPos, Val);
	staticval( Pos, Val).

boundedbest( [Pos | PosList], Alpha, Beta, GoodPos, GoodVal)  :-
	alphabeta( Pos, Alpha, Beta, _, Val),
	goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal).

goodenough( [], _, _, Pos, Val, Pos, Val)  :-  !.

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val)  :-
	min_to_move( Pos),
	Val > Beta, !;
	max_to_move( Pos), Val < Alpha, !.

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal)  :-
	newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),
	boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1),
	betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds( Alpha, Beta, Pos, Val, Val, Beta)  :-
	min_to_move( Pos), Val > Alpha, !.

newbounds( Alpha, Beta, Pos, Val, Alpha, Val)  :-
	max_to_move( Pos), Val < Beta, !.

newbounds( Alpha, Beta, _, _, Alpha, Beta).

betterof( Pos, Val, _, Val1, Pos, Val)  :-
	min_to_move( Pos), Val > Val1, !;
	max_to_move( Pos), Val < Val1, !.

betterof( _, _, Pos1, Val1, Pos1, Val1).


