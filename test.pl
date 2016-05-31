% Search, Navigate & Actuate
% Assignment Tic-Tac-Toe
% 30-5-2016
% Jim Badrie 10465553 & Fabian
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
% Tic-Tac-Toe	                                   %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic
	board/1.

play_tic_tac_toe:-
	asserta(board([-,-,-,-,-,-,-,-,-])),
	play_turn.


play_turn:-
	board(Pos),
	pretty_print,
	nl,
	write('Your turn: '),
	read(Turn),
	X is Turn-1,
	ins(o, Pos, X, NewPos),
	retract(board(Pos)),
	asserta(board(NewPos)),
	check_winner.

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
	play_turn.



computer_turn:-
	board(Pos),
	alphabeta(Pos, -50, 50, NewPos, _),
	retract(board(Pos)),
	asserta(board(NewPos)),
	check_winner.


outcome(Val):-
	Val = +1,
	pretty_print,
	nl,
	write('You lose.').

outcome(Val):-
	Val = -1,
	pretty_print,
	nl,
	write('You win.').

outcome(Val):-
	Val = 0,
	pretty_print,
	nl,
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


% get_moves/3 takes

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




ins(Element, List, Nth, Result):-
	N is Nth +1,
	length([_| T], N),
	append(T, [_| R], List),
	append(T, [Element| R], Result).

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

countO( [], 0).

countO([o | Tail], O):-
	countO(Tail, NewO),
	O is NewO + 1.

countO([Head | Tail], O):-
	Head \= 'o',
	countO(Tail, O).

countX( [], 0).

countX([x | Tail], X):-
	countX(Tail, NewX),
	X is NewX + 1.

countX([Head | Tail], X):-
	Head \= 'x',
	countX(Tail, X).

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
pretty_print:-
	write('The current board:'),
	nl,
	board([A,B,C,D,E,F,G,H,I]),
	write('_____'), nl,
	write('|' ), write(A), write(B), write(C), write('|'), nl,
	write('|' ), write(D), write(E), write(F), write('|'), nl,
	write('|' ), write(G), write(H), write(I), write('|'), nl,
	write('-----'), nl.







alphabeta( Pos, Alpha, Beta, GoodPos, Val)  :-
	moves( Pos, PosList), !,
	boundedbest( PosList, Alpha, Beta, GoodPos, Val);
	staticval( Pos, Val).                              % Static value of Pos

boundedbest( [Pos | PosList], Alpha, Beta, GoodPos, GoodVal)  :-
	alphabeta( Pos, Alpha, Beta, _, Val),
	goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal).

goodenough( [], _, _, Pos, Val, Pos, Val)  :-  !.	   % No other candidate

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val)  :-
	min_to_move( Pos),
	Val > Beta, !;                               % Maximizer attained upper bound
	max_to_move( Pos), Val < Alpha, !.                 % Minimizer attained lower bound

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal)  :-
	newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),    % Refine bounds
	boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1),
	betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds( Alpha, Beta, Pos, Val, Val, Beta)  :-
	min_to_move( Pos), Val > Alpha, !.                 % Maximizer increased lower bound

newbounds( Alpha, Beta, Pos, Val, Alpha, Val)  :-
	max_to_move( Pos), Val < Beta, !.                 % Minimizer decreased upper bound

newbounds( Alpha, Beta, _, _, Alpha, Beta).          % Otherwise bounds unchanged

betterof( Pos, Val, _, Val1, Pos, Val)  :-        % Pos better than Pos1
	min_to_move( Pos), Val > Val1, !;
	max_to_move( Pos), Val < Val1, !.

betterof( _, _, Pos1, Val1, Pos1, Val1).             % Otherwise Pos1 better







