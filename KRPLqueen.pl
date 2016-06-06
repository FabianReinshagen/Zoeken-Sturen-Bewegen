% -*- Mode: Prolog -*-

% Position is represented by Side..Wx : Wy..Qx : Qy .. Bx : By .. Depth
% Side is side to move next ( us or them )
% Wx, Wy are X and Y coordinates of the white king
% Qx, Qy are X and Y coordinates of the white queen
% Bx, By are the X and Y coordinates of the black king
% depth is depth of position in the search tree

mode(queen).

% call the general original move predicates for king moves etc.
move(A,B,C,D):-
        moveGeneral(A,B,C,D).

move( queenmove, us..W..Qx : Qy..B..D, Qx:Qy - Q, them..W..Q..B..D1 ):-
	D1 is D + 1,
	coord( I ),		% integer between 1 and 8
	% move horizontally or vertically or diagonally
	(
                diag(Qx:Qy,X1:Y1,I),
                Q = X1 : Y1
        ;
		Q = Qx : I
	;
		Q = I : Qy
        ),
	Q \== Qx : Qy,	% Must have moved
	not inway( Qx : Qy, W, Q ),	% white king not in way
	not inway( Qx : Qy, B, Q ).	% black king not in way

move( checkmove, Pos, Qx : Qy - Qx1 : Qy1, Pos1 ):-
	wk( Pos, W ),	% white king position
	wq( Pos, Qx : Qy ),		% white queen position
	bk( Pos, Bx : By ),	% black king position
	% place black king and white queen on line
	(
		Qx1 = Bx,
		Qy1 = Qy
	;
		Qx1 = Qx,
		Qy1 = By
        ;
                diag(Qx1:Qy1,Bx:By)
	),
	% not the white king between the rook and black king
	not inway( Qx1 : Qy, W, Bx : By ),
	move( queenmove, Pos, Qx : Qy - Qx1 : Qy1, Pos1 ).

move( legal, us..P, M, P1 ) :-
	(
		MC = kingdiagfirst
	;
		MC = queenmove
	),
	move( MC, us..P, M, P1 ).

queenexposed( Side..W..Q..B.._D, _ ) :-
	dist( W, Q, D1 ),
	dist( B, Q, D2 ),
	(
		Side = us, !,
		D1 > D2 + 1
	;
		Side = them, !,
		D1 > D2
	).

%new:
queendivides( _Side..Wx : Wy..Qx : Qy..Bx : By.._D, _ ) :-
	ordered( Wx, Qx, Bx ), !;
	ordered( Wy, Qy, By ).

%Queen divides diagonally
queendivides( _Side..Wx : Wy..Qx : Qy..Bx : By.._D, _ ) :-
	ordered( Wx, Qx, Bx ),
	ordered( Wy, Qy, By ).

queenlost( _.._W..B..B.._ ,_).	% queen has been captured

queenlost( them..W..Q..B.._ ,_) :-
	ngb( B, Q ),	% black king attacks queen
	not ngb( W, Q ).	% white king does not defend

empty_goal(_,_).

did_not_move_queen(Pos,Pos):-
        wq(Pos,Q0),
        wq(Pos,Q1),
        Q0==Q1.
