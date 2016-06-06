% -*- Mode: Prolog -*-
% $Id: KRAPqueen.pl,v 1.1 2004/05/31 19:47:25 mtjspaan Exp $

% King and Queen vs king in Advice Language 0

% all rules

edge_rule :: if their_king_edge and kings_close
	then [ mate_in_1 ].

edge_rule :: if their_king_edge and kings_close
	then [ mate_in_2, squeeze, approach, keeproom, divide_in_2, divide_in_3 ].

else_rule :: if true
	then [ squeeze, approach, keeproom, divide_in_2, divide_in_3 ].

% pieces of advice
% structure:
% advice( NAME, BETTERGOAL, HOLDINGGOAL: USMOVECONSTRAINT: 
%		THEMMOVECONSTRAINT

advice( mate_in_1, 
	mate :
	not queenlost and their_king_edge :
	( depth = 0 ) and checkmove:
	nomove ).

advice( mate_in_2, 
	mate :
	not queenlost and their_king_edge :
	( depth = 0 ) and legal then ( depth = 2 ) and checkmove:
	( depth = 1 ) and legal ).

%
	
advice( squeeze, 
	newroomsmaller and not queenexposed and queendivides and not stalemate and their_king_edge and moves_less :
	not queenlost:
	( depth = 0 ) and legal then ( depth = 2 ) and queenmove:
	( depth = 1 ) and legal and moves_less
	% and their_king_edge
	).
	
	%( depth = 0 ) and queenmove:
	%nomove ).

moves_less(Pos,_) :-
	legalmove(Pos,Move,Pos1),
	(not(their_king_edge(Pos1,_)),!,fail);
	fail
	;
	true.

advice( squeeze, 
	newroomsmaller and not queenexposed and queendivides and not stalemate :
	not queenlost :
	( depth = 0 ) and queenmove:
	nomove ).
	
advice( approach, 
okapproachedsquare and not queenexposed and not stalemate and (queendivides or lpatt) and (roomgt2 or not our_king_edge):
	not queenlost:
	( depth = 0 ) and kingdiagfirst:
	nomove ).

advice( keeproom, 
themtomove and not queenexposed and not stalemate and queendivides and okorndle and (roomgt2 or not our_king_edge):
	not queenlost:
	( depth = 0 ) and kingdiagfirst:
	nomove ).

advice( divide_in_2, 
themtomove and queendivides and not queenexposed and not stalemate :
	not queenlost:
	( depth < 3 ) and legal:
	( depth < 2 ) and legal ).

advice( divide_in_3, 
themtomove and queendivides and not queenexposed and not stalemate :
	not queenlost:
	( depth < 5 ) and legal:
	( depth < 4 ) and legal ).


