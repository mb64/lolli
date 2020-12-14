MODULE mahjongg.

% THE GAME:
%
% Mahjongg (also known as Taipei) is a game consisting of a number of square
% tiles.  Tiles are distinguished in types and there are several tiles of each
% type (preferably in even number).  At the beginning of the game, the tiles
% are arranged according to some initial configuration.
%
% A tile is covered if there is another tile on top of it, hiding it in
% totality or in part.  Two tiles are adjacent if their left or right side
% are in contact.  A tile is free if it is not covered and there is no adjacent
% tile either on its left or on its right.
%
% A move in the game consists in identifying two free tiles of the same type
% and removing this pair from the configuration.  The game is won if it is
% possible to match in this manner all tiles.

% ENCODING:
%
% Each tile is represented by the term
%	(tile Type Instance)
% where Type denotes the type of the tile, and Instance permits to distinguish
% different occurrences of this tile.
%
% Tile T being present in the current configuration is represented by the
% formula
%	in T
% if T has been removed, we use the formula
%	out T
% 
% The board is represented by one linear clause for each tile.  The clause
% represents the constraints on the freedom of the tile.  If tile T is covered
% by tiles T_top_1, ..., T_top_n (n = 4, at most), has T_left_1, ..., T_left_l
% as its left neighbors and T_right_1, ..., T_right_r as its right neighbors
% (r and l are at most 2), the constraints on T are represented by the clause
%	LINEAR   (in T :- out T_top_1,   ..., out T_top_n,
%                         out T_left_1,  ..., out T_left_l)
%              & (in T :- out T_top_1,   ..., out T_top_n,
%                         out T_right_1, ..., out T_right_r)
% we simplify this expression when some of the neighboring tiles are not
% present in the initial configuration.
%
% Removing a tile T from the board is achieved by solving the goal
%	in T
% and asserting in the intuitionistic context
%	out T
% This fact needs to be intuitionistic since T might constrain more than one
% other tile (and possibly zero).
% The goal
%	in T
% succeeds only if the corresponding constraint is solvable, i.e. if one of
% the two additive conjuncts succeeds.  This happens only if all the "out"
% statements in one of these it have been asserted in precedence.
%
% A tile is present on the board only if its constraint is in the linear
% context.  When the tile is removed, the constraint itself is consumed.

% EXECUTION
%
% In order to play the game, write the desired initial configuration in a
% separate module, load it and execute "play".  For example, if the initial
% configuration (or a program that generates it) has been encoded in the
% module "init", a sample execution will be
%	?- init --o play.
%
% It might be convenient to use parametric modules to try different initial
% configurations sharing the same layout.  In this case, the module "init"
% will accept the encoding of the tiles, T_1, ..., T_n as input, and the
% execution will be fired by
%	?- init T_1, ..., T_n --o play.
%
% "play" relies on the predicate "hook" to perform actions before starting
% the game, for example printing the initial configuration, and/or generating
% it.

play :-
	hook,
	match nil.

match Sol :-
	in (tile T N1), in (tile T N2),
	   out (tile T N1)
	=> out (tile T N2)
	=> match ((move (tile T N1) (tile T N2)) :: Sol).
match Sol <= printSol Sol.

printSol ((move T1 T2) :: Sol) :-
	printSol Sol,
	write T1, write_sans " <-> ", write T2, nl.
printSol nil.



% Modified version: adds simple heuristics and shows how many moves are left
%
% match M Sol :-
%	in (tile T N1),
%	in (tile T N2),
%	N1 > N2,		% Eliminates symmetric moves on backtracking
%	   out (tile T N1)
%	=> out (tile T N2)
%	=> (MM is M - 2,					% Debugging
%	    write_sans "	", write (tile T N1),		% Debugging
%	    write_sans " <=> ", write (tile T N2),		% Debugging
%	    write_sans "		", write MM,		% Debugging
%	    write_sans " tiles left.", nl,			% Debugging
%	    match MM ((move (tile T N1) (tile T N2)) :: Sol)).
% match M Sol <= printSol Sol.

