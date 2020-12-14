MODULE socks1 t11 t12 t21 t22 t31 t32 t41 t42.

% This program plays "mahjongg" on a board of the following shape
%
%	                +----------+
%	+----------+----|t41       |-----+
%	|t11       |t21 |          | t31 |
%	|          |    |          |     |
%	|          |    |          |     |
%	|          |    +----------+     |
%	+----------+----|t42       |-----+
%	|t12       |t22 |          | t32 |
%	|          |    |          |     |
%	|          |    |          |     |
%	|          |    +----------+     |
%	+----------+----------+----------+
%
% Tiles are of four types named like the seasons (the player can actually
% chose the name he or she wants.  There are two tiles of each type.

LINEAR  in t11                      & (in t11 :-          out t21).
LINEAR  in t12                      & (in t12 :-          out t22).
LINEAR (in t21 :- out t41, out t11) & (in t21 :- out t41, out t31).
LINEAR (in t22 :- out t42, out t12) & (in t22 :- out t42, out t32).
LINEAR (in t31 :-          out t21) &  in t31                     .
LINEAR (in t32 :-          out t22) &  in t32                     .
LINEAR	in t41.
LINEAR  in t42.

hook :-						% Simply displays the board
	nl, write_sans "Initial configuration:", nl,
	write_sans "		", write t41, nl,
	write t11, write_sans "	", write t21,
		   write_sans "	", write t31, nl,
	write_sans "		", write t42, nl,
	write t12, write_sans "	", write t22,
		   write_sans "	", write t32, nl, nl.

