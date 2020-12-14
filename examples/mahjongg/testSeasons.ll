MODULE testSeasons.

% Succeeds
%
test1 :-
	(seasons (tile b 1) (tile d 1) (tile a 1) (tile c 1)
		 (tile a 2) (tile b 2) (tile c 2) (tile d 2))
	--o mahjongg
	--o play.

% fails
%
test2 :-
	(seasons (tile b 1) (tile d 1) (tile a 1) (tile d 2)
		 (tile a 2) (tile b 2) (tile c 2) (tile c 1))
	--o mahjongg
	--o play.
