MODULE bishop.

% Carsten Schuermann

% The Bishop game: Start it with run.
%
% Given is a 4 times 5 board. Each field on the shorter left side 
% of the board is occupied with a black bishop, every field on 
% the right is occupied with a white bishop. 

% The goal of the puzzle is it to exchange the white and the 
% black bishops. A bishop moves according to the chess rules,
% always diagonal. Note, that the bishops are never allowed
% to threaten each other.

%   ___________________________________________________________________
%  |            |............|             |.............|             |   
%  |    BBBB    |............|             |.............|    WWWW     |   
%  |     BB     |............|             |.............|     WW      |   
%  |     BB     |............|             |.............|     WW      |   
%  |   BBBBBB   |............|             |.............|   WWWWWW    |   
%  |____________|____________|_____________|_____________|_____________|
%  |............|            |.............|             |.............|   
%  |....BBBB....|            |.............|             |....WWWW.....|   
%  |.....BB.....|            |.............|             |.....WW......|   
%  |.....BB.....|            |.............|             |.....WW......|   
%  |...BBBBBB...|            |.............|             |...WWWWWW....|   
%  |____________|____________|_____________|_____________|_____________|
%  |            |............|             |.............|             |   
%  |    BBBB    |............|             |.............|    WWWW     |   
%  |     BB     |............|             |.............|     WW      |   
%  |     BB     |............|             |.............|     WW      |   
%  |   BBBBBB   |............|             |.............|   WWWWWW    |   
%  |____________|____________|_____________|_____________|_____________|
%  |............|            |.............|             |.............|   
%  |....BBBB....|            |.............|             |....WWWW.....|   
%  |.....BB.....|            |.............|             |.....WW......|   
%  |.....BB.....|            |.............|             |.....WW......|   
%  |...BBBBBB...|            |.............|             |...WWWWWW....|   
%  |____________|____________|_____________|_____________|_____________|

% Solution: 
% We restrict the board to white fields only. No bishop can move from a 
% black field to a white field. These ten fields have to capture the
% lines along which the bishops can move. Every field is labeled by
% two coordinates : the frist coordinate represents the diagonals bottom left
% to top right direction, the second bottom right to top left.

%   ____________              _____________               _____________
%  |       (1,3)|            |        (2,2)|             |        (3,1)|   
%  |    BBBB    |            |             |             |    WWWW     |   
%  |     BB     |            |             |             |     WW      |   
%  |     BB     |            |             |             |     WW      |   
%  |   BBBBBB   |            |             |             |   WWWWWW    |   
%  |____________|____________|_____________|_____________|_____________|
%               |       (1,2)|             |        (2,1)|                 
%               |            |             |             |                 
%               |            |             |             |                 
%               |            |             |             |                 
%               |            |             |             |                 
%   ____________|____________|_____________|_____________|_____________ 
%  |       (0,2)|            |        (1,1)|             |        (2,0)|   
%  |    BBBB    |            |             |             |    WWWW     |   
%  |     BB     |            |             |             |     WW      |   
%  |     BB     |            |             |             |     WW      |   
%  |   BBBBBB   |            |             |             |   WWWWWW    |   
%  |____________|____________|_____________|_____________|_____________|
%               |       (0,1)|             |        (1,0)|                 
%               |            |             |             |                 
%               |            |             |             |                 
%               |            |             |             |                 
%               |            |             |             |                 
%               |____________|             |_____________|             

% This definition can be represented by the following picture:
%
%   Y    _   
% 3 |  _|_|_ 
% 2 | |_|_|_|_
% 1 | |_|_|_|_|
% 0 |   |_|_|
%   +--------------- X
%      0 1 2 3

% The board is represented in the linear context as a set of ten assumptions.
% Every position is assigned with a label, if it is currently occupied
% by a black figure, white figure, or if it is free:
   

LINEAR board             z     (s (s z)) black.
LINEAR board             z         (s z) free.

LINEAR board         (s z) (s (s (s z))) black.
LINEAR board         (s z)     (s (s z)) free.
LINEAR board         (s z)         (s z) free.
LINEAR board         (s z)             z free.

LINEAR board     (s (s z))     (s (s z)) free.
LINEAR board     (s (s z))         (s z) free.
LINEAR board     (s (s z))             z white.

LINEAR board (s (s (s z)))         (s z) white.

% A figure can move on the diagonal. We define two judgments, one for 
% the movement in X, one for the movement in Y direction.
% Since bishops are not supposed to to threaten each other, 
% it must be checked, if a figure of the opposite color is
% occupying a field on the diagonal, the figure is supposed to move.

move MoveColor StayColor :- 
  board X1 Y1 MoveColor, board X1 Y2 free, 
  board _ Y2 StayColor -> fail
  | (board X1 Y2 MoveColor -o board X1 Y1 free  -o run).

move MoveColor StayColor :- 
  board X1 Y1 MoveColor, board X2 Y1 free,
  board X2 _ StayColor -> fail
  | (board X2 Y1 MoveColor -o board X1 Y1 free  -o run).

move :- move black white.
move :- move white black.

% The goal  situation looks as follows. If the goal situation is not 
% yet reached, then the actual boardposition may have been visited
% earlier. In this case the actual search branch should not be 
% persued any further. If it hasen't the actual state has to
% be put in the linear or intuitionistic context, for future steps
% to recognize loops.

checkclose :-
 board             z     (s (s z)) white,
 board             z         (s z) free,
 board         (s z) (s (s (s z))) white,
 board         (s z)     (s (s z)) free,
 board         (s z)         (s z) free,
 board         (s z)             z free,
 board     (s (s z))     (s (s z)) free,
 board     (s (s z))         (s z) free,
 board     (s (s z))             z black,
 board (s (s (s z)))         (s z) black, erase.

checkclose :-
 board             z     (s (s z)) C1,
 board             z         (s z) C2,
 board         (s z) (s (s (s z))) C3,
 board         (s z)     (s (s z)) C4,
 board         (s z)         (s z) C5,
 board         (s z)             z C6,
 board     (s (s z))     (s (s z)) C7,
 board     (s (s z))         (s z) C8,
 board     (s (s z))             z C9,
 board (s (s (s z)))         (s z) C10,
 closed C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 -> fail
 | (board             z     (s (s z)) C1 -o
   board             z         (s z) C2 -o
   board         (s z) (s (s (s z))) C3 -o
   board         (s z)     (s (s z)) C4 -o
   board         (s z)         (s z) C5 -o
   board         (s z)             z C6 -o
   board     (s (s z))     (s (s z)) C7 -o
   board     (s (s z))         (s z) C8 -o
   board     (s (s z))             z C9 -o
   board (s (s (s z)))         (s z) C10 -o 
   closed C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 -o move).

output(black,"B").
output(white,"W").
output(free, ".").

run         :- 
 board             z     (s (s z)) C1,
 board             z         (s z) C2,
 board         (s z) (s (s (s z))) C3,
 board         (s z)     (s (s z)) C4,
 board         (s z)         (s z) C5,
 board         (s z)             z C6,
 board     (s (s z))     (s (s z)) C7,
 board     (s (s z))         (s z) C8,
 board     (s (s z))             z C9,
 board (s (s (s z)))         (s z) C10,
 board             z     (s (s z)) C1 -o
 board             z         (s z) C2 -o
 board         (s z) (s (s (s z))) C3 -o
 board         (s z)     (s (s z)) C4 -o
 board         (s z)         (s z) C5 -o
 board         (s z)             z C6 -o
 board     (s (s z))     (s (s z)) C7 -o
 board     (s (s z))         (s z) C8 -o
 board     (s (s z))             z C9 -o
 board (s (s (s z)))         (s z) C10 -o 
 (output (C1,D1),
 output (C2,D2),
 output (C3,D3),
 output (C4,D4),
 output (C5,D5),
 output (C6,D6),
 output (C7,D7),
 output (C8,D8),
 output (C9,D9),
 output (C10,D10),
 write_sans D3, write_sans " ",write_sans  D7, write_sans " ",write_sans D10, nl,
 write_sans " ",write_sans  D4, write_sans " ",write_sans D8, nl,
 write_sans D1, write_sans " ",write_sans  D5, write_sans " ",write_sans D9, nl,
 write_sans " ",write_sans  D2, write_sans " ",write_sans D6, nl,nl, checkclose).


