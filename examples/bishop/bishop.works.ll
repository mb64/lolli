MODULE bishop.

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


move MoveColor StayColor :- 
  board X1 Y1 MoveColor, board X1 Y2 free, 
  board _ Y2 StayColor -> fail
  | (board X1 Y2 MoveColor -o board X1 Y1 free  -o printboard).

move MoveColor StayColor :- 
  board X1 Y1 MoveColor, board X2 Y1 free,
  board X2 _ StayColor -> fail
  | (board X2 Y1 MoveColor -o board X1 Y1 free  -o printboard).

move :- move black white.
move :- move white black.

checkclose :-
 board             z     (s (s z)) black,
 board             z         (s z) free,
 board         (s z) (s (s (s z))) white,
 board         (s z)     (s (s z)) free,
 board         (s z)         (s z) free,
 board         (s z)             z free,
 board     (s (s z))     (s (s z)) free,
 board     (s (s z))         (s z) free,
 board     (s (s z))             z white,
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

printboard :- 
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

list :- board X Y Z, 
     write_sans "(", write_sans X, write_sans ") (",write_sans  Y, write_sans ") : ",write_sans Z, nl,
     list.
list.


init :- position z (s (s z)) black.
init :- position (s (s (s z))) (s z) white.
init :- position  (s (s z)) z white.
init :- position (s z) (s (s (s z)))  black.

black :- move black white.
white :- move white black.