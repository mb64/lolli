MODULE tomnjerry2.

LINEAR sq (s (s (s (s (s (s (s (s z)))))))) z z.
LINEAR sq (s (s (s (s (s (s (s z))))))) (s z) z.
LINEAR sq (s (s (s (s (s (s z)))))) z (s z).
LINEAR sq (s (s (s (s (s z))))) (s (s z)) z.
LINEAR sq (s (s (s (s z)))) (s z) (s z).
LINEAR sq (s (s (s z))) z (s (s z)).
LINEAR sq (s (s z)) (s (s z)) (s z).
LINEAR sq (s z) (s z) (s (s z)).
LINEAR sq z (s (s z)) (s (s z)).

LINEAR card c1 (top r) (bot p) (bot l) (top w).
LINEAR card c2 (top l) (bot w) (bot r) (top p).
LINEAR card c3 (top l) (bot w) (bot r) (top w).
LINEAR card c4 (top r) (bot w) (bot p) (top l).
LINEAR card c5 (top l) (bot w) (bot r) (top p).
LINEAR card c6 (top l) (bot p) (bot w) (top r).
LINEAR card c7 (top p) (bot w) (bot l) (top r).
LINEAR card c8 (top l) (bot p) (bot r) (top w).
LINEAR card c9 (top r) (bot p) (bot l) (top p).

fit (top U) (bot U).
fit (bot U) (top U).

rotate S W N E S W N E.
rotate S W N E E S W N.
rotate S W N E N E S W.
rotate S W N E W N E S.

check z z _S _W.
check z (s Y) S _W :- place z Y N _E , fit S N.
check (s X) z _S W :- place X z _N E , fit W E.
check (s X) (s Y) S W :- place X (s Y) _N E , fit W E ,
		         place (s X) Y N _E , fit S N.

solve (s I) (next C R)
      :- sq I X Y , card C S W N E , rotate S W N E S1 W1 N1 E1 ,
	 {check X Y S1 W1} ,
	 (place X Y N1 E1 => solve I R).

solve z done.
