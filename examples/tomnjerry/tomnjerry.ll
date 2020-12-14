MODULE tomnjerry.

LINEAR sq (s (s (s (s (s (s (s (s z)))))))) z z.
LINEAR sq (s (s (s (s (s (s (s z))))))) (s z) z.
LINEAR sq (s (s (s (s (s (s z)))))) z (s z).
LINEAR sq (s (s (s (s (s z))))) (s (s z)) z.
LINEAR sq (s (s (s (s z)))) (s z) (s z).
LINEAR sq (s (s (s z))) z (s (s z)).
LINEAR sq (s (s z)) (s (s z)) (s z).
LINEAR sq (s z) (s z) (s (s z)).
LINEAR sq z (s (s z)) (s (s z)).

LINEAR card c1 r p l w.
LINEAR card c2 l w r p.
LINEAR card c3 l w r w.
LINEAR card c4 r w p l.
LINEAR card c5 l w r p.
LINEAR card c6 l p w r.
LINEAR card c7 p w l r.
LINEAR card c8 l p r w.
LINEAR card c9 r p l p.

check z z S W.
check z (s Y) S W :- place z Y S E.
check (s X) z S W :- place X z N W.
check (s X) (s Y) S W :- place X (s Y) N W , place (s X) Y S E.

solve (s I) (next C R) 
      :- sq I X Y , card C S W N E ,
	 {check X Y S W} ,
	 (place X Y N E => solve I R).

solve z done.
