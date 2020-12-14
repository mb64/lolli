MODULE solitaire.

LINEAR b z z peg.
LINEAR b (s z) z peg.
LINEAR b (s z) (s z) peg.
LINEAR b (s (s z)) z peg.
LINEAR b (s (s z)) (s z) empty.
LINEAR b (s (s z)) (s (s z)) peg.
LINEAR b (s (s (s z))) z peg.
LINEAR b (s (s (s z))) (s z) peg.
LINEAR b (s (s (s z))) (s (s z)) peg.
LINEAR b (s (s (s z))) (s (s (s z))) peg.
LINEAR b (s (s (s (s z)))) z peg.
LINEAR b (s (s (s (s z)))) (s z) peg.
LINEAR b (s (s (s (s z)))) (s (s z)) peg.
LINEAR b (s (s (s (s z)))) (s (s (s z))) peg.
LINEAR b (s (s (s (s z)))) (s (s (s (s z)))) peg.

jump (s N) :-
          b X Y peg , b (s X) Y peg , b (s (s X)) Y empty ,
          ((b X Y empty , b (s X) Y empty , b (s (s X)) Y peg)
	   -o jump N).
jump (s N) :-
          b X Y peg , b (s X) (s Y) peg , b (s (s X)) (s (s Y)) empty ,
          ((b X Y empty , b (s X) (s Y) empty , b (s (s X)) (s (s Y)) peg)
	   -o jump N).
jump (s N) :- b X Y peg , b X (s Y) peg , b X (s (s Y)) empty ,
          ((b X Y empty , b X (s Y) empty , b X (s (s Y)) peg)
	   -o jump N).
jump (s N) :-
          b (s (s X)) Y peg , b (s X) Y peg , b X Y empty ,
	  ((b (s (s X)) Y empty , b (s X) Y empty , b X Y peg)
	   -o jump N).
jump (s N) :-
          b (s (s X)) (s (s Y)) peg , b (s X) (s Y) peg , b X Y empty ,
	  ((b (s (s X)) (s (s Y)) empty , b (s X) (s Y) empty , b X Y peg)
	   -o jump N).
jump (s N) :-
          b X (s (s Y)) peg , b X (s Y) peg , b X Y empty ,
          ((b X (s (s Y)) empty , b X (s Y) empty , b X Y peg)
	   -o jump N).
jump z :- erase.
