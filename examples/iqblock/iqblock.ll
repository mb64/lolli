MODULE iqblock.

init X Y K :- (sq X Y -o next X Y K).
next z z K :- K.
next z (s Y) K :- init z Y K.
next (s X) z K :- init X (s (s (s (s (s (s (s z))))))) K.
next (s X) (s Y) K :- init (s X) Y K.

LINEAR
  tile blue1 (n::n::n::e::e::s::w::nil)
& tile blue2 (e::e::e::s::s::w::n::nil)
& tile blue3 (n::e::s::e::n::n::n::nil)
& tile blue4 (n::n::e::s::s::e::e::nil)
& tile blue5 (n::e::s::ne::s::s::s::nil)
& tile blue6 (e::e::e::n::n::w::s::nil)
& tile blue7 (e::e::n::w::w::n::n::nil)
& tile blue8 (e::n::w::n::e::e::e::nil).

LINEAR
  tile forest1 (n::n::n::e::e::nil)
& tile forest2 (e::e::e::s::s::nil)
& tile forest3 (e::e::n::n::n::nil)
& tile forest4 (n::n::sse::e::e::nil)
& tile forest5 (e::e::s::s::s::nil)
& tile forest6 (e::e::e::n::n::nil)
& tile forest7 (e::e::wwn::n::n::nil)
& tile forest8 (n::n::e::e::e::nil).

LINEAR
  tile tan1 (n::e::e::e::s::w::w::nil)
& tile tan2 (n::n::n::e::s::s::s::nil).

LINEAR
  tile green1 (e::e::s::s::e::e::nil)
& tile green2 (n::n::e::e::n::n::nil)
& tile green3 (e::e::n::n::e::e::nil)
& tile green4 (n::n::sse::e::s::s::nil).

LINEAR
  tile purple1 (n::e::s::e::e::nil)
& tile purple2 (n::n::n::e::s::nil)
& tile purple3 (e::e::e::s::w::nil)
& tile purple4 (e::n::w::ne::n::nil)
& tile purple5 (e::e::e::n::w::nil)
& tile purple6 (e::n::w::n::n::nil)
& tile purple7 (n::e::s::ne::e::nil)
& tile purple8 (n::e::s::s::s::nil).

LINEAR
  tile yellow1 (n::e::s::ne::nil)
& tile yellow2 (n::e::s::s::nil)
& tile yellow3 (e::e::n::w::nil)
& tile yellow4 (e::n::w::n::nil)
& tile yellow5 (e::e::s::w::nil)
& tile yellow6 (e::n::w::ne::nil)
& tile yellow7 (n::e::s::e::nil)
& tile yellow8 (n::n::e::s::nil).

LINEAR
  tile pink1 (e::e::n::n::nil)
& tile pink2 (n::n::sse::e::nil)
& tile pink3 (n::n::e::e::nil)
& tile pink4 (e::e::s::s::nil).

LINEAR
  tile orange1 (e::n::n::nil)
& tile orange2 (e::e::wwn::nil)
& tile orange3 (n::n::e::nil)
& tile orange4 (e::e::s::nil)
& tile orange5 (n::e::e::nil)
& tile orange6 (e::s::s::nil)
& tile orange7 (e::e::n::nil)
& tile orange8 (n::n::sse::nil).

LINEAR
  tile grass1 (n::e::n::e::s::s::w::nil)
& tile grass2 (n::n::e::s::e::s::w::nil)
& tile grass3 (n::n::e::e::s::w::s::nil)
& tile grass4 (n::e::e::s::s::w::n::nil).

LINEAR
  tile brown1 (n::e::s::s::s::s::nil)
& tile brown2 (e::e::e::e::n::w::nil)
& tile brown3 (e::n::w::n::n::n::nil)
& tile brown4 (n::e::s::ne::e::e::nil)
& tile brown5 (e::e::e::e::s::w::nil)
& tile brown6 (e::n::w::ne::n::n::nil)
& tile brown7 (n::e::s::e::e::e::nil)
& tile brown8 (n::n::n::n::e::s::nil).

fits (n :: Ds) X (s Y) K :- sq X Y , (filled X (s Y) => fits Ds X Y K).
fits (e :: Ds) (s X) Y K :- sq X Y , (filled (s X) Y => fits Ds X Y K).
fits (s :: Ds) X Y K :- sq X (s Y) , (filled X Y => fits Ds X (s Y) K).
fits (w :: Ds) X Y K :- sq (s X) Y , (filled X Y => fits Ds (s X) Y K).
fits (ne :: Ds) (s X) (s Y) K :-
   sq X Y , (filled (s X) (s Y) => fits Ds X Y K).
fits (sse :: Ds) (s X) Y :-
   sq X (s (s Y)) , (filled (s X) Y => fits Ds X (s (s Y))).
fits (wwn :: Ds) X (s Y) K :-
   sq (s (s X)) Y , (filled X (s Y) => fits Ds (s (s X)) Y).
fits nil X Y K :- (filled X Y => K).

findnext z z nil.
findnext z (s Y) L :- nextplace z Y L.
findnext (s X) z L :- nextplace X (s (s (s (s (s (s (s z))))))) L.
findnext (s X) (s Y) L :- nextplace (s X) Y L.

nextplace X Y L :- {filled X Y} , findnext X Y L.
nextplace X Y L :- sq X Y , place X Y L.

place X Y (C::L) :- 
      % write X , write Y , nl ,
      tile C Ds ,
      % write C , nl ,
      fits Ds X Y (findnext X Y L).

solve L :- init (s (s (s (s (s (s (s z))))))) (s (s (s (s (s (s (s z)))))))
           (nextplace (s (s (s (s (s (s (s z)))))))
	              (s (s (s (s (s (s (s z))))))) L).
