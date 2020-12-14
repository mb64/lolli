MODULE original
      t1b1 t1c1 t1d1 t1e1 t1f1 t1g1 t1h1 t1i1 t1j1 t1k1 t1l1 t1m1 
                t1d2 t1e2 t1f2 t1g2 t1h2 t1i2 t1j2 t1k2 
           t1c3 t1d3 t1e3 t1f3 t1g3 t1h3 t1i3 t1j3 t1k3 t1l3 
      t1b4 t1c4 t1d4 t1e4 t1f4 t1g4 t1h4 t1i4 t1j4 t1k4 t1l4 t1m4 
t1a45 t1b5 t1c5 t1d5 t1e5 t1f5 t1g5 t1h5 t1i5 t1j5 t1k5 t1l5 t1m5 t1n45 t1o45
           t1c6 t1d6 t1e6 t1f6 t1g6 t1h6 t1i6 t1j6 t1k6 t1l6 
                t1d7 t1e7 t1f7 t1g7 t1h7 t1i7 t1j7 t1k7 
      t1b8 t1c8 t1d8 t1e8 t1f8 t1g8 t1h8 t1i8 t1j8 t1k8 t1l8 t1m8

                     t2e2 t2f2 t2g2 t2h2 t2i2 t2j2 
                     t2e3 t2f3 t2g3 t2h3 t2i3 t2j3 
                     t2e4 t2f4 t2g4 t2h4 t2i4 t2j4 
                     t2e5 t2f5 t2g5 t2h5 t2i5 t2j5 
                     t2e6 t2f6 t2g6 t2h6 t2i6 t2j6 
                     t2e7 t2f7 t2g7 t2h7 t2i7 t2j7 

                          t3f3 t3g3 t3h3 t3i3 
                          t3f4 t3g4 t3h4 t3i4 
                          t3f5 t3g5 t3h5 t3i5 
                          t3f6 t3g6 t3h6 t3i6 

                               t4g4 t4h4 
                               t4g5 t4h5 

                                 t5gh45.


% In its original presentation, mahjongg consists of 144 tiles, 36 types with
% 4 instances each.  The standard initial configuration is represented below.
% It has 5 layers.  The overlapping points are shaded.  Numerous variations
% on the shape of the initial configuration typically are offered.
%
%     +---+---+---+---+---+---+---+---+---+---+---+---+       Level 1 (lowest)
%     |   |   |   |   |   |   |   |   |   |   |   |   |
%     |   |   |   |   |   |   |   |   |   |   |   |   |
%     +---+---+---+---+---+---+---+---+---+---+---+---+
%             |   |...|...|...|...|...|...|   |
%             |   |...|...|...|...|...|...|   |
%         +---+---+---+---+---+---+---+---+---+---+
%         |   |   |...|...|...|...|...|...|   |   |
%         |   |   |...|...|...|...|...|...|   |   |
%     +---+---+---+---+---+---+---+---+---+---+---+---+
%     |   |   |   |...|...|...|...|...|...|   |   |   |
% +---+   |   |   |...|...|...|...|...|...|   |   |   +---+---+
% |   +---+---+---+---+---+---+---+---+---+---+---+---+   |   |
% +---+   |   |   |...|...|...|...|...|...|   |   |   +---+---+
%     |   |   |   |...|...|...|...|...|...|   |   |   |
%     +---+---+---+---+---+---+---+---+---+---+---+---+
%         |   |   |...|...|...|...|...|...|   |   |
%         |   |   |...|...|...|...|...|...|   |   |
%         +---+---+---+---+---+---+---+---+---+---+
%             |   |...|...|...|...|...|...|   |
%             |   |...|...|...|...|...|...|   |
%     +---+---+---+---+---+---+---+---+---+---+---+---+
%     |   |   |   |   |   |   |   |   |   |   |   |   |
%     |   |   |   |   |   |   |   |   |   |   |   |   |
%     +---+---+---+---+---+---+---+---+---+---+---+---+
%
%                 +---+---+---+---+---+---+                   Level 2
%                 |   |   |   |   |   |   |
%                 |   |   |   |   |   |   |
%                 +---+---+---+---+---+---+
%                 |   |...|...|...|...|   |
%                 |   |...|...|...|...|   |
%                 +---+---+---+---+---+---+
%                 |   |...|...|...|...|   |
%                 |   |...|...|...|...|   |
%                 +---+---+---+---+---+---+
%                 |   |...|...|...|...|   |
%                 |   |...|...|...|...|   |
%                 +---+---+---+---+---+---+
%                 |   |...|...|...|...|   |
%                 |   |...|...|...|...|   |
%                 +---+---+---+---+---+---+
%                 |   |   |   |   |   |   |
%                 |   |   |   |   |   |   |
%                 +---+---+---+---+---+---+
%
%                     +---+---+---+---+                       Level 3
%                     |   |   |   |   |
%                     |   |   |   |   |
%                     +---+---+---+---+
%                     |   |...|...|   |
%                     |   |...|...|   |
%                     +---+---+---+---+
%                     |   |...|...|   |
%                     |   |...|...|   |
%                     +---+---+---+---+
%                     |   |   |   |   |
%                     |   |   |   |   |
%                     +---+---+---+---+
%
%                         +---+---+                           Level 4
%                         |   |   |
%                         |  .|.  |
%                         +---+---+
%                         |  .|.  |
%                         |   |   |
%                         +---+---+
%
%                           +---+                             Level 5 (highest)
%                           |   |
%                           |   |
%                           +---+
%
% There are the following types of tiles (our encoding is shown in
% parentheses):
% - plants (pl)
% - seasons (ss)
% - squares (sq)
% - chinese letters F C N S E W (cF, cC, cN, cS, cE, cW)
% - coins 1 to 9 (y1, y2, y3, y4, y5, y6, y7, y8, y9)
% - birds (bi)
% - sticks 2 to 9 (s2, s3, s4, s5, s6, s7, s8, s9)
% - chinese numbers 1 to 9 (c1, c2, c3, c4, c5, c6, c7, c8, c9)





% Layer 5
LINEAR in t5gh45.


% Layer 4
LINEAR (in t4g4 :- out t5gh45           ) & (in t4g4 :- out t5gh45, out t4h4 ).
LINEAR (in t4h4 :- out t5gh45, out t4g4 ) & (in t4h4 :- out t5gh45           ).

LINEAR (in t4g5 :- out t5gh45           ) & (in t4g5 :- out t5gh45, out t4h5 ).
LINEAR (in t4h5 :- out t5gh45, out t4g5 ) & (in t4h5 :- out t5gh45           ).


% Layer 3
LINEAR (in t3f3                         ) & (in t3f3 :-             out t3g3 ).
LINEAR (in t3g3 :-             out t3f3 ) & (in t3g3 :-             out t3h3 ).
LINEAR (in t3h3 :-             out t3g3 ) & (in t3h3 :-             out t3i3 ).
LINEAR (in t3i3 :-             out t3h3 ) & (in t3i3                         ).

LINEAR (in t3f4                         ) & (in t3f4 :-             out t3g4 ).
LINEAR (in t3g4 :- out t4g4,   out t3f4 ) & (in t3g4 :- out t4g4,   out t3h4 ).
LINEAR (in t3h4 :- out t4h4,   out t3g4 ) & (in t3h4 :- out t4h4,   out t3i4 ).
LINEAR (in t3i4 :-             out t3h4 ) & (in t3i4                         ).

LINEAR (in t3f5                         ) & (in t3f5 :-             out t3g5 ).
LINEAR (in t3g5 :- out t4g5,   out t3f5 ) & (in t3g5 :- out t4g5,   out t3h5 ).
LINEAR (in t3h5 :- out t4h5,   out t3g5 ) & (in t3h5 :- out t4h5,   out t3i5 ).
LINEAR (in t3i5 :-             out t3h5 ) & (in t3i5                         ).

LINEAR (in t3f6                         ) & (in t3f6 :-             out t3g6 ).
LINEAR (in t3g6 :-             out t3f6 ) & (in t3g6 :-             out t3h6 ).
LINEAR (in t3h6 :-             out t3g6 ) & (in t3h6 :-             out t3i6 ).
LINEAR (in t3i6 :-             out t3h6 ) & (in t3i6                         ).


% Layer 2
LINEAR (in t2e2                         ) & (in t2e2 :-             out t2f2 ).
LINEAR (in t2f2 :-             out t2e2 ) & (in t2f2 :-             out t2g2 ).
LINEAR (in t2g2 :-             out t2f2 ) & (in t2g2 :-             out t2h2 ).
LINEAR (in t2h2 :-             out t2g2 ) & (in t2h2 :-             out t2i2 ).
LINEAR (in t2i2 :-             out t2h2 ) & (in t2i2 :-             out t2j2 ).
LINEAR (in t2j2 :-             out t2i2 ) & (in t2j2                         ).

LINEAR (in t2e3                         ) & (in t2e3 :-             out t2f3 ).
LINEAR (in t2f3 :- out t3f3,   out t2e3 ) & (in t2f3 :- out t3f3,   out t2g3 ).
LINEAR (in t2g3 :- out t3g3,   out t2f3 ) & (in t2g3 :- out t3g3,   out t2h3 ).
LINEAR (in t2h3 :- out t3h3,   out t2g3 ) & (in t2h3 :- out t3h3,   out t2i3 ).
LINEAR (in t2i3 :- out t3i3,   out t2h3 ) & (in t2i3 :- out t3i3,   out t2j3 ).
LINEAR (in t2j3 :-             out t2i3 ) & (in t2j3                         ).

LINEAR (in t2e4                         ) & (in t2e4 :-             out t2f4 ).
LINEAR (in t2f4 :- out t3f4,   out t2e4 ) & (in t2f4 :- out t3f4,   out t2g4 ).
LINEAR (in t2g4 :- out t3g4,   out t2f4 ) & (in t2g4 :- out t3g4,   out t2h4 ).
LINEAR (in t2h4 :- out t3h4,   out t2g4 ) & (in t2h4 :- out t3h4,   out t2i4 ).
LINEAR (in t2i4 :- out t3i4,   out t2h4 ) & (in t2i4 :- out t3i4,   out t2j4 ).
LINEAR (in t2j4 :-             out t2i4 ) & (in t2j4                         ).

LINEAR (in t2e5                         ) & (in t2e5 :-             out t2f5 ).
LINEAR (in t2f5 :- out t3f5,   out t2e5 ) & (in t2f5 :- out t3f5,   out t2g5 ).
LINEAR (in t2g5 :- out t3g5,   out t2f5 ) & (in t2g5 :- out t3g5,   out t2h5 ).
LINEAR (in t2h5 :- out t3h5,   out t2g5 ) & (in t2h5 :- out t3h5,   out t2i5 ).
LINEAR (in t2i5 :- out t3i5,   out t2h5 ) & (in t2i5 :- out t3i5,   out t2j5 ).
LINEAR (in t2j5 :-             out t2i5 ) & (in t2j5                         ).

LINEAR (in t2e6                         ) & (in t2e6 :-             out t2f6 ).
LINEAR (in t2f6 :- out t3f6,   out t2e6 ) & (in t2f6 :- out t3f6,   out t2g6 ).
LINEAR (in t2g6 :- out t3g6,   out t2f6 ) & (in t2g6 :- out t3g6,   out t2h6 ).
LINEAR (in t2h6 :- out t3h6,   out t2g6 ) & (in t2h6 :- out t3h6,   out t2i6 ).
LINEAR (in t2i6 :- out t3i6,   out t2h6 ) & (in t2i6 :- out t3i6,   out t2j6 ).
LINEAR (in t2j6 :-             out t2i6 ) & (in t2j6                         ).

LINEAR (in t2e7                         ) & (in t2e7 :-             out t2f7 ).
LINEAR (in t2f7 :-             out t2e7 ) & (in t2f7 :-             out t2g7 ).
LINEAR (in t2g7 :-             out t2f7 ) & (in t2g7 :-             out t2h7 ).
LINEAR (in t2h7 :-             out t2g7 ) & (in t2h7 :-             out t2i7 ).
LINEAR (in t2i7 :-             out t2h7 ) & (in t2i7 :-             out t2j7 ).
LINEAR (in t2j7 :-             out t2i7 ) & (in t2j7                         ).


% Layer 1
LINEAR (in t1b1                         ) & (in t1b1 :-             out t1c1 ).
LINEAR (in t1c1 :-             out t1b1 ) & (in t1c1 :-             out t1d1 ).
LINEAR (in t1d1 :-             out t1c1 ) & (in t1d1 :-             out t1e1 ).
LINEAR (in t1e1 :-             out t1d1 ) & (in t1e1 :-             out t1f1 ).
LINEAR (in t1f1 :-             out t1e1 ) & (in t1f1 :-             out t1g1 ).
LINEAR (in t1g1 :-             out t1f1 ) & (in t1g1 :-             out t1h1 ).
LINEAR (in t1h1 :-             out t1g1 ) & (in t1h1 :-             out t1i1 ).
LINEAR (in t1i1 :-             out t1h1 ) & (in t1i1 :-             out t1j1 ).
LINEAR (in t1j1 :-             out t1i1 ) & (in t1j1 :-             out t1k1 ).
LINEAR (in t1k1 :-             out t1j1 ) & (in t1k1 :-             out t1l1 ).
LINEAR (in t1l1 :-             out t1k1 ) & (in t1l1 :-             out t1m1 ).
LINEAR (in t1m1 :-             out t1l1 ) & (in t1m1                         ).

LINEAR (in t1d2                         ) & (in t1d2 :-             out t1e2 ).
LINEAR (in t1e2 :- out t2e2,   out t1d2 ) & (in t1e2 :- out t2e2,   out t1f2 ).
LINEAR (in t1f2 :- out t2f2,   out t1e2 ) & (in t1f2 :- out t2f2,   out t1g2 ).
LINEAR (in t1g2 :- out t2g2,   out t1f2 ) & (in t1g2 :- out t2g2,   out t1h2 ).
LINEAR (in t1h2 :- out t2h2,   out t1g2 ) & (in t1h2 :- out t2h2,   out t1i2 ).
LINEAR (in t1i2 :- out t2i2,   out t1h2 ) & (in t1i2 :- out t2i2,   out t1j2 ).
LINEAR (in t1j2 :- out t2j2,   out t1i2 ) & (in t1j2 :- out t2j2,   out t1k2 ).
LINEAR (in t1k2 :-             out t1j2 ) & (in t1k2                         ).

LINEAR (in t1c3                         ) & (in t1c3 :-             out t1d3 ).
LINEAR (in t1d3 :-             out t1c3 ) & (in t1d3 :-             out t1e3 ).
LINEAR (in t1e3 :- out t2e3,   out t1d3 ) & (in t1e3 :- out t2e3,   out t1f3 ).
LINEAR (in t1f3 :- out t2f3,   out t1e3 ) & (in t1f3 :- out t2f3,   out t1g3 ).
LINEAR (in t1g3 :- out t2g3,   out t1f3 ) & (in t1g3 :- out t2g3,   out t1h3 ).
LINEAR (in t1h3 :- out t2h3,   out t1g3 ) & (in t1h3 :- out t2h3,   out t1i3 ).
LINEAR (in t1i3 :- out t2i3,   out t1h3 ) & (in t1i3 :- out t2i3,   out t1j3 ).
LINEAR (in t1j3 :- out t2j3,   out t1i3 ) & (in t1j3 :- out t2j3,   out t1k3 ).
LINEAR (in t1k3 :-             out t1j3 ) & (in t1k3 :-             out t1l3 ).
LINEAR (in t1l3 :-             out t1k3 ) & (in t1l3                         ).

LINEAR (in t1b4 :-             out t1a45) & (in t1b4 :-             out t1c4 ).
LINEAR (in t1c4 :-             out t1b4 ) & (in t1c4 :-             out t1d4 ).
LINEAR (in t1d4 :-             out t1c4 ) & (in t1d4 :-             out t1e4 ).
LINEAR (in t1e4 :- out t2e4,   out t1d4 ) & (in t1e4 :- out t2e4,   out t1f4 ).
LINEAR (in t1f4 :- out t2f4,   out t1e4 ) & (in t1f4 :- out t2f4,   out t1g4 ).
LINEAR (in t1g4 :- out t2g4,   out t1f4 ) & (in t1g4 :- out t2g4,   out t1h4 ).
LINEAR (in t1h4 :- out t2h4,   out t1g4 ) & (in t1h4 :- out t2h4,   out t1i4 ).
LINEAR (in t1i4 :- out t2i4,   out t1h4 ) & (in t1i4 :- out t2i4,   out t1j4 ).
LINEAR (in t1j4 :- out t2j4,   out t1i4 ) & (in t1j4 :- out t2j4,   out t1k4 ).
LINEAR (in t1k4 :-             out t1j4 ) & (in t1k4 :-             out t1l4 ).
LINEAR (in t1l4 :-             out t1k4 ) & (in t1l4 :-             out t1m4 ).
LINEAR (in t1m4 :-             out t1l4 ) & (in t1m4 :-             out t1n45).

LINEAR (in t1a45                        ) & (in t1a45:- out t1b4,   out t1b5 ).
LINEAR (in t1n45:- out t1m4,   out t1m5 ) & (in t1n45:-             out t1o45).
LINEAR (in t1o45:-             out t1n45) & (in t1o45                        ).

LINEAR (in t1b5 :-             out t1a45) & (in t1b5 :-             out t1c5 ).
LINEAR (in t1c5 :-             out t1b5 ) & (in t1c5 :-             out t1d5 ).
LINEAR (in t1d5 :-             out t1c5 ) & (in t1d5 :-             out t1e5 ).
LINEAR (in t1e5 :- out t2e5,   out t1d5 ) & (in t1e5 :- out t2e5,   out t1f5 ).
LINEAR (in t1f5 :- out t2f5,   out t1e5 ) & (in t1f5 :- out t2f5,   out t1g5 ).
LINEAR (in t1g5 :- out t2g5,   out t1f5 ) & (in t1g5 :- out t2g5,   out t1h5 ).
LINEAR (in t1h5 :- out t2h5,   out t1g5 ) & (in t1h5 :- out t2h5,   out t1i5 ).
LINEAR (in t1i5 :- out t2i5,   out t1h5 ) & (in t1i5 :- out t2i5,   out t1j5 ).
LINEAR (in t1j5 :- out t2j5,   out t1i5 ) & (in t1j5 :- out t2j5,   out t1k5 ).
LINEAR (in t1k5 :-             out t1j5 ) & (in t1k5 :-             out t1l5 ).
LINEAR (in t1l5 :-             out t1k5 ) & (in t1l5 :-             out t1m5 ).
LINEAR (in t1m5 :-             out t1l5 ) & (in t1m5 :-             out t1n45).

LINEAR (in t1c6                         ) & (in t1c6 :-             out t1d6 ).
LINEAR (in t1d6 :-             out t1c6 ) & (in t1d6 :-             out t1e6 ).
LINEAR (in t1e6 :- out t2e6,   out t1d6 ) & (in t1e6 :- out t2e6,   out t1f6 ).
LINEAR (in t1f6 :- out t2f6,   out t1e6 ) & (in t1f6 :- out t2f6,   out t1g6 ).
LINEAR (in t1g6 :- out t2g6,   out t1f6 ) & (in t1g6 :- out t2g6,   out t1h6 ).
LINEAR (in t1h6 :- out t2h6,   out t1g6 ) & (in t1h6 :- out t2h6,   out t1i6 ).
LINEAR (in t1i6 :- out t2i6,   out t1h6 ) & (in t1i6 :- out t2i6,   out t1j6 ).
LINEAR (in t1j6 :- out t2j6,   out t1i6 ) & (in t1j6 :- out t2j6,   out t1k6 ).
LINEAR (in t1k6 :-             out t1j6 ) & (in t1k6 :-             out t1l6 ).
LINEAR (in t1l6 :-             out t1k6 ) & (in t1l6                         ).

LINEAR (in t1d7                         ) & (in t1d7 :-             out t1e7 ).
LINEAR (in t1e7 :- out t2e7,   out t1d7 ) & (in t1e7 :- out t2e7,   out t1f7 ).
LINEAR (in t1f7 :- out t2f7,   out t1e7 ) & (in t1f7 :- out t2f7,   out t1g7 ).
LINEAR (in t1g7 :- out t2g7,   out t1f7 ) & (in t1g7 :- out t2g7,   out t1h7 ).
LINEAR (in t1h7 :- out t2h7,   out t1g7 ) & (in t1h7 :- out t2h7,   out t1i7 ).
LINEAR (in t1i7 :- out t2i7,   out t1h7 ) & (in t1i7 :- out t2i7,   out t1j7 ).
LINEAR (in t1j7 :- out t2j7,   out t1i7 ) & (in t1j7 :- out t2j7,   out t1k7 ).
LINEAR (in t1k7 :-             out t1j7 ) & (in t1k7                         ).

LINEAR (in t1b8                         ) & (in t1b8 :-             out t1c8 ).
LINEAR (in t1c8 :-             out t1b8 ) & (in t1c8 :-             out t1d8 ).
LINEAR (in t1d8 :-             out t1c8 ) & (in t1d8 :-             out t1e8 ).
LINEAR (in t1e8 :-             out t1d8 ) & (in t1e8 :-             out t1f8 ).
LINEAR (in t1f8 :-             out t1e8 ) & (in t1f8 :-             out t1g8 ).
LINEAR (in t1g8 :-             out t1f8 ) & (in t1g8 :-             out t1h8 ).
LINEAR (in t1h8 :-             out t1g8 ) & (in t1h8 :-             out t1i8 ).
LINEAR (in t1i8 :-             out t1h8 ) & (in t1i8 :-             out t1j8 ).
LINEAR (in t1j8 :-             out t1i8 ) & (in t1j8 :-             out t1k8 ).
LINEAR (in t1k8 :-             out t1j8 ) & (in t1k8 :-             out t1l8 ).
LINEAR (in t1l8 :-             out t1k8 ) & (in t1l8 :-             out t1m8 ).
LINEAR (in t1m8 :-             out t1l8 ) & (in t1m8                         ).


hook :-
	write_sans "Board loaded", nl.
