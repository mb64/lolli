MODULE test.

test N :-
      buildList N L1,
      append L1 (end::nil) L.


buildList N (N::L) :-                   % Builds a list of N elements.
      N > 0,                            % Prints "*"s to show that it
      N1 is N - 1,                      % is alive (and to waste time).
      write_sans "*",
      buildList N1 L.
buildList 0 nil.

append (N::L1) L2 (N::L3) :-            % Append.
      write_sans "#",
      append L1 L2 L3.
append nil L2 L2.


test2 N C Avg Min Max :-
	test3 N C C Avg Min Max.

test3 N C V Avg Min Max :-
	C > 0,
	timing (test N) T,
	C1 is (C-1),
	test3 N C1 V Avg1 Min1 Max1,
	Avg is ((T/V) + Avg1),
	(T < Min1 -> Min = T | Min = Min1),
	(T > Max1 -> Max = T | Max = Max1).
test3 N 0 _ 0 9999999 0.

