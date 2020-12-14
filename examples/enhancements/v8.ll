% This modules implements an example that shows the speed-up obtained by
% going from version 0.7 to version 0.8 of the Lolli interpreter.
%
% Here is an example of use on the two implementations:
%
% Version 0.7:
% 
% Starting Lolli version 0.701, November 30, 1992
%   (built with Standard ML of New Jersey, Version 0.93, February 15, 1993)...
% ?- load v8.
% loading v8...
% ?- timing (test 50) T.
% **************************************************
% ##################################################
% This way was better!
% T_1 <- 22138608
% yes
%
%
% Version 0.8:
%
% Starting Lolli version 0.8, Feb 19, 1995 -fp
%   (built with Standard ML of New Jersey, Version 0.93, February 15, 1993)...
% ?- load v8.
% loading v8...
% ?- timing (test 50) T.
% 
% This way was better!
% T_1 <- 976
% yes



MODULE v8.

test N :- (a & b N), c.


a.

b N :- c, heavyComputation N.                     % Fails
b N :- nl, write_sans "This way was better!",nl.  % Succeeds

LINEAR c.


heavyComputation N :-                   % Wastes time.
      buildList N L,
      nl,
      reverse L R.

buildList N (N::L) :-                   % Builds a list of N elements.
      N > 0,                            % Prints "*"s to show that it
      N1 is N - 1,                      % is alive (and to waste time).
      write_sans "*",
      buildList N1 L.
buildList 0 nil.

reverse (N::L) R :-                     % Naive reverse. Prints "#"s.
      reverse L R1,
      write_sans "#",
      append R1 (N::nil) R.
reverse nil nil.

append (N::L1) L2 (N::L3) :-            % Append.
      append L1 L2 L3.
append nil L2 L2.
