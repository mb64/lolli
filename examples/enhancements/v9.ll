% This modules implements an example that shows the speed-up obtained by
% going from version 0.8 to version 0.9 of the Lolli interpreter.
%
% Here is an example of use on the two implementations:
%
% Version 0.8:
% 
% Starting Lolli version 0.8b, Mar 13, 1995 -fp
%   (built with Standard ML of New Jersey, Version 0.93, February 15, 1993)...
% ?- load v9.
% loading v9...
% ?- timing (test 50) T.
% **************************************************
% ##################################################
% This way was better!
% T_24848 <- 18784096
% yes
%
%
% Version 0.9:
% 
% Starting Lolli version 0.9, Feb 28, 1995 -ic
%   (built with Standard ML of New Jersey, Version 0.93, February 15, 1993)...
% ?- load v9.
% loading v9...
% ?- timing (test 50) T.
% 
% This way was better!
% T_1 <- 3904
% yes



MODULE v9.

test N :- ((a,erase) & b N), true.


LINEAR a.
LINEAR c.

b N :- c  & (heavyComputation N, a).                  % Fails
b N :- a,c, nl,write_sans "This way was better!",nl.  % Succeeds


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
