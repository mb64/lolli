%% File: examples.pro

%% Some simple propositional linear logic formulas.  Not all are
%% theorems of linear logic.

formula(1,  a -> a).
formula(2,  a -> b -> a x b).
formula(3,  a => bang(a)).
formula(4,  a & b -> a & b).
formula(5,  a => a & a).
formula(6,  a => a x a).
formula(7,  a & b -> a).
formula(8,  a -> a & a).
formula(9,  a => b => bang(a & b)).
formula(10, a & b => bang(a) x bang(b)).
formula(11, (a -> b -> c) -> (a => b => c)).
formula(12, (a x b -> c) -> (a -> b -> c)).
formula(13, a -> b -> a x erase).
formula(14, a -> b -> erase x a).
formula(15, a -> b -> b x erase x a).
formula(16, a -> b => b x erase x a).
formula(17, (a & b) -> (a & b) -> (a x a) & (a x b) & (b x b)).
formula(18, a => b -> b x bang(a)).
formula(19, a => b -> bang(a) x b).
formula(20, a -> a x a).
formula(21, a -> a -> a).
formula(22, a -> b -> a & b).
formula(23, a & b -> a x b).
formula(24, (a -> b -> c) -> (a -> b) -> a -> c).
formula(25, (a -> b -> c) -> (a -> b) -> (a -> a -> c)).
formula(26, a -> b -> c -> erase).

test(N) :- formula(N,F), prove(nil, nil, F).

% To use these examples, load both llinterp.pro and this file.
% The query    ?- test(N).   will show that the formulas
% 20, 21, 22, 23, 24 are not provable, that 17 has eight proofs,
% and that 25 has two proofs.
