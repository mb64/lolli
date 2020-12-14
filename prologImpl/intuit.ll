%% File: intuit.ll

%% The following is a theorem prover for propositional, intuitionistic
%% logic follows a proof system given in Roy Dyckhoff's "Theorem proving
%% in intuitionistic logic" (unpublished, Sept 1990).  This decision
%% procedure works by formulating propositional, intuitionistic logic
%% without contraction.
%% This program is a decision procedure even under depth first search.

% Propositional intuitionistic logic has four logical constants.
%	   false		         %   the absurd proposition
:- op(145,xfy,imp). %    implication
:- op(140,xfy,or).  %    disjunction
:- op(140,xfy,and). %    conjunction

% There are four introduction rules.
pv(A and B) <- pv(B) & pv(A).
pv(A imp B) <- hyp(A) -> pv(B).
pv(A or  B) <- pv(A).
pv(A or  B) <- pv(B).

% There are eight elimination rules.  The imp-left rule breaks into
% five rules.
pv(G) <- hyp(A and B) x (hyp(A) -> hyp(B) -> pv(G)).
pv(G) <- hyp(A or  B) x ((hyp(B) -> pv(G)) & (hyp(A) -> pv(G))).
pv(G) <- hyp(false) x erase.
pv(G) <- hyp((C imp D) imp B) x ((hyp(D imp B) -> pv(C imp D)) & (hyp(B) -> pv(G))).
pv(G) <- hyp((C and D) imp B) x (hyp(C imp (D imp B)) -> pv(G)).
pv(G) <- hyp((C or  D) imp B) x (hyp(C imp B) -> hyp(D imp B) -> pv(G)).
pv(G) <- hyp(false     imp B) x pv(G).
pv(G) <- hyp(        A imp B) x isatom(A) x hyp(A) x (hyp(B) -> hyp(A) -> pv(G)).

% This specifies a 
pv(G) <- hyp(G) x erase.

% Describe object-level atomic formulas.
isatom(a)     <- true.
isatom(b)     <- true.
isatom(c)     <- true.
isatom(d)     <- true.
isatom(false) <- true.

% Here are some sample intuitionistic formulas.  Not all are provable.

prop(1, a imp a).
prop(2, (a or b) imp (b or a)).
prop(3, (a and b) imp (b and a)).
prop(4, a imp b imp a).
prop(5, (a imp (b imp c)) imp ((a imp b) imp (a imp c))).
prop(6, ((a imp b) imp a) imp ((a imp b) imp b)).
prop(7, ((a imp b) imp a) imp a).
prop(8, ((a imp b) and (c imp a)) imp (c imp b)).
prop(9, (a or b) imp (a imp c) imp (b imp c) imp c).
prop(10,(a or b) imp (a imp c) imp ((b and d) imp c) imp d imp c).
prop(11,((a imp b) or (a imp c)) imp a imp (b or c)).
prop(12,(a or b) imp (a imp c) imp (b imp (d imp c)) imp d imp c).
prop(13,((a and a) and (a imp b) imp b)).
prop(14,a imp a imp (a imp b) imp b).

prop(15,(c or (c imp false)) imp (((a imp b) and ((a imp c) imp b)) imp b)).
prop(16,(b or (b imp false)) imp (((a imp b) and ((a imp c) imp b)) imp b)).
prop(17,(a or (a imp false)) imp (((a imp b) and ((a imp c) imp b)) imp b)).
prop(18,(b imp (b imp false)) imp ((b imp false) imp false) imp b).
