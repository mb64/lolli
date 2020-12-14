%% File: natlang.ll

%% Simple parser for English.  Non-vacuous gaps and "island"
%% constraints are addressed by linear logic connectives.

sent(P1,P2)        <- bang(np(P1,P0)) x vp(P0,P2).
vp(P1,P2)          <- tv(P1,P0) x np(P0,P2).
vp(P1,P2)          <- stv(P1,P0) x sbar(P0,P2).
sbar(that::P1,P2)  <- sent(P1,P2).
np(P1,P2)          <- pn(P1,P2). 
rel(whom::X,Y)     <- np(Z,Z) -> sent(X,Y).  % Need an embedded universal for Z
pn(mary::L,L)      <- true.
pn(bob::L,L)       <- true.
pn(ann::L,L)       <- true.
tv(loves::L,L)     <- true.
tv(married::L,L)   <- true.
stv(believes::L,L) <- true.

exrel(1, whom::mary::believes::that::bob::married::nil).
exrel(2, whom::bob::married::nil).
exrel(3, whom::mary::believes::that::married::ann::nil).
exrel(4, whom::bob::married::ann::nil).

%% The following addition to the grammar results in a left-recursive program.
% sent(P1,P2)        <- sent(P1,and::P3) & sent(P3,P2).
%% This additional clause should allow a parse of the following.
% exrel(5, whom::ann::married::and::bob::believes::that::mary::loves::nil).
