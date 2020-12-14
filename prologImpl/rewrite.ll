%% File: rewrite.ll

%% Examples showing how to do permutations and multiset
%% rewriting.

% load(List,Goal) first moves all the members of List into
% the bound context and then calls Goal.  Elements are stored
% using the item predicate.

load(nil, G)   <- G.
load(X::L, G)  <- item(X) -> load(L,G).

% unload(List) is provable if and only if List represents the 
% unbounded context, in some order.

unload(nil)    <- true.
unload(X::L)   <- item(X) x unload(L).


% perm(L,K) is true if and only if L and K are permutations of
% each other.

perm(L, K)     <- bang(load(L,unload(K))).

% rewrite(L, K) will do multiset rewriting modulo the rewrite
% rules lists using the rew predicate.

rewrite(L, K) <- bang(load(L, rew(K))).

rew(K)		<- unload(K).

rew(K)  <- item(a) x item(b) x (item(b) -> (item(c) -> rew(K))).
rew(K)  <- item(a) x item(a) x             (item(a) -> rew(K)).
