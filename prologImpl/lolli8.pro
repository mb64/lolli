%% File: llinterp.pro

%% An interpreter for the fragment of propositional, intuitutionistic
%% linear logic described in the paper
%%  "Logic Programming in a Fragment of Intuitionistic Linear Logic" 
%%   by Joshua S. Hodas and Dale Miller, to appear in the
%%   Journal of Information and Computation.

%  The logic being interpreted contains the following logical connectives:
%   true/0	 	a constant (empty tensor, written as 1 in the paper)
%   erase/0		a constant (erasure, written as Top in the paper)
%   bang/1	 	the modal usually written as !
:- op(145,xfy,->).  %    linear implication, written as -o in the paper
:- op(145,xfy,=>).  %    intuitionistic implication
:- op(140,xfy,x ).  %    multiplicative conjunction (tensor)
:- op(140,xfy,& ).  %    additive conjunction

%  Non-logical constants
%     del/0   denotes a deleted bounded formula
%     nil/0   empty list constructor
:- op(150,xfy,::).  %    non-empty list constructor

% The following code is taken directly from the paper.

   isR(erase).
   isR(B)        :- isA(B).
   isR(B1 & B2)  :- isR(B1), isR(B2).
   isR(B1 -> B2) :- isG(B1), isR(B2).
   isR(B1 => B2) :- isG(B1), isR(B2).
   
   isG(true).
   isG(erase).
   isG(B)        :- isA(B).
   isG(B1 -> B2) :- isR(B1), isG(B2).
   isG(B1 => B2) :- isR(B1), isG(B2).
   isG(B1 &  B2) :- isG(B1), isG(B2).
   isG(B1 x  B2) :- isG(B1), isG(B2).
   isG(bang(B))  :- isG(B).
   
   prove(I,I, true).
   prove(I,O, erase)   :- subcontext(O,I).
   prove(I,O, G1 & G2) :-				% Modified -ic
	prove(I,O,G1),
	contextDifference(I, O, M, Nil),
	prove(M, Nil, G2).
   prove(I,O, R -> G)  :- prove(R :: I, del :: O,G).
   prove(I,O, R => G)  :- prove(bang(R) :: I, bang(R) :: O,G).
   prove(I,O, G1 x G2) :- prove(I,M,G1), prove(M,O,G2).
   prove(I,I, bang(G)) :- prove(I,I,G).
   prove(I,O, A)       :- isA(A), pickR(I,M,R), bc(M,O,A,R).
   
   bc(I,I,A, A).
   bc(I,O,A, G -> R)  :- bc(I,M,A,R), prove(M,O,G).
   bc(I,O,A, G => R)  :- bc(I,O,A,R), prove(O,O,G).
   bc(I,O,A, R1 & R2) :- bc(I,O,A,R1); bc(I,O,A,R2).
   
   pickR(bang(R)::I, bang(R)::I, R).
   pickR(R::I,       del::I,     R) :- isR(R).
   pickR(S::I,       S::O,       R) :- pickR(I,O,R).

   subcontext(del::O, R ::I) :- isR(R), subcontext(O,I).
   subcontext(S::O,   S::I)  :- subcontext(O,I). 
   subcontext(nil,    nil).

% 	contextDifference(I, O, M, Nil)
% Given contexts I and O as input, this relation returns as output
% the context M resulting from the multiset difference of I and O,
% and the empty context Nil corresponding to I, O and M. -ic
   contextDifference(R::I, R::O, del::M, del::Nil) :-
	contextDifference(I, O, M, Nil).
   contextDifference(R::I, del::O, R::M, del::Nil) :-
	isR(R),
	contextDifference(I, O, M, Nil).
   contextDifference(nil, nil, nil, nil).


%%%%%%%%%%%%%%%%%%%%%%%%%
% The following code provides the hooks into application programs.
:- op(150,yfx,<-). %     the converse of the linear implication

% Applications using this interpreter are specified using the <-/2
% functor (denoting the converse of linear implication).  We shall
% assume that clauses so specified are implicitly banged (belong
% to the unbounded part of the initial context) and that the first
% argument to -> is atomic.  The following clause is the hook to
% clauses specified using <-.

prove(I,O, A) :- isA(A), A <- G, prove(I,O,G).

% A few input/output non-logicals.

prove(I,I, write(X)) :- write(X).
prove(I,I, read(X)) :- read(X).
prove(I,I, nl) :- nl.

% The following is a flexible specification of isA/1

notA(erase).  notA(true).  notA(del).
notA(_ & _).  notA(_ x _).  notA(_ -> _).  notA(_ => _).  notA(bang(_)).
notA(write(_)).  notA(read(_)).   notA(nl).
isA(A) :- \+ (notA(A)).

% ?- dynamic('<-').
% The following is a dummy definition for Sicstus
% whatever <- whatever.
