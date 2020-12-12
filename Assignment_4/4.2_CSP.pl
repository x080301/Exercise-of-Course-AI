% We simlify the representation of a CSP a little to make the implementation simpler.

% We restrict attention to finite CSPs.

% We normalize the variables to be x_1,...,x_n
% Thus, the number n is enough to represent the variables.

% We normalize the domains to D_i = {0,...,l_i-1}.
% For finite CSPs, this is without loss of generality.

% domain(L,Xs) holds if Xs consists of 0,...,L-1

domain(L,Xs) :- L>0, Ld is L-1, domain(Ld,Xsd), Xs=[Ld|Xsd].
domain(0,[]).

% domainOf(Ds,I,Xs) holds if D is Ds_I
domainOf([L|_],1,Xs) :- domain(L,Xs).
domainOf([_|Ls],I,Xs) :- I>1, Id is I-1, domainOf(Ls,Id,Xs).

% Thus, the numbers l_i are enough to represent the domains,
% and the list Ds=[l_1,...,l_n] is enough to represent
% all variables and their domains.

% Because the variables are numbered, we can represent a total
% assignment a of the form x_1:=v_1, ..., x_n:=v_n simply as
% the list [v_1,...,v_n].
% We represent a partial assignment by using the value 'none' for
% every unassigned variable.

% apply(a,i,v) holds if a(x_i)=v for an assignment a
apply([V|_],1,V). 
apply([_|Vs],I,V) :- I>1, Id is I-1, apply(Vs,Id,V).

% extends(a,i,u,a') holds if assignemnt a' arises by extending
% assignment a with x_i:=u
extend([_|Vs],1,U,VsE) :- VsE = [U|Vs].
extend([V|Vs],I,U,VsE) :- I>1, Id is I-1, extend(Vs,Id,U,Vs2), VsE = [V|Vs2].

% isAssignment(Doms, Vals) holds if Vals is a possible (possibly partial)
% assignment for the domains given by Doms
isAssignment([],[]).
isAssignment([_|Ds],A) :- isAssignment(Ds,Vs), A=[none|Vs].
isAssignment([D|Ds],A) :- isAssignment(Ds,Vs), domain(D,Xs), contains(Xs,V), A=[V|Vs].

% empty(Ds, Vs) holds if Vs is the empty assignment for Ds
isEmpty([_|Ds],[none|Vs]) :- isEmpty(Ds,Vs).
isEmpty([],[]).

% isTotal(Vals) holds if Vals is a total assignment
isTotal([]).
isTotal([V|Vs]) :- not(V=none), isTotal(Vs).

% We represent a binary constraint C_ij for i<j and variables x_i and x_j as
% the list of pairs [(v,w),...] such that v in D_i, w in D_j, and (v,w) in C_ij.

% We represent a C_ij as the term constraint(i,j,p), where p
% is the list of pairs (v,w) such that (v,w) in C_ij.

% consistent(Cs,a) holds if the assignment a satisfies all constraints in Cs
% for which a is defined
% NEW: fixed definition of this predicate
consistent([],_).
consistent([C|Cs],Vs) :- C=constraint(I,_,_), apply(Vs,I,A), A=none, consistent(Cs,Vs).
consistent([C|Cs],Vs) :- C=constraint(_,J,_), apply(Vs,J,B), B=none, consistent(Cs,Vs).
consistent([C|Cs],Vs) :- C=constraint(I,J,P), apply(Vs,I,A), apply(Vs,J,B),
    not(A=none), not(B=none), contains(P,(A,B)), consistent(Cs,Vs).

% solutions are total consistent assignments
isSolution(Ds,Cs,Vs) :- isAssignment(Ds,Vs), isTotal(Vs), consistent(Cs,Vs).

% --------------------------------------------
% auxiliary functions:

% membership in a list
contains([H|_],X) :- H=X.
contains([_|T],X) :- contains(T,X).

% --------------------------------------------

% The following is an example of how to instantiate our framework
% with a concrete CSP.

% The domains: we use n=2 and 0<=x_1<5, 0<=x_2<3, 0<=x_3<2
testDomains([5,3,2]).
% The constraints: C_12 is x_1=2x_2, and C_23 is x_2<=x_3, and we do not use C_13.
testConstraints([constraint(1,2,[(4,2),(2,1),(0,0)]),
                 constraint(2,3,[(0,0),(0,1),(1,1)])
                ]).
% a possible solution
testAssignment([2,1,1]).

% --------------------------------------------
% NEW: 4 queens as a test case

% We use a new representation to better fit into the above framework:
% Queen i always occurs in column i
% R_i is the row of queen i
% variables V = R_1, R_2, R_3, R_4
% domains: D_i = {0,1,2,3}
% constraints:
%  C_ij = "R_i != R_j and DD_ij" for i,j=1,...,4
%  where
%   - R_i != R_j expresses that no two queens are in the same row
%   - D_i != D_j + 1 expresses that queen i and queen j are not in the same diagonal
		%        - 1
queensDomains([4,4,4,4]).

% the following line must be copied and adapted to get the pairs in the other 5 constraints
% [(0,1),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,1),(2,3),(3,0),(3,1),(3,2)])
queensConstraints([
  constraint(1,2, [(0,2),(0,3),(1,3),(2,0),(3,0),(3,1)]),
  constraint(1,3, [(0,1),(0,3), (1,0), (1,2), (2,1), (2,3), (3,0), (3,2)]),
  constraint(1,4, [(0,1),(0,2), (1,0), (1,2), (1,3), (2,0), (2,1), (2,3), (3,1), (3,2)]),
  constraint(2,3, [(0,2),(0,3), (1,3), (2,0), (3,0), (3,1)]),
  constraint(2,4, [(0,1),(0,3), (1,0), (1,2), (2,1), (2,3), (3,0), (3,2)]),
  constraint(3,4, [(0,2),(0,3), (1,3), (2,0), (3,0), (3,1)])  
]).

% --------------------------------------------

% We can run the query
% testDomains(Ds), testConstraints(Cs), testAssignment(Vs), isSolution(Ds,Cs,Vs)
% to check if this is a solution

% We can also run the query
% testDomains(Ds), testConstraints(Cs), isSolution(Ds,Cs,Vs)
% to find all solutions (using the fact that Prolog's DFS behavior is just
% naive backtracking algorithm for a CSP.

% In other words, this solves any CSP with Ds and Cs as input and Vs as output:
solve_naive(Ds,Cs,Vs) :- isSolution(Ds,Cs,Vs).

%state [none,none,2,none] means you randomly chose field 2 for the 3rd row as a start
%state2[none,1,2,none]
mrv(Cs,[H|State],State2):-
    H == none,
    mrv(Cs,State,State2).
    %degreeV(_)

solve(Ds,Cs,Vs):- random(0,4,N), mrv(Cs,[none, N, none, none], Vs), lCv(Ds,Cs,Vs), isAssignment(Ds,Vs), isTotal(Vs), consistent(Cs,Vs).
% Write a better implementation of solve(Ds,Cs,Vs) that uses the
% MRV, degree (as a tie-breaker for MRV), and least constraining value heuristics.

% Hint: Implement it first with dummy heuristics that pick
% - some unassigned variables x_i
% - any order for the values for x_i.
% Then improve the heuristics step by step.
