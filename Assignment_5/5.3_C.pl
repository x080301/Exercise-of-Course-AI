% --------------------------------------------
% auxiliary functions:

% --------------------------------------------
% finds the Ith member of a list: apply(Input_list,Index,Output_member)
% apply(a,i,v) holds if a(x_i)=v for an assignment a
apply([V|_],1,V). 
apply([_|Vs],I,V):- I>1, Id is I-1, apply(Vs,Id,V).






% Consider the Prolog framework for binary CSPs from the previous assignment. (For this exercise, only the provided code is relevant. You will not build on your solutions.) Implement the following Prolog predicates:
% To represent problems with tightened domains, we extend our code as follows: 
% A list [L_1,...,L_n] of lists represents a CSP with tightened domains. 
% (We can think of the L_i as unary constraints by the way.)
% Each L_i is a list of values from D_i, and only those values are legal.
% If Ds = [l_1,...,l_n], then initially, L_i=[0,...,l_i − 1]. 
% When tightening the domains, we remove values from L_i.
% Let Ds, Ls, Cs be such a CSP. 

% --------------------------------------------
% arcconsOne(Ds,Ls,Cs,I,J) holds if x_I is arc−consistent relative to x_J 
% input: Ds(list of domains of all nodes), Ls (list of tightened domains of all the nodes), Cs(constraints), I & J index
% i.e., for every value for x_I from L_I there is a value for x_J from L_J 
% such that C_IJ (if present) is satisfied 

% arcconsOne(Ds,Ls,Cs,I,J):− ???


% --------------------------------------------
% arcconsAll(Ds,Ls,Cs) holds if the whole CSP is arc−consistent 
% input Ds(list of domains of all nodes), Ls (list of tightened domains of all the nodes), Cs(constraints)
% use arcconsOne(see above)

%arcconsAll(Ds,Ls,Cs):− ???


% --------------------------------------------
% Given an untightened CSP Ds,Cs as before, makeArccons(Ds,Cs,Ls) returns Ls such that the CSP is arc−consistent.

%makeArccons(Ds,Cs,Ls):− ???


% write makeArccons (arcconsOne and makeArccons are writen by others)