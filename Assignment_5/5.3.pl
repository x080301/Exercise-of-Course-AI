% --------------------------------------------
% 4 queens as a test case

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
queensDomains([[0,1,2,3],[0,1,2,3],[0,1,2,3],[0,1,2,3]]).

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




% --------------------------------------------
% auxiliary functions:

% apply(a,i,v) holds if a(x_i)=v for an assignment a
apply([V|_],1,V). 
apply([_|Vs],I,V) :- I>1, Id is I-1, apply(Vs,Id,V).

% membership in a list
contains([H|_],X) :- H=X.
contains([_|T],X) :- contains(T,X).

% len(List,Len) holds if Len= length of List
len(List,Len):-
    len(List,0,Len).
len([],I,I).
len([_|R],I,Len):-
    Id is I + 1,
    len(R,Id,Len).
% --------------------------------------------



% --------------------------------------------
% To represent problems with tightened domains, we extend our code as follows: 
% A list [L_1,...,L_n] of lists represents a CSP with tightened domains.
% (We can think of the L_i as unary constraints by the way.)
% Each L_i is a list of values from D_i, and only those values are legal.
% If Ds = [l_1,...,l_n], then initially, L_i=[0,...,l_i − 1]. 
% When tightening the domains, we remove values from L_i.
% Let Ds, Ls, Cs be such a CSP. 
% arcconsOne(Ds,Ls,Cs,I,J) holds if x_I is arc−consistent relative to x_J 
% i.e., for every value for x_I from L_I there is a value for x_J from L_J 
% such that C_IJ (if present) is satisfied
% --------------------------------------------





% --------------------------------------------
% cuv_exist(Cs,U,V,Value_pairs):-
cuv_exist([C|_],U,V,Value_pairs,Uid):-
    C=constraint(U,V,Value_pairs),Uid=0;C=constraint(V,U,Value_pairs),Uid=1.
cuv_exist([_|R],U,V,Value_pairs,Uid):-
   	cuv_exist(R,U,V,Value_pairs,Uid).

% test(Value_pairs):-queensConstraints(Cs),cuv_exist(Cs,1,2,Value_pairs).


%value_pair_exist(Di,Li,Dj,Lj,Value_pairs,Uid):-
value_pair_exist(_,[],_,_,_,_).
value_pair_exist(Di,[Vi|Lir],Dj,Lj,Value_pairs,Uid):-
    value_pair_exist(Di,Lir,Dj,Lj,Value_pairs,Uid),
    
    contains(Di,Vi),
    (
    	Uid=0,
        contains(Value_pairs,(Vi,Vj))
    ;   
       	Uid=1,
        contains(Value_pairs,(Vj,Vi))
    ),
    contains(Dj,Vj),
    contains(Lj,Vj).

% test:-value_pair_exist([0,1,2,3],[0,1],[0,1,2,3],[2,3],[(0,2), (0,3), (1,3), (2,0), (3,0), (3,1)],0).
% true
% test:-value_pair_exist([0,1,2,3],[0,1],[0,1,2,3],[0],[(0,2), (0,3), (1,3), (2,0), (3,0), (3,1)],0).
% false


% A variable u ∈ V is arc consistent relative to another variable v ∈ V 
arcconsOne(_,_,_,I,I).
arcconsOne(Ds,Ls,Cs,I,J):-
    not(I=J),
    
    apply(Ds,I,Di),
    apply(Ls,I,Li),
    apply(Ds,J,Dj),
    apply(Ls,J,Lj),
    
    (
    	not(cuv_exist(Cs,I,J,_,_))
    		% if either Cuv not∈ C, 
    	;
        cuv_exist(Cs,I,J,Value_pairs,Uid),
        value_pair_exist(Di,Li,Dj,Lj,Value_pairs,Uid)
        	% or for every value d ∈ Du there exists a value d ∈ Dv such that (d, d) ∈ Cuv.
    ).
% testLs([[0,1],[2,3],[],[]]).
% true.
% testLs([[0,1],[0],[],[]]).
% false.
% test:-queensDomains(Ds),queensConstraints(Cs),testLs(Ls),arcconsOne(Ds,Ls,Cs,4,4).
% --------------------------------------------


        


% --------------------------------------------

traverse_in_J(_,_,_,_,0).
traverse_in_J(Ds,Ls,Cs,I,J):-
    J>0,
    
    arcconsOne(Ds,Ls,Cs,I,J),
    
    Jd is J - 1,
    traverse_in_J(Ds,Ls,Cs,I,Jd).

traverse_in_I(_,_,_,0,_).
traverse_in_I(Ds,Ls,Cs,I,Lenj):-
    I>0,
    
    traverse_in_J(Ds,Ls,Cs,I,Lenj),
    
    Id is I - 1,
    traverse_in_I(Ds,Ls,Cs,Id,Lenj).

% arcconsAll(Ds,Ls,Cs) holds if the whole CSP is arc−consistent 
arrconsAll(Ds,Ls,Cs):-
    len(Ls,Num_nodes),
    traverse_in_I(Ds,Ls,Cs,Num_nodes,Num_nodes).


% test:-queensDomains(Ds),queensConstraints(Cs),testLs(Ls),arrconsAll(Ds,Ls,Cs).
% testLs([[0,1,2,3],[0,1,2,3],[0,1,2,3],[0,1,2,3]]).
% true.
% testLs([[0,1,2,3],[0,1,2,3],[0,1,2,3],[0]]).
% false
% --------------------------------------------





% --------------------------------------------
% Given an untightened CSP Ds,Cs as before, makeArccons(Ds,Cs,Ls) returns Ls
% such that the CSP is arc−consistent.

du_dv_exist(Value_pairs,Du,Lv,Uid):-  
    (   
    Uid=0,
    contains(Value_pairs,(Du,Dv))
    ;   
    Uid=1,
    contains(Value_pairs,(Dv,Du))
    ),
    contains(Lv,Dv).

get_new_Vs(_,_,_,[],[]).
get_new_Vs(Value_pairs,Lv,Uid,[Du|Vsr],New_Vs):-
    get_new_Vs(Value_pairs,Lv,Uid,Vsr,New_Vsd),
    (
    du_dv_exist(Value_pairs,Du,Lv,Uid),
    New_Vs=[Du|New_Vsd]
    ;   
    not(du_dv_exist(Value_pairs,Du,Lv,Uid)),
    New_Vs=New_Vsd
    ).
    
    

get_new_Ls(Ls,U,V,Value_pairs,Uid,New_Ls):-
	get_new_Ls(Ls,Ls,U,V,Value_pairs,Uid,New_Ls).
get_new_Ls([Vs|Lsr],Ls,1,V,Value_pairs,Uid,New_Ls):-
    apply(Ls,V,Lv),
    
    get_new_Vs(Value_pairs,Lv,Uid,Vs,New_Vs),
    New_Ls=[New_Vs|Lsr].

get_new_Ls([L|Lsr],Ls,U,V,Value_pairs,Uid,New_Ls):-
    U>1,
    Ud is U-1,
    get_new_Ls(Lsr,Ls,Ud,V,Value_pairs,Uid,New_Lsd),
 	New_Ls=[L|New_Lsd].
    

rivise(Ls,Cs,U,V,New_Ls):-
    not(cuv_exist(Cs,U,V,_,_)),
    New_Ls=Ls
    ;
    cuv_exist(Cs,U,V,Value_pairs,Uid),
    get_new_Ls(Ls,U,V,Value_pairs,Uid,New_Ls).

% test(New_Ls):-testLs(Ls),queensConstraints(Cs),rivise(Ls,Cs,1,2,New_Ls).
% testLs([[0,1,2,3],[0,1,2],[0,1,2,3],[0,1,2,3]]).
% ->New_Ls = [[0, 2, 3], [0, 1, 2], [0, 1, 2, 3], [0, 1, 2, 3]]
    

generate_list([],[]).
generate_list([C|Csr],M):-
    generate_list(Csr,Md),
    C=constraint(U,V,_),
    append(Md,[(U,V),(V,U)],M).

% test(M):-queensConstraints(Cs),generate_list(Cs,M).
% M = [(3,4), (4,3), (2,4), (4,2), (2,3), (3,2), (1,4), (4,1), (1,3), (3,1), (1,2), (2,1)]

get_New_Ms([],Ms,_,Ms).
get_New_Ms([C|Csr],Ms,U,New_Ms):-
    get_New_Ms(Csr,Ms,U,New_Msd),
    (   
    (C=constraint(U,W,_);C=constraint(W,U,_)),
        (
        not(contains(New_Msd,(W,U))),
        New_Ms=[(W,U)|New_Msd]
        ;   
        contains(New_Msd,(W,U)),
        New_Ms=New_Msd
        )
    ;   
    not(C=constraint(U,W,_);C=constraint(W,U,_)),
        New_Ms=New_Msd
    ).
    
% getM( [ (2,4), (4,2), (2,3), (3,2), (1,4), (4,1), (1,3), (3,1), (1,2), (2,1)]).
% test(New_Ms):-queensConstraints(Cs),getM(Ms),get_New_Ms(Cs,Ms,3,New_Ms).
% M = [(4,3), (2,4), (4,2), (2,3), (3,2), (1,4), (4,1), (1,3), (3,1), (1,2), (2,1)]

do_while_loop([],_,Ls,Ls).
do_while_loop([M|Msr],Cs,Ls,New_Ls):-
    M=(U,V),
    rivise(Ls,Cs,U,V,Ls_d),
    apply(Ls,U,Du),
    apply(Ls_d,U,New_Du),
    (
    Du=New_Du,
        New_Ms=Msr
    ;   
    not(Du=New_Du),
        get_New_Ms(Cs,Msr,U,New_Ms)
    ),
	do_while_loop(New_Ms,Cs,Ls_d,New_Ls).

% makeArccons(Ds,Cs,Ls) :− ???
makeArccons(Ds,Cs,Ls,New_Ls):-% AC-3
    generate_list(Cs,Ms),%generates a list of varible pairs that shoud be check.
    do_while_loop(Ms,Cs,Ls,New_Ls),
    arrconsAll(Ds,New_Ls,Cs).
% --------------------------------------------


% --------------------------------------------
test1(New_Ls):-queensDomains(Ds),queensConstraints(Cs),testLs1(Ls),makeArccons(Ds,Cs,Ls,New_Ls).
testLs1([[0,1,2,3],[0,1,2,3],[0,1,2,3],[0,1,2,3]]).
% ?-test1(New_Ls).
% New_Ls = [[0, 1, 2, 3], [0, 1, 2, 3], [0, 1, 2, 3], [0, 1, 2, 3]]
test2(New_Ls):-queensDomains(Ds),queensConstraints(Cs),testLs2(Ls),makeArccons(Ds,Cs,Ls,New_Ls).
testLs2([[0,1,2,3],[0,1,2],[0,1,2,3],[1,2,3]]).
% ?-test2(New_Ls).
% New_Ls = [[0, 2, 3], [0, 1, 2], [0, 3], [1, 2, 3]]
test3(New_Ls):-queensDomains(Ds),queensConstraints(Cs),testLs3(Ls),makeArccons(Ds,Cs,Ls,New_Ls).
testLs3([[2,3],[0],[0,1,2,3],[2,3]]).
% ?-test3(New_Ls).
% New_Ls = [[], [], [], []]
% --------------------------------------------


