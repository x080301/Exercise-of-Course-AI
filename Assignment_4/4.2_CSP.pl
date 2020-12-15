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
isTotal([V|Vs]) :- not(V==none), isTotal(Vs).

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

% counts members of a list
count([],0).
count([_|R],Num):-
    count(R,Num_d),
    Num is Num_d+1.

% puts something on the Idth place of the list.
put([X|R],Value,Id,Idl,Listout):-
    (
      Id=Idl,
      Listout=[Value|R]
      ;
      not(Id=Idl),
      Idld is Idl+1,
      put(R,Value,Id,Idld,Listout_d),
      Listout=[X|Listout_d]
    ).
put(List_in,Value,Id,List_out):-put(List_in,Value,Id,1,List_out).

% Number of members in a contrains list
count_member_with_Xa(_,[],0).
count_member_with_Xa(Xa,[X|R],Number_members):-
    count_member_with_Xa(Xa,R,Number_members_d),
    (
      X=(Xa,_),Number_members is Number_members_d + 1
      ;
      not(X=(Xa,_)),Number_members is Number_members_d
    ).

count_member_with_Xb(_,[],0).
count_member_with_Xb(Xb,[X|R],Number_members):-
    count_member_with_Xb(Xb,R,Number_members_d),
    (
      X=(_,Xb),Number_members is Number_members_d + 1
      ;
      not(X=(_,Xb)),Number_members is Number_members_d
    ).


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


% count the possible value pairs

count_in_contrains(_,_,_,[],0).
count_in_contrains(X1,Id1,Id2,[C|Csr],Number_possible_value_pairs):-
    count_in_contrains(X1,Id1,Id2,Csr,Number_possible_value_pairs_d),
    (
      C=constraint(Id1,Id2,Pairs),count_member_with_Xa(X1,Pairs,Number_members_1)
      ;
      not(C=constraint(Id1,Id2,_)),Number_members_1 is 0
    ),
    (
      C=constraint(Id2,Id1,Pairs),count_member_with_Xb(X1,Pairs,Number_members_2)
      ;
      not(C=constraint(Id2,Id1,_)),Number_members_2 is 0
    ),
    Number_possible_value_pairs is Number_possible_value_pairs_d+Number_members_1+Number_members_2.

count_possible_value_pairs(_,_,_,_,[],0).
count_possible_value_pairs(X1,Id1,Id2,Cs,[V|Vsr],Number_possible_value_pairs):-
    Id2d is Id2+1,
    count_possible_value_pairs(X1,Id1,Id2d,Cs,Vsr,Number_possible_value_pairs_d),
    (
      V==none,count_in_contrains(X1,Id1,Id2,Cs,Number_possible_value_pairs_0)
      ;
      not(V==none),Number_possible_value_pairs_0 is 0
    ),
    Number_possible_value_pairs is Number_possible_value_pairs_0+Number_possible_value_pairs_d.

% gets a list of Constraining Values

get_cv_list([],_,_,_,[]).
get_cv_list([X1|Xsr],Id1,Cs,Vs,List_cv):-
    
    get_cv_list(Xsr,Id1,Cs,Vs,List_cv_d),

    count_possible_value_pairs(X1,Id1,1,Cs,Vs,Number_possible_value_pairs),

    append([Number_possible_value_pairs],List_cv_d,List_cv).
  
% order the list of cv.


find_max([],[],_,_,PMax_id,PMax_id).
find_max([Cv|Lcvr],List_cv_set_max_m1,Pmax,Id,PMax_id,Max_id):-
    Idd is Id +1,
    (
      Cv>Pmax,PMax_idd=Id,Pmaxd=Cv,
      find_max(Lcvr,List_cv_set_max_m1_d,Pmaxd,Idd,PMax_idd,Max_id)
      ;
      not(Cv>Pmax),
      find_max(Lcvr,List_cv_set_max_m1_d,Pmax,Idd,PMax_id,Max_id)
    ),
    (
      Id=Max_id,append([-1],List_cv_set_max_m1_d,List_cv_set_max_m1)
      ;
      not(Id=Max_id),append([Cv],List_cv_set_max_m1_d,List_cv_set_max_m1)
    ).

finished_order([-1]).
finished_order([-1|R]):-finished_order(R).

order_cv_list(List_cv,[]):-finished_order(List_cv).
order_cv_list(List_cv,Ordered_lcv_value):-

    not(finished_order(List_cv)),
    find_max(List_cv,List_cv_set_max_m1,-1,1,_,Max_id),

    order_cv_list(List_cv_set_max_m1,Ordered_lcv_value_d),

    Max_id_m1 is Max_id-1,
    append([Max_id_m1],Ordered_lcv_value_d,Ordered_lcv_value).



        

generate_list(_,_,_,[],[]).
generate_list(Id,Idl,Num,[H|Vsr],Vsout):-
    Idld is Idl+1,
    generate_list(Id,Idld,Num,Vsr,Vsoutd),

    (
      Idl==Id,Vsout=[Num|Vsoutd]
      ;
      not(Idl==Id),Vsout=[H|Vsoutd]
    ). 

calculate_rv([],_,_,_,0).
calculate_rv([X|Xsr],Id,Cs,Vs,Rv):-
    calculate_rv(Xsr,Id,Cs,Vs,Rvd),

    generate_list(Id,1,X,Vs,Vsout),
    

    (
      consistent(Cs,Vsout),Rv is Rvd+1
      ;
      not(consistent(Cs,Vsout)),Rv is Rvd
    ).

% get a list of flags. Where remaining values = MRV, sets flag 1. Where not sets 0.
get_mrv([],_,_,_,Pmrv,Pmrv,[]).
get_mrv([L|Dsr],Id,Cs,Vs,Pmrv,Mrv,Listout):-
    
    apply(Vs,Id,C),

    

    Idd is Id+1,
    (
      not(integer(C)),    
      domain(L,Xs),
      calculate_rv(Xs,Id,Cs,Vs,Rv),

      (
        Rv<Pmrv,Pmrv_d is Rv
        ;
        not(Rv<Pmrv),Pmrv_d is Pmrv
      ),

      get_mrv(Dsr,Idd,Cs,Vs,Pmrv_d,Mrv,Listout_d),

      (
        Rv==Mrv,Listout=[1|Listout_d]
        ;
        not(Rv==Mrv),Listout=[0|Listout_d]
      )
      ;
      integer(C),

      get_mrv(Dsr,Idd,Cs,Vs,Pmrv,Mrv,Listout_d),
      Listout=[0|Listout_d]
      
    ).
    
% caculates degrees
caculate_degree([],_,0).
caculate_degree([C|Csr],Id,Degree):-

    caculate_degree(Csr,Id,Degree_d),
    
    (
      (C=constraint(Id,_,L);C=constraint(_,Id,L)),
      count(L,Lnum),
      Degree is Degree_d+16-Lnum
      ;
      not(C=constraint(Id,_,_);C=constraint(_,Id,_)),
      Degree is Degree_d
    ).

% caculates degrees where flag is 1, and returns id of the maxdegree.
get_degree([],_,_,-1,-1).
get_degree([Flag|Flagesr],Cs,Id,Pmaxdegree,Pmaxid):-
    
    Idd is Id+1,
    get_degree(Flagesr,Cs,Idd,Pmaxdegree_d,Pmaxid_d),
    (
      Flag=1,
      caculate_degree(Cs,Id,Degree),
      (        
        Degree>Pmaxdegree_d,
        Pmaxdegree=Degree,
        Pmaxid=Id
        ;
        not(Degree>Pmaxdegree_d),
        Pmaxdegree=Pmaxdegree_d,
        Pmaxid=Pmaxid_d
      )
      ;
      Flag=0,
      Pmaxdegree=Pmaxdegree_d,
      Pmaxid=Pmaxid_d
    ).
    
% chooses a variable according to mrv and degree
set_new_state([],_,_,[]).
set_new_state([V|Vsr],Id,Max_id,State2):-
    Idd is Id+1,
    set_new_state(Vsr,Idd,Max_id,State2_d),
    (
      Id=Max_id,
      State2=[Empty_V|State2_d]
      ;
      not(Id=Max_id),
      State2=[V|State2_d]
    ).


caculate_cv_in_Xs(_,[],_,_,0).
caculate_cv_in_Xs(Cs,[X|Xsr],Vs,Id,Num):-
    caculate_cv_in_Xs(Cs,Xsr,Vs,Id,Num_d),
    put(Vs,X,Id,New_Vs),
    (
      consistent(Cs,New_Vs),
      Num is Num_d+1
      ;
      not(consistent(Cs,New_Vs)),
      Num is Num_d
    ).

caculate_cv_in_Vs(_,[],_,[],_,0).
caculate_cv_in_Vs(Cs,[L|Dsr],Vs,[V|Vsr],Id,Num):-
    Idd is Id+1,
    caculate_cv_in_Vs(Cs,Dsr,Vs,Vsr,Idd,Num_d),
    (
      integer(V),
      Num is Num_d
      ;
      not(integer(V)),
      domain(L,Xs),
      caculate_cv_in_Xs(Cs,Xs,Vs,Id,Num_m),
      Num is Num_m+Num_d
    ).

caculate_cv_in_Vs(Cs,Ds,Vs,Num):-
    caculate_cv_in_Vs(Cs,Ds,Vs,Vs,1,Num).

get_cv_list(_,_,[],_,_,[]).
get_cv_list(Cs,Ds,[X|Xsr],Vs,Id,List_out):-
    get_cv_list(Cs,Ds,Xsr,Vs,Id,List_out_d),
    
    put(Vs,X,Id,New_Vs),
    caculate_cv_in_Vs(Cs,Ds,New_Vs,Num),
    List_out=[Num|List_out_d].



% chooses a value according to lcv
% the value shoud rules out the fewest values in the remaining variables,
% in other words most possibole values in the remaining variables,

lCv(Cs,Ds,[L|Dsr],Vs,[V|Vsr],Id):-
    (
      (integer(V);atom(V)),
      Idd is Id+1,
      lCv(Cs,Ds,Dsr,Vs,Vsr,Idd)
      ;
      not(integer(V);atom(V)),
        domain(L,Xs),
        get_cv_list(Cs,Ds,Xs,Vs,Id,Listout),
        
        order_cv_list(Listout,Ordered_cv),
        contains(Ordered_cv,V)
    ).

lCv(Cs,Ds,Vs):-lCv(Cs,Ds,Ds,Vs,Vs,1).


%state [none,none,2,none] means you randomly chose field 2 for the 3rd row as a start
%state2[none,1,2,none]<-State2 = [none, Empty_value, 2, none], the Empty value will be set in lCv

mrv(Ds,Cs,State,State2):-
    get_mrv(Ds,1,Cs,State,10000,_,Listout),% get a list of flags. Where Rv=Mrv, sets flag 1; where not, sets 0.
    get_degree(Listout,Cs,1,_,Max_id), % caculates degree where flag is 1, and returns the id of the maxdegree.
    set_new_state(State,1,Max_id,State2) % chooses a variable according to mrv and degree    
    .%write(State2),nl.
    

loops(Ds,Cs,Vs,Vs_u):-
    
    
    mrv(Ds,Cs,Vs,Vs_d),
    !,
    lCv(Cs,Ds,Vs_d),
  
    
    isAssignment(Ds,Vs_d),
 
    
    consistent(Cs,Vs_d),

    
    (
      isTotal(Vs_d),   
      Vs_u=Vs_d     
      ;
      not(isTotal(Vs_d)),
      loops(Ds,Cs,Vs_d,Vs_u)
    ).

%solve(Ds,Cs,Vs):- random(0,4,N), mrv(Cs,[none, N, none, none], Vs), lCv(Ds,Cs,Vs), isAssignment(Ds,Vs), isTotal(Vs), consistent(Cs,Vs).
solve(Ds,Cs,Vs):-contains([0,1,2,3],N),loops(Ds,Cs,[none, N, none, none],Vs).
test(Vs):-queensDomains(Ds),queensConstraints(Cs),solve(Ds,Cs,Vs).

testR(Vs):-queensDomains(Ds),queensConstraints(Cs),random(0,4,N),loops(Ds,Cs,[none, N, none, none],Vs).

% --------------------------
% test 
% --------------------------
% ?- test(Vs).
% Vs = [2, 0, 3, 1] ;
% Vs = [2, 0, 3, 1] ;
% Vs = [1, 3, 0, 2] ;
% Vs = [1, 3, 0, 2] ;
% --------------------------
% ?- queensDomains(Ds),queensConstraints(Cs),mrv(Ds,Cs,[none,none,2,none],State2).
% ...
% State2 = [none, _12610, 2, none] .

% _12610(V) is bonded to an integer in lCv
% --------------------------
% ?- queensDomains(Ds),queensConstraints(Cs),lCv(Cs,Ds,[none, V, 2, none]).
% ...
% V = 3 ;
% ...
% V = 0 ;
% ...
% V = 1 ;
% ...
% V = 2 ;
% --------------------------


