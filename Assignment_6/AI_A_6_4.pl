% We use a simple fragment of first−order logic 
% truth for truth 
% conj(F,G) for conjunction 
% univ(x,F) for universal quantification 
% applyF(p,L) for applications of predicate p (a string) to list L of terms 
% vr(x) for variables where x is a string 
% applyT(f,L) for applications of function f (a string) to list L of terms
% We represent substitutions as list S=[(x,t),...] where x is a variable and t a term 

% lookup(S,X,T) hold if S contains (X,T)
lookup([(X1,T1)|R],X,T):-
    (
    	not((X1=X,T1=T)),
        lookup(R,X,T)
        ;
        X1=X,T1=T
    ).
% test:-lookup([(x1,t1),(x2,t2)],x1,t1).
% true.


check_Variable(Variable,S,Variable_new):-
    lookup(S,Variable,vr(Variable_new))
    ;   
    not(lookup(S,Variable,_)),
    Variable_new=Variable.
% test(Variable_new):-check_Variable("x1",[("x1",vr("t1")),("x2",vr("t2"))],Variable_new).
% Variable_new = "t1".

% subsF(F,S,F2) holds if formula F2 arises from F via substitution S
subsF(univ(Variable,applyF(Predicate,Terms)),
      S,
      univ(Variable_new,applyF(Predicate,Terms_new))):-
    
    check_Variable(Variable,S,Variable_new),
    subTs(Terms,S,Terms_new).
    
% subsT(T,S,T2) holds if term T2 arises from T via substitution S 
subsT(vr(Term),S,Term_new):-
    lookup(S,Term,Term_new)
    ;   
    not(lookup(S,Term,_)),
    Term_new=vr(Term).

% subsTs(Ts,S,T2) holds list of terms Ts2 arises from Ts via substitution S
subTs([],_,[]).
subTs([Term|Tsr],S,Terms_new):-
    subTs(Tsr,S,Terms_new_d),
    subsT(Term,S,Term_new),
    Terms_new=[Term_new|Terms_new_d].


% test inputs: 
% 
% subsF(univ("x",applyF("p",[vr("x")])),[("x",vr("y"))],F). 
% -> F = univ("y", applyF("p", [vr("y")]))
% The first occurrence and second occurrence of "y" are the same Variable before and after the substitution,
% so there is NO variable capture.
% 
% subsF(univ("y",applyF("p",[vr("x")])),[("x",vr("y"))],F).
% -> F = univ("y", applyF("p", [vr("y")])).
% The first occurrence and second occurrence of "y" are the same Variable after the substitution,
% but before substitution they are not,
% so there is a variable capture.
% 
% subsF(univ("y",applyF("p",[vr("x")])),[("x",vr("z"))],F).
% -> F = univ("y", applyF("p", [vr("z")]))
% There is NO variable capture.