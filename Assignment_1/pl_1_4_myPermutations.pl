myPermutations([],[]).

myPermutations(X,P_0):-
						member(Y,X),
						select(Y,X,R),
						myPermutations(R,P_1),
						append([Y],P_1,P_0).