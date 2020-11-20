removeDuplicates([],[]).

removeDuplicates([X|R],A_0):-
							delete(R,X,R_without_X),
							removeDuplicates(R_without_X,A_1),
							append([X],A_1,A_0).