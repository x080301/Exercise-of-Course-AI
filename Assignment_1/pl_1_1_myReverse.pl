myReverse([],[]).

myReverse([X|R],L):-
					myReverse(R,S),
					append(S,[X],L).