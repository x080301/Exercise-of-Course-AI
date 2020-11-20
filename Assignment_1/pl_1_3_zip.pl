zip([],_,[]).
zip(_,[],[]).

zip([X1|R1],[X2|R2],List_result_0):-
									zip(R1,R2,List_result_1),
									append([[X1,X2]],List_result_1,List_result_0).