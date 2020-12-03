% tree(V,TS) represents a tree.
% V must be a string - the label/value/data V of the root node
% TS must be a list of trees - the children/subtrees of the root node
% In particular, a leaf node is a tree with the empty list of children

istree(tree(V,TS)) :- string(V), istreelist(TS).

% istreelist(TS) holds if TS is a list of trees.
% Note that the children are a list not a set, i.e., they are ordered.
istreelist([]).
istreelist([T|TS]) :- istree(T), istreelist(TS).

% The following predicates define search algorithms that take a tree T
% and visit every node each time writing out its label on a new line.

% dfs(T) visits every node in depth-first order
% dfs(T) :- ???
% bfs(T) visits every node in beadth-first order
% dfs(T) :- ???
% itd(T):- visits every node in iterative deepening order
% itd(T) :- ???

dfs(T):-
    a_dfs([T],0).

a_dfs([],_D).

a_dfs([tree(V,Tchildren)| Tsiblings ],D):-
    write(D),
    write(":"),
    write(V),
    nl,
    D2 is D+1,
    a_dfs(Tchildren,D2),
    a_dfs(Tsiblings,D).

%%%%%%%%%

print_gen([],_).
print_gen([tree(V,_)|Tsiblings],D):-
    write(D),
    write(":"),
    write(V),
	nl,
   	print_gen(Tsiblings,D).

bfs3(T):-
    print_gen([T],0),
    bfs_a3([T],0).

bfs_a3([],_D).
bfs_a3([tree(V,Tchildren)|Tsiblings],D):-
    D2 is D+1,
    print_children([tree(V,Tchildren)],D2),
    bfs_a3(Tsiblings,D),
    bfs_a3(Tchildren,D2).

print_children([],_D).
print_children([tree(_V,Tchildren)|Tsiblings],D):-
    print_gen(Tchildren,D),
    print_children(Tsiblings,D).

% itd(T):- visits every node in iterative deepening order

count_depth([],0).
count_depth(tree(_,[]))
count_depth(tree(N,[L,R]),Depth):-
                            count_depth(L,DL),
                            count_depth(R,DR),
                            DL>=DR,
                            Depth=DL+1.

count_depth(tree(N,[L,R]),Depth):-
                            count_depth(L,DL),
                            count_depth(R,DR),
                            DL<DR,
                            Depth=DR+1.                        




%tree("A",[tree("B",[tree("D",[]), tree("E",[tree("F",[]),tree("G",[])])]),tree("C",[])])

%%%%%%%%%%%%%%%%%%%%
%GREEDY SEARCH