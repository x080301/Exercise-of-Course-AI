% Problem 1.2.1: Construction of binary tree 
%-----------------------------------------------------------------------------------------------------------

% function:
% distribution of the list. The list is divided 1 head and 2 sublist: list for left tree, list for right tree.
list_distri(_,[],[],[]).
list_distri(X0,[X1|R],Llist,Rlist):-
                                    X0<X1,% Numbers smaller than the head are in the left tree.
                                        list_distri(X0,R,Llist,Rlist0),
                                        append([X1],Rlist0,Rlist).
list_distri(X0,[X1|R],Llist,Rlist):-
                                    X0>=X1,% Numbers larger than the head are in the right tree.
                                        list_distri(X0,R,Llist0,Rlist),
                                        append([X1],Llist0,Llist).

%construction of the tree
construct([],nil):-!.
construct([X|R],tree(X,Ltree,Rtree)):-% creat a certain node, in which the head of the list is stored.
                                        list_distri(X,R,Llist,Rlist),% Distribution of the list.
                                        construct(Llist,Ltree),% Recursively create the left tree
                                        construct(Rlist,Rtree).% Recursively create the right tree

%************
%example invocations:
/*
A.
?- construct([3,2,4,1,5],T).

Result:
T = tree(3, tree(2, tree(1, nil, nil), nil), tree(4, nil, tree(5, nil, nil))) 

B.
?- construct([3,1,3,4,2,4,1,5],T).

Result:
T = tree(3, tree(1, tree(1, nil, nil), tree(3, tree(2, nil, nil), nil)), tree(4, tree(4, nil, nil), tree(5, nil, nil))) 
*/


% Problem 1.2.2: Count nodes and leaves
%-----------------------------------------------------------------------------------------------------------

count_nodes(nil,0).% When the stored data is nil, end recursion
count_nodes(tree(N,L,R),Num_node):- %count the nodes
                                    count_nodes(L,Num_node_L),% Recursively count the nodes of the left tree
                                    count_nodes(R,Num_node_R),% Recursively count the nodes of the right tree
                                    Num_node is 1+Num_node_L+Num_node_R.% Number of nodes of the whole tree is sum of that of the left childtree, of right, and itself.


count_leaves(tree(N,nil,nil),1).% When both left and right childtree of a node is nil, the node is a leaf. End recursion
count_leaves(nil,0).% not node, end recursion
count_leaves(tree(N,L,R),Num_node):-% count the leaves
                                    count_leaves(L,Num_node_L), % Recursively count the leaves of the left tree
                                    count_leaves(R,Num_node_R), % Recursively count the leaves of the left tree
                                    Num_node is Num_node_L+Num_node_R.% Number of leaves of the whole tree is sum of that of the left and right.

%************
%example invocations:
/*
?- count_nodes(tree(3, tree(2, tree(1, nil, nil), nil), tree(4, nil, tree(5, nil, nil))),Number). --------> Number = 5
?- count_leaves(tree(3, tree(2, tree(1, nil, nil), nil), tree(4, nil, tree(5, nil, nil))),Number).--------> Number = 2 
*/


% Problem 1.2.4:  Whether a binary tree is symmetric
%-----------------------------------------------------------------------------------------------------------



% Problem 1.2.4:  Whether a binary tree is symmetric
%-----------------------------------------------------------------------------------------------------------

% Let us call a binary tree symmetric if you can draw a vertical 
% line through the root node and then the right subtree is the mirror
% image of the left subtree.
% Write a predicate symmetric/1 to check whether a given binary
% tree is symmetric. Hint: Write a predicate mirror/2 first to check
% whether one tree is the mirror image of another.

% symmetric(T) :- the binary tree T is symmetric.

tree(nil).
tree(t(_,L,R)) :- tree(L),tree(R).

symmetric(nil).
symmetric(t(_,L,R)) :- mirror(L,R).

mirror(nil,nil).
mirror(t(_,L1,R1),t(_,L2,R2)) :- mirror(L1,R2), mirror(R1,L2).


%************
%example invocations:
/*
?- symmetric(t(a,t(b,nil,nil),nil)).												--------> false.
?- symmetric(t(a,t(b,t(c,nil,nil),t(d,nil,nil)),t(b,t(d,nil,nil),t(d,nil,nil)))).	--------> true.
*/