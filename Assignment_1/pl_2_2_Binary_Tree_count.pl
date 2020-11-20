count_nodes(nil,0).
count_nodes(tree(N,L,R),Num_node):-
                                    count_nodes(L,Num_node_L),
                                    count_nodes(R,Num_node_R),
                                    Num_node is 1+Num_node_L+Num_node_R.





count_leaves(tree(N,nil,nil),1).
count_leaves(nil,0).
count_leaves(tree(N,L,R),Num_node):-
                                    count_leaves(L,Num_node_L),
                                    count_leaves(R,Num_node_R),
                                    Num_node is Num_node_L+Num_node_R.
