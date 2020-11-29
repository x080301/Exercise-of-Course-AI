% we use edge(From,To,Distance) to represent edges
% a graph is a list of edges
% a path is a list of nodes
% node_in_search(Name,Path_from_F,Distance_from_F)


% finds the node with mininal distance from F, returns as Minimum_node.

minimum_in_List_unsearched(   [],Possible_node,Possible_node). % when all nodes are compared, the possible node is the Minimum_node.

minimum_in_List_unsearched( [node_in_search(Name,Path_from_F,Distance_from_F)|R],
                            node_in_search(Name_P,Path_from_F_P,Possible_minimum_distance),
                            Minimum_node):-

                                        Distance_from_F=inf,
                                            minimum_in_List_unsearched(   R,
                                                                                    node_in_search(Name_P,Path_from_F_P,Possible_minimum_distance),
                                                                                    Minimum_node).


minimum_in_List_unsearched( [node_in_search(Name,Path_from_F,Distance_from_F)|R],
                            node_in_search(Name_P,Path_from_F_P,Possible_minimum_distance),
                            Minimum_node):-

                                        Distance_from_F><inf,
                                        Distance_from_F>=Possible_minimum_distance,
                                            minimum_in_List_unsearched(   R,
                                                                                    node_in_search(Name_P,Path_from_F_P,Possible_minimum_distance),
                                                                                    Minimum_node).

minimum_in_List_unsearched( [node_in_search(Name,Path_from_F,Distance_from_F)|R],
                            node_in_search(Name_P,Path_from_F_P,Possible_minimum_distance),
                            Minimum_node):-

                                        Distance_from_F><inf,
                                        Distance_from_F<Possible_minimum_distance, 
                                            minimum_in_List_unsearched(   R,
                                                                                    node_in_search(Name,Path_from_F,Distance_from_F), %replaces the possible node
                                                                                    Minimum_node).


% delete searched node in List_unsearched 
% delete_node_in_LU(Minimum_node_name,List_unsearched,Undated_LU,Minimum_node):-

delete_node_in_LU(  Minimum_node_name,
                    [node_in_search(Name,Path_from_F,Distance_from_F)|R],
                    Undated_LU,
                    Minimum_node):-
                                                                                                                
                                Minimum_node_name=Name,

                                    Minimum_node is node_in_search(Name,Path_from_F,Distance_from_F),
                                    Undated_LU is R.

delete_node_in_LU(  Minimum_node_name,
                    [node_in_search(Name,Path_from_F,Distance_from_F)|R],
                    Undated_LU,
                    Minimum_node):-
                                                                                                                
                                Minimum_node_name><Name,

                                    delete_node_in_LU(  Minimum_node_name,
                                                        R,
                                                        Undated_LU_1,
                                                        Minimum_node_1),
                                
                                Minimum_node is Minimum_node_1,
                                append([node_in_search(Name,Path_from_F,Distance_from_F)],Undated_LU_1,Undated_LU).                       



% update one node according to Graph
% update_one_node(Graph,Minimum_node_name,Minimum_node_path,Node,Updated_node)

is_edge(edge(Minimum_node_name,Name,_),Minimum_node_name,Name).
is_edge(edge(Name,Minimum_node_name,_),Minimum_node_name,Name).                            

update_one_node([],_,_,Node,Node).% when the edge to the Minimum_node exists not, Updated_node is Node.

update_one_node(    [edge(From,To,Distance)|R],
                    Minimum_node_name,
                    Minimum_node_path,
                    node_in_search( Name,
                                    Path_from_F,
                                    Distance_from_F),
                    Updated_node):-

                                is_edge(edge(From,To,Distance),Minimum_node_name,Name),

                                    append(Minimum_node_path,Name,New_Path_from_F),
                                    New_Distance_from_F=Distance_from_F+Distance,
                                    Updated_node is node_in_search(Name,New_Path_from_F,New_Distance_from_F).

update_one_node(    [edge(From,To,Distance)|R],
                    Minimum_node_name,
                    Minimum_node_path,
                    node_in_search( Name,
                                    Path_from_F,
                                    Distance_from_F),
                    Updated_node):-

                                not(is_edge(edge(From,To,Distance),Minimum_node_name,Name)),

                                    update_one_node(    R,
                                                        Minimum_node_name,
                                                        node_in_search( Name,
                                                                        Path_from_F,
                                                                        Distance_from_F),
                                                        Updated_node).


% update distance value of nodes in List_unsearched
% update_node_in_LU(Graph,Minimum_node_name,Minimum_node_path,List_unsearched,Updated_list_unsearched)

update_node_in_LU(_,_,_,[],[]).
update_node_in_LU(Graph,Minimum_node_name,Minimum_node_path,[Node|R],Updated_list_unsearched):-      

                                                                        update_one_node(Graph,Minimum_node_name,Minimum_node_path,Node,Updated_node),
                                                                        update_node_in_LU(Graph,Minimum_node_name,R,Updated_list_unsearched_0),% iteratively updates the rest of the list
                                                                        append([Updated_node],Updated_list_unsearched_0,Updated_list_unsearched).

% undates the 2 lists
% update_lists(Graph,List_searched,List_unsearched,Minimum_node_name,Minimum_node_path,U_list_Searched,U_list_unsearched):-

update_lists(Graph,List_searched,List_unsearched,Minimum_node_name,Minimum_node_path,U_list_Searched,U_list_unsearched):-

                                                                                delete_node_in_LU(Minimum_node_name,List_unsearched,list_unsearched_deleted,Minimum_node),
                                                                                append(List_searched,[Minimum_node],U_list_Searched),% add Minimum_node in List_searched
                                                                                update_node_in_LU(Graph,Minimum_node_name,Minimum_node_path,List_unsearched,U_list_unsearched)
                                                                                




% search iteratively
% search_iteration(Graph,List_searched,List_unsearched,To_Node_Name,P)

search_iteration(Graph,List_searched,[X_List_unsearched|R_List_unsearched],To_Node_Name,P):-

                                                                        minimum_in_List_unsearched(   [X_List_unsearched|R_List_unsearched],
                                                                                                      X_List_unsearched,
                                                                                                      node_in_search(Name,Path_from_F,Distance_from_F)),

                                                                        Name=To_Node_Name, % returns Path_from_F as P, when the goal node is reached
                                                                            P is Path_from_F.

search_iteration(Graph,List_searched,[X_List_unsearched|R_List_unsearched],To_Node_Name,P):-

                                                                        minimum_in_List_unsearched(   [X_List_unsearched|R_List_unsearched],
                                                                                                      X_List_unsearched,
                                                                                                      node_in_search(Name,Path_from_F,Distance_from_F)),
                                                                                                                
                                                                        Name><To_Node_Name, % undates the 2 lists and continues iteration, when the goal node is not reached
                                                                            
                                                                            update_lists(Graph,List_searched,[X_List_unsearched|R_List_unsearched],Name,Path_from_F,U_list_Searched,U_list_unsearched),                                                                          
                                                                            search_iteration(Graph,U_list_Searched,U_list_unsearched,To_Node_Name,P).


% constructs List_unsearched, which stores the unsearched nodes
% construct_list(Graph,List_unsearched_0,List_unsearched) 
% List_unsearched0 should consist of From_node_Name

% isinlist(List_unsearched,Node_name)
isinlist([Node_name|_],Node_name).
isinlist([X|R],Node_name):-isinlist(R,Node_name).

% one_node_in(List0,list1,Node_name)
one_node_in(List0,list1,Node_name):-
                                isinlist(List0,Node_name),
                                    list1 is List0.
one_node_in(List0,list1,Node_name):-
                                not(isinlist(List0,Node_name)),
                                    append(List0,[Node_name],list1).

construct_list([],List_unsearched_0,List_unsearched_0).
construct_list([edge(From,To,Distance)|R],List_unsearched_0,List_unsearched):-

                                                                            one_node_in(List_unsearched_0,List_unsearched_1,From),
                                                                            one_node_in(List_unsearched_1,List_unsearched_2,To),

                                                                            construct_list(R,List_unsearched_2,List_unsearched).

% finds a path P from F to T using edges in Es

search(Es,F,T,P):-
                % constructs 2 lists:
                construct_list(Es,[F],[_|List_unsearched]),
                List_searched is [node_in_search(F,[],0)],

                search_iteration(Es,List_searched,List_unsearched,T,P). % iterative search. P is the shortest found path.

