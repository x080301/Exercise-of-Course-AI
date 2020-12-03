% we use edge(From,To,Distance) to represent edges
% a graph is a list of edges
% a path is a list of nodes
% node_in_search(Name,Path_from_F,Distance_from_F)


% finds the node with mininal distance from F, returns as Minimum_node.

minimum_in_List_unsearched(   [],Possible_node,Possible_node). % when all nodes are compared, the possible node is the Minimum_node.

minimum_in_List_unsearched( [node_in_search(_,_,Distance_from_F)|R],
                            node_in_search(Name_P,Path_from_F_P,Possible_minimum_distance),
                            Minimum_node):-

                                        Distance_from_F=infinite,
                                            minimum_in_List_unsearched(   R,
                                                                                    node_in_search(Name_P,Path_from_F_P,Possible_minimum_distance),
                                                                                    Minimum_node).


minimum_in_List_unsearched( [node_in_search(_,_,Distance_from_F)|R],
                            node_in_search(Name_P,Path_from_F_P,Possible_minimum_distance),
                            Minimum_node):-

                                        not(Distance_from_F=infinite),
                                        Distance_from_F>=Possible_minimum_distance,
                                            minimum_in_List_unsearched( R,
                                                                        node_in_search(Name_P,Path_from_F_P,Possible_minimum_distance),
                                                                        Minimum_node).

minimum_in_List_unsearched( [node_in_search(Name,Path_from_F,Distance_from_F)|R],
                            node_in_search(_,_,Possible_minimum_distance),
                            Minimum_node):-

                                        not(Distance_from_F=infinite),
                                        Distance_from_F<Possible_minimum_distance,
                                            minimum_in_List_unsearched( R,
                                                                        node_in_search(Name,Path_from_F,Distance_from_F), %replaces the possible node
                                                                        Minimum_node).


% delete searched node in List_unsearched 
% delete_node_in_LU(Minimum_node_name,List_unsearched,Undated_LU,Minimum_node):-

delete_node_in_LU(  Minimum_node_name,
                    [node_in_search(Name,Path_from_F,Distance_from_F)|R],
                    Undated_LU,
                    Minimum_node):-
                                                                                                                
                                Minimum_node_name=Name,

                                    Minimum_node = node_in_search(Name,Path_from_F,Distance_from_F),
                                    Undated_LU = R.

delete_node_in_LU(  Minimum_node_name,
                    [node_in_search(Name,Path_from_F,Distance_from_F)|R],
                    Undated_LU,
                    Minimum_node):-
                                                                                                                
                                not(Minimum_node_name=Name),

                                    delete_node_in_LU(  Minimum_node_name,
                                                        R,
                                                        Undated_LU_1,
                                                        Minimum_node_1),
                                
                                Minimum_node = Minimum_node_1,
                                append([node_in_search(Name,Path_from_F,Distance_from_F)],Undated_LU_1,Undated_LU).                       



% update one node according to Graph
% update_one_node(Graph,Minimum_node,Node,Updated_node)

is_edge(edge(Minimum_node_name,Name,_),Minimum_node_name,Name).
is_edge(edge(Name,Minimum_node_name,_),Minimum_node_name,Name).                            

update_one_node([],_,Node,Node).% when the edge to the Minimum_node exists not, Updated_node = Node.

update_one_node(    [edge(From,To,Distance)|_],
                    node_in_search( 
                                    Minimum_node_name,
                                    Minimum_node_path,
                                    Minimum_node_dstance),
                    node_in_search( Name,
                                    _,
                                    _),
                    Updated_node):-

                                is_edge(edge(From,To,Distance),Minimum_node_name,Name),

                                    append(Minimum_node_path,Name,New_Path_from_F),
                                    New_Distance_from_F is Minimum_node_dstance+Distance,
                                    Updated_node = node_in_search(Name,New_Path_from_F,New_Distance_from_F).

update_one_node(    [edge(From,To,Distance)|R],
                    node_in_search( 
                                    Minimum_node_name,
                                    Minimum_node_path,
                                    Minimum_node_dstance),
                    node_in_search( Name,
                                    Path_from_F,
                                    Distance_from_F),
                    Updated_node):-

                                not(is_edge(edge(From,To,Distance),Minimum_node_name,Name)),

                                    update_one_node(    R,
                                                        node_in_search( 
                                                                        Minimum_node_name,
                                                                        Minimum_node_path,
                                                                        Minimum_node_dstance),
                                                        node_in_search( Name,
                                                                        Path_from_F,
                                                                        Distance_from_F),
                                                        Updated_node).


% update distance value of nodes in List_unsearched
% update_node_in_LU(Graph,Minimum_node,List_unsearched,Updated_list_unsearched)

update_node_in_LU(_,_,[],[]).
update_node_in_LU(  Graph,
                    node_in_search( Minimum_node_name,
                                    Minimum_node_path,
                                    Minimum_node_dstance),
                    [Node|R],
                    Updated_list_unsearched):-      

                                                                        update_one_node(Graph,node_in_search(Minimum_node_name,Minimum_node_path,Minimum_node_dstance),Node,Updated_node),
                                                                        update_node_in_LU(Graph,Minimum_node_name,R,Updated_list_unsearched_0),% iteratively updates the rest of the list
                                                                        append([Updated_node],Updated_list_unsearched_0,Updated_list_unsearched).

% undates the 2 lists
% update_lists(Graph,List_searched,List_unsearched,Minimum_node,U_list_Searched,U_list_unsearched):-

update_lists(Graph,List_searched,List_unsearched,node_in_search(Minimum_node_name,Minimum_node_path,Minimum_node_dstance),U_list_Searched,U_list_unsearched):-

                                                                                delete_node_in_LU(Minimum_node_name,List_unsearched,list_unsearched_deleted,Minimum_node),
                                                                                append(List_searched,[Minimum_node],U_list_Searched),% add Minimum_node in List_searched
                                                                                update_node_in_LU(Graph,node_in_search( Minimum_node_name,Minimum_node_path,Minimum_node_dstance),List_unsearched,U_list_unsearched).
                                                                                




% search iteratively
% search_iteration(Graph,List_searched,List_unsearched,To_Node_Name,P)

search_iteration(_,_,[X_List_unsearched|R_List_unsearched],To_Node_Name,P):-

                                                                        minimum_in_List_unsearched(   [X_List_unsearched|R_List_unsearched],
                                                                                                      X_List_unsearched,
                                                                                                      node_in_search(Name,Path_from_F,_)),

                                                                        Name=To_Node_Name, % returns Path_from_F as P, when the goal node is reached
                                                                            P = Path_from_F.

search_iteration(Graph,List_searched,[X_List_unsearched|R_List_unsearched],To_Node_Name,P):-

                                                                        minimum_in_List_unsearched(   [X_List_unsearched|R_List_unsearched],
                                                                                                      X_List_unsearched,
                                                                                                      node_in_search(Name,Path_from_F,Distance_from_F)),
                                                                                                                
                                                                        not(Name=To_Node_Name), % undates the 2 lists and continues iteration, when the goal node is not reached
                                                                            
                                                                            update_lists(Graph,List_searched,[X_List_unsearched|R_List_unsearched],node_in_search(Name,Path_from_F,Distance_from_F),U_list_Searched,U_list_unsearched),                                                                          
                                                                            search_iteration(Graph,U_list_Searched,U_list_unsearched,To_Node_Name,P).


% constructs List_unsearched, which stores the unsearched nodes
% construct_list(Graph,List_unsearched_0,List_unsearched) 
% List_unsearched0 should consist of From_node_Name

% isinlist(List_unsearched,Node)
isinlist([Node|_],Node).
isinlist([_|R],Node):-isinlist(R,Node).

% one_node_in(List0,List1,Node)
one_node_in(List0,List1,Node):-
                                isinlist(List0,Node),
                                    List1 = List0.
one_node_in(List0,List1,Node):-
                                not(isinlist(List0,Node)),
                                    append(List0,[Node],List1).

construct_list([],List_unsearched_0,List_unsearched_0).
construct_list([edge(From,To,_)|R],List_unsearched_0,List_unsearched):-
                                                                            
                                                                    From_node = node_in_search(From,[],infinite),
                                                                    To_Node = node_in_search(To,[],infinite),
                                                                    one_node_in(List_unsearched_0,List_unsearched_1,From_node),
                                                                    one_node_in(List_unsearched_1,List_unsearched_2,To_Node),

                                                                    construct_list(R,List_unsearched_2,List_unsearched).

% finds a path P from F to T using edges in Es

search(Es,F,T,P):-
                % constructs 2 lists:
                construct_list(Es,[node_in_search(F,[],infinite)],[_|List_unsearched]),
                List_searched = [node_in_search(F,[],0)],

                % search iteratively
                search_iteration(Es,List_searched,List_unsearched,T,P). % iterative search. P is the shortest found path.


% Test
%-----------------------------------------------------------------------------------------------------------

% ?-search([edge(a,b,15),edge(a,c,10),edge(a,c,6),edge(b,d,6)],a,d,P).
% 
% P=[a,b,d]