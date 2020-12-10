
% -----------------------------------------------------------
% datas
% -----------------------------------------------------------

% edge(From,To,Distance)            ->  edge
% node(Name,Distance2F,Path2F)      ->  node 
% a graph is a list of edges
% a path is a list of nodes




% -----------------------------------------------------------
% tool facts
% -----------------------------------------------------------

get_node_name(node(Name,_,_),Name).
get_node_distance2F(node(_,Distance2F,_),Distance2F).
get_node_path2F(node(_,_,Path2F),Path2F).
get_edge_node(edge(From,To,_),From,To).




% -----------------------------------------------------------
% search_in_not_reached_points(List_not_reached_nodes,Possible_Min_distance,Min_distance,Nearst_point,Updated_list)
% finds out the node with Min_distance in the list, and deletes it from that list.
% input List_not_reached_nodes,Possible_Min_distance
% returns Min_distance,Nearst_node,Updated_list
% -----------------------------------------------------------

% when the whole list is searched, Possible_Min_distance is the Min_distance.
search_in_not_reached_points([],Possible_Min_distance,Possible_Min_distance,none,[]).

search_in_not_reached_points([X|R],Possible_Min_distance,Min_distance,Nearst_node,Updated_list):-

    get_node_distance2F(X,X_Distance2F),

    % when Possible_Min_distance>X_Distance2F, replaces it with X_Distance2F.
    
    (
    X_Distance2F=infinite,
        Possible_Min_distance_0=Possible_Min_distance
    ;
    not(X_Distance2F=infinite),
        (
            Possible_Min_distance=none,
                Possible_Min_distance_0=X_Distance2F
            ;
            
            not(Possible_Min_distance=none),
            (
                (Possible_Min_distance>X_Distance2F),
                    Possible_Min_distance_0=X_Distance2F
                
                ;
                
                (Possible_Min_distance=<X_Distance2F),
                    Possible_Min_distance_0=Possible_Min_distance
            )
        )
    ),
    
    % iteration
    search_in_not_reached_points(R,Possible_Min_distance_0,Min_distance_0,Nearst_node_0,Updated_list_0),

    (
        Nearst_node_0=X,

            Nearst_node=Nearst_node_0,
            Min_distance=Min_distance_0,
            Updated_list=Updated_list_0 % delete X from the list
        ;
        not(Nearst_node_0=X),

            (    
            Min_distance_0=X_Distance2F, % when X is the node with Min_distance:

                Nearst_node=X,
                Min_distance=none_0, % avoid multiple minimum
                Updated_list=Updated_list_0 % delete X from the list
                
                ;

            not(Min_distance_0=X_Distance2F),
                
                Nearst_node=Nearst_node_0,
                Min_distance=Min_distance_0,
                Updated_list=[X|Updated_list_0]
            )
    ).



% -----------------------------------------------------------
% init(Es,F,List_not_reached_nodes)
% loads the graph, returns List_not_reached_nodes
% -----------------------------------------------------------

init([],_,[]).
init([X|R],F,List_not_reached_nodes):-

    init(R,F,List_not_reached_nodes_0),

    asserta(X),% loads the graph to the memory.

    (
        (get_edge_node(X,F,N);get_edge_node(X,N,F)),
            List_not_reached_nodes=[node(F,0,[F]),node(N,infinite,none)|List_not_reached_nodes_0]
    ;
        not(get_edge_node(X,F,_);get_edge_node(X,_,F)),
            get_edge_node(X,N_1,N_2),
            List_not_reached_nodes=[node(N_1,infinite,none),node(N_2,infinite,none)|List_not_reached_nodes_0]
    ).
    
  



% -----------------------------------------------------------
% update_not_reached_nodes(List_not_reached_nodes,Nearst_node,Updated_list)
% returns Updated_list
% -----------------------------------------------------------

update_not_reached_nodes([],_,[]).
update_not_reached_nodes([X|R],Nearst_node,Updated_list):-

    update_not_reached_nodes(R,Nearst_node,Updated_list_0),
    
    get_node_distance2F(X,X_Distance2F),
    get_node_name(X,X_name),
    get_node_name(Nearst_node,N_name),

    (
        
         
        (edge(X_name,N_name,Distance);edge(N_name,X_name,Distance)),
        

            get_node_distance2F(Nearst_node,Distance2F),
            get_node_path2F(Nearst_node,Path2F),

            New_distance2F is Distance2F+Distance,
            (
                
                not(X_Distance2F=infinite),
                X_Distance2F=<New_distance2F,

                    Updated_list=[X|Updated_list_0]
                ;
                
                not(X_Distance2F=infinite),
                X_Distance2F>New_distance2F,

                    append(Path2F,[X_name],New_path2F),
                    New_x=node(X_name,New_distance2F,New_path2F),

                    Updated_list=[New_x|Updated_list_0]

                ;

                X_Distance2F=infinite,

                    append(Path2F,[X_name],New_path2F),
                    New_x=node(X_name,New_distance2F,New_path2F),

                    Updated_list=[New_x|Updated_list_0]


            )
        ;

        not((edge(X_name,N_name,Distance);edge(N_name,X_name,Distance))),

            Updated_list=[X|Updated_list_0]
    ).
    
  



% -----------------------------------------------------------
% retract all datas in database, or the function search(Es,F,T,P) can be used only once.
% -----------------------------------------------------------
retract_all:-

    retract(get_list(_)),
    repeat,
    retract(edge(_,_,_)),
    not(edge(_,_,_)).



    
  



% -----------------------------------------------------------
% search(Es,F,T,P) finds a path P from F to T using edges in Es
% -----------------------------------------------------------

goal_path_is(P,node(T,_,P),T). % name of Nearst_node = T -> T node reached -> returns Path2F of Nearst_node as P.

loops(T,P):-

    repeat,

    get_list(List_not_reached_nodes),
    search_in_not_reached_points(List_not_reached_nodes,none,_,Nearst_node,List_without_nearst_node),

    retract(get_list(_)),
    update_not_reached_nodes(List_without_nearst_node,Nearst_node,Updated_list),
    asserta(get_list(Updated_list)),


    goal_path_is(P,Nearst_node,T).


search(Es,F,T,P):-

    init(Es,F,List_not_reached_nodes),
    asserta(get_list(List_not_reached_nodes)),

    loops(T,P),

    retract_all.


% -----------------------------------------------------------
% tests
% -----------------------------------------------------------

% ?- search([edge(a,b,15),edge(a,c,10),edge(b,c,6),edge(b,d,6)],a,d,P).
% ----->    P = [a, b, d] .
% ?- search([edge(a,b,17),edge(a,c,10),edge(b,c,6),edge(b,d,6)],a,d,P).
% changes length of edge ab to see whether path changes.
% ̀----->    P = [a, c, b, d].
% ?- search([edge(a,b,15),edge(a,c,10),edge(b,c,6),edge(b,d,20)],a,d,P).
% changes length of edge bd to see whether an infinite loop occurs
% ----->    P = [a, b, d].
% ?- search([edge(a,b,15),edge(a,c,10),edge(b,c,6),edge(b,d,20),edge(d,e,7)],a,d,P).
% adds a needless node to see whether the search can stop correctly.
% ----->    P = [a, b, d].