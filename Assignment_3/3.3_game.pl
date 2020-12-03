% calculates the utility value
% get_utility_value(N,Player,Value)
% Player Max: 1, Player Min: -1
% Value has only two possibility: 1(Max wins), -1(Min wins) 

% when N is 1,2 or 3, the Player, who is acting, wins.
get_utility_value(1,Player,Player).
get_utility_value(2,Player,Player).
get_utility_value(3,Player,Player).

% when N > 3
%   when the utility values with tree actions are the same, returns this value.
get_utility_value(1,1,1,_,1).
get_utility_value(-1,-1,-1,_,-1).
%   when the three utility values are not same, the results depends on the Player, so returns 1 when Max, -1 when Min.
get_utility_value((N_1,N_2,N_3,Player,Value):- (N_1+N_2+N_3)<3,(N_1+N_2+N_3)>-3,Value=Player.

get_utility_value(N,Player,Value):-
    N>3,

        Player_next=Player*(-1),

        N_1 is N-1,
            get_utility_value(N_1,Player_next,Value_1),
        N_2 is N-2,
            get_utility_value(N_2,Player_next,Value_2),
        N_3 is N-3,
            get_utility_value(N_3,Player_next,Value_3),
        
        get_utility_value(N_1,N_2,N_3,Player,Value).
        
