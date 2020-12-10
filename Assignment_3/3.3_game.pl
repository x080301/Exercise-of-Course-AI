
% -----------------------------------------------------------
% get_utility_value calculates the utility value of a decision.
% get_utility_value(N,Player,Value)
% Player Max: 1, Player Min: -1
% returns Value, which has only two possibility: 1(Max wins), -1(Min wins) 
% -----------------------------------------------------------
%   when the utility values with tree actions are the same, returns this value.
get_utility_value_result(1,1,1,_,1).
get_utility_value_result(-1,-1,-1,_,-1).
%   when the three utility values are not same, the results depends on the Player, so returns 1 when Max, -1 when Min.
get_utility_value_result(N_1,N_2,N_3,Player,Value):-(N_1+N_2+N_3)<3,(N_1+N_2+N_3)>(-3),Value=Player.


% when N is 1,2 or 3, the Player, who is acting, wins.
get_utility_value(1,Player,Player).
get_utility_value(2,Player,Player).
get_utility_value(3,Player,Player).

% when N > 3
get_utility_value(N,Player,Value):-
    N>3,

        Player_next is Player*(-1),

        N_1 is N-1,
            get_utility_value(N_1,Player_next,Value_1),
        N_2 is N-2,
            get_utility_value(N_2,Player_next,Value_2),
        N_3 is N-3,
            get_utility_value(N_3,Player_next,Value_3),
        
        get_utility_value_result(Value_1,Value_2,Value_3,Player,Value).


% -----------------------------------------------------------
% decides how many matchs are taken away
% decision(Player,N,Action)
% returns 1,2 or 3 as Action
% -----------------------------------------------------------

% when the Max can find a Max or the Min can find a Min, returns that Action.
% when can not -> randomly decides one.
make_decision(Player,Player,_,_,1).
make_decision(Player,_,Player,_,2).
make_decision(Player,_,_,Player,3).
make_decision(Player,Value_1,Value_2,Value_3,Action):-
    not(Player=Value_1),
    not(Player=Value_2),
    not(Player=Value_3),
    random(1,4,Action).

% when remaining matchs =< 3, the Player will take all the remains to win the game.
decision(_,1,1).
decision(_,2,2).
decision(_,3,3).

% when remaining matchs >3, first calculates the utility value, and make decision.
decision(Player,N,Action):-

    Player_next is Player*(-1),

    N_1 is N-1,
        get_utility_value(N_1,Player_next,Value_1),
    N_2 is N-2,
        get_utility_value(N_2,Player_next,Value_2),
    N_3 is N-3,
        get_utility_value(N_3,Player_next,Value_3),

    make_decision(Player,Value_1,Value_2,Value_3,Action).

% -----------------------------------------------------------
% prints the current state, action and state_dot
% print(Player,Action,Remains)
% -----------------------------------------------------------

print(1,Action,0):- write("Max take "),write(Action),write(" matchs, and win the game."),nl.
print(-1,Action,0):-write("Min take "),write(Action),write(" matchs, and win the game."),nl.
print(Player,Action,Remains):-
    Remains>0,
        (Player=1,write("Max take ");Player=(-1),write("Min take ")),

        write(Action),write(" matchs, and "),
        write(Remains),write(" remain"),
        nl.

% -----------------------------------------------------------
% runs the game with Max and Min
% game(N)
% N is the number of matchs at the begining.
% -----------------------------------------------------------

% state(Round,N,Player)

% initiates the game
init_game(N):-

    asserta(state(1,N,1)).

% runs the game

run_game:-
    
    repeat,

    state(Round,N,Player),
    retract(state(Round,N,Player)),
    write("Round "),write(Round),write(": "),

    decision(Player,N,Action),

    Remains is N-Action,
    print(Player,Action,Remains),

    New_round is Round+1,
    New_player is Player*(-1),
    assertz(state(New_round,Remains,New_player)),

    Remains=<0.

game(N):-

    init_game(N),

    run_game.
    
% -----------------------------------------------------------
% test and discussion 
% -----------------------------------------------------------

% ?-game(N). N is how many matchs at the begining.

% Example:

% ?-game(10).
% 
% Round 1: Max take 2 matchs, and 8 remain
% Round 2: Min take 2 matchs, and 6 remain
% Round 3: Max take 2 matchs, and 4 remain
% Round 4: Min take 3 matchs, and 1 remain
% Round 5: Max take 1 matchs, and win the game.
% true .

% ?-game(12).
%
% Round 1: Max take 1 matchs, and 11 remain
% Round 6: Round 2: Min take 3 matchs, and 8 remain
% Round 3: Max take 3 matchs, and 5 remain
% Round 4: Min take 1 matchs, and 4 remain
% Round 5: Max take 1 matchs, and 3 remain
% Round 6: Min take 3 matchs, and win the game.
% true .

% Unless (N mod 4) = 0, Max can always win the game.
% When (N mod 4) = 0, Min wins.