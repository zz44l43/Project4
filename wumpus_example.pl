%% dummy wumpus.pl
:- module(wumpus, [initialState/5, guess/3, updateState/4]).
initialState(NR,NC,XS,YS, state).

guess(state, state, [north, shoot, east, shoot, south, shoot, west, shoot]).

updateState(state, _, _, state).
