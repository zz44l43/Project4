:- module(wumpus,[initialState/5, guess/3, updateState/4]).

initialState(NR,NC,XS,YS, "ADB"):-
    write(NR),
    write(NC),
    write("HERE"),
    write("finished"),
    nl().

guess(State, State, [east, east,east, south, shoot]).

updateState(State, _, Feedback, State):-
    write(Feedback).
