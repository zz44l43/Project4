generate_edges(X,Y,Dx,Dy,E).

generate_edges_row(R,C,E).

generate_edges_point(X,Y,MinX,MaxX,MinY,MaxY,E):-
    generate_edges_point_west(X,Y,MinX,MaxX,MinY,MaxY,WE),
    generate_edges_point_east(X,Y,MinX,MaxX,MinY,MaxY,EE),
    generate_edges_point_north(X,Y,MinX,MaxX,MinY,MaxY,NE),
    generate_edges_point_south(X,Y,MinX,MaxX,MinY,MaxY,SE),
    TE = [WE,EE,NE,SE],
    removeEmpty(TE,E).

test(O1,O2,O3,L):-
    L = [O1,O2,O3].


removeEmpty([],[]).
removeEmpty([X|Xs],E):-
    (
        X \= ""
        -> E = [X|Ee],
        removeEmpty(Xs,Ee)
        ;
        removeEmpty(Xs,E)
    ).

checkNonEmpty(Path,X):-
    (
        Path \= ""
        -> X = true
    ).
generate_edges_point_west(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        X > MinX
        -> NewX is X - 1,
        E = ((X,Y), "west", (NewX,Y))
        ;
        E = ""
    ).

generate_edges_point_east(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        X < MaxX
        -> NewX is X + 1,
        E = ((X,Y), "east", (NewX,Y))
        ;
        E = ""
    ).

generate_edges_point_north(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        Y > MinY
        -> NewY is Y - 1,
        E = ((X,Y), "north", (X,NewY))
        ;
        E = ""
    ).

generate_edges_point_south(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        Y < MaxY
        -> NewY is Y + 1,
        E = ((X,Y), "south", (X,NewY))
        ;
        E = ""
    ).
