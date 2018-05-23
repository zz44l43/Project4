initialState(NR,NC,XS,YS,State):-
    generate_edges(NR,NC,E),
    insert_edges(E),
    get_map(NR,NC,Map),
    replace(XS-YS-"",XS-YS-"empty",Map,MapMarked),
    get_state_init(XS,YS,MapMarked,State).

guess(StateO,State,Guess):-


replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

get_state_init(XS,YS,Map,State):-
    Hist = [],
    State = [Xs-YS,Map,Hist].

get_Row(Row,C,O):-
    (
        C = 1
        -> O = [(Row,0)]
        ;
        Cc is C - 1,
        O = [(Row,Cc)|O1],
        get_Row(Row,Cc,O1) 
    ).
        
get_map(R,O):-
    get_map(R,R,O),
    length(O, L),
    print(L).
        

get_map(R,C,O):-
    (
        R > 1
        -> Rr is R -1,
        get_Roww(R,C,1,Ro),
        append(Ro,O1,O),
        get_map(Rr,C,O1)
        ;
        get_Roww(R,C,1,O)
    ).

get_Roww(Row,C,A,O):-
    (
        C > 1
        -> Cc is C-1,
        Aa is A + 1,
        O = [(Row-A-"")|O1],
        get_Roww(Row,Cc,Aa,O1)
        ;
        O =[(Row-A-"")]
    ).

%% find a simple Path from Start to End
find(Start, End, Path) :-
        find(Start, End, [Start], Path).
    %% find(Start, End, Previous, Path).
    %% find a simple Path from Start to End
    %% having visited Previous already
    find(Start, Start, _Previous, []).
    find(Start, End, Previous, [Dirn|Path]) :-
        edge(Start, Dirn, Med),
        \+ member(Med, Previous), % dont visit previous places
        find(Med, End, [Med|Previous], Path).


insert_edges([]).
insert_edges([(From,Dir,To)|List]):-
    assert(edge(From,Dir,To)),
    insert_edges(List).
        

generate_edges(Row,Column,E):-
    getMinX(Column,MinX),
    getMaxX(Column,MaxX),
    getMinY(Row,MinY),
    getMaxY(Row,MaxY),
    generate_edges(Row,Column,MinX,MaxX,MinY,MaxY,1,E).
    

generate_edges(Row,Column,MinX,MaxX,MinY,MaxY,RowCounter,E):-
    (
        RowCounter =< Row
        -> generate_edges_row(RowCounter,Column,MinX,MaxX,MinY,MaxY,1,RowEdges), 
        append(RowEdges,OtherEdges,E),
        NewRowCounter is RowCounter + 1,
        generate_edges(Row,Column,MinX,MaxX,MinY,MaxY,NewRowCounter,OtherEdges)
        ;
        E=[]
    ).

generate_edges_row(Row,Column,MinX,MaxX,MinY,MaxY,ColumnCounter,E):-
    (
        ColumnCounter =< Column
        -> CurrentX is ColumnCounter,
        CurrentY is Row,
        generate_edges_point(CurrentX,CurrentY,MinX,MaxX,MinY,MaxY,PointEdges),
        append(PointEdges, OtherEdges, E),
        NewColumnCounter is ColumnCounter + 1,
        generate_edges_row(Row,Column,MinX,MaxX,MinY,MaxY,NewColumnCounter,OtherEdges)
        ;
        E = []
    ).


generate_edges_point(X,Y,MinX,MaxX,MinY,MaxY,E):-
    generate_edges_point_west(X,Y,MinX,MaxX,MinY,MaxY,WE),
    generate_edges_point_east(X,Y,MinX,MaxX,MinY,MaxY,EE),
    generate_edges_point_north(X,Y,MinX,MaxX,MinY,MaxY,NE),
    generate_edges_point_south(X,Y,MinX,MaxX,MinY,MaxY,SE),
    TE = [WE,EE,NE,SE],
    removeEmpty(TE,E).

test(O1,O2,O3,L):-
    L = [O1,O2,O3].

getMinX(Column,1).
getMaxX(Column,Column).

getMinY(Row,1).
getMaxY(Row,Row).

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
        E = (X-Y, "west", NewX-Y)
        ;
        E = ""
    ).

generate_edges_point_east(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        X < MaxX
        -> NewX is X + 1,
        E = (X-Y, "east", NewX-Y)
        ;
        E = ""
    ).

generate_edges_point_north(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        Y > MinY
        -> NewY is Y - 1,
        E = (X-Y, "north", X-NewY)
        ;
        E = ""
    ).

generate_edges_point_south(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        Y < MaxY
        -> NewY is Y + 1,
        E = (X-Y, "south", X-NewY)
        ;
        E = ""
    ).
