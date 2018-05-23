:- module(wumpus,[initialState/5, guess/3, updateState/4]).

run(NR,NC,XS,YS,Guess):-
    initialState(NR,NC,XS,YS,Ss),
    write(Ss),
    guess(Ss,Ss,Guess).

initialState(NR,NC,XS,YS,State):-
    generate_edges(NR,NC,E),
    insert_edges(E),
    get_map(NR,NC,Map),
    replace(XS-YS-"",XS-YS-"empty",Map,MapMarked),
    get_state_init(XS,YS,MapMarked,State).

guess(StateO,StateO,Guess):-
    getNextSpot(StateO,Spot),
    write("Spot"),
    write(Spot),
    getCoordinate(Spot,Coordinate),
    write(Coordinate),
    write("      "),
    getCurrentPosition(StateO,C),
    write(C),
    find(C,Coordinate,FindGuess),
    write(FindGuess),
    getTarget(StateO,Target),
    write("Targe!"),
    write(Target),
    (
        Target = "empty"
        -> write("FOUNDDD"),
        Guess = FindGuess
        ;
        last(FindGuess,Last),
        write("Last"),
        write(Last),
        replace(Last,Target,FindGuess,Guess)
    ).    


updateState(State,[],_,State).
updateState(State,_,[],State).
updateState(StateO, [OneGuess|Guess], [OneFeedback|Feedback], State):-
    getCurrentPosition(StateO,CurrentPosition),
    getPositionAfterFeedback(CurrentPosition,OneGuess,PostPosition),
    write("Position"),
    write(PostPosition),
    updateMap(PostPosition,OneFeedback,StateO,MapState),
    write("Map"),
    move(MapState,OneGuess,OneFeedback,MoveState),
    write("Move"),
    write(MoveState),
    updateFact(PostPosition,OneGuess,OneFeedback),
    updateState(MoveState,Guess,Feedback,State).

move(StateO,OneGuess,OneFeedback,State):-
    (
        (OneFeedback = "empty"; OneFeedback = "stench";OneFeedback = "smell";OneFeedback = "damp")
        -> getCurrentPosition(StateO,CurrentPosition),
        write(CurrentPosition),
        getPositionAfterFeedback(CurrentPosition,OneGuess,PostPosition),
        write(PostPosition),
        updateCurrentPosition(PostPosition,StateO,State)
        ; OneFeedback = "wumpus"
        -> getCurrentPosition(StateO,CurrentPosition),
        getPositionAfterFeedback(CurrentPosition,OneGuess,PostPosition),
        updateTarget(PostPosition,StateO,State)
        ;
        State = StateO
    ).

updateTarget(Target,[CurrentPosition,Map,Hist,_],[CurrentPosition,Map,Hist,Target]).

updateFact(CurrentPosition,Guess,OneFeedback):-
    (
        (OneFeedback = "pit"; OneFeedback = "wall")
        ->deletePath(CurrentPosition)
        ;
        true
    ).

updateMap(Xs-Ys,Feedback, [CurrentPosition,MapO,Hist,Target], [CurrentPosition,Map,Hist,Target]):-    
    replace(Xs-Ys-"",Xs-Ys-Feedback,MapO,Map).

updateCurrentPosition(X-Y,[_,Map,Hist,Target],[X-Y,Map,Hist,Target]).

deletePath(X-Y):-
    (
        edge(X-Y,_,_)
        ->retract(edge(X-Y,_,_)),
        deletePath(X-Y)
        ;
        edge(_,_,X-Y)
        ->retract(edge(_,_,X-Y)),
        deletePath(X-Y)
        ;
        true
    ).

getTarget([CurrentPosition,Map,Hist,Target], Target).

getPositionAfterFeedback(X-Y, Dir, PostPosition):-
    (
        Dir = "east"
        -> NewX is X + 1,
        PostPosition = NewX-Y
        ; Dir = "west"
        -> NewX is X - 1,
        PostPosition = NewX-Y
        ; Dir = "north"
        -> NewY is Y - 1,
        PostPosition = X-NewY
        ;Dir = "south"
        -> NewY is Y + 1,
        PostPosition = X-NewY
    ).


replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- 
    write(R),
    replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- 
    H \= O, 
    replace(O, R, T, T2).

getCoordinate(X-Y-S,X-Y).

getCurrentPosition([C,Map,Hist,_],C).

getNextSpot([C,Map,Hist,Target],Spot):-first_elem(Map,Spot).

first_elem([],null).
first_elem([X-Y-S|Other],Ele):-
    (
        S = ""
        -> Ele = X-Y-S
        ;first_elem(Other,Ele)
    ).

get_state_init(XS,YS,Map,State):-
    Hist = [],
    State = [XS-YS,Map,Hist,"empty"].
        
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
    generate_edges(Row,Column,MinX,MaxX,MinY,MaxY,1,E),
    write(E).
    

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
        E = (X-Y, west, NewX-Y)
        ;
        E = ""
    ).

generate_edges_point_east(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        X < MaxX
        -> NewX is X + 1,
        E = (X-Y, east, NewX-Y)
        ;
        E = ""
    ).

generate_edges_point_north(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        Y > MinY
        -> NewY is Y - 1,
        E = (X-Y, north, X-NewY)
        ;
        E = ""
    ).

generate_edges_point_south(X,Y,MinX,MaxX,MinY,MaxY,E):-
    (
        Y < MaxY
        -> NewY is Y + 1,
        E = (X-Y, south, X-NewY)
        ;
        E = ""
    ).
