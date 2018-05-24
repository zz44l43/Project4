:- module(wumpus,[initialState/5, guess/3, updateState/4]).
repl2(L, A, OutL):-reverse(L, [_|T]), reverse([A|T], OutL).
diffSet([], X, X).

diffSet([H|T1],Set,Z):-
 member(H, Set),       % NOTE: arguments swapped!
 !, delete(T1, H, T2), % avoid duplicates in first list
 delete(Set, H, Set2), % remove duplicates in second list
 diffSet(T2, Set2, Z).

diffSet([H|T], Set, [H|Set2]) :-
 diffSet(T,Set,Set2).

is_member(X, Y) :-
    member(X,Y).

is_too_long(L,Xs):-
    length(Xs,LengthList),
    L < LengthList.
filter_long_list(A,In,Out):-
    exclude(is_too_long(A),In,Out).

filter_list(A, In, Out) :-
    exclude(is_member(A), In, Out).

every_second_one([],[]).
every_second_one([Dir,Dir|Other],[Dir,Dir|OtherS]):-
    every_second_one(Other,OtherS).
every_second_one([Dir,Dir2|Other],[Dir,Dir2,shoot,OtherS]):-
    every_second_one([Dir2|Other],[Dir2|OtherS]).


initialState(NR,NC,XS,YS,State):-
    generate_edges(NR,NC,E),
    insert_edges(E),
    get_map(NR,NC,Map),
    replace(XS-YS-"",XS-YS-empty,Map,MapMarked),
    State = [XS-YS,MapMarked,[],empty,XS-YS].

reset_current_position([XS,Map,Hist,Target,X-Y],State):-
    State = [X-Y,Map,Hist,Target,X-Y].

guess(StateO,State,Guess):-
    reset_current_position(StateO,ResetState),
    getHist(ResetState,Hist),
    getTarget(ResetState,Target),
    getNextSpot(ResetState,Spot),
    getCoordinate(Spot,Target,Coordinate),
    getOrig(ResetState,Orig),
    getHist(ResetState,Hist),
    findall(FindGuess,find(Orig,Coordinate,FindGuess),AllGuess),
    nl(),
    subtract(AllGuess, Hist, SubGuess),
    filter_list([],SubGuess,FilteredGuess),
    filter_list(shoot,FilteredGuess,FilteredGuess2),
    filter_long_list(5,FilteredGuess2,FilteredGuess3),
    (
        FilteredGuess3 = []
        -> updateMap(Coordinate,wall,ResetState,UpdateMapState),
        deletePath(Coordinate),
        getNextSpot(UpdateMapState,NewSpot),
        getCoordinate(NewSpot,Target,NewCoordinate),
        findall(NewFindGuess,find(Orig,NewCoordinate,NewFindGuess),NewAllGuess),
        subtract(NewAllGuess, Hist, NewSubGuess),
        filter_list([],NewSubGuess,NewFilteredGuess),
        filter_list(shoot,NewFilteredGuess,NewFilteredGuess2),
        filter_long_list(4,NewFilteredGuess2,FilteredGuess3),
        (
            FilteredGuess3 = []
            -> updateMap(NewCoordinate,wall,UpdateMapState,SecondUpdateMapState),
            deletePath(NewCoordinate),
            getNextSpot(SecondUpdateMapState,SecondNewSpot),
            getCoordinate(SecondNewSpot,Target,SecondNewCoordinate),
            findall(SecondNewFindGuess,find(Orig,SecondNewCoordinate,SecondNewFindGuess),SecondNewAllGuess),
            subtract(SecondNewAllGuess, Hist,SecondNewSubGuess),
            filter_list([],SecondNewSubGuess,SecondNewFilteredGuess),
            filter_list(shoot,SecondNewFilteredGuess,SecondNewFilteredGuess2),
            filter_long_list(8,NewFilteredGuess2,FilteredGuess3),
            NewestState = SecondUpdateMapState
            ;
            NewestState = UpdateMapState
        )
        ;
        NewestState = ResetState
    ),
    nl,
    (
        FilteredGuess3 = []
        ->
        (
            FilteredGuess2 = []
            -> nth0(0,NewFilteredGuess2,SelectedGuess)
            ;
            nth0(0,FilteredGuess2,SelectedGuess)
        )
        ;
        nth0(0,FilteredGuess3,SelectedGuess)
    ),
    
    getTarget(NewestState,Target),
    updateHist(NewestState,SelectedGuess,HistoryState),
    add_shoot(SelectedGuess,Guess),
    State = HistoryState.


add_shoot([],[]).
add_shoot([Dir|OtherS],[Dir,shoot|Other]):-
    add_shoot(OtherS,Other).


updateState(State,[],_,State).
updateState(State,_,[],State).
updateState(StateO, [OneGuess|Guess], [OneFeedback|Feedback], State):-
    (
        OneGuess = shoot
        -> 
        updateState(StateO,Guess,Feedback,State)
        ;
        getCurrentPosition(StateO,CurrentPosition),
        getPositionAfterFeedback(CurrentPosition,OneGuess,PostPosition),
        updateMap(PostPosition,OneFeedback,StateO,MapState),
        move(MapState,OneGuess,OneFeedback,MoveState),
        updateFact(PostPosition,OneGuess,OneFeedback),
        updateState(MoveState,Guess,Feedback,State)
    ).
   
move(StateO,OneGuess,OneFeedback,State):-
    (
        (OneFeedback = empty; OneFeedback = stench; OneFeedback = smell;OneFeedback = damp)
        -> getCurrentPosition(StateO,CurrentPosition),
        getPositionAfterFeedback(CurrentPosition,OneGuess,PostPosition),
        updateCurrentPosition(PostPosition,StateO,State)
        ; OneFeedback = wumpus
        -> getCurrentPosition(StateO,CurrentPosition),
        getPositionAfterFeedback(CurrentPosition,OneGuess,PostPosition),
        updateTarget(PostPosition,StateO,State)
        ;
        State = StateO
    ).

updateTarget(Target,[CurrentPosition,Map,Hist,TargetO,Orig],[CurrentPosition,Map,Hist,Target,Orig]).

updateFact(CurrentPosition,Guess,OneFeedback):-
    (
        (OneFeedback = pit; OneFeedback = wall)
        ->deletePath(CurrentPosition)
        ;
        true
    ).

updateMap(Xs-Ys,Feedback, [CurrentPosition,MapO,Hist,Target,Orig], [CurrentPosition,Map,Hist,Target,Orig]):-    
    replace(Xs-Ys-"",Xs-Ys-Feedback,MapO,Map).

updateCurrentPosition(X-Y,[_,Map,Hist,Target,Orig],[X-Y,Map,Hist,Target,Orig]).

updateHist([C,Map,Hist,Target,Orig],NewHist,State):-
    WholeHist = [NewHist|Hist],
    State = [C,Map,WholeHist,Target,Orig].

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

getHist([CurrentPosition,Map,Hist,Target,Orig], Hist).
getTarget([CurrentPosition,Map,Hist,Target,Orig], Target).
getOrig([CurrentPosition,Map,Hist,Target,Orig], Orig).

getPositionAfterFeedback(X-Y, Dir, PostPosition):-
    (
        Dir = east
        -> NewX is X + 1,
        PostPosition = NewX-Y
        ; Dir = west
        -> NewX is X - 1,
        PostPosition = NewX-Y
        ; Dir = north
        -> NewY is Y - 1,
        PostPosition = X-NewY
        ;Dir = south
        -> NewY is Y + 1,
        PostPosition = X-NewY
    ).


replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- 
    replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- 
    H \= O, 
    replace(O, R, T, T2).

getCoordinate(null,Target,Target).
getCoordinate(X-Y-S,Target,Coordinate):-
    (
        Target = empty
        -> Coordinate = X-Y
        ; Coordinate = Target
    ).

getCurrentPosition([C,Map,Hist,Target,Orig],C).

getNextSpot([C,Map,Hist,Target,Orig],Spot):-
    first_elem(Map,Spot).

first_elem([],null).
first_elem([X-Y-S|Other],Ele):-

    (
        S = ""
        -> 
        Ele = X-Y-S
        ;first_elem(Other,Ele)
    ).

get_state_init(XS,YS,Map,[XS-YS,Map,[],empty,XS-YS]).
        
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
