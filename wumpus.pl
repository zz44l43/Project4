 /*
        The University of Melbourne 

        Department of Computing and Information Systems 

        Declarative Programming 

        COMP90048 

        Author:        Zhi Zheng (327965)

        E-mail:        zhiz2@student.unimelb.edu.au

    

        This program is practice software; 
        The objective of this project is to practice and assess your understanding of logic programming and Prolog. You will write code to solve a small planning problem.


*/

%Main module of the program that have the name of the module - wumpus and three other defined functions
:- module(wumpus,[initialState/5, guess/3, updateState/4]).

% is_member/2.
% To check wether an item is contain within a List.
is_member(X, Y) :-
    member(X,Y).

%is_too_long/2
%check if a list is too long, super long path is not desired in this game.
is_too_long(L,Xs):-
    length(Xs,LengthList),
    L < LengthList.

%filter_long_list/3
%Filter out the list that is way too wrong by excluding them out of the list.
filter_long_list(A,In,Out):-
    exclude(is_too_long(A),In,Out).

%filter_list/3
%exclude out a member out of a list, such as the shoot because it really meanning less if did not hit anything.
filter_list(A, In, Out) :-
    exclude(is_member(A), In, Out).

/*
1 of three main function.
To initialized the system by inserting edges and generates all the edges/path in the system
Initialzied the map inthe system and with the starting point to be marked as the empty.
In the end it also return a state ojbect, this object contains the current position of the robot bascially it is the starting point
The state object also contain other property items
such as the History of the guess record, the wumpus's coordinate if found.
*/
initialState(NR,NC,XS,YS,State):-
    generate_edges(NR,NC,E),
    insert_edges(E),
    get_map(NR,NC,Map),
    replace(XS-YS-"",XS-YS-empty,Map,MapMarked),
    State = [XS-YS,MapMarked,[],empty,XS-YS].

%reset_current_position/2
%reset the current position of the state as the state needed to monitor the robot moveent in the update section
%Hence once the monitor is over, it needs to be reset to the true initial coordinate.
reset_current_position([XS,Map,Hist,Target,X-Y],State):-
    State = [X-Y,Map,Hist,Target,X-Y].

/*
1 of the 3 main functions in this applicaiton. 
This function respondes for make what to guess for the next period of the instrunctions. 
THe program first reset the state of the State.
Then try to grab the history result of the state.
Try to get the target that is the position where the wumpus hide.
Try to find the next spot to the exmplore on by calling the getNextSpot and getCoordinate.
Then use the findall to find the path between two points.
the findall will return all the path between two points.
This is not so desired from our perspective because the robot moving around too much is not always a good thing.
It can get instrcuted by many mistaken instruction at the beginning and start making more problems.
All the path will be deleted from the system so next time wehn we generate the edges/paths it will not be involved in our computation.
if there is no enough edges to move between two points. Then the destination point will be marked as wall and never allowed to move again.
There are three checks of that to ensure the correct and meanning full path are used each time.
The Robot will shot wildly as it move across each instructions so wumpus will be killed quickly.
*/
guess(StateO,State,Guess):-
    reset_current_position(StateO,ResetState),
    getHist(ResetState,Hist),
    getTarget(ResetState,Target),
    getNextSpot(ResetState,Spot),
    getCoordinate(Spot,Target,Coordinate),
    getOrig(ResetState,Orig),
    getHist(ResetState,Hist),
    findall(FindGuess,find(Orig,Coordinate,FindGuess),AllGuess),
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


%To add shot to each of the instrunction so the robot will fire wildly.
add_shoot([],[]).
add_shoot([Dir|OtherS],[Dir,shoot|Other]):-
    add_shoot(OtherS,Other).

/*
1 of three main functions in the program.
This function responsible to post process the state and the system after the feedback of guess is returned.
The feeback of guess provied meanning full information and is the only way that the robot is able to detec the whole map.
It will tell the robot which instrunction point has dangerous such as wall or pit 
It will also tell the robot where the wumpus is and give some signall when the wumpus is around. 
The behavioiur of the system will very much depend on this function.
The system will get what is current position that robot is on the field.
Ten it will try to get the position that the system try to move to - but it may or may not move to the point depend onthe feedback again
For example, if the feedback indicates the intended robot move will hit a wall, then the robot will just on the ground and do nothing.
Then the update function will try to update the map state by mark on the points what the feedback is.
It will also remove all the relative edges in the system to maintain the system in a good state so when the guess function is making a decision on instrcuction 
it will ingnore all these already removed positions.
The function will recursively doing this process .
*/
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
% monitor the movement of the robot with 1 single instrunction and feedback.
% depend on the feedback, if the feedback indicates that the movement is safe such as empty, stench ,smell or damp.
% If the feedback of the instruction point is regard to wumpus that represents a lots and we will be clear on our goal and misisons.
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
%Once we found the wumpus we will need to update its position/
updateTarget(Target,[CurrentPosition,Map,Hist,TargetO,Orig],[CurrentPosition,Map,Hist,Target,Orig]).

%One of the most important function.
%This function will call another function to delete the edges in the system.
updateFact(CurrentPosition,Guess,OneFeedback):-
    (
        (OneFeedback = pit; OneFeedback = wall)
        ->deletePath(CurrentPosition)
        ;
        true
    ).
% Refersh the map as we have the latest changse.
updateMap(Xs-Ys,Feedback, [CurrentPosition,MapO,Hist,Target,Orig], [CurrentPosition,Map,Hist,Target,Orig]):-    
    replace(Xs-Ys-"",Xs-Ys-Feedback,MapO,Map).

% modify the position 
updateCurrentPosition(X-Y,[_,Map,Hist,Target,Orig],[X-Y,Map,Hist,Target,Orig]).

% Updatethe history list of the state object as it will be previous guesses basically.
% Previous guess kept to not generate the same insturction.
updateHist([C,Map,Hist,Target,Orig],NewHist,State):-
    WholeHist = [NewHist|Hist],
    State = [C,Map,WholeHist,Target,Orig].

% Delete all the edges that realated to the given position.No matter it is at which side of the edges.
% Example of the edge:  edge(X-Y, Dir, Xx-Yy).
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
%Get the history/previous guess from the state
getHist([CurrentPosition,Map,Hist,Target,Orig], Hist).

%Get the target/wumpus position to remmber that in the state.
getTarget([CurrentPosition,Map,Hist,Target,Orig], Target).

%Get the original position of the robot.
getOrig([CurrentPosition,Map,Hist,Target,Orig], Orig).

%Determine which position you will be on by supporting the directions
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

%helper functions to replace intended value in the list.
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- 
    replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- 
    H \= O, 
    replace(O, R, T, T2).

%Return the position coordinator (X,Y) to the caller. 
%This will be the position that used by system to work out the destination of next instrunctions.
getCoordinate(null,Target,Target).
getCoordinate(X-Y-S,Target,Coordinate):-
    (
        Target = empty
        -> Coordinate = X-Y
        ; Coordinate = Target
    ).
%Return current position that marked in the state.
getCurrentPosition([C,Map,Hist,Target,Orig],C).

%Used with getCoordinates to discover the destination of our next instrunctions.
getNextSpot([C,Map,Hist,Target,Orig],Spot):-
    first_elem(Map,Spot).
%Get the first element in the map that will be serverd as the next destination to explore the map and path gnerateor. 
first_elem([],null).
first_elem([X-Y-S|Other],Ele):-

    (
        S = ""
        -> 
        Ele = X-Y-S
        ;first_elem(Other,Ele)
    ).
%Init the state by supporting map and intiial position etc.
get_state_init(XS,YS,Map,[XS-YS,Map,[],empty,XS-YS]).

%Get and initialized of map in the system.
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

%inswert a new edges to the system.
insert_edges([]).
insert_edges([(From,Dir,To)|List]):-
    assert(edge(From,Dir,To)),
    insert_edges(List).
        
%processs of add a new edges in the ystem
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
%various points had started to invite teacher to move to oter export.
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
