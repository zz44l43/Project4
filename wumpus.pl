
/** <wumpus> Project 4 Documentation
 * 

The University of Melbourne 

Department of Computing and Information Systems 

Declarative Programming  COMP90048 

Wumpus is a planning problem. You need to find and kill a Wumpus hiding in an unknown maze. T
he player sends in a series of disposable robots each with a fixed list of instructions to follow. 
Each robot follows its instructions until it is destroyed or it finishes the instructions, or it kills the Wumpus. 
After a robot is finished the player gets feedback on what the robot sensed following the instructions. 
The aim is to explore the maze and find and shoot the Wumpus.

@author Zhi Zheng StudentID: 327965
@E-mail:        zhiz2@student.unimelb.edu.au


*/


:- module(wumpus,[initialState/5, guess/3, updateState/4]).


%!      initialState(+NR:int,+NC:int,+XS:int,+YS:int,-StateO) is det.
%       initialState/5
%       
%       1 of three main function.
%       To initialized the system by inserting edges and generates all the edges/path in the system
%       Initialzied the map inthe system and with the starting point to be marked as the empty.
%       In the end it also return a state ojbect, this object contains the current position of the robot bascially it is the starting point
%       The state object also contain other property items
%       such as the History of the guess record, the wumpus's coordinate if found.
%       

initialState(NR,NC,XS,YS,State):-
    generate_edges(NR,NC,E),
    insert_edges(E),
    get_map(NR,NC,Map),
    replace(XS-YS-_,XS-YS-empty,Map,UpdatedMap),
	get_initial_state(NR,NC,XS-YS,UpdatedMap,[],empty,[],[],State).

%!      get_initial_state(+NumberRow:int,+NumberColumn:int,+InitialX:int-InitialY:int,+Map:map,+History:list,+WumpusPoint:atom,+StenchPoints:list,+SmellPoints:list, -Map:map)  is det.
%       Init the state of the game
get_initial_state(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints)).

%!      is_search_mode(+State:state) is det.
%       is_search_mode/1
%       determine which mode to be used in the guess. If we didn't find wumpus then searching mode otherwise we know which point we're going.

is_search_mode(State):-
	get_state_wumpus_point(State,WumpusPoint),
	(
		WumpusPoint = empty
		-> true
		;
		false
	).

%!      get_search_mode(+State:state, -SearchMode:atom) is det.
%       get_search_mode/2
%       Determine the mode so we can narrow down the search area. 
get_search_mode(State,SearchMode):-
    get_state_wumpus_point(State,WumpusPoint),
    get_state_smell_point(State,SmellPoints),
    get_state_stench_point(State,StenchPoints),
	(
		WumpusPoint \= empty
		-> SearchMode = wumpus
		;
        SmellPoints \= []
        -> SearchMode = smell
        ;
        StenchPoints \= []
        -> SearchMode = stench
        ;
        SearchMode = none
	).

%!      get_manhattan_points_smell(+State:state, +SmellPoints:list -ManhattanPoints:list) is det.
%       get_manhattan_points_smell/3
%       Get all the manhattan points surround a set of points. This is usefull for both smell and stench cases.
get_manhattan_points_smell(_,[],[]).
get_manhattan_points_smell(State,[SmellPoint|SmellPoints],ManhattanPoints):-
    get_manhattan_points(SmellPoint,3,State,ManhattanPointsForOnePoint),
    ManhattanPoints = [ManhattanPointsForOnePoint|OtherManhattanPoints],
    get_manhattan_points_smell(State,SmellPoints,OtherManhattanPoints).

%!      points_intersections(+Points:list, +RestPoints:list -FinalPoints:list) is det.
%       points_intersections/3
%       Intersection between all the points.
points_intersections(Points,[],Points).
points_intersections(Points,[OtherPoints|RestPoints],FinalPoints):-
    intersection(Points,OtherPoints,CurrentIntersctions),
    points_intersections(CurrentIntersctions,RestPoints,FinalPoints).

%!      smell_intersction(+State:state, -Points:list) is det.
%       smell_intersction/2
%       Intersection between all the smell points.
smell_intersction(State,Points):-
    get_state_smell_point(State,SmellPoints),
    get_manhattan_points_smell(State,SmellPoints,ManhanttanPoints),
    nth0(0,ManhanttanPoints,FirstPoint),
    points_intersections(FirstPoint,ManhanttanPoints,Points).

%!      get_points_stench(+State:state, +StenchPoints:list -Points:list) is det.
%       get_points_stench/3
%       Get all the points for stench.
get_points_stench(_,[],[]).
get_points_stench(State,[StenchPoint|StenchPoints],Points):-
    get_manhattan_points(StenchPoint,1,State,DistancePoints),
    Points = [DistancePoints|OtherDistancePoints],
    get_points_stench(State,StenchPoints,OtherDistancePoints).

%!      stench_intersection(+State:state, -Points:list) is det.
%       stench_intersection/2
%       Get all the intersected points for all the stench. So we can narrow down.
stench_intersection(State,Points):-
    get_state_stench_point(State,StenchPoints),
    get_points_stench(State,StenchPoints,DistancePoints),
    nth0(0,DistancePoints,FirstPoint),
    points_intersections(FirstPoint,DistancePoints,Points).

%!      guess(+StateO:state, -State:state, -Points:list) is det.
%       guess/3
%       Make guesses/instruction for the robots to move. Mainly consist with east west south or north.
%       The function will act differently based on weather it is on search mode or not.
%       In search mode. a guess will be conducted based on a range or a random point that we have some cludes about the map.
%       Otherwise, we know where the wumpus is and make guesses/instruction by targeting that point.
%       Shooting will be conducted if there is a change of a direction in the guesses or instructions. E.g east west will become east west shoot. 
guess(StateO,State,Guess):-
	(
        is_search_mode(StateO)
        -> get_search_mode_next_point(StateO,StateSearch,SearchGuess)
        ;
        get_state_wumpus_point_path(StateO,StateSearch,SearchGuess)
	),
    set_state_history(StateSearch,SearchGuess,State),
    shoot_dir_change(SearchGuess,Guess).

%!      updateState(+StateO:state, +Guess:list, +Feedback:list, -State:state) is det.
%       updateState/4
%       Update all the state and maps based on the previous guess and the feedbacks.
%       It will be mainly calling another updateState/5 for the specific updates.
updateState(StateO, Guess, Feedback, State):-
	get_state_initial_point(StateO,InitialPoint),
	updateState(StateO,Guess,Feedback,InitialPoint,State).

%!      updateState(+StateO:state, +Guess:list, +Feedback:list, +X:int-Y:int:atom -State:state) is det.
%       updateState/5
%       Update all the state and maps based on the previous guess and the feedbacks.
%       X-Y Determines the current position of the robot on the map after the guess and feedback.
%       Generally get the position after the guess
%       Update the map according based on the feedback.
%       Update the state include removing the edges(Facts) in the system.
updateState(State,[],[],_,State).
updateState(State,_,[],_,State).
updateState(StateO,[shoot|Guess], [_|Feedback], X-Y, State):-
    updateState(StateO,Guess, Feedback, X-Y, State).

%!      updateState(+StateO:state, +Guess:list, +Feedback:list, +X:int-Y:int:atom -State:state) is det.
%       updateState/5
%       Update all the state and maps for stench feedback.
updateState(StateO,[Dir|Guess], [stench|Feedback], X-Y, State):-
    getPositionAfterFeedback(X-Y, Dir,PostPosition),
    get_state_stench_point(StateO,StenchPoints),
    (
        \+ member(PostPosition,StenchPoints)
        -> updateMap(StateO,PostPosition,stench,MapState),
        set_state_stench(MapState,PostPosition,StenchState)
        ;
        StenchState = StateO
    ),
	updateState(StenchState,Guess,Feedback,PostPosition,State).

%!      updateState(+StateO:state, +Guess:list, +Feedback:list, +X:int-Y:int:atom -State:state) is det.
%       updateState/5
%       Update all the state and maps for stench smell.	
updateState(StateO,[Dir|Guess], [smell|Feedback], X-Y, State):-
        getPositionAfterFeedback(X-Y, Dir,PostPosition),
        get_state_smell_point(StateO,SmellPoints),
        (
            \+ member(PostPosition,SmellPoints)
            -> 	updateMap(StateO,PostPosition,smell,MapState),
            set_state_smell(MapState,PostPosition,SmellState)
            ;
            SmellState = StateO
        ),
        updateState(SmellState,Guess,Feedback,PostPosition,State).

%!      updateState(+StateO:state, +Guess:list, +Feedback:list, +X:int-Y:int:atom -State:state) is det.
%       updateState/5
%       Update all the state and maps for empty feedback.
updateState(StateO,[Dir|Guess], [empty|Feedback], X-Y, State):-
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,empty,MapState),
	updateState(MapState,Guess,Feedback,PostPosition,State).

%!      updateState(+StateO:state, +Guess:list, +Feedback:list, +X:int-Y:int:atom -State:state) is det.
%       updateState/5
%       Update all the state and maps for damp feedback.
updateState(StateO,[Dir|Guess], [damp|Feedback], X-Y, State):-
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,damp,MapState),
	updateState(MapState,Guess,Feedback,PostPosition,State).

%!      updateState(+StateO:state, +Guess:list, +Feedback:list, +X:int-Y:int:atom -State:state) is det.
%       updateState/5
%       Update all the state and maps for wall feedback.
%       remove the any edges(FACT) around this point as it has no need anymore.
updateState(StateO,[Dir|Guess], [wall|Feedback], X-Y, State):-
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,wall,MapState),
	delete_edges(PostPosition),
	updateState(MapState,Guess,Feedback,X-Y,State).

%!      updateState(+StateO:state, +Guess:list, +Feedback:list, +X:int-Y:int:atom -State:state) is det.
%       updateState/5
%       Update all the state and maps for pit feedback.
%       remove the any edges(FACT) around this point as it has no need anymore.
updateState(StateO,[Dir|Guess], [pit|Feedback], X-Y, State):-
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,pit,MapState),
	delete_edges(PostPosition),
	updateState(MapState,Guess,Feedback,X-Y,State).

%!      updateState(+StateO:state, +Guess:list, +Feedback:list, +X:int-Y:int:atom -State:state) is det.
%       updateState/5
%       Update all the state and maps for wumpus feedback.
updateState(StateO,[Dir|Guess], [wumpus|Feedback], X-Y, State):-
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,wumpus,MapState),
	set_state_wumpus(MapState,PostPosition,WumpusState),
	updateState(WumpusState,Guess,Feedback,PostPosition,State).

%!      updateMap(+StateO:state, +X:int-Y:int:atom, +Feedback:list,-State:state) is det.
%       updateMap/4
%       Update the map accordingly based on the feedback for a specific X-Y position and return a new state.
updateMap(State,X-Y,Feedback,ReplacedState):-
	get_state_map(State,MapO),
    replace(X-Y-_,X-Y-Feedback,MapO,Map),
	set_state_map(State,Map,ReplacedState).

%!      getPositionAfterFeedback(+X:int-Y:int:atom, +Dir:atom,-X:int-Y:int:atom) is det.
%       getPositionAfterFeedback/3
%       Get the position based on the feedback from the current X-Y. 
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

%!      get_state_wumpus_point_path(+StateO:state -StateO:state, SearchGuess:list) is det.
%       get_state_wumpus_point_path/3
%       Find all the guess/instructions if the wumpus point is known.
get_state_wumpus_point_path(StateO,NewState,SearchGuess):-
	get_state_wumpus_point(StateO,WumpusPoint),
    get_all_path_point(StateO,WumpusPoint,NewPaths),
    (
        NewPaths = []
        -> path_by_random(StateO,NewState,RandomPath),
        pick_path_point(StateO,RandomPath,SearchGuess)
        ;NewState = StateO,
        pick_path_point(StateO,NewPaths,SearchGuess)
    ).


%       To add shot to each of the instrunction so the robot will fire wildly.
%       add_shoot/2
add_shoot([],[]).
add_shoot([Dir|OtherS],[Dir,shoot|Other]):-
    add_shoot(OtherS,Other).


%       Get the next point in the search mode.
%       get_search_mode_next_point/3
get_search_mode_next_point(StateO,NewState,Guess):-
	path_by_random(StateO,NewState,Guess).

%!      get_all_path_point(+StateO:state -StateO:state, SearchGuess:list) is det.
%       get_all_path_point/3
%       Find all the guess/instructions from a point to another.
%       if the amount round is more than 40 then we will find that search for all the path.
%       @tbd Determine the best value for the round rather than current 40. It should be based on the size of the map. Optimize requires.
get_all_path_point(State,Point,NewPaths):-
    get_state_history(State,History),
    get_state_initial_point(State,InitialPoint),
    get_state_round(State,Round),
    (
        Round < 40
        ->  findall(FindPath,find(InitialPoint,Point,FindPath),AllPaths)
        ;
        findall(FindPath,find_no_restriction(InitialPoint,Point,FindPath),AllPaths)
    ),
	subtract(AllPaths,History,NewPaths).

%!      path_by_random(+StateO:state -StateO:state, SearchGuess:list) is det.
%       path_by_random/3
%       Find all the guess/instructions by choosing a random point on the map. 
%       This point can be purely radomlized or randomized within a known area of points.
path_by_random(State,NewState,Path):-
    get_search_mode(State,Mode),
    (
        Mode = stench
        -> pick_stench_point(State,Point)
        ;
        Mode = smell
        -> pick_smell_point(State,Point)
        ;
        pick_point(State,Point)
    ),
    get_all_path_point(State,Point,NewPaths),
    (
        NewPaths = []
        -> updateMap(State,Point,wall,UpdatedState),
        delete_edges(Point),
        path_by_random(UpdatedState,NewState,Path)
        ;
        NewState = State,
        pick_path_point(State,NewPaths,Path)
    ).

%!      pick_path_point(+StateO:state -StateO:state, SearchGuess:list) is det.
%       pick_path_point/3
%       Find a single series of guesses/instructions from paths.
pick_path_point(State,NewPaths,Path):-
	pick_distance(State,Distance),
	get_mini_paths(Distance,MiniPaths),
    nth0(0,NewPaths,TestPath),
    (
        is_list(TestPath)
        ->filter_short_length_lists(MiniPaths,NewPaths,NoShortPath),
        sort_atoms_by_length(NoShortPath,SortedPath),
        nth0(0,SortedPath,Path)
        ;
        Path = NewPaths
    ).

%!      pick_path_point(+MiniPaths:list +AllPaths:list, FilteredPath:list) is det.
%       pick_path_point/3
%       we try to explore a large points on the map in 1 guess so a 1 instruction is not so desired. 
%       Thus we remove them if we have alternative paths to be selected.
filter_short_length_lists(MiniPaths, AllPaths, FilteredPath):-
    filter_short_length_list(MiniPaths,AllPaths,NoShortPath),
    (
        NoShortPath = []
        -> FilteredPath = AllPaths
        ;
        FilteredPath = NoShortPath
    ).

%!      sort_atoms_by_length(+Atoms:list -ByLength:list) is det.
%       sort_atoms_by_length/2
%       Sort a list of guess by their length. So we choose a minimumn instruction from 1 point to another.
sort_atoms_by_length(Atoms, ByLength) :-
	map_list_to_pairs(length, Atoms, Pairs),
	keysort(Pairs, Sorted),
	pairs_values(Sorted, ByLength).

%       Detemrine the minimum path requires from 1 point to another so we utilized the information.
get_mini_paths(Distance,MiniPaths):-
	(
		Distance > 0
		-> MiniPaths is Distance + 1
		;
		MiniPaths is -1
	).

%is_too_long/2
%check if a list is too short, super short path is not desired in this game.
is_too_short(L,Xs):-
    length(Xs,LengthList),
    L >= LengthList.

%filter_short_length_list/3
%Filter out the list that is way too wrong by excluding them out of the list.
filter_short_length_list(A,In,Out):-
    exclude(is_too_short(A),In,Out).

%!      pick_stench_point(+State:state -Point:atom) is det.
%       pick_stench_point/2
%       Get a point to be selected when we know there is stench on the map.
%       Find the intersection between stench points.
pick_stench_point(State,Point):-
    stench_intersection(State,IntersectedPoints),
    get_state_map(State,Map),
    pick_valid_points(IntersectedPoints,Map,Points),
    removeEmpty(Points,NonEmptyPoints),
    nth0(0,NonEmptyPoints,Point).   

%!      pick_smell_point(+State:state -Point:atom) is det.
%       pick_smell_point/2
%       Get a point to be selected when we know there is smell on the map.
%       Find the intersection between smell points, a mahattan range.
pick_smell_point(State,Point):-
    smell_intersction(State,IntersectedPoints),
    get_state_map(State,Map),
    pick_valid_points(IntersectedPoints,Map,Points),
    removeEmpty(Points,NonEmptyPoints),
    (
        NonEmptyPoints = []
        -> pick_point(State,Point)
        ; nth0(0,NonEmptyPoints,Point)
    ).

%!      pick_point(+State:state -Point:atom) is det.
%       pick_point/2
%       Get a point that is unknown on the map either on a selected range or randomly.
pick_point(State,Point):-
	pick_distance(State,Distance),
	(
		Distance > 0
		->pick_point_in_a_distance(State,Distance,DistancePoints),
        get_state_map(State,Map),
        pick_valid_points(DistancePoints,Map,Points)
		;
		pick_point_random(State, Points)
	),
    removeEmpty(Points,NonEmptyPoints),
	nth0(0,NonEmptyPoints,Point).

%!      pick_valid_point(+X:int-Y:int, +Points:list, -Point:list) is det.
%       pick_valid_point/3
%       if the intend selected X-Y is unknown on the map then it will be return.
%       This is used in determining wether picked points are valid to be selected for determining the path because we dont need to select the point that we have explored before.
pick_valid_point(_,[],empty).
pick_valid_point(X-Y,[Xx-Yy-Status|MapPoints],Point):-
    (
        X = Xx, Y = Yy, Status = unknown
        ->Point = X-Y
        ;
        pick_valid_point(X-Y,MapPoints,Point)
    ).

%!      pick_valid_points(Points:int, +Map:list, -Point:list) is det.
%       pick_valid_points/3
%       if the intend selected list of X-Y is unknown on the map then it will be return.
%       This is used in determining wether picked points are valid to be selected for determining the path because we dont need to select the point that we have explored before.
pick_valid_points([],_,[]).
pick_valid_points([Point|Points],Map,ValidPoints):-
    pick_valid_point(Point,Map,ValidPoint),
    ValidPoints = [ValidPoint|OtherPoints],
    pick_valid_points(Points,Map,OtherPoints).

%!      pick_point_random(State:state,-Points:list) is det.
%       pick_point_random/2
%       Pick all the poinst that is unknown on the map. 
pick_point_random(State, Points):-
	get_state_map(State,Map),
	get_map_points_by_feedback(Map,unknown,Points).

%!      pick_point_in_a_distance(State:state,+Distatnce:int. -Points:list) is det.
%       pick_point_in_a_distance/3
%       Pick all the points in a range. in a radius of the distance value.
pick_point_in_a_distance(State,Distance,Points):-
	get_state_initial_point(State,InitialPoint),
	get_point_distance_points(InitialPoint,Distance,State,Points).

%!      pick_distance(State:state, -Distatnce:int) is det.
%       pick_distance/2
%       We only explore a samll range at beginning. then as there is more information we have no limitation on the radius of the points to be discovered.
pick_distance(State,Distance):-
	get_state_round(State,Round),
	(
		Round < 3
		-> Distance = 2
		;
		Round < 6
		-> Distance = 4
		;
		Distance = -1
	).



/** 

All the getter for the State below

*/

get_state_round(State,Round):-
	get_state_history(State,History),
	length(History,Round).

get_state_initial_point((_,_,InitialX-InitialY,_,_,_,_,_), InitialX-InitialY).
get_state_row_number((NumberRow,_,_,_,_,_,_,_), NumberRow).
get_state_column_number((_,NumberColumn,_,_,_,_,_,_), NumberColumn).
get_state_initial_x((_,_,InitialX-_,_,_,_,_,_), InitialX).
get_state_initial_y((_,_,_-InitialY,_,_,_,_,_), InitialY).
get_state_map((_,_,_,Map,_,_,_,_), Map).
get_state_history((_,_,_,_,History,_,_,_), History).
get_state_wumpus_point((_,_,_,_,_,WumpusPoint,_,_), WumpusPoint).
get_state_stench_point((_,_,_,_,_,_,StenchPoints,_), StenchPoints).
get_state_smell_point((_,_,_,_,_,_,_,SmellPoints), SmellPoints).

set_state_history((NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints),NewGuess,(NumberRow,NumberColumn,InitialX-InitialY,Map,[NewGuess | History],WumpusPoint,StenchPoints,SmellPoints)).
set_state_map((NumberRow,NumberColumn,InitialX-InitialY,_,History,WumpusPoint,StenchPoints,SmellPoints),NewMap,(NumberRow,NumberColumn,InitialX-InitialY,NewMap,History,WumpusPoint,StenchPoints,SmellPoints)).
set_state_wumpus((NumberRow,NumberColumn,InitialX-InitialY,Map,History,_,StenchPoints,SmellPoints),WumpusPoint,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints)).
set_state_stench((NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints),NewStenchPoints,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,[NewStenchPoints|StenchPoints],SmellPoints)).
set_state_smell((NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints),NewSmellPoint,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,[NewSmellPoint|SmellPoints])).

get_min_x(_,1).
get_max_x((_,NumberColumn,_,_,_,_,_),NumberColumn).
get_min_y(_,1).
get_max_y((NumberRow,_,_,_,_,_,_),NumberRow).

/** 

Determine the x an y values from a X or Y point in a radius(DISTANCE).
e.g 2-2 with distance of 1 will get 1-2, 2-1, 3-2,2-3
*/

get_west_x_distance_points(X,MinX,Distance,XWestPoints):-
	(
		X > MinX, Distance > 0
		-> NewX is X - 1,
		NewDistance is Distance - 1,
		XWestPoints = [NewX| XWestOtherPoints],
		get_west_x_distance_points(NewX,MinX,NewDistance,XWestOtherPoints)
		;
		XWestPoints = []
	).
	
get_east_x_distance_points(X,MaxX,Distance,XEastPoints):-
	(
		X < MaxX, Distance > 0
		-> NewX is X + 1,
		NewDistance is Distance - 1,
		XEastPoints = [NewX|XEastOtherPoints],
		get_east_x_distance_points(NewX,MaxX,NewDistance,XEastOtherPoints)
		;
		XEastPoints = []
	).
	
get_south_y_distance_points(Y,MaxY,Distance,YSouthPoints):-
	(
		Y < MaxY, Distance > 0
		-> NewY is Y + 1,
		NewDistance is Distance - 1,
		YSouthPoints = [NewY|YSouthOtherPoints],
		get_south_y_distance_points(NewY,MaxY,NewDistance,YSouthOtherPoints)
		;
		YSouthPoints = []
	).
	
get_north_y_distance_points(Y,MinY,Distance,YNorthPoints):-
	(
		Y > MinY, Distance > 0
		-> NewY is Y - 1,
		NewDistance is Distance - 1,
		YNorthPoints = [NewY| YNorthOtherPoints],
		get_north_y_distance_points(NewY,MinY,NewDistance,YNorthOtherPoints)
		;
		YNorthPoints = []
	).

get_x_distance_points(X,MinX,MaxX,Distance,XPoints):-
	get_west_x_distance_points(X,MinX,Distance,XWestPoints),	
	get_east_x_distance_points(X,MaxX,Distance,XEastPoints),
	XEastPointsInclude = [X|XEastPoints],
	append(XWestPoints,XEastPointsInclude,XPoints).	

get_y_distance_points(Y,MinY,MaxY,Distance,YPoints):-
	get_north_y_distance_points(Y,MinY,Distance,YNorthPoints),	
	get_south_y_distance_points(Y,MaxY,Distance,YSouthPoints),
	YSouthPointsInclude = [Y|YSouthPoints],
	append(YNorthPoints,YSouthPointsInclude,YPoints).	

combine_points_map([],_,[]).
combine_points_map([X|XPoints],YPoints,Points):-
	combine_single_point_map(X,YPoints,XYPoints),
	append(XYPoints,OtherXYPoints,Points),
	combine_points_map(XPoints,YPoints,OtherXYPoints).

combine_points([],_,[]).
combine_points([X|XPoints],YPoints,Points):-
	combine_single_point(X,YPoints,XYPoints),
	append(XYPoints,OtherXYPoints,Points),
	combine_points(XPoints,YPoints,OtherXYPoints).

combine_single_point(_,[],[]).
combine_single_point(X,[Y|YPoints],[X-Y|Points]):-
	combine_single_point(X,YPoints,Points).

combine_single_point_map(_,[],[]).
combine_single_point_map(X,[Y|YPoints],[X-Y-unknown|Points]):-
	combine_single_point_map(X,YPoints,Points).

%!      get_point_distance_points(+X:int-Y:int, +Distatnce:int, +State:state, -Points:list) is det.
%       get_point_distance_points/4
%       Get all the X-Y position based on the radis(Distance) from a point.
get_point_distance_points(X-Y,Distance,State,Points):-
	get_min_x(State,MinX),
	get_max_x(State,MaxX),
	get_min_y(State,MinY),
	get_max_y(State,MaxY),
	get_x_distance_points(X,MinX,MaxX,Distance,XPoints),
	get_y_distance_points(Y,MinY,MaxY,Distance,YPoints),
	combine_points(XPoints,YPoints,CombinedPoints),
	delete(CombinedPoints,X-Y,Points).

get_manhattan_points(X-Y,Distance,State,Points):-
	get_point_distance_points(X-Y,Distance,State,SquarePoints),
	filter_bigger_distance(X-Y,SquarePoints,Distance,Points).

filter_bigger_distance(_,[],_,[]).
filter_bigger_distance(CenterPoint,[CurrentPoint|OtherPoints],Distance,FilteredPoints):-
	(
		bigger_than_distance(CenterPoint,CurrentPoint, Distance)
		-> FilteredPoints = [CurrentPoint|OtherFilteredPoints],
			filter_bigger_distance(CenterPoint,OtherPoints,Distance,OtherFilteredPoints)
		;
		filter_bigger_distance(CenterPoint,OtherPoints,Distance,FilteredPoints)
	).

bigger_than_distance(X1-Y1,X2-Y2,Distance):-
	XDiff is X2 - X1,
	YDiff is Y2 - Y1,
	abs(XDiff,XAbsDiff),
	abs(YDiff,YAbsDiff),
	TotalDiff = XAbsDiff + YAbsDiff,
	(
		TotalDiff =< Distance
		-> true
		;
		false
	).


getMinX(_,1).
getMaxX(Column,Column).

getMinY(_,1).
getMaxY(Row,Row).

%!      generate_edges(+Row:int +Column:int, E:list) is det.
%       generate_edges/3
%       Generates all the possible paths for each point on the map.
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
    generate_edges_point_west(X,Y,MinX,WE),
    generate_edges_point_east(X,Y,MaxX,EE),
    generate_edges_point_north(X,Y,MinY,NE),
    generate_edges_point_south(X,Y,MaxY,SE),
    TE = [WE,EE,NE,SE],
    removeEmpty(TE,E).

generate_edges_point_west(X,Y,MinX,E):-
    (
        X > MinX
        -> NewX is X - 1,
        E = (X-Y, west, NewX-Y)
        ;
        E = empty
    ).

generate_edges_point_east(X,Y,MaxX,E):-
    (
        X < MaxX
        -> NewX is X + 1,
        E = (X-Y, east, NewX-Y)
        ;
        E = empty
    ).

generate_edges_point_north(X,Y,MinY,E):-
    (
        Y > MinY
        -> NewY is Y - 1,
        E = (X-Y, north, X-NewY)
        ;
        E = empty
    ).

generate_edges_point_south(X,Y,MaxY,E):-
    (
        Y < MaxY
        -> NewY is Y + 1,
        E = (X-Y, south, X-NewY)
        ;
        E = empty
    ).


/** 
 * 
 * 
        All the utility function to help the above oprations.

*
*/

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
        length(Previous,PLength),
        \+ PLength > 10,
        find(Med, End, [Med|Previous], Path).

%% find a simple Path from Start to End
find_no_restriction(Start, End, Path) :-
        find_no_restriction(Start, End, [Start], Path).

find_no_restriction(Start, Start, _Previous, []).
find_no_restriction(Start, End, Previous, [Dirn|Path]) :-
        edge(Start, Dirn, Med),
        \+ member(Med, Previous), % dont visit previous places
        find_no_restriction(Med, End, [Med|Previous], Path).        

get_map_points_by_feedback([],_,[]).
get_map_points_by_feedback([_-_-OtherFeedback|MapPoints],Feedback,Points):-
	OtherFeedback \= Feedback,
	get_map_points_by_feedback(MapPoints,Feedback,Points).
get_map_points_by_feedback([X-Y-Feedback|MapPoints],Feedback,Points):-
	Points = [X-Y|OtherPoints],
	get_map_points_by_feedback(MapPoints,Feedback,OtherPoints).


%Get and initialized of map in the system.
get_map(NumberRow,NumberColumn,Map):-
    numlist(1,NumberRow,AllRows),
    numlist(1,NumberColumn,AllColumns),
    combine_points_map(AllColumns,AllRows,Map).

%inswert a new edges to the system.
insert_edges([]).
insert_edges([(From,Dir,To)|List]):-
    assert(edge(From,Dir,To)),
    insert_edges(List).

delete_edges(X-Y):-
    (
        edge(X-Y,_,_)
        ->retract(edge(X-Y,_,_)),
        delete_edges(X-Y)
        ;
        edge(_,_,X-Y)
        ->retract(edge(_,_,X-Y)),
        delete_edges(X-Y)
        ;
        true
    ).

removeEmpty([],[]).
removeEmpty([X|Xs],E):-
    (
        X \= empty
        -> E = [X|Ee],
        removeEmpty(Xs,Ee)
        ;
        removeEmpty(Xs,E)
    ).

replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- 
    replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- 
    H \= O, 
    replace(O, R, T, T2).


shoot_dir_change([Dir1|Dirs],NewGuess):-
    shoot_dir_change(Dirs,Dir1,ShootGuess),
    AddedGuess = [Dir1|ShootGuess],
    flatten(AddedGuess, NewGuess).

shoot_dir_change([], _Previous, []).
shoot_dir_change([Dir1|Dirs],Previous,[AddDir|NewGuess]):-
    (
        Dir1 \= Previous
        -> AddDir = [Dir1 | shoot],
        shoot_dir_change(Dirs,Dir1,NewGuess)
        ;
        AddDir = Dir1,
        shoot_dir_change(Dirs,Dir1,NewGuess)
    ).