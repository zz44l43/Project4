initialState(NR,NC,XS,YS,State):-
    generate_edges(NR,NC,E),
    insert_edges(E),
    get_map(NR,NC,Map),
	nl(),
	write(Map),
    replace(XS-YS-_,XS-YS-empty,Map,UpdatedMap),
	get_initial_state(NR,NC,XS-YS,UpdatedMap,[],empty,[],[],State),
	write(State).

get_initial_state(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints)).

is_search_mode(State):-
	get_state_wumpus_point(State,WumpusPoint),
	(
		WumpusPoint = empty
		-> true
		;
		false
	).

guess(StateO,State,Guess):-
	(
	is_search_mode(StateO)
	->get_search_mode_next_point(StateO,StateSearch,SearchGuess)
	;
	get_state_wumpus_point_path(StateO,StateSearch,SearchGuess)
	),
	set_state_history(StateSearch,SearchGuess,State),
	add_shoot(SearchGuess,Guess),
	nl(),
	write("State is "),
	write(State),
	nl(),
	write("Guess is "),
	write(Guess).
	
updateState(StateO, Guess, Feedback, State):-
	get_state_initial_point(StateO,InitialPoint),
	updateState(StateO,Guess,Feedback,InitialPoint,State).

updateState(State,[],[],_,State).

updateState(StateO,[shoot|Guess], [_|Feedback], X-Y, State):-
	nl(),
	write("SHOOT STATE UPDATE"),
    updateState(StateO,Guess, Feedback, X-Y, State).

updateState(StateO,[Dir|Guess], [stench|Feedback], X-Y, State):-
	nl(),
	write("STENCH STATE UPDATE"),
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,stench,MapState),
	set_state_stench(MapState,PostPosition,StenchState),
	write(StenchState),
	updateState(StenchState,Guess,Feedback,PostPosition,State).
	
updateState(StateO,[Dir|Guess], [smell|Feedback], X-Y, State):-
	nl(),
	write("SMELL STATE UPDATE"),
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,smell,MapState),
	set_state_smell(MapState,PostPosition,SmellState),
	write(SmellState),
	updateState(SmellState,Guess,Feedback,PostPosition,State).

updateState(StateO,[Dir|Guess], [empty|Feedback], X-Y, State):-
	nl(),
	write("EMPTY STATE UPDATE"),
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,empty,MapState),
	write(MapState),
	updateState(MapState,Guess,Feedback,PostPosition,State).

updateState(StateO,[Dir|Guess], [wall|Feedback], X-Y, State):-
	nl(),
	write("WALL STATE UPDATE"),
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,wall,MapState),
	delete_edges(PostPosition),
	write(MapState),
	updateState(MapState,Guess,Feedback,X-Y,State).

updateState(StateO,[Dir|Guess], [pit|Feedback], X-Y, State):-
	nl(),
	write("PIT STATE UPDATE"),
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,pit,MapState),
	delete_edges(PostPosition),
	write(MapState),
	updateState(MapState,Guess,Feedback,X-Y,State).


updateState(StateO,[Dir|Guess], [wumpus|Feedback], X-Y, State):-
	nl(),
	write("WUMPUS STATE UPDATE"),
	getPositionAfterFeedback(X-Y, Dir,PostPosition),
	updateMap(StateO,PostPosition,empty,MapState),
	set_state_wumpus(MapState,PostPosition,WumpusState),
	write(WumpusState),
	updateState(WumpusState,Guess,Feedback,PostPosition,State).

updateMap(State,X-Y,Feedback,ReplacedState):-
	get_state_map(State,MapO),
    replace(X-Y-_,X-Y-Feedback,MapO,Map),
	set_state_map(State,Map,ReplacedState).

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

get_state_wumpus_point_path(StateO,StateO,SearchGuess):-
	get_state_wumpus_point(StateO,WumpusPoint),
	pick_path_point(StateO,WumpusPoint,SearchGuess).


%To add shot to each of the instrunction so the robot will fire wildly.
add_shoot([],[]).
add_shoot([Dir|OtherS],[Dir,shoot|Other]):-
    add_shoot(OtherS,Other).

get_search_mode_next_point(StateO,StateO,Guess):-
	path_by_random(StateO,Guess).


path_by_random(State,Path):-
	pick_point(State,Point),
	pick_path_point(State,Point,Path).

pick_path_point(State,Point,Path):-
	get_state_history(State,History),
	get_state_initial_point(State,InitialPoint),
    findall(FindPath,find(InitialPoint,Point,FindPath),AllPaths),
	subtract(AllPaths,History,NewPaths),
	pick_distance(State,Distance),
	get_mini_paths(Distance,MiniPaths),
	filter_short_length_list(MiniPaths,NewPaths,NoShortPath),
	sort_atoms_by_length(NoShortPath,SortedPath),
	nth0(0,SortedPath,Path).

sort_atoms_by_length(Atoms, ByLength) :-
	map_list_to_pairs(length, Atoms, Pairs),
	keysort(Pairs, Sorted),
	pairs_values(Sorted, ByLength).

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


pick_point(State,Point):-
	pick_distance(State,Distance),
	(
		Distance > 0
		->pick_point_in_a_distance(State,Distance,Points)
		;
		pick_point_random(State, Points)
	),
	nth0(0,Points,Point).
	
pick_point_random(State, Points):-
	get_state_map(State,Map),
	get_map_points_by_feedback(Map,unknown,Points).

pick_point_in_a_distance(State,Distance,Points):-
	get_state_initial_point(State,InitialPoint),
	get_point_distance_points(InitialPoint,Distance,State,Points).

pick_distance(State,Distance):-
	get_state_round(State,Round),
	(
		Round < 3
		-> Distance = 2
		;
		Round < 6
		-> Distance = 4
		;
		Round = -1
	).

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
get_state_stench_point((_,_,_,_,_,_,_,SmellPoints), SmellPoints).

set_state_history((NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints),NewGuess,(NumberRow,NumberColumn,InitialX-InitialY,Map,[NewGuess | History],WumpusPoint,StenchPoints,SmellPoints)).
set_state_map((NumberRow,NumberColumn,InitialX-InitialY,_,History,WumpusPoint,StenchPoints,SmellPoints),NewMap,(NumberRow,NumberColumn,InitialX-InitialY,NewMap,History,WumpusPoint,StenchPoints,SmellPoints)).
set_state_wumpus((NumberRow,NumberColumn,InitialX-InitialY,Map,History,_,StenchPoints,SmellPoints),WumpusPoint,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints)).
set_state_stench((NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints),NewStenchPoints,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,[NewStenchPoints|StenchPoints],SmellPoints)).
set_state_smell((NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,SmellPoints),NewSmellPoint,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,[NewSmellPoint|SmellPoints])).

get_min_x(_,1).
get_max_x((_,NumberColumn,_,_,_,_,_),NumberColumn).
get_min_y(_,1).
get_max_y((NumberRow,_,_,_,_,_,_),NumberRow).

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

get_map_points_by_feedback([],_,[]).
get_map_points_by_feedback([_-_-OtherFeedback|MapPoints],Feedback,Points):-
	OtherFeedback \= Feedback,
	get_map_points_by_feedback(MapPoints,Feedback,Points).
get_map_points_by_feedback([X-Y-Feedback|MapPoints],Feedback,Points):-
	Points = [X-Y-Feedback|OtherPoints],
	get_map_points_by_feedback(MapPoints,Feedback,OtherPoints).


%Get and initialized of map in the system.
get_map(NumberRow,NumberColumn,Map):-
	numlist(1,NumberRow,AllRows),
	numlist(1,NumberColumn,AllColumns),
	combine_points_map(AllRows,AllColumns,Map).

%inswert a new edges to the system.
insert_edges([]).
insert_edges([(From,Dir,To)|List]):-
    assert(edge(From,Dir,To)),
	write(edge(From,Dir,To)),
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