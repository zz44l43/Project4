initialState(NR,NC,XS,YS,State):-
    generate_edges(NR,NC,E),
    insert_edges(E),
    get_map(NR,NC,Map),
	nl(),
	write(Map),
    replace(XS-YS-_,XS-YS-empty,Map,UpdatedMap),
	get_initial_state(NR,NC,XS-YS,UpdatedMap,[],empty,[],State),
	write(State).

get_initial_state(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints)).


is_search_mode(State):-
	get_state_wumpus_point(State,WumpusPoint),
	(
		WumpusPoint = empty
		-> true
		;
		false
	).

get_state_row_number((NumberRow,_,_,_,_,_,_), NumberRow).
get_state_column_number((_,NumberColumn,_,_,_,_,_), NumberColumn).
get_state_initial_x((_,_,InitialX-_,_,_,_,_), InitialX).
get_state_initial_y((_,_,_-InitialY,_,_,_,_), InitialY).
get_state_map((_,_,_,Map,_,_,_), Map).
get_state_history((_,_,_,_,History,_,_), History).
get_state_wumpus_point((_,_,_,_,_,WumpusPoint,_), WumpusPoint).
get_state_stench_point((_,_,_,_,_,_,StenchPoints), StenchPoints).

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
combine_single_point_map(X,[Y|YPoints],[X-Y-unkown|Points]):-
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