get_initial_state(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints,(NumberRow,NumberColumn,InitialX-InitialY,Map,History,WumpusPoint,StenchPoints)).


get_state_row_number(NumberRow,_,_,_,_,_,_, NumberRow).
get_state_column_number(_,NumberColumn,_,_,_,_,_, NumberColumn).
get_state_initial_x(_,_,InitialX-_,_,_,_,_, InitialX).
get_state_initial_y(_,_,_-InitialY,_,_,_,_, InitialY).
get_state_map(_,_,_,Map,_,_,_, Map).
get_state_history(_,_,_,_,History,_,_, History).
get_state_wumpus_point(_,_,_,_,_,WumpusPoint,_, WumpusPoint).
get_state_stench_point(_,_,_,_,_,_,StenchPoints, StenchPoints).


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
	append(XWestPoints,XEastPoints,XPoints).	

get_y_distance_points(Y,MinY,MaxY,Distance,YPoints):-
	get_north_y_distance_points(Y,MinY,Distance,YNorthPoints),	
	get_south_y_distance_points(Y,MaxY,Distance,YSouthPoints),
	append(YNorthPoints,YSouthPoints,YPoints).	

combine_points([],_,[]).
combine_points([X|XPoints],YPoints,Points):-
	combine_single_point(X,YPoints,XYPoints),
	append(XYPoints,OtherXYPoints,Points),
	combine_points(XPoints,YPoints,OtherXYPoints).

combine_single_point(_,[],[]).
combine_single_point(X,[Y|YPoints],[X-Y|Points]):-
	combine_single_point(X,YPoints,Points).

get_point_distance_points(X-Y,Distatnce,State,Points):-
	get_min_x(State,MinX),
	get_max_x(State,MaxX),
	get_min_y(State,MinY),
	get_max_y(State,MaxY),
	get_x_distance_points(X,MinX,MaxX,Distatnce,XPoints),
	get_y_distance_points(Y,MinY,MaxY,Distatnce,YPoints),
	combine_points(XPoints,YPoints,Points).
	