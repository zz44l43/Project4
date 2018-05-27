get_search_mode_smell_guess(StateO,State,SearchGuess):-
    get_state_smell_point(StateO,SmellPoints),
    get_manhattan_points_smell(StateO,SmellPoints,ManhattanPoints),