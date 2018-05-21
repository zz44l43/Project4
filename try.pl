read_file(Filename, Content) :-

        open(Filename, read, Stream),

        read_lines(Stream, Content),

        close(Stream).





% read_line/3.

read_line(Stream, Line, Last) :-

        get_char(Stream, Char),

        (   Char = end_of_file

        ->  Line = [],

                Last = true
        ).

% print_puzzle/3.

print_puzzle(SolutionFile, Puzzle) :-

        open(SolutionFile, write, Stream),

        maplist(print_row(Stream), Puzzle),

        close(Stream).


% read_line/2.

read_line(Stream, Row) :-

        maplist(put_puzzle_char(Stream), Row),

        nl(Stream).

% put_puzzle_char/2.


% print_row/2.

print_row(Stream, Row) :-

        maplist(put_puzzle_char(Stream), Row),

        nl(Stream).


put_puzzle_char(Stream, Char) :-

        (   var(Char)

        ->  put_char(Stream, '_')

        ;   put_char(Stream, Char)

        ).

% read_lines/2.

read_lines(Stream, Content) :-

        read_line(Stream, Line, Last),

        (   Last = true

        ->  (   Line = []

                ->  Content = []

                ;   Content = [Line]

                )

        ;  Content = [Line|Content1],

                read_lines(Stream, Content1)

        ).

get_Row(Row,C,O):-
        (
            C = 1
            -> O = [(Row,0)]
            ;
            Cc is C - 1,
            O = [(Row,Cc)|O1],
            get_Row(Row,Cc,O1) 
        ).
        
get_Map(R,O):-
    get_Map(R,R,O).

get_Map(R,C,O):-
    (
        R > 0
        -> Rr is R -1,
        get_Roww(R,C,0,Ro),
        append(Ro,O1,O),
        get_Map(Rr,C,O1)
        ;
        get_Roww(R,C,0,O)
    ).

get_Roww(Row,C,A,O):-
        (
            C > 0
            -> Cc is C-1,
            Aa is A + 1,
            O = [(Row,A)|O1],
            get_Roww(Row,Cc,Aa,O1)
            ;
            O =[(Row,A)]
        ).
