my_read_file(File, List):-
                open(File, read, Stream),
                read_lines(Stream, List),
                close(Stream).

read_lines(Stream, Content) :-
        read_line(Stream, Line, Last),
        (   Last = true
        ->  (   Line = []
                ->  Content = []
                ;   Content = [Line]
                )
        ;  Content = [Line|Content1],
                write(Content),
                read_lines(Stream, Content1)
        ).

read_line(Stream, Line, Last) :-
        get_char(Stream, Char),
        (   Char = end_of_file
        ->  Line = [],
                Last = true
        ; Char = '\n'
        ->  Line = [],
                Last = false
        ;   Line = [Char|Line1],
                read_line(Stream, Line1, Last)
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
    get_Map(R,R,O),
    length(O, L),
    print(L).
        

get_Map(R,C,O):-
    (
        R > 1
        -> Rr is R -1,
        get_Roww(R,C,1,Ro),
        append(Ro,O1,O),
        get_Map(Rr,C,O1)
        ;
        get_Roww(R,C,1,O)
    ).

get_Roww(Row,C,A,O):-
        (
            C > 1
            -> Cc is C-1,
            Aa is A + 1,
            O = [(Row,A, " ")|O1],
            get_Roww(Row,Cc,Aa,O1)
            ;
            O =[(Row,A," ")]
        ).

generate_path((X,Y),Row,Col,(Dx,Dy),Edges):-
        (
                Dx > X
                -> NewX is X + 1,
                Edges = [((X,Y), "east", (NewX,Y))|Follow],
                generate_path((NewX,Y),Row,Col, (Dx,Dy),Follow)
                ;
                Dy > Y
                -> NewY is Y + 1,
                Edges = [((X,Y), "south", (X,NewY))|Follow],
                generate_path((X,NewY),Row,Col, (Dx,Dy),Follow)
                ;
                Edges = []
        ).

move_x(X,Dx,Path):-
        {
                Dx > X
                -> NewX is X + 1,
                Path = [NewX|Follow],
                move_x(NewX,Dx,Follow)
                ;
                Path = [X]
        }.


move(X,Y,Dx,Dy,Path):-
        move_x2(X,Dx,XPath),
        move_y2(Y,Dy,YPath),
        pair(XPath,YPath,Path).

pair([],_,[]).
pair([X|Xs],Y,Path):-
        pair_s(X,Y,Path),
        pair(Xs,Y,Path).

pair_s(_,[],[]).
pair_s(X,[Y|Ys],[X-Y|Path]):-
        pair_s(X,Ys,Path).



move_x2(Dx,Dx,[Dx]).
move_x2(X,Dx,[X|Path]):-
        (
        X<Dx
        -> NewX is X + 1,
        move_x2(NewX, Dx,Path)
        ).

move_y2(Dy,Dy,[Dy]).
move_y2(Y,Dy,[Y|Path]):-
        (
        Y<Dy
        -> NewY is Y + 1,
        move_y2(NewY, Dy, Path)
        ).

aa((X,Y),E):-
        NewX is X + 1,
        E = (NewX,Y).

points(L) :- setof( (X,Y), (member(X,[0,1,2,3,4,5,6,7,8,9,10]), member(Y,[0,1,2,3,4,5,6,7,8,9,10])), L).