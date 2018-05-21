 

        The University of Melbourne 

        Department of Computing and Information Systems 

        Declarative Programming 

        COMP90048 

    Author:        Zhi Zheng

    E-mail:        zhiz2@student.unimelb.edu.au

    

        This program is practice software; 

        he objective of this project is to practice and assess the understanding of logic programming and Prolog. 

        Software is inteded solve fillin crossword puzzles problem.

        

*/


%   loaded_with_clpfd in order to use transpose function.

:- ensure_loaded(library(clpfd)).


% main/3.

%       The starting point of the program it will execute read the puzzle and words from the inputs names.

%       Then valid and solve the puzzle input file

%       In the last, print out the puzzel.


main(PuzzleFile, WordlistFile, SolutionFile) :-

        read_file(PuzzleFile, Puzzle),

        read_file(WordlistFile, Wordlist),

        valid_puzzle(Puzzle),

        solve_puzzle(Puzzle, Wordlist, Solved),

        print_puzzle(SolutionFile, Solved).


% read_file/2.

read_file(Filename, Content) :-

        open(Filename, read, Stream),

        read_lines(Stream, Content),

        close(Stream).


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


% read_line/3.

read_line(Stream, Line, Last) :-

        get_char(Stream, Char),

        (   Char = end_of_file

        ->  Line = [],

                Last = true

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

% valid_puzzle/1.

valid_puzzle([]).

valid_puzzle([Row|Rows]) :-

        maplist(samelength(Row), Rows).


% samelength/2.

samelength([], []).

samelength([_|L1], [_|L2]) :-

        same_length(L1, L2).


% fill/1.

%       Filling in the variable for the slot.

fill(_).


% transform/2.

%       Transform the puzzle to local variables or solid. If it's '_' then assign a logical variable, 

%       Otherwise assign solid to it.

transform([], []).

transform([E|Es], [Sq|Sqs]) :-

        samelength(Es, Sqs),

        (E == '_' -> Sq = fill(_);

        E == '#' -> Sq = 'solid';

        Sq = fill(E)

        ),

        transform(Es, Sqs).
                                 

% nofill/2.

%       Convert the filled slots to unfilled recursively.

nofill([],[]).

nofill([fill(F)|Fs],[F|Fd]):-

    samelength(Fs,Fd),

    nofill(Fs,Fd).


% trans_puzzle/2.

%       Tranform each of the puzzle slots to it's transformed versiont that is by assigning the logical variables and solid to each slot.

trans_puzzle([],[]).

trans_puzzle(Rs, Ts):-

        maplist(transform, Rs, Ts).


% trans_fill/2.

%       Recursively convert the filled puzzles to list of slots by calling extract functions and resturn all the solutions to it by using setof.

trans_fill([],[]).

trans_fill([T|TransformedList], Slots):-

        (setof(E, extract(E, T), Eslot) ->

        append(Eslot, Slots2, Slots),

        trans_fill(TransformedList, Slots2);

        trans_fill(TransformedList, Slots)).


% extract/2.

%       The extract function below collectively convert the list of transformed puzzle to a list slots.

%       Each slots list consisted the slots that existed between solid.

extract(X, Y) :-

        append(X, [solid|_], Y),

        length(X, L),

        L > 1,

        \+ member(solid, X).


% extract/2.    

extract(X, Y) :-

        append(_, [solid|X], Y),

        length(X, L),

        L > 1,

        \+ member(solid, X).


% extract/2.            

extract(X, Y) :-

        append(Front, [solid|_], Y),

        append(_, [solid|X], Front),

        length(X, L),

        L > 1,

        \+ member(solid, X).


% extract/2.    

extract(X,X) :-

        length(X, L),

        L > 1,

        \+ member(solid, X).


% extract/2.    

extract(X,X) :-

        length(X, L),

        L > 1,

        \+ member(solid, X).


% match/3.

%       Match the slot to it's potential matching words by using member funciton to directly compare the logical variables with the words.

match([],[],[]).

match(Slot, Words, M) :-

        setof(Slot, member(Slot, Words), M).


% matches/4.

%       Recursively match the slot and counts to find all the possilbe matching words by calling match function.

matches([],_,[],[]).

matches([Slot|Slots],Words,[M|All_matches], [triple(Slot,M,C)|Triples]) :-

        match(Slot,Words,M),

        length(M,C),

        matches(Slots, Words, All_matches, Triples).


% get_counts/2.

%       Recursively find the smallest count within the set.

%       The set contains the slot and its all possilbe mathes from the words and number of these matches words.

get_counts([],[]).

get_counts([C1|Cs],[triple(_,_,C1)|Tail]):-

        same_length(Cs,Tail),

        get_counts(Cs,Tail).


% sel_slot/3.

%       Select a slot has the fewest potential matches and start from there. 

sel_slot(Slot,Matches,List):-

        get_counts(Counts, List),

        sort(Counts, [C1|_]),

        member(triple(Slot,Matches,C1), List).


% fillslot/2.

%       fill the slot with the potential matches words.

fillslot(Slot,Matches):-

        member(Slot, Matches).


% fillall/2.

%       Recursively fill all the slots with Words by calling matches, sel_slot, fillslot, select words and slots.

%       First find all the self defined data set called Triples by calling matches. This will return a list of Triples that contains the slot and its all possilbe mathes from the words and number of these matches words.

%       Secondly, select the slot that ready to be filled by calling sel_slot.

%       Thridly, fill the slot.

%       Select the word that be filled and removed from the avalible words to be filled by using select function.

%       Select the slot that be filled and removed from the avalible slots to be filled by using select function.

                                                                                                                                                                               206,1-8       75%
fillall([],[]).

fillall(Slots, Words) :-

        matches(Slots, Words, _, Triples),

        sel_slot(S, Match, Triples),

        fillslot(S, Match),

        select(S, Words, Words2),

        select(S,Slots,Slots2),

        fillall(Slots2, Words2).


% unfill/2.     

unfill([], []).

unfill([fill(X)|Ys],[X|Xs]) :-

    samelength(Xs, Ys),

    unfill(Ys, Xs).


%unfill_list/2. 

%       Transform the filled list to it unfilled status by remove the fill from it. 

unfill_list(Filledlist, Unfilllist) :-

    maplist(unfill, Filledlist, Unfilllist).


%reform/2.

%       Add back the '#' for printing purpose.

reform([], []).

reform([S|Ss], [C|Cs]) :-

    samelength(Ss, Cs),

    (S = fill(X) -> C = X;

    C = '#'

    ),

    reform(Ss, Cs).


% puzzle_reconstruct/2.

%       Tranform the slots by calling the reform to reconstruct the puzzel with #.

puzzle_reconstruct(Tslot, Result) :-

    maplist(reform, Tslot, Result).


% solve_puzzle/3.

%       Steps to solve the puzzle.

%       Firstly, transformt the puzzle by assigning logical varialbe by calling the trans_puzzle.

%       Secondly, fill up the all the puzzle by removing the #.

%       Thridly, transpose the original puzzle.

%       Fourthly, fill up the transposed puzzle again.

%       Fifithly, append the transposed and non-transposed list to form the total list.

%       Sixly, Unfill the puzzle list.

%       Then call the fillall to fill the puzzel with words.

%       Lastly, reconstruct the puzzle to form the final result.
                                                                                                                          253,1-8       99%

solve_puzzle(Puzzle, Wordlist, Result) :-

        trans_puzzle(Puzzle, Tslot),

        trans_fill(Tslot,Fslots),

        transpose(Tslot, Transposedslots),

        trans_fill(Transposedslots,Fslots2),

        append(Fslots,Fslots2,Totalslot),

        unfill_list(Totalslot, Unfillslots),

        fillall(Unfillslots, Wordlist),

    puzzle_reconstruct(Tslot, Result).


