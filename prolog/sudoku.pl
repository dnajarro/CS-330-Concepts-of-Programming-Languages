% A Sudoku solver.  The basic idea is for each position,
% check that it is a digit with `digit`.  Then verify that the digit
% chosen doesnt violate any constraints (row, column, and cube).
% If no constraints were violated, proceed further.  If a constraint
% was violated, then backtrack to the last digit choice and move from
% there (the Prolog engine should handle this for you automatically).
% If we reach the end of the board with this scheme, it means that
% the whole thing is solved.

% YOU SHOULD FILL IN THE SOLVE PROCEDURE, DOWN BELOW.

digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

numBetween(Num, Lower, Upper) :-
        Num >= Lower,
        Num =< Upper.

% cubeBounds: (RowLow, RowHigh, ColLow, ColHigh, CubeNumber)
cubeBounds(0, 2, 0, 2, 0).
cubeBounds(0, 2, 3, 5, 1).
cubeBounds(0, 2, 6, 8, 2).
cubeBounds(3, 5, 0, 2, 3).
cubeBounds(3, 5, 3, 5, 4).
cubeBounds(3, 5, 6, 8, 5).
cubeBounds(6, 8, 0, 2, 6).
cubeBounds(6, 8, 3, 5, 7).
cubeBounds(6, 8, 6, 8, 8).

% Given a board and the index of a column of interest (0-indexed),
% returns the contents of the column as a list.
% columnAsList: (Board, ColumnNumber, AsRow)
columnAsList([], _, []).
columnAsList([Head|Tail], ColumnNum, [Item|Rest]) :-
        nth0(ColumnNum, Head, Item),
        columnAsList(Tail, ColumnNum, Rest).

% given which row and column we are in, gets which cube
% is relevant.  A helper ultimately for `getCube`.
% cubeNum: (RowNum, ColNum, WhichCube)
cubeNum(RowNum, ColNum, WhichCube) :-
        cubeBounds(RowLow, RowHigh, ColLow, ColHigh, WhichCube),
        numBetween(RowNum, RowLow, RowHigh),
        numBetween(ColNum, ColLow, ColHigh).

% Drops the first N elements from a list.  A helper ultimately
% for `getCube`.
% drop: (InputList, NumToDrop, ResultList)
drop([], _, []):-!.
drop(List, 0, List):-!.
drop([_|Tail], Num, Rest) :-
        Num > 0,
        NewNum is Num - 1,
        drop(Tail, NewNum, Rest).

% Takes the first N elements from a list.  A helper ultimately
% for `getCube`.
% take: (InputList, NumToTake, ResultList)
take([], _, []):-!.
take(_, 0, []):-!.
take([Head|Tail], Num, [Head|Rest]) :-
        Num > 0,
        NewNum is Num - 1,
        take(Tail, NewNum, Rest).

% Gets a sublist of a list in the same order, inclusive.
% A helper for `getCube`.
% sublist: (List, Start, End, NewList)
sublist(List, Start, End, NewList) :-
        drop(List, Start, TempList),
        NewEnd is End - Start + 1,
        take(TempList, NewEnd, NewList).

% Given a board and cube number, gets the corresponding cube as a list.
% Cubes are 3x3 portions, numbered from the top left to the bottom right,
% starting from 0.  For example, they would be numbered like so:
%
% 0  1  2
% 3  4  5
% 6  7  8
%
% getCube: (Board, CubeNumber, ContentsOfCube)
getCube(Board, Number, AsList) :-
        cubeBounds(RowLow, RowHigh, ColLow, ColHigh, Number),
        sublist(Board, RowLow, RowHigh, [Row1, Row2, Row3]),
        sublist(Row1, ColLow, ColHigh, Row1Nums),
        sublist(Row2, ColLow, ColHigh, Row2Nums),
        sublist(Row3, ColLow, ColHigh, Row3Nums),
        append(Row1Nums, Row2Nums, TempRow),
        append(TempRow, Row3Nums, AsList).

% Given a board, solve it in-place.
% After calling `solve` on a board, the board should be fully
% instantiated with a satisfying Sudoku solution.

replaceNums(_, _, _, _, 9) :- !.

replaceNums(Board, Cols, Cubes, RowNum, ColNum) :-
        cubeNum(RowNum, ColNum, CubeNum),
        % get current row
        nth0(RowNum, Board, CurRow),
        % get current column
        nth0(ColNum, Cols, CurCol),
        % get current cube
        nth0(CubeNum, Cubes, CurCube),
        nth0(ColNum, CurRow, F),
        (nonvar(F), !; var(F), digit(F), is_set(CurRow), is_set(CurCol),
         is_set(CurCube)),
         NewColNum is ColNum + 1,
        replaceNums(Board, Cols, Cubes, RowNum, NewColNum).

fillOut([_|_], _, _, 9, _).

fillOut([FirstRow | Rest], ColsAsList, CubesAsList, RowNum, ColNum) :-
        % write("Row: "), write(RowNum), write("Col: "), write(ColNum), write([FirstRow|Rest]), nl,
        replaceNums([FirstRow | Rest], ColsAsList, CubesAsList, RowNum, ColNum),
        NewRowNum is RowNum + 1,
        fillOut([FirstRow|Rest], ColsAsList, CubesAsList, NewRowNum, 0).

preprocess(Board, ColsAsList, CubesAsList) :-
        columnAsList(Board, 0, ColA),
        columnAsList(Board, 1, ColB),
        columnAsList(Board, 2, ColC),
        columnAsList(Board, 3, ColD),
        columnAsList(Board, 4, ColE),
        columnAsList(Board, 5, ColF),
        columnAsList(Board, 6, ColG),
        columnAsList(Board, 7, ColH),
        columnAsList(Board, 8, ColI),

        append([], [ColA], [OneCol]),
        append([OneCol], [ColB], TwoCols),
        append(TwoCols, [ColC], ThreeCols),
        append(ThreeCols, [ColD], FourCols),
        append(FourCols, [ColE], FiveCols),
        append(FiveCols, [ColF], SixCols),
        append(SixCols, [ColG], SevenCols),
        append(SevenCols, [ColH], EightCols),
        append(EightCols, [ColI], ColsAsList),

        getCube(Board, 0, CubeA),
        getCube(Board, 1, CubeB),
        getCube(Board, 2, CubeC),
        getCube(Board, 3, CubeD),
        getCube(Board, 4, CubeE),
        getCube(Board, 5, CubeF),
        getCube(Board, 6, CubeG),
        getCube(Board, 7, CubeH),
        getCube(Board, 8, CubeI),

        append([], [CubeA], [OneCube]),
        append([OneCube], [CubeB], TwoCubes),
        append(TwoCubes, [CubeC], ThreeCubes),
        append(ThreeCubes, [CubeD], FourCubes),
        append(FourCubes, [CubeE], FiveCubes),
        append(FiveCubes, [CubeF], SixCubes),
        append(SixCubes, [CubeG], SevenCubes),
        append(SevenCubes, [CubeH], EightCubes),
        append(EightCubes, [CubeI], CubesAsList).

solve(Board) :- preprocess(Board, ColsAsList, CubesAsList),
fillOut(Board, ColsAsList, CubesAsList, 0, 0).

% Prints out the given board.
printBoard([]).
printBoard([Head|Tail]) :-
        write(Head), nl,
        printBoard(Tail).

test1(Board) :-
        Board = [[2, _, _, _, 8, 7, _, 5, _],
                 [_, _, _, _, 3, 4, 9, _, 2],
                 [_, _, 5, _, _, _, _, _, 8],
                 [_, 6, 4, 2, 1, _, _, 7, _],
                 [7, _, 2, _, 6, _, 1, _, 9],
                 [_, 8, _, _, 7, 3, 2, 4, _],
                 [8, _, _, _, _, _, 4, _, _],
                 [3, _, 9, 7, 4, _, _, _, _],
                 [_, 1, _, 8, 2, _, _, _, 5]],
        solve(Board),
        printBoard(Board).

test2(Board) :-
        Board = [[_, _, _, 7, 9, _, 8, _, _],
                 [_, _, _, _, _, 4, 3, _, 7],
                 [_, _, _, 3, _, _, _, 2, 9],
                 [7, _, _, _, 2, _, _, _, _],
                 [5, 1, _, _, _, _, _, 4, 8],
                 [_, _, _, _, 5, _, _, _, 1],
                 [1, 2, _, _, _, 8, _, _, _],
                 [6, _, 4, 1, _, _, _, _, _],
                 [_, _, 3, _, 6, 2, _, _, _]],
        solve(Board),
        printBoard(Board).
