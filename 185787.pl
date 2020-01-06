:- module(iaib185787, [iaib185787/3]).

look_ahead_on(4).

evaluate_board(board(Black, White), Score) :-
    list_count(Black, fig(man, _, _), BlackMans),
    list_count(Black, fig(king, _, _), BlackKings),
    list_count(White, fig(man, _, _), WhiteMans),
    list_count(White, fig(king, _, _), WhiteKings),
    evaluate_pieces(White, WhiteMiddle),
    evaluate_pieces(Black, BlackMiddle),
    Score is (10 * WhiteMans + 50 * WhiteKings + WhiteMiddle - 10 * BlackMans - 50 * BlackKings - BlackMiddle).

list_member(List, Member)               :- member(Member, List).
list_append(List1, List2, Result)       :- append(List1, List2, Result).
list_delete_one(List, Element, NewList) :- select(Element, List, NewList).
list_length(List, Length)               :- length(List, Length).
list_absent(List, Element)              :- \+ (member(Element, List)).
list_subtract(List1, List2, Result)     :- subtract(List1, List2, Result).
list_last_element(List, Element)        :- last(List, Element).

% Count number of elements in a list which are unifiable with provided element.
list_count([], _, 0).
list_count([FirstElement | Tail], Element, Occurrences) :-
    unifiable(FirstElement, Element, _),
    list_count(Tail, Element, Tmp),
    Occurrences is (Tmp + 1), !.
list_count([_ | Tail], Element, Occurrences) :- list_count(Tail, Element, Occurrences).

% Figure position evaluation. Just checking for center of board atm.

evaluate_pieces([], 0).
evaluate_pieces([fig(_, Y, X) | Tail], Score) :-
    evaluate_pieces(Tail, Tmp),
    (
        (X >= 3, X =< 4, Y >= 2, Y =< 5, Score is (Tmp + 1))
            ;
        (Score is Tmp)
    ), !.

% There are two sides: Black and White.
side(white).
side(black).
other_side(black, white).
other_side(white, black).

coordinate(X) :- list_member([0, 1, 2, 3, 4, 5, 6, 7], X).

cell(X, Y) :- coordinate(Y), coordinate(X).

pos(X, Y) :-
    cell(X, Y),
    Z is ((X + Y) mod 2),
    Z = 0.

% There are two kinds of figures: man and king.
fig(man, X, Y)  :- pos(X, Y).
fig(king, X, Y) :- pos(X, Y).

diagonal_is_free(_, _, X, Y, X, Y) :- !.

diagonal_is_free(List1, List2, X1, Y1, X2, Y2) :-
    Dx is sign(X2 - X1), Dy is sign(Y2 - Y1),
    NextX is (X1 + Dx), NextY is (Y1 + Dy),
    list_absent(List1, fig(_, NextX, NextY)),
    list_absent(List2, fig(_, NextX, NextY)),
    diagonal_is_free(List1, List2, NextX, NextY, X2, Y2).

% Possible moves for man. (X1, Y1) - starting position, (X2, Y2) - destination position.
possible_move(white, fig(man, X1, Y1), fig(Type, X2, Y2)) :-
    Y2 is (Y1 + 1), Y2 =< 7,
    (X2 is X1 + 1 ; X2 is X1 - 1), X2 >= 0, X2 =< 7,
    ( (Y2 = 7, Type = king) ; (Y2 < 7, Type = man) ).

possible_move(black, fig(man, X1, Y1), fig(Type, X2, Y2)) :-
    Y2 is (Y1 - 1), Y2 >= 0,
    (X2 is X1 + 1 ; X2 is X1 - 1), X2 =< 7, X2 >= 0,
    ( (Y2 = 0, Type = king) ; (Y2 > 0, Type = man) ).

% Possible moves for king.
possible_move(_, fig(king, X1, Y1), fig(king, X2, Y2)) :-
    fig(king, X2, Y2), X2 >= 0, X2 =< 7, Y2 >= 0, Y2 =< 7,
    X1 \= X2,  Y1 \= Y2,
    % Can move diagonally (in any direction)
    DiagDiff1 is (X1 - Y1), DiagDiff2 is (X2 - Y2),
    DiagSum1 is (X1 + Y1), DiagSum2 is (X2 + Y2),
    (DiagDiff1 = DiagDiff2 ; DiagSum1 = DiagSum2).

% For men only need to check that the destination cell is free.
way_is_free(ListWithMove, ListWithoutMove, fig(man, _, _), fig(_, X2, Y2)) :-
    list_absent(ListWithMove,    fig(_, X2, Y2)),
    list_absent(ListWithoutMove, fig(_, X2, Y2)).

% For king all cells on diagonal between (X1, Y1) and (X2, Y2) should be free.
way_is_free(ListWithMove, ListWithoutMove, fig(king, X1, Y1), fig(king, X2, Y2)) :-
    diagonal_is_free(ListWithMove, ListWithoutMove, X1, Y1, X2, Y2).

move_figure(Side, ListWithMove, ListWithoutMove, NewListWithMove) :-
    list_delete_one(ListWithMove, StartingPosition, TmpListWithMove),
    possible_move(Side, StartingPosition, DestPosition),
    way_is_free(ListWithMove, ListWithoutMove, StartingPosition, DestPosition),
    list_append(TmpListWithMove, [DestPosition], NewListWithMove).

% Move figure of the specified 'Side' on board.
move_figure(black, board(Black, White), board(NewBlack, White)) :-
    move_figure(black, Black, White, NewBlack).

move_figure(white, board(Black, White), board(Black, NewWhite)) :-
    move_figure(white, White, Black, NewWhite).


diagonal_positions(pos(X, Y), _, _, []) :- (X < 0 ; X > 7 ; Y < 0 ; Y > 7), !.
diagonal_positions(pos(X, Y), Dx, Dy, [pos(X, Y) | Tail]) :-
    NextX is (X + Dx), NextY is (Y + Dy),
    diagonal_positions(pos(NextX, NextY), Dx, Dy, Tail).


must_eat(fig(_, X1, Y1), Depth) :-
        continueEating(X, Y),
        (
            (X = 0 , Y = 0)
                ;
            (
             not((X = 0, Y = 0)),
             NewX is X - 1,
             NewY is Y - 1,
             look_ahead_on(InitialDepth),
             Depth == InitialDepth,
%             write(X1),write(NewY),nl,
%             write(Y1),write(NewX),nl,
%             write(Depth),nl,nl,
             Y1 == NewX,
             X1 == NewY)
        ).

% take with man
possible_capture(Side, fig(man, X1, Y1), fig(_, X2, Y2), fig(Type, X3, Y3), Depth) :-
    must_eat(fig(man, X1, Y1), Depth),
    Dx is (X2 - X1), Dy is (Y2 - Y1),
    AbsDx is abs(Dx), AbsDy is abs(Dy),
    AbsDx = 1, AbsDy = 1,
    X3 is (X2 + Dx), Y3 is (Y2 + Dy),
    X3 >= 0, X3 =< 7, Y3 >= 0, Y3 =< 7,
    (
        ( Side = white, ((Y3 = 7, Type = king) ; (Y3 < 7, Type = man)) ) ;
        ( Side = black, ((Y3 = 0, Type = king) ; (Y3 > 0, Type = man)) )
    ).

% take with king
possible_capture(_, fig(king, X1, Y1), fig(_, X2, Y2), fig(king, X3, Y3), Depth) :-
    must_eat(fig(king, X1, Y1), Depth),
    Sum1 is (X1 + Y1), Sum2 is (X2 + Y2),
    Dif1 is (X1 - Y1), Dif2 is (X2 - Y2),
    (Sum1 = Sum2 ; Dif1 = Dif2),
    Dx is sign(X2 - X1), Dy is sign(Y2 - Y1),
    NextX is (X2 + Dx), NextY is (Y2 + Dy),
    diagonal_positions(pos(NextX, NextY), Dx, Dy, AllowableDestPositions),
    list_member(AllowableDestPositions, pos(X3, Y3)).



way_for_capture_is_free(CapturingList, CapturedList, fig(man, _, _), fig(_, _, _), fig(_, X3, Y3)) :-
    list_absent(CapturingList, fig(_, X3, Y3)),
    list_absent(CapturedList,  fig(_, X3, Y3)).

way_for_capture_is_free(CapturingList, CapturedList, fig(king, X1, Y1), fig(_, X2, Y2), fig(_, X3, Y3)) :-
    Dx is sign(X2 - X1), Dy is sign(Y2 - Y1),
    PrevX is (X2 - Dx), PrevY is (Y2 - Dy),
    diagonal_is_free(CapturingList, CapturedList, X1, Y1, PrevX, PrevY),
    diagonal_is_free(CapturingList, CapturedList, X2, Y2, X3, Y3).

potential_intermediate_king_pos(Side, CapturingList, CapturedList, CapturedFiguresList, pos(X1, Y1), pos(X2, Y2), pos(X3, Y3), Depth) :-
    list_member(CapturedList, fig(_, X2, Y2)),
    possible_capture(Side, fig(king, X1, Y1), fig(_, X2, Y2), fig(king, X3, Y3), Depth),
    way_for_capture_is_free(CapturingList, CapturedList, fig(king, X1, Y1), fig(_, X2, Y2), fig(king, X3, Y3)),
    list_absent(CapturedFiguresList, fig(_, X2, Y2)).

potential_intermediate_king_pos_with_capture(Side, CapturingList, CapturedList, CapturedFiguresList, pos(X1, Y1), pos(X2, Y2), pos(X3, Y3), Depth) :-
    potential_intermediate_king_pos(Side, CapturingList, CapturedList, CapturedFiguresList, pos(X1, Y1), pos(X2, Y2), pos(X3, Y3), Depth),
    list_delete_one(CapturingList, fig(king, X1, Y1), TmpCapturingList),
    list_append(TmpCapturingList, [fig(king, X3, Y3)], NewCapturingList),
    list_append(CapturedFiguresList, [fig(man, X2, Y2)], NewCapturedFiguresList),
    potential_intermediate_king_pos(Side, NewCapturingList, CapturedList, NewCapturedFiguresList, pos(X3, Y3), _, _, Depth).

potential_intermediate_king_pos_list(Side, CapturingList, CapturedList, CapturedFiguresList, pos(X1, Y1), pos(X2, Y2), List, Depth) :-
    findall(Position, potential_intermediate_king_pos_with_capture(Side, CapturingList, CapturedList, CapturedFiguresList, pos(X1, Y1), pos(X2, Y2), Position, Depth), List).

allowable_intermediate_capture_pos(_, _, _, _, fig(man, _, _), _, _, _) :- !.

allowable_intermediate_capture_pos(Side, CapturingList, CapturedList, CapturedFiguresList, fig(king, X1, Y1), fig(_, X2, Y2), fig(king, X3, Y3), Depth) :-
    potential_intermediate_king_pos_list(Side, CapturingList, CapturedList, CapturedFiguresList, pos(X1, Y1), pos(X2, Y2), IntermediatePositionsWithCapture, Depth),
    (
        (IntermediatePositionsWithCapture = [], true, !) ;
        (list_member(IntermediatePositionsWithCapture, pos(X3, Y3)), !)
    ).

capture_single_figure(Side, CapturingList, CapturedList, NewCapturingList, CapturedFiguresList, NewCapturedFiguresList, CapturingFigure, NewCapturingFigure, Depth) :-
    list_delete_one(CapturingList, CapturingFigure, TmpCapturingList),
    list_member(CapturedList, CapturedFigure),
    possible_capture(Side, CapturingFigure, CapturedFigure, NewCapturingFigure, Depth),
    way_for_capture_is_free(CapturingList, CapturedList, CapturingFigure, CapturedFigure, NewCapturingFigure),
    CapturedFigure = fig(_, X2, Y2),
    allowable_intermediate_capture_pos(Side, CapturingList, CapturedList, CapturedFiguresList, CapturingFigure, CapturedFigure, NewCapturingFigure, Depth),
    list_absent(CapturedFiguresList, fig(_, X2, Y2)),
    list_append(TmpCapturingList, [NewCapturingFigure], NewCapturingList),
    list_append(CapturedFiguresList, [CapturedFigure], NewCapturedFiguresList).

capture_figures(Side, CapturingList, CapturedList, NewCapturingList, CapturedFiguresList, NewCapturedFiguresList, CapturingFigure, NewCapturingFigure, Depth) :-
    capture_single_figure(Side, CapturingList, CapturedList, TmpCapturingList, CapturedFiguresList, TmpCapturedFiguresList, CapturingFigure, TmpCapturingFigure, Depth),
    capture_multiple_figures(Side, TmpCapturingList, CapturedList, NewCapturingList, TmpCapturedFiguresList, NewCapturedFiguresList, TmpCapturingFigure, NewCapturingFigure, Depth).

capture_multiple_figures(Side, CapturingList, CapturedList, NewCapturingList, CapturedFiguresList, NewCapturedFiguresList, CapturingFigure, NewCapturingFigure, Depth) :-
    capture_single_figure(Side, CapturingList, CapturedList, _, CapturedFiguresList, _, CapturingFigure, _, Depth), !,
    capture_figures(Side, CapturingList, CapturedList, NewCapturingList, CapturedFiguresList, NewCapturedFiguresList, CapturingFigure, NewCapturingFigure, Depth).
capture_multiple_figures(_, CapturingList, _, CapturingList, CapturedList, CapturedList, CapturingFigure, CapturingFigure, _).

capture(black, board(Black, White), board(NewBlack, NewWhite), Depth) :-
    capture_figures(black, Black, White, NewBlack, [], CapturedList, _, _, Depth),
    list_subtract(White, CapturedList, NewWhite).

capture(white, board(Black, White), board(NewBlack, NewWhite), Depth) :-
    capture_figures(white, White, Black, NewWhite, [], CapturedList, _, _, Depth),
    list_subtract(Black, CapturedList, NewBlack).

move_capture(Side, Board1, Board2, Depth) :- capture(Side, Board1, Board2, Depth).
move(Side, Board1, Board2, Depth) :- capture(Side, Board1, _, Depth), !, move_capture(Side, Board1, Board2, Depth).

move(Side, board(Black, White), board(NewBlack, NewWhite), _) :-
    move_figure(Side, board(Black, White), board(NewBlack, NewWhite)).

swap_node_type(min, max).
swap_node_type(max, min).

best_score_and_board(max, Board1, Score1, Board2, Score2, BestBoard, BestScore) :-
    (Score1 >= Score2, !, BestBoard = Board1, BestScore = Score1) ;
    (Score1  < Score2, BestBoard = Board2, BestScore = Score2).
best_score_and_board(min, Board1, Score1, Board2, Score2, BestBoard, BestScore) :-
    (Score1 =< Score2, !, BestBoard = Board1, BestScore = Score1) ;
    (Score1  > Score2, BestBoard = Board2, BestScore = Score2).

alpha_beta_prune(max, _, Beta, Value)  :- Value > Beta.
alpha_beta_prune(min, Alpha, _, Value) :- Value < Alpha.

update_alpha_beta(max, Alpha, Beta, Value, NewAlpha, Beta) :- (Value > Alpha, !, NewAlpha = Value) ; (NewAlpha = Alpha).
update_alpha_beta(min, Alpha, Beta, Value, Alpha, NewBeta) :- (Value < Beta,  !, NewBeta = Value) ; (NewBeta = Beta).

possible_moves_list(Side, Board, List, Depth) :- findall(NewBoard, move(Side, Board, NewBoard, Depth), List).

find_best_move(_, _, [BestBoard], BestBoard, BestScore, 0, _, _) :-
    evaluate_board(BestBoard, BestScore), !.

find_best_move(Side, NodeType, [Board1 | Tail], BestBoard, BestScore, 0, Alpha, Beta) :-
    evaluate_board(Board1, Score1),
    (
        (alpha_beta_prune(NodeType, Alpha, Beta, Score1), !, BestBoard = Board1, BestScore = Score1) ;
        (
            find_best_move(Side, NodeType, Tail, Board2, Score2, 0, Alpha, Beta),
            best_score_and_board(NodeType, Board1, Score1, Board2, Score2, BestBoard, BestScore)
        )
    ).

find_best_move(Side, NodeType, [BestBoard], BestBoard, BestScore, Depth, Alpha, Beta) :-
    Depth > 0,
    NewDepth is (Depth - 1),
    swap_node_type(NodeType, NextNodeType),
    other_side(Side, OtherSide),
    minmax(OtherSide, NextNodeType, BestBoard, _, BestScore, NewDepth, Alpha, Beta), !.

find_best_move(Side, NodeType, [Board1 | Tail], BestBoard, BestScore, Depth, Alpha, Beta) :-
    Depth > 0,
    NewDepth is (Depth - 1),
    swap_node_type(NodeType, NextNodeType),
    other_side(Side, OtherSide),
    minmax(OtherSide, NextNodeType, Board1, _, Score1, NewDepth, Alpha, Beta),
    (
        (alpha_beta_prune(NodeType, Alpha, Beta, Score1), !, BestBoard = Board1, BestScore = Score1) ;
        (
            update_alpha_beta(NodeType, Alpha, Beta, Score1, NewAlpha, NewBeta),
            find_best_move(Side, NodeType, Tail, Board2, Score2, Depth, NewAlpha, NewBeta),
            best_score_and_board(NodeType, Board1, Score1, Board2, Score2, BestBoard, BestScore)
        )
    ).

minmax(Side, _, Board, Board, Score, Depth, _, _) :-
    possible_moves_list(Side, Board, [], Depth),
    evaluate_board(Board, Score), !.

minmax(Side, NodeType, Board, BestBoard, BestScore, Depth, Alpha, Beta) :-
    possible_moves_list(Side, Board, PossibleMoves, Depth),
    find_best_move(Side, NodeType, PossibleMoves, BestBoard, BestScore, Depth, Alpha, Beta).

smart_move(white, Board, NewBoard) :-
    look_ahead_on(Depth),
    minmax(white, max, Board, NewBoard, _, Depth, -100000, 100000).
smart_move(black, Board, NewBoard) :-
    look_ahead_on(Depth),
    minmax(black, min, Board, NewBoard, _, Depth, -100000, 100000).

play_move(Side) :-
    make_board(B1),
    smart_move(Side, B1, B2),
    update_board(Side, B1, B2).

update_board(Side, B1, B2) :- remove_pieces(Side, B1, B2).

remove_pieces(Side, B1, B2) :-
    (
        (
            findall(fig(_, TempX, TempY), get_eaten(fig(_, TempX, TempY), B1, B2, Side), List),
            List = [_|_],
            move_pieces_after_remove(Side, B1, B2, X, Y, List),
            NewX is X + 1, NewY is Y + 1,
            retract(ruut(NewY, NewX, _)), asserta(ruut(NewY, NewX, 0)) , write("Case 1")
        )
            ;
        (
            findall(fig(_, TempX, TempY), get_eaten(fig(man, TempX, TempY), B1, B2, Side), List),
            List = [], move_pieces(Side, B1, B2), write("Case 2")
        )
    ).

get_eaten(Figure, board(Blacks1, Whites1), board(Blacks2, Whites2), Side) :-
        ( Side = black, member(Figure, Whites1), not(member(Figure, Whites2)) ) ;
        ( Side = white, member(Figure, Blacks1), not(member(Figure, Blacks2)) ).


move_pieces(Side, B1, B2) :-
    get_move(fig(BEFORE, X1, Y1), B1, B2, Side),
    get_move(fig(AFTER, X, Y), B2, B1, Side),
    NewX is X + 1, NewX1 is X1 + 1, NewY is Y + 1, NewY1 is Y1 + 1,
    (
        (BEFORE = AFTER, move_answer_figure(NewY, NewX, NewY1, NewX1))
            ;
        (not(BEFORE = AFTER), move_answer_figure_and_update(NewY, NewX, NewY1, NewX1))
    ).

:- dynamic best/3.
closestRem([], _, _) :- asserta(best(1000, 0, 0)).
closestRem([fig(_, X, Y)|Tail], X1, Y1) :- closestRem(Tail, X1, Y1),
    TX1 is X - X1, pow(TX1, 2, TempX),
    TY1 is Y - Y1, pow(TY1, 2, TempY),
    Temp is TempX + TempY,
    retract(best(Last, LastX, LastY)),
    (
        (Temp < Last, asserta(best(Temp, X, Y)))
            ;
        (Temp >= Last, asserta(best(Last, LastX, LastY)))
    ).

move_pieces_after_remove(Side, B1, B2, RemX, RemY, Rems) :-
    get_move(fig(AFTER,_,_), B1, B2, Side),
    get_move(fig(BEFORE,X,Y), B2, B1, Side),
    closestRem(Rems, X, Y),

    best(_, RemX, RemY), abolish(best/3),
    NewX is X + 1, NewY is Y + 1,
    (
        (
        TempX is RemX - X,
        (
            (TempX < 0, AfterX is NewX + (RemX - X) - 1)
                ;
            (TempX > 0, AfterX is NewX + (RemX - X) + 1)
        ), TempY is RemY - Y,
        (
            (TempY < 0, AfterY is NewY + (RemY - Y) - 1)
                ;
            (TempY > 0, AfterY is NewY + (RemY - Y) + 1)
        ),

            (
                (BEFORE = AFTER, move_answer_figure(NewY, NewX, AfterY, AfterX))
                    ;
                (not(BEFORE = AFTER),
                    ((not(((Side = black, AfterY = 1) ; (Side = white, AfterY = 8))), (move_answer_figure(NewY, NewX, AfterY, AfterX)))
                        ;
                        (((Side = black, AfterY = 1) ; (Side = white, AfterY = 8)),
                            (Temp1 is NewY - AfterY, Temp2 is NewX - AfterX, abs(Temp1, Diagonal1), abs(Temp2, Diagonal2),
                                (
                                    (Diagonal1 = Diagonal2, Diagonal2 = 2, move_answer_figure_and_update(NewY, NewX, AfterY, AfterX))
                                        ;
                                    (not((Diagonal1 = Diagonal2, Diagonal2 = 2)), move_answer_figure(NewY, NewX, AfterY, AfterX))
                                )
                            )
                        )
                    )
                )
            )
        )
     ).

move_answer_figure_and_update(X, Y, X1, Y1) :-
    retract(ruut(X1, Y1, _)),
    retract(ruut(X, Y, C)),
    asserta(ruut(X, Y, 0)), NewC is C * 10,
    asserta(ruut(X1, Y1, NewC)).

move_answer_figure(X, Y, X1, Y1) :-
    retract(ruut(X1, Y1, _)),
    retract(ruut(X, Y, C)),
    asserta(ruut(X, Y, 0)),
    asserta(ruut(X1, Y1, C)).

get_move(Figure, board(Blacks1, Whites1), board(Blacks2, Whites2), Side) :-
    ( Side = black, member(Figure, Blacks2), not(member(Figure, Blacks1)) )
        ;
    ( Side = white, member(Figure, Whites2), not(member(Figure, Whites1))).

make_board(board(Blacks, Whites)) :- make_blacks(Blacks), make_whites(Whites).

make_blacks(Blacks) :-
    findall(Black, transfer_black(Black), Blacks).

transfer_black(Black) :-
    ruut(Y1, X1, C), X is X1 - 1, Y is Y1 - 1,
    (
        (C = 2, Black = fig(man, X, Y))
            ;
        (C = 20, Black = fig(king, X, Y))).

make_whites(Whites) :-
    findall(White, transfer_whites(White), Whites).

transfer_whites(White) :-
    ruut(Y1, X1, C), X is X1 - 1, Y is Y1 - 1,
    (
        (C = 1, White = fig(man, X, Y))
            ;
        (C = 10, White = fig(king, X, Y))
    ).

:- dynamic continueEating/2.

iaib185787(Color, X, Y) :-
    (
        asserta(continueEating(X, Y)),
        ((Color = 1, write("WHITE"), nl, play_move(white))
            ;
        (Color = 2, write("BLACK"), nl, play_move(black))
            ;
        true),
        abolish(continueEating/2)
    ).

iaib185787(_, _, _).
