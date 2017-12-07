
EmptyBoard = [[0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0]].

winLine(P, [X1, X2, X3, X4, X5, X6, X7]) :-
	equal(X1, X2, X3, X4, P) ;
    equal(X2, X3, X4, X5, P) ;
    equal(X3, X4, X5, X6, P) ;
    equal(X4, X5, X6, X7, P) .
    
winCol(P, [X1, X2, X3, X4, X5, X6]) :-
    equal(X1, X2, X3, X4, P) ;
    equal(X2, X3, X4, X5, P) ;
    equal(X3, X4, X5, X6, P) .


% winPos(+Player, +Board) 
% % True if Player win in Board. 
winPos(P, [[X1, X2, X3, X4, X5, X6, X7],
       [X8, X9, X10, X11, X12, X13, X14],
       [X15, X16, X17, X18, X19, X20, X21],
       [X22, X23, X24, X25, X26, X27, X28],
       [X29, X30, X31, X32, X33, X34, X35],
       [X36, X37, X38, X39, X40, X41, X42]]) :-
    
    winLine(P, [X1, X2, X3, X4, X5, X6, X7]);
    winLine(P, [X8, X9, X10, X11, X12, X13, X14]);
    winLine(P, [X15, X16, X17, X18, X19, X20, X21]);
    winLine(P, [X22, X23, X24, X25, X26, X27, X28]);
    winLine(P, [X29, X30, X31, X32, X33, X34, X35]);
    winLine(P,  [X36, X37, X38, X39, X40, X41, X42]);
    
    winCol(P, [X1, X8, X15, X22, X29, X36]);
    winCol(P, [X2, X9, X16, X23, X30, X37]);
    winCol(P, [X3, X10, X17, X24, X31, X38]);
    winCol(P, [X4, X11, X18, X25, X32, X39]);
    winCol(P, [X5, X12, X19, X26, X33, X40]);
    winCol(P, [X6, X13, X20, X27, X34, X41]);
    winCol(P, [X7, X14, X21, X28, X35, X42]);

    
    equal(X4, X10, X16, X22, P) ;    % 1st diag
    equal(X5, X11, X17, X23, P) ;    % 1st diag
    equal(X6, X12, X18, X24, P) ;    % 1st diag
    equal(X7, X13, X19, X25, P) ;    % 1st diag
    
    equal(X11, X17, X23, X29, P) ;    % 1st diag
    equal(X12, X18, X24, X30, P) ;    % 1st diag
    equal(X13, X19, X25, X31, P) ;    % 1st diag
    equal(X14, X20, X26, X32, P) ;    % 1st diag
    
    equal(X18, X24, X30, X36, P) ;    % 1st diag
    equal(X19, X25, X31, X37, P) ;    % 1st diag
    equal(X20, X26, X32, X38, P) ;    % 1st diag
    equal(X21, X27, X33, X39, P) ;    % 1st diag
    
    
    equal(X1, X9, X17, X25, P) ;    % 2nd diag
    equal(X2, X10, X18, X26, P) ;    % 2nd diag
    equal(X3, X11, X19, X27, P) ;    % 2nd diag
    equal(X4, X12, X20, X28, P) ;    % 2nd diag
    
    equal(X8, X16, X24, X32, P) ;    % 2nd diag
    equal(X9, X17, X25, X33, P) ;    % 2nd diag
    equal(X10, X18, X26, X34, P) ;    % 2nd diag
    equal(X11, X19, X27, X35, P) ;    % 2nd diag
    
    equal(X15, X23, X31, X39, P) ;    % 2nd diag
    equal(X16, X24, X32, X40, P) ;    % 2nd diag
    equal(X17, X25, X33, X41, P) ;    % 2nd diag
    equal(X18, X26, X34, X42, P) .   % 2nd diag


% drawPos(+Player, +Board) 
% True if the game is a draw. 
drawPos(_,Board) :-
    \+ member(0, Board).

equal(X, X, X, X, X).

% utility(+Pos, -Val) :-
% True if Val is the result of the evaluation function at Pos. 
% We will only evaluate for final position. 
% So we will only have MAX win, MIN win or draw. 
% We will use  1 when MAX win 
%             -1 when MIN win 
%              0 otherwise. 
utility([o, win, _], 1).       % Previous player (MAX) wins. 
utility([x, win, _], -1).      % Previous player (MIN) wins. 
utility([_, draw, _], 0).

moves(Pos,NextPosList):-
    findall(NextPos, move(Pos, NextPos), NextPosList).
move([X1, play, Board], [X2, win, NextBoard]) :-
    nextPlayer(X1, X2), move_aux(X1, Board, NextBoard), winPos(X1, NextBoard), !.
move([X1, play, Board], [X2, draw, NextBoard]) :-
    nextPlayer(X1, X2), move_aux(X1, Board, NextBoard), drawPos(X1,NextBoard), !.
move([X1, play, Board], [X2, play, NextBoard]) :-
    nextPlayer(X1, X2), move_aux(X1, Board, NextBoard).

% True if NextBoard is Board with an empty case replaced by Player mark. 
move_aux(P, [0|Bs], [P|Bs]). % TO FUCKING DO! URGENT SHIT STUFF
move_aux(P, [B|Bs], [B|B2s]) :-
     move_aux(P, Bs, B2s).

% nextPlayer(X1, X2) 
% True if X2 is the next player to play after X1. 
nextPlayer(o, x). 
nextPlayer(x, o).

% minimax(Pos, BestNextPos, Val) 
% Pos is a position, Val is its minimax value. 
% Best move from Pos leads to position BestNextPos. 
minimax(Pos, BestNextPos, Val) :-
    moves(Pos,NextPosList), 
    best(NextPosList, BestNextPos, Val), !.

minimax(Pos, _, Val) :-
    utility(Pos, Val).

best([Pos], Pos, Val) :-
    minimax(Pos, _, Val), !.

best([Pos1 | PosList], BestPos, BestVal) :-
    minimax(Pos1, _, Val1), 
    best(PosList, Pos2, Val2), 
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :- % Pos0 better than Pos1 
    min_to_move(Pos0),                         % MIN to move in Pos0 
    Val0 > Val1, !                             % MAX prefers the greater value 
    ; 
    max_to_move(Pos0),                         % MAX to move in Pos0 
    Val0 < Val1, !.                            % MIN prefers the lesser value 
betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

min_to_move([o, _, _]). 
max_to_move([x, _, _]).

