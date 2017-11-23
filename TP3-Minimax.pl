      
EmptyBoard = [0, 0, 0, 0, 0, 0, 0, 0, 0].

% winPos(+Player, +Board) 
% % True if Player win in Board. 
winPos(P, [X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
    equal(X1, X2, X3, X4, P) ;    % 1st line
    equal(X2, X3, X4, X5, P) ;    % 1st line
    equal(X3, X4, X5, X6, P) ;    % 1st line
    equal(X4, X5, X6, X7, P) ;    % 1st line
    
    equal(X8, X9, X10, X11, P) ;    % 2nd line
    equal(X9, X10, X11, X12, P) ;    % 2nd line
    equal(X10, X11, X12, X13, P) ;    % 2nd line
    equal(X11, X12, X13, X14, P) ;    % 2nd line
    
    equal(X15, X16, X17, X18, P) ;    % 3rd line
    equal(X16, X17, X18, X19, P) ;    % 3rd line
    equal(X17, X18, X19, X20, P) ;    % 3rd line
    equal(X18, X19, X20, X21, P) ;    % 3rd line
  
    equal(X22, X23, X24, X25, P) ;    % 4th line
    equal(X23, X24, X25, X26, P) ;    % 4th line
    equal(X24, X25, X26, X27, P) ;    % 4th line
    equal(X25, X26, X27, X28, P) ;    % 4th line
    
    equal(X29, X30, X31, X32, P) ;    % 5th line
    equal(X30, X31, X32, X33, P) ;    % 5th line
    equal(X31, X32, X33, X34, P) ;    % 5th line
    equal(X32, X33, X34, X35, P) ;    % 5th line
    
    equal(X36, X37, X38, X39, P) ;    % 6th line
    equal(X37, X38, X39, X40, P) ;    % 6th line
    equal(X38, X39, X41, X42, P) ;    % 6th line
    equal(X39, X40, X41, X42, P) ;    % 6th line
    
    equal(X1, X8, X15, X22, P) ;    % 1st col
    equal(X8, X15, X22, X29, P) ;    % 1st col
    equal(X15, X22, X29, X36, P) ;    % 1st col
    
    equal(X2, X9, X16, X23, P) ;    % 2nd col
    equal(X9, X16, X23, X30, P) ;    % 2nd col
    equal(X16, X23, X30, X37, P) ;    % 2nd col
    
    equal(X3, X10, X17, X24, P) ;    % 3rd col
    equal(X10, X17, X24, X31, P) ;    % 3rd col
    equal(X17, X24, X31, X38, P) ;    % 3rd col
    
    equal(X4, X11, X18, X25, P) ;    % 4th col
    equal(X11, X18, X25, X32, P) ;    % 4th col
    equal(X18, X25, X32, X39, P) ;    % 4th col
    
    equal(X5, X12, X19, X26, P) ;    % 5th col
    equal(X12, X19, X26, X33, P) ;    % 5th col
    equal(X19, X26, X33, X40, P) ;    % 5th col
    
    equal(X6, X13, X20, X27, P) ;    % 6th col
    equal(X13, X20, X27, X34, P) ;    % 6th col
    equal(X20, X27, X34, X41, P) ;    % 6th col
    
    equal(X7, X14, X21, X28, P) ;    % 7th col
    equal(X14, X21, X28, X35, P) ;    % 7th col
    equal(X21, X28, X35, X42, P) ;    % 7th col
    
    equal(X4, X10, X16, X22, P) ;    % 1st diag
    equal(X5, X11, X17, X23, P) ;    % 1st diag
    equal(X6, X12, X18, X24, P) ;    % 1st diag
    equal(X7, X13, X19, X25, P) ;    % 1st diag
    
    equal(X11, X17, X23, X29, P) ;    % 1st diag
    equal(X, X, X, X, P) ;    % 1st diag
    equal(X, X, X, X, P) ;    % 1st diag
    equal(X, X, X, X, P) ;    % 1st diag
    
    equal(X, X, X, X, P) ;    % 1st diag
    equal(X, X, X, X, P) ;    % 1st diag
    equal(X, X, X, X, P) ;    % 1st diag
    equal(X, X, X, X, P) ;    % 1st diag
    
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag
    equal(X, X, X, X, P) ;    % 2nd diag


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

% True if NextBoard is Board whit an empty case replaced by Player mark. 
move_aux(P, [0|Bs], [P|Bs]).
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

% ----------------------------------------------
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% 
% --------------------------

EmptyBoard = [0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,
              0,0,0,0,0,0,0].

% winPos(+Player, +Board) 
% % True if Player win in Board. 
winPos(P, [X1, X2, X3, X4, X5, X6, X7, 
          X8, X9, X10, X11, X12, X13, X14,
          X15, X16, X17, X18, X19, X20]) :-
    equal(X1, X2, X3, P) ;    % 1st line
    equal(X4, X5, X6, P) ;    % 2nd line
    equal(X7, X8, X9, P) ;    % 3rd line
    equal(X1, X4, X7, P) ;    % 1st col
    equal(X2, X5, X8, P) ;    % 2nd col
    equal(X3, X6, X9, P) ;    % 3rd col
    equal(X1, X5, X9, P) ;    % 1st diag
    equal(X3, X5, X7, P).     % 2nd diag

% drawPos(+Player, +Board) 
% True if the game is a draw. 
drawPos(_,Board) :-
    \+ member(0, Board).

