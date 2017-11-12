% -----------------------------------------------------------------------
% Trabalho pr�tico: factos de cidades com localiza��o baseada em
% latitude e longitude e predicado auxiliar para calcular a dist�ncia
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------

%city(name,latitude,longitude)
city(brussels,50.8462807,4.3547273).
city(tirana,41.33165,19.8318).
city(andorra,42.5075025,1.5218033).
city(vienna,48.2092062,16.3727778).
city(minsk,53.905117,27.5611845).
%city(sarajevo,43.85643,18.41342).
%city(sofia,42.6976246,23.3222924).
%city(zagreb,45.8150053,15.9785014).
%city(nicosia,35.167604,33.373621).
city(prague,50.0878114,14.4204598).
%city(copenhagen,55.6762944,12.5681157).
city(london,51.5001524,-0.1262362).
city(tallinn,59.4388619,24.7544715).
%city(helsinki,60.1698791,24.9384078).
%city(paris,48.8566667,2.3509871).
%city(marseille,43.296386,5.369954).
%city(tbilisi,41.709981,44.792998).
%city(berlin,52.5234051,13.4113999).
%city(athens,37.97918,23.716647).
city(budapest,47.4984056,19.0407578).
city(reykjavik,64.135338,-21.89521).
city(dublin,53.344104,-6.2674937).
city(rome,41.8954656,12.4823243).
city(pristina,42.672421,21.164539).
%city(riga,56.9465346,24.1048525).
%city(vaduz,47.1410409,9.5214458).
%city(vilnius,54.6893865,25.2800243).
%city(luxembourg,49.815273,6.129583).
city(skopje,42.003812,21.452246).
city(valletta,35.904171,14.518907).
city(chisinau,47.026859,28.841551).
%city(monaco,43.750298,7.412841).
%city(podgorica,42.442575,19.268646).
%city(amsterdam,52.3738007,4.8909347).
%city(belfast,54.5972686,-5.9301088).
city(oslo,59.9138204,10.7387413).
city(warsaw,52.2296756,21.0122287).
%city(lisbon,38.7071631,-9.135517).
%city(bucharest,44.430481,26.12298).
city(moscow,55.755786,37.617633).
city(san_marino,43.94236,12.457777).
city(edinburgh,55.9501755,-3.1875359).
city(belgrade,44.802416,20.465601).
%city(bratislava,48.1483765,17.1073105).
%city(ljubljana,46.0514263,14.5059655).
%city(madrid,40.4166909,-3.7003454).
%city(stockholm,59.3327881,18.0644881).
%city(bern,46.9479986,7.4481481).
city(kiev,50.440951,30.5271814).
city(cardiff,51.4813069,-3.1804979).

% UTILS ------------------------------------------------------------------------
% 
%  dist_cities(brussels,prague,D).
%  D = 716837.
dist_cities(C1,C2,Dist):-
    city(C1,Lat1,Lon1),
    city(C2,Lat2,Lon2),
    distance(Lat1,Lon1,Lat2,Lon2,Dist).

degrees2radians(Deg,Rad):-
	Rad is Deg*0.0174532925.

% distance(latitude_first_point,longitude_first_point,latitude_second_point,longitude_second_point,distance
% in meters)
distance(Lat1, Lon1, Lat2, Lon2, Dis2):-
	degrees2radians(Lat1,Psi1),
	degrees2radians(Lat2,Psi2),
	DifLat is Lat2-Lat1,
	DifLon is Lon2-Lon1,
	degrees2radians(DifLat,DeltaPsi),
	degrees2radians(DifLon,DeltaLambda),
	A is sin(DeltaPsi/2)*sin(DeltaPsi/2)+ cos(Psi1)*cos(Psi2)*sin(DeltaLambda/2)*sin(DeltaLambda/2),
	C is 2*atan2(sqrt(A),sqrt(1-A)),
	Dis1 is 6371000*C,
	Dis2 is round(Dis1).

% distance(50.8462807,4.3547273,50.0878114,14.4204598,D).
% Online: http://www.movable-type.co.uk/scripts/latlong.html
%

 
% Given three colinear points p, q, r, the function checks if
% point q lies on line segment 'pr'
%onSegment(P, Q, R)
onSegment((PX,PY), (QX,QY), (RX,RY)):-
    QX =< max(PX,RX),
    QX >= min(PX,RX),
    QY =< max(PY,RY),
    QY >= min(PY,RY).

 
% To find orientation of ordered triplet (p, q, r).
% The function returns following values
% 0 --> p, q and r are colinear
% 1 --> Clockwise
% 2 --> Counterclockwise

orientation((PX,PY), (QX,QY), (RX,RY), Orientation):-
	Val is (QY - PY) * (RX - QX) - (QX - PX) * (RY - QY),
	(
		Val == 0, !, Orientation is 0;
		Val >0, !, Orientation is 1;
		Orientation is 2
	).
 
orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4):-
    orientation(P1, Q1, P2,O1),
    orientation(P1, Q1, Q2,O2),
    orientation(P2, Q2, P1,O3),
    orientation(P2, Q2, Q1,O4).
 

% The main function that returns true if line segment 'p1q1'
% and 'p2q2' intersect.
doIntersect(P1,Q1,P2,Q2):-
    % Find the four orientations needed for general and
    % special cases
	orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4),
	
	(	
    % General case
    O1 \== O2 , O3 \== O4,!;

    % Special Cases
    % p1, q1 and p2 are colinear and p2 lies on segment p1q1
    O1 == 0, onSegment(P1, P2, Q1),!;
 
    % p1, q1 and p2 are colinear and q2 lies on segment p1q1
    O2 == 0, onSegment(P1, Q2, Q1),!;
 
    % p2, q2 and p1 are colinear and p1 lies on segment p2q2
    O3 == 0, onSegment(P2, P1, Q2),!;
 
     % p2, q2 and q1 are colinear and q1 lies on segment p2q2
    O4 == 0, onSegment(P2, Q1, Q2),!
    ).
    
linearCoord(City,X,Y):-
	city(City,Lat,Lon),
	geo2linear(Lat,Lon,X,Y).

geo2linear(Lat,Lon,X,Y):-
	degrees2radians(Lat,LatR),
	degrees2radians(Lon,LonR),
	X is round(6371*cos(LatR)*cos(LonR)),
	Y is round(6371*cos(LatR)*sin(LonR)).

totalCities(X) :-
	findall(C, city(C,_,_), L),
	length(L,Y),
	X is Y + 1.

totalDistance([C1, C2], Dist) :- dist_cities(C1, C2, Dist), !.
totalDistance([C1|[C2|T]], Dist) :- 
    totalDistance([C2|T], NewDist),
    dist_cities(C1, C2, Dist2),
    Dist is Dist2 + NewDist.
	
doCross(City1, City2, City3, City4) :-
    linearCoord(City1, C1x, C1y),
    linearCoord(City2, C2x, C2y),
    linearCoord(City3, C3x, C3y),
    linearCoord(City4, C4x, C4y),
    doIntersect((C1x, C1y), (C2x, C2y), (C3x, C3y), (C4x, C4y)).

% ------------------------------------------------------------------------

%Limit 10 Cities
tsp1(City, Visited, Distance) :-
	findall(Dist-Visit, tsp(City,Visit,Dist), List),
	sort(1, @<, List, Sorted),
	visitList(Visited, Distance, Sorted).

visitList(Visit, Dist, [D-V|_] ) :-
	Visit = V, Dist = D.

tsp(City, Visited, Distance) :-
	get_road(City, City, Visited, Distance), 
	length(Visited, X), totalCities(Y), X == Y.

get_road(Start, End, Visited, Result) :-
    get_road(Start, End, [Start], 0, Visited, Result).

get_road(Start, End, Waypoints, DistanceAcc, Visited, TotalDistance) :-
    dist_cities(Start, End, Distance),
    reverse([End|Waypoints], Visited),
    TotalDistance is DistanceAcc + Distance.

get_road(Start, End, Waypoints, DistanceAcc, Visited, TotalDistance) :-
    dist_cities(Start, Waypoint, Distance),
    \+ member(Waypoint, Waypoints),
    NewDistanceAcc is DistanceAcc + Distance,
    get_road(Waypoint, End, [Waypoint|Waypoints], NewDistanceAcc, Visited, TotalDistance).
	
% ------------------------------------------------------------------------
	
tsp2(Orig, Path) :-
    findall(X, city(X,_,_), AllCities),
    delete(AllCities, Orig, ToCheck),
    findPath(ToCheck, [Orig], FinalPath),
    append(FinalPath, [Orig], Path).

findPath([], Path, Path).
findPath(ToCheck, FinalPath, F) :- 
    bestNextNode(FinalPath, Next),
    append(FinalPath, [Next], NewFinalPath),
    delete(ToCheck, Next, NewToCheck),
    findPath(NewToCheck, NewFinalPath, F).

bestNextNode(LA, Next) :-
    last(LA, Act),
    %calcular todos os nodos adjacentes nao visitados e
	% guardar um tuplo com estimativa e novo caminho
	findall((EstX, [X|LA]),
	(city(X, _, _), \+ member(X, LA), dist_cities(X, Act, EstX)), Novos),
	%ordenar pela estimativa
	sort(Novos, NovosOrd),
	%extrair o melhor que está à cabeça
	NovosOrd = [(_,[Next|_])|_].

% ------------------------------------------------------------------------

tsp3(Orig, Path) :-
    tsp2(Orig, NPath),
    tsp3_for_1(NPath, [], Path).

tsp3_for_1([A|[B|[C|[]]]], L, LFinal) :-
    append(L, [A], L1),
    append(L1, [B], L2),
    append(L2, [C], LFinal).
    
tsp3_for_1([H1|[H2|T]], LTemp, LFinal) :-
    tsp3_for_2(LTemp, [H1|[H2|T]], H1, H2, T, NList),
    NList = [X|NNList],
    append(LTemp, [X], NLTemp),
    tsp3_for_1(NNList, NLTemp, LFinal).

tsp3_for_2(_, L, _, _, [_|[_|[]]], L).
tsp3_for_2(LO, LOrig, C1, C2, [H1|[H2|T]], LFinal) :-
    (doCross(C1, C2, H1, H2) ->
    	fixCross(LOrig, C2, H1, [], LFinal)
    	;
    	tsp3_for_2(LO, LOrig, C1, C2, [H2|T], LFinal)
    ).

fixCross([], _, _, LFinal, LFinal).
fixCross([H|T], C1, C2, NLista, LFinal) :-
    (H = C1, append(NLista, [C2], NewLAlterada);
    H = C2, append(NLista, [C1], NewLAlterada);
    H \= C1, H \= C2, append(NLista, [H], NewLAlterada)),
    fixCross(T, C1, C2, NewLAlterada, LFinal).

% ------------------------------------------------------------------------

temperature(1).
constant(0.9).
e(2.71828).


tsp4(Orig, Iter) :- write('Start'), nl, tsp3(Orig, Cam), totalDistance(Cam, Custo), temperature(T), simulatedAnnealing(Cam, Custo, Iter, T, CamX, CustoX),
	nl, write('Resultado'), nl, write('Caminho = '), write(CamX), nl,write('Custo = '), write(CustoX), !.

simulatedAnnealing(Original, Cold, 0, _, CamFinal, CustoFinal) :- CamFinal = Original, CustoFinal = Cold, !.
simulatedAnnealing(Original, Cold, Iter, T, CamFinal, CustoFinal) :- Iter2 is Iter-1, newAdjacent(Original, NewL), totalDistance(NewL, Cnew),
	((Cnew < Cold), newTemperature(T, T1), simulatedAnnealing(NewL, Cnew, Iter2, T1, CamFinal, CustoFinal);
	checkMove(Original, Cold, NewL, Cnew, T, LRes, CRes), newTemperature(T, T1),
	simulatedAnnealing(LRes, CRes, Iter2, T1, CamFinal, CustoFinal)), !.

newAdjacent(S1, Sn) :- length(S1, T1), interval(T1, Start, End), random_between(Start, End, Pos1), random_between(Start, End, Pos2),
	nth1(Pos1, S1, E1), nth1(Pos2, S1, E2),
	removeElementPos(Pos1, S1, S2), insertElementPos(Pos1, E2, S2, S3),
	removeElementPos(Pos2, S3, S4), insertElementPos(Pos2, E1, S4, Sn).

removeElementPos(Pos, List, NewList) :- nth1(Pos, List, _, NewList), !.

insertElementPos(Pos, Elem, List, NewList) :- nth1(Pos, NewList, Elem, List), !.

interval(T1, Start, End):- Start is 2, End is T1 - 1.

checkMove(Original, Cold, NewLista, Cnew, T, NewL, NewC) :- random(R), probAccept(Cold, Cnew, T, AcceptPorb),
	(((AcceptPorb > 1; AcceptPorb > R), (NewC = Cnew, NewL = NewLista)); NewL = Original, NewC = Cold).

probAccept(Cold, Cnew, T, AcceptPorb):- e(E), exp(E, ((Cold-Cnew) / T), AcceptPorb).

exp(X, Y, R) :- R is round(X**Y).

newTemperature(T, NewTemperature) :- constant(C), NewTemperature is C * T.
