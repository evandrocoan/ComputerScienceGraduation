% Image processing package in Prolog (a initial tentative)
% Prof. A. G. Silva - UFSC - June 2015


% EXAMPLE OF ARRAY
% -------------------------

matrix([[4,3,2,1,0,0,0,0,0,0],
        [1,2,3,0,1,9,0,0,0,0],
        [0,1,0,0,1,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,3,0,0,0,1,0,0,0],
        [0,7,1,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,5,1,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,100]]).


% ARRAY TO LIST OF COORDINATES
% -------------------------

coordLine([], _, _, []).

coordLine([H|T], Lin, Col, [(Lin,Col,H)|Tm]) :-
    Col1 is Col + 1,
    coordLine(T, Lin, Col1, Tm).

coordAux([], _, _, []) :- !.

coordAux([H|T], Lin, Col, [Hm|Tm]) :-
    Lin1 is Lin + 1,
    coordLine(H, Lin1, Col, Hm),
    coordAux(T, Lin1, Col, Tm).

coord2coord([], C, C).

coord2coord([H|T], C, Coord) :-
    append(C,H,Cx),
    coord2coord(T, Cx, Coord).

coord(Mat, Coord) :-
    coordAux(Mat, -1, 0, CoordMat),
    coord2coord(CoordMat, [], Coord).


% IMAGE OF ZEROS
% -------------------------
% zerosAuxLine((3,4),[],Sb).

zerosAuxLine((X,_), S, S) :-
    X < 0,
    !.
zerosAuxLine((_,X), S, S) :-
    X =< 0,
    !.
zerosAuxLine((H,W), Sa, S) :-
    Wa is W - 1,
    zerosAuxLine((H,Wa), [(H,Wa,0)|Sa], S).

zerosAuxSet((X,_), S, S) :-
    X < 0,
    !.
zerosAuxSet((_,X), S, S) :-
    X < 0,
    !.
zerosAuxSet((H,W), Sa, S) :-
    Ha is H - 1,
    zerosAuxLine((Ha,W), [], Sb),
    append(Sb, Sa, Sc),
    zerosAuxSet((Ha,W), Sc, S).

zeros((H,W), S) :-
    zerosAuxSet((H,W), [], S).


% GET|PUT PIXEL
% -------------------------

getPixel([(A,B,V)|_], (X,Y,V)) :-
    A == X,
    B == Y,
    !.
getPixel([_|St], (X,Y,Z)) :-
    getPixel(St, (X,Y,Z)).

putPixel(_, [], []) :- 
    !.
putPixel((A,B,V), [(A,B,_)|T1], [(A,B,V)|T2]) :-
    putPixel((A,B,V), T1, T2),
    !.
putPixel((A,B,V), [(Ax,Bx,Vx)|T1], [(Ax,Bx,Vx)|T2]) :-
    Ax \= A,
    putPixel((A,B,V), T1, T2).
putPixel((A,B,V), [(Ax,Bx,Vx)|T1], [(Ax,Bx,Vx)|T2]) :-
    Bx \= B,
    putPixel((A,B,V), T1, T2).


% NEIGHBORHOOD
% -------------------------

above(S, (X,Y,_), (Xa,Y,V)) :-
    X > 0,
    Xa is X - 1,
    getPixel(S, (Xa,Y,V)).

below(S, (X,Y,_), (Xa,Y,V)) :-
    Xa is X + 1,
    getPixel(S, (Xa,Y,V)).

left(S, (X,Y,_), (X,Ya,V)) :-
    Y > 0,
    Ya is Y - 1,
    getPixel(S, (X,Ya,V)).

right(S, (X,Y,_), (X,Ya,V)) :-
    Ya is Y + 1,
    getPixel(S, (X,Ya,V)).

neighbor(S, (X,Y,V), E) :-
    above(S, (X,Y,V), E).
neighbor(S, (X,Y,V), E) :-
    below(S, (X,Y,V), E).
neighbor(S, (X,Y,V), E) :-
    left(S, (X,Y,V), E).
neighbor(S, (X,Y,V), E) :-
    right(S, (X,Y,V), E).

n4(S, (X,Y,V), N) :-
    findall(E, neighbor(S, (X,Y,V), E), N).


% TESTS
% -------------------------

test1(M,S,V) :-
    matrix(M), 
    coord(M,S), 
    getPixel(S,(0,0,V)).

test2(M,S,V,X) :-
    matrix(M), 
    coord(M,S), 
    above(S,(1,0,V),X).

test3(M,S,V,X) :-
    matrix(M), 
    coord(M,S), 
    n4(S,(1,4,V),X).

test4(M,S,V,S1,V1) :-
    matrix(M), 
    coord(M,S), 
    getPixel(S,(5,1,V)), 
    putPixel((5,1,777),S,S1), 
    getPixel(S1,(5,1,V1)).


/*

?- test1(M,S,V).
M = [[4, 0, 0, 0, 0, 0, 0, 0|...], [1, 1, 0, 0, 1, 9, 0|...], [0, 1, 0, 0, 1, 0|...], [0, 0, 0, 0, 0|...], [0, 0, 3, 0|...], [0, 7, 1|...], [0, 0|...], [0|...], [...|...]|...],
S = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
V = 4.

?- test2(M,S,V,X).
M = [[4, 0, 0, 0, 0, 0, 0, 0|...], [1, 1, 0, 0, 1, 9, 0|...], [0, 1, 0, 0, 1, 0|...], [0, 0, 0, 0, 0|...], [0, 0, 3, 0|...], [0, 7, 1|...], [0, 0|...], [0|...], [...|...]|...],
S = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
X = (0, 0, 4).

?- test3(M,S,V,X).
M = [[4, 0, 0, 0, 0, 0, 0, 0|...], [1, 1, 0, 0, 1, 9, 0|...], [0, 1, 0, 0, 1, 0|...], [0, 0, 0, 0, 0|...], [0, 0, 3, 0|...], [0, 7, 1|...], [0, 0|...], [0|...], [...|...]|...],
S = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
X = [ (0, 4, 0), (2, 4, 1), (1, 3, 0), (1, 5, 9)].

?- test4(M,S,V,S1,V1).
M = [[4, 0, 0, 0, 0, 0, 0, 0|...], [1, 1, 0, 0, 1, 9, 0|...], [0, 1, 0, 0, 1, 0|...], [0, 0, 0, 0, 0|...], [0, 0, 3, 0|...], [0, 7, 1|...], [0, 0|...], [0|...], [...|...]|...],
S = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
V = 7,
S1 = [ (0, 0, 4), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0), (0, 5, 0), (0, 6, 0), (0, ..., ...), (..., ...)|...],
V1 = 777

*/
