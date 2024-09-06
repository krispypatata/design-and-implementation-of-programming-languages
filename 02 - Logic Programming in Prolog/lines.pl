vertical(line(point(X, Y), point(X, Z))).
horizontal(line(point(X, Y), point(Z, Y))).

/*
QUERIES

?- vertical(line(point(1,1), point(1,3))).
true.

?- vertical(line(point(1,1), point(3,2))).
false.

?- horizontal(line(point(1,2), point(3,2))).
true.

?- horizontal(line(point(1,1), point(1,3))).
false.

?- horizontal(line(point(1,1), point(2,Y))).
Y = 1.

?- horizontal(line(point(2,3), P)).
P = point(_, 3).

?- vertical(line(P, point(1,3))).
P = point(1, _).
*/