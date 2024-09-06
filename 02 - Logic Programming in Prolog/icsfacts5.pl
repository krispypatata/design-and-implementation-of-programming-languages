loves(kei, bulacs).
loves(ippo, bulacs).
jealous(X, Y) :- loves(X, Z), loves(Y, Z).
/*X will get jealous on Y if X loves Z and Y loves Z*/

/*
OUTPUT TERMINAL:

?- loves(kei, bulacs).
true.

?- jealous(kei, ippo).
true.

?- jealous(ippo, kei).
true.

?- jealous(kei, bulacs).
false.

?- jealous(ippo, W).
W = kei ;
W = ippo.

% ibabalik lang ay yung una pag ginamitan ng '!'
?- jealous(ippo, W), !.
W = kei.
*/


/*
PROOF SEARCH

?- jealous(A, B).
A = B, B = kei ;
A = kei,
B = ippo ;
A = ippo,
B = kei ;
A = B, B = ippo.
*/