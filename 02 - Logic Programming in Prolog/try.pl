% Good weapons
good_weapon(childe).
good_weapon(jean).

% Artifact sets completion
completes_artifact_set(heechul, heizou).
completes_artifact_set(heechul, jean).
completes_artifact_set(eunhyuk, kazuha).
completes_artifact_set(eunhyuk, childe).
completes_artifact_set(donghae, childe).
completes_artifact_set(donghae, heizou).
completes_artifact_set(kyuhyun, jean).
completes_artifact_set(kyuhyun, kazuha).

% Ascending talents
ascends_talent(heechul, kazuha).
ascends_talent(heechul, childe).
ascends_talent(eunhyuk, jean).
ascends_talent(eunhyuk, heizou).
ascends_talent(donghae, kazuha).
ascends_talent(donghae, heizou).
ascends_talent(kyuhyun, heizou).
ascends_talent(kyuhyun, kazuha).


% Rule for becoming stronger
stronger(Player, Character) :-
    completes_artifact_set(Player, Character);
    (good_weapon(Character), ascends_talent(Player, Character)).

/*
?- stronger(heechul, heizou).
?- stronger(heechul, childe).
?- stronger(heechul, kazuha).
?- stronger(eunhyuk, kazuha).
?- stronger(eunhyuk, jean).
?- stronger(eunhyuk, heizou).
?- stronger(donghae, childe).
?- stronger(donghae, jean).
?- stronger(kyuhyun, childe).
?- stronger(kyuhyun, jean).


*/