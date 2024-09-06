/*
DESCRIPTION: This program implements the instructions provided in the lab exercise 3 handout.
AUTHOR: Gabinete, Keith Ginoel S.
DATE: September 06, 2024
*/

/*
PART I.

?- atom(a_n_e_m_o_c_u_l_u_s_65).
true.

?- atom(________).
false.

?- atom(TsUrUm1_1sL4nD).
false.

?- atom(aMBER).
true.

?- atom(SkywardAtlas1).
false.

?- atom(q1x1nG_g3n3r4L_s3cr3t4ry).
true.

?- atom('moss!, viola!, rose!').
true.

?- atom(7ARCHONS).
ERROR: Syntax error: Operator expected
ERROR: atom(
ERROR: ** here **
ERROR: 7ARCHONS) . 
?- atom(_wisdom_of_bansei).
false.

?- atom(Kamisato Ayato).
ERROR: Syntax error: Operator expected
ERROR: atom(Kamisato
ERROR: ** here **
ERROR:  Ayato) . 
*/

/*
PART II.
We can use the built-in predicate functor
functor(insert_something_here, Functor, Arity).

?- functor(boss_opponents(liyue(oceanid), liyue('primo geovishap'), inazuma(maguu_kenki), inazuma('thunder manifestation'), sumeru(Jadeplume_terrorshroom)), Functor, Arity).
Functor = boss_opponents,
Arity = 5.

?- functor(Story_Quests('Sea of Clouds, Sea of People', 'A Strange and Friendless Road'), Functor, Arity).
ERROR: Syntax error: Operator expected
ERROR: functor(Story_Quest
ERROR: ** here **
ERROR: s('Sea of Clouds, Sea of People', 'A Strange and Friendless Road'), Functor, Arity) .

?- functor(domains(artifacts(momiji_dyed_court, 'slumbering court'), weapon_materials(court_of_flowing_sands), talent_materials(violet_court)), Functor, Arity).
Functor = domains,
Arity = 3.

?- functor(inazuma_artifact_set('ocean-hued clams', 'husk of opulent dreams', vermillion_hereafter, 'emblem of_severed fate'), Functor, Arity).
Functor = inazuma_artifact_set,
Arity = 4.

?- functor(_adventurers_guild(Fischl, Bennett, Katheryne), Functor, Arity).
ERROR: Syntax error: Operator expected
ERROR: functor(_adventurers_guil
ERROR: ** here **
ERROR: d(Fischl, Bennett, Katheryne), Functor, Arity) . 

?- functor(totalPlayableCryoCharacters(10), Functor, Arity).
Functor = totalPlayableCryoCharacters,
Arity = 1.

?- functor(hangouts(noelle('chivalric training', ('knightly exam prep')), diona('the cat and the cocktail')), Functor, Arity).
Functor = hangouts,
Arity = 2.

?- functor('Geo Characters'(Ningguang, noelle, zh0ngl1, Albedo, gorou, 'Arataki Itto'), Functor, Arity).
Functor = 'Geo Characters',
Arity = 6.

?- functor(gunnhildr_clan(gunnhildr(echkhard(frederica(jean, barbara)))), Functor, Arity).
Functor = gunnhildr_clan,
Arity = 1.

?- functor(5WeaponType(Sword, Bow, Claymore, Catalyst, Polearm), Functor, Arity).
ERROR: Syntax error: Operator expected
ERROR: functor(
ERROR: ** here **
ERROR: 5WeaponType(Sword, Bow, Claymore, Catalyst, Polearm), Functor, Arity) .
*/

/*
PART III.
*/

hasGoodWeapon('Childe').
hasGoodWeapon('Jean').

hasCompletedAnArtifactSet('Heechul', 'Heizou').
hasCompletedAnArtifactSet('Heechul', 'Jean').
hasAscendedATalent('Heechul', 'Kazuha').
hasAscendedATalent('Heechul', 'Childe').

hasCompletedAnArtifactSet('Eunhyuk', 'Kazuha').
hasCompletedAnArtifactSet('Eunhyuk', 'Childe').
hasAscendedATalent('Eunhyuk', 'Jean').
hasAscendedATalent('Eunhyuk', 'Heizou').

hasCompletedAnArtifactSet('Donghae', 'Childe').
hasCompletedAnArtifactSet('Donghae', 'Heizou').
hasAscendedATalent('Donghae', 'Kazuha').
hasAscendedATalent('Donghae', 'Heizou').

hasCompletedAnArtifactSet('Kyuhyun', 'Jean').
hasCompletedAnArtifactSet('Kyuhyun', 'Kazuha').
hasAscendedATalent('Kyuhyun', 'Heizou').
hasAscendedATalent('Kyuhyun', 'Kazuha').

/* ', !' is added on each condition to ensure that if a condition is 
satisfied then there's no need to check the remaining condition(s) 
(To prevent multiple query outputs)*/
willGetStronger(P, C) :- hasCompletedAnArtifactSet(P, C), !.
willGetStronger(P, C) :- (hasGoodWeapon(C), hasAscendedATalent(P, C)), !.

/*
QUERIES

?- willGetStronger('Heechul', 'Heizou').
true.

?- willGetStronger('Heechul', 'Childe').
true.

?- willGetStronger('Heechul', 'Kazuha').
false.

?- willGetStronger('Eunhyuk', 'Kazuha').
true.

?- willGetStronger('Eunhyuk', 'Jean').
true.

?- willGetStronger('Eunhyuk', 'Heizou').
false.

?- willGetStronger('Donghae', 'Childe').
true.

?- willGetStronger('Donghae', 'Jean').
false.

?- willGetStronger('Kyuhyun', 'Childe').
false.

?- willGetStronger('Kyuhyun', 'Jean').
true.
*/