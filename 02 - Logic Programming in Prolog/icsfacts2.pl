happy(regi).
listensToMusic(kat).
listensToMusic(regi) :- happy(regi).
lipSyncs(kat) :- listensToMusic(kat).
lipSyncs(regi) :- listensToMusic(regi).