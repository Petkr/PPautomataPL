# PPautomataPL

## špecifikácia

\
\
Program na prevádzanie konečných automatov na regulárne výrazy a späť v Prologu.

Dve základné operácie:
* DFA -> Regex - prevod deterministického konečného automatu na regulárny výraz
* Regex -> LNFA - prevod regulárneho výrazu na nedeterministický konečný automat s lambda prechodmi


## DFA -> Regex

DFA zadané ako:
* zoznam prechodov
* počiatočný stav
* prijímajúce stavy

Prechod je trojica (zo_stavu, symbol, do_stavu).\
Stavy zadaného DFA sú všetky spomenuté stavy v zozname prechodov.\
Ak nejaký počiatočný a prijímajúci stav nie je spomenutý v zozname prechodov - chyba.\
Abeceda zadaného DFA sú analogicky všetky spomenuté symboly.


## Regex

Regulárny výraz používa na zreťazenie vždy explicitne operátor '.' (bodka).\
Operátor zjednotenia a iterácie štandardne, teda '+' a '*', resp.\
Prázdny reťazec reprezentuje '~' a prázdny jazyk '@'.


## Regex -> LNFA

LNFA sa vypíše ako:
* stavy
* abeceda
* prechody cez symboly z abecedy
* lambda prechody
* počiatočné stavy
* prijímajúce stavy

\
\
Peter Fačko
