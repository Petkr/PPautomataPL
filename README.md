# PPautomataPL - užívateľská príručka

Program na prevádzanie konečných automatov na regulárne výrazy a späť v Prologu.

Podporuje dve operácie:
* [dfa_to_regex](#dfa_to_regex) - prevod deterministického konečného automatu na regulárny výraz
* [regex_to_lnfa](#regex_to_lnfa) - prevod regulárneho výrazu na nedeterministický konečný automat s lambda prechodmi

Všetky identifikátory objektov v automatoch a regulárnych výrazoch môžu byť ľubovoľné reťazce.

Všetky vstupy musia byť ukončené bodkou.


## Regulárny výraz

Regulárny výraz používa na zreťazenie vždy explicitne operátor '.' (bodka).\
Operátor zjednotenia a iterácie štandardne, teda '+' a '*', resp.\
Prázdny reťazec reprezentuje '~' a prázdny jazyk '@'.


## dfa_to_regex

Vytvorí z deterministického konečného automatu [regulárny výraz](#regulárny-výraz) reprezentujúci rovnaký jazyk.

Výsledný regulárny výraz nie je zjednodušený na minimálnu dĺžku, ale program sa snaží aplikovať niektoré triviálne zjednodnušenia.

Vstupný automat je zadaný ako trojica:
* [zoznam prechodov](#zoznam-prechodov)
* [počiatočný stav](#počiatočný-stav)
* [zoznam prijímajúcich stavov](#prijímajúce-stavy)

### Stavy

Stav zadaného automatu je stav, ktorý sa nachádza ako vychádzajúci alebo koncový stav nejakého prechodu zo zoznamu prechodov, alebo failState.

### Abeceda

Symbolom automatu je symbol, ktorý sa nachádza v nejakom prechode zo zoznamu prechodov.

### Zoznam prechodov

Zoznam trojíc v tvare [e(zo_stavu, symbol, do_stavu), ...].

Ak sa v zozname vyskytuje viacero prechodov s rovnakým vychádzajúcim stavom a symbolom, ignorujú sa všetky okrem posledného.

Nemusí spolu tvoriť totálnu funkciu. Chýbajúce prechody sú doplnené ako prechody do stavu failState.

### Počiatočný stav

Musí byť v množine [stavov automatu](#stavy), inak vypíše false.

### Prijímajúce stavy

Zadané ako zoznam v tvare [...].\
Musia byť v množine [stavov automatu](#stavy), inak vypíše false.

### Príklad

```
?- dfa_to_regex.
enter a list of edges in format e(FromState, Symbol, ToState):
|: [e(0,a,0), e(0,b,1)].
enter the name of the start state:
|: 0.
enter a list of accept states:
|: [1].
regex: a*.b
true .
```

Zadaný je automat s nasledovnou tabuľkou prechodov:

|           | a         | b         |
| --------- | --------- | --------- |
| -> 0      | 0         | 1         |
| <- 1      | failState | failState |
| failState | failState | failState |


## regex_to_lnfa

Vytvorí zo zadaného [regulárneho výrazu](#regulárny-výraz) nedeterministický konečný automat s lambda prechodmi prijímajúci rovnaký jazyk.

Regulárny výraz musí byť zadaný v úvodzovkách, napr. "a*.b".

Regulárny výraz nie je nijak zjednodušovaný pred vytvorením automatu.

Výsledný automat je vypísaný ako šestica:
* stavy
* abeceda
* prechody cez symboly abecedy, v tvare *zo_stavu* -*symbol*-> *do_stavu*
* lambda prechody, v tvare *zo_stavu* -~-> *do_stavu*
* počiatočné stavy
* prijímajúce stavy

### Príklad

```
?- regex_to_lnfa.
enter a regex in quotes:
|: "a*.b".
States are: 0, 1, 2, 3, 4, 5
Alphabet is: a, b
Regular edges are: 2 -a-> 3, 4 -b-> 5
Lambda edges are: 0 -~-> 1, 0 -~-> 2, 1 -~-> 4, 3 -~-> 1, 3 -~-> 2
Start states are: 0
Accept states are: 5
true .
```

Výsledkom je automat s nasledovnou tabuľkou prechodov:

|      | a   | b   | ~     |
| ---- | --- | --- | ----- |
| -> 0 | {}  | {}  | {1,2} |
| 1    | {}  | {}  | {4}   |
| 2    | {3} | {}  | {}    |
| 3    | {}  | {}  | {1,2} |
| 4    | {}  | {5} | {}    |
| <- 5 | {}  | {}  | {}    |

\
\
© 2020 Peter Fačko
