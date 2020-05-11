# PPautomataPL - programátorská príručka

## Reprezentácia symbolov a stavov

Ľubovoľné atomy.

## Reprezentácia DFA

DFA je reprezentované v programe ako formálne, teda ako pätica:
* množina stavov - zoznam
* množina symbolov - zoznam
* prechodová funkcia
* počiatočný stav
* množina prijímajúcich stavov - zoznam

Prechodová funkcia je reprezentovaná ako zoznam zoznamov stavov, resp. ako matica stavov.\
Riadky zodpovedajú stavom a stĺpce symbolom. Prvok na pozícii zodpovedajúcej stavu *s* a symbolu *c* je stav, do ktorého smeruje prechod po prečítaní symbolu *c* v stave *s*.\
Vyhľadávanie v matici zo zadaného stavu a symbolu je lineárne voči súčtu veľkostí množiny stavov a abecedy.

## Reprezentácia LNFA

DFA je reprezentované v programe podobne ako formálne, ako šestica:
* množina stavov - zoznam
* množina symbolov - zoznam
* prechodová funkcia
* množina počiatočných stavov - zoznam
* množina prijímajúcich stavov - zoznam

Prechodová funkcia je reprezentovaná ako zoznam zoznamov zoznamov stavov, resp. ako matica množín stavov.\
Riadky zodpovedajú stavom a stĺpce symbolom. Prvok na pozícii zodpovedajúcej stavu *s* a symbolu *c* je množina stavov, do ktorých smerujú prechody po prečítaní symbolu *c* v stave *s*.\
Vyhľadávanie v matici zo zadaného stavu a symbolu je lineárne voči súčtu veľkostí množiny stavov a abecedy.

## Reprezentácia regulárneho výrazu

Regulárny výraz je reprezentovaný v programe ako zakorenený strom.\
Listové vrcholy reprezentujú symboly. Nelistové vrcholy sú troch druhov: alternujúci, reťaziaci a iterujúci, podľa troch možných operácií v regulárnych výrazoch. Deti vrcholu sú v poradí operandy operácie danej typom vrcholu.

Táto reprezentácia umožňuje príjemnejšie programovanie zjednodušovacích pravidiel a jednoduchšie zreťazenie a zjednotenie regulárnych jazykov.

Príklad: výraz `a.b.c.(a*+b)` reprezentuje v programe term `bb([a,b,c,pp([hh(a), b])])`

## dfa_to_regex

Pozostáva z troch podčastí:

### 1) Prevod zadanej trojice na reprezentáciu DFA

Najprv prejde zoznam a vytvorí množinu [spomenutých stavov](manual.md#stavy), resp. symbolov. Potom druhým prechodom nastaví za každý prechod v zozname stav v matici.

### 2) Vytvorenie regulárneho výrazu

Použije algoritmus podobný Floyd–Warshallovmu na hľadanie najkratšej cesty. Myšlienka je nasledovná:\
DFA budeme chápať ako orientovaný graf.\
Slovo, ktoré prijíma, je cesta z počiatočného stavu do nejakého prijímajúceho.\
Jazyk, ktorý DFA reprezentuje je množina slov, ktoré prijíma, teda potrebujeme nájsť všetky cesty z počiatočného stavu do prijímajúcich stavov.\
Cesty do rôznych prijímajúcich stavov sú rôzne, teda výsledná množina je disjunktným zjednotením množín pre jednotlivé prijímajúce vrcholy, '+' v regulárnom výraze.\
F-W pozorovanie: Zadanie sa dá navyše parametrizovať množinou vrcholov, ktoré majú hľadané cesty dovolené použiť.\
Ak je povolená množina prázdna, môže tvoriť cestu iba jedna hrana.\
Pre výslednú množinu povolíme všetky vrcholy.

R(z,d,V) je regex pre cesty zo stavu *z* do stavu *d* s povolenými vrcholmi *V*.\
Pre R(z)(d)V a nejaký vrchol *v* z *V* platí\
R(z,d,V) = R(z,d,V\\{v}) + R(z,v,V\\{v}).R(v,v,V\\{v})*.R(v,d,V\\{v})

V reči automatov: buď nepoužijeme stav *v*, alebo zoberieme všetky slová, ktoré skončia zo *z* vo *v*, pripojíme všetky slová, ktoré sa z *v* dostanú späť do *v* a za ne pripojíme všetky slová z *v* do *d*.

Pri vytváraní regexu s prázdnou množinou povolených stavov je potrebné určiť všetky prechody medzi dvoma stavmi. Na to sa výborne hodí backtracking v Prologu, vďaka ktorému stačí vytvoriť predikát na určenie výsledného stavu po prečítaní symbolu a použiť findall.

#### Zjednodušovanie regulárneho výrazu

Program aplikuje pri vytvorení nasledovné identity pre zjednodušenie regulárnych výrazov:

~* = ~\
@* = @\
r** = r*\
r_1. ... .r_n = @, ak niektorý z členov (r) je @\
r_1. ... .r_n = s_1. ... .s_k, pričom (s) je (r) bez \~\
(r_1+ ... +r_n)\* = (s_1+ ... +s_k)\*, pričom (s) je (r) bez \~\
r_1+ ... +r_n = s_1+ ... +s_k, pričom (s) je (r) bez @\
(\~+r).r* = r*

### 3) Výpis regulárneho výrazu

Výpis je takmer priamočiarý z reprezentácie, jedine zátvorky sa musia vypisovať vtedy, keď nadradená operácia má väčšiu prioritu ako dcérska.

## regex_to_lnfa

Pozostáva z dvoch podčastí:

### 1) Načítanie regulárneho výrazu

Pre zjednodušenie tu program používa "trik". Definuje tri operátory zodpovedajúce operátorom v regulárnych výrazoch. Prečíta zadaný regex ako reťazec a v ňom nahradí znaky '.', '+' a '*' za príslušné operátory. Potom z výsledného reťazca vytvorí term. Vďaka definovanej priorite, asociativite a arite operátorov, dostane "zadarmo" správnu reprezentáciu bez definície nového významu bodky, plusu a hviezdičky. Potom stačí túto reprezentáciu používajúcu operátory previesť na [tú](#reprezentácia-regulárneho-výrazu), ktorá sa používa vo zvyšku programu.

### 2) Prevod regulárneho výrazu na LNFA

Vstupný regulárny výraz nie je nijak zjednodušený ani inak modifikovaný.

Definovaný rekurzívne.

#### Prázdny jazyk

| @    | ~  |
| ---- | -- |
| -> 0 | {} |
| <- 1 | {} |

#### Prázdne slovo

| ~    | ~   |
| ---- | --- |
| -> 0 | {1} |
| <- 1 | {}  |

#### Slovo s jedným symbolom

pre ľubovoľný symbol *s*

| *s*  | *s* | ~  |
| ---- | --- | -- |
| -> 0 | {1} | {} |
| <- 1 | {}  | {} |

Operácie predpokladajú, že všetky "medzi-automaty" majú práve jeden prijímajúci stav.

#### Zreťazenie

Pre dva:\
Výsledný automat je disjunktným zjednotením (premenuje stavy, ak je to potrebné) automatov pre jednotlivé regexy s navyše jedným lambda prechodom medzi prijímajúcim stavom prvého a počiatočným stavom druhého automatu.\
Prijímajúci stav výsledného automatu je prijímajúci stav druhého a počiatočný je počiatočný prvého.

Zreťazenie zoznamu regexov je postupným zreťazením i-teho a i+1-eho regexu od i=0.

#### Zjednotenie:

Výsledný automat je disjunktným zjednotením (premenuje stavy, ak je to potrebné) automatov pre jednotlivé regexy s dvoma pridanými stavmi, *b* a *e*.\
Výsledný automat má navyše lambda prechody z *b* do všetkých počiatočných stavov a zo všetkých prijímajúcich stavov do *e*.\
*b* je počiatočný stav výsledného automatu a *e* je jeho prijímajúci.

#### Iterácia

Pre iteráciu regexu *R*.\
Výsledný automat má pridané dva stavy, *b* a *e*.\
Pre *bb* počiatočný stav *R* a *ee* prijímajúci stav *R* má navyše lambda prechody:\
*b*->*bb*, *ee*->*e*, *b*->*e*, *ee*->*bb*

\
\
© 2020 Peter Fačko
