# Casa de marcat
# proiect realizat de Calin Adriana Mihaela

    In acest proiect am explorat tranzitia dintre scanarea unui cod de bare si adaugarea produsului
aferent in cos. M-am axat atat pe citirea codului si translatarea sa in produs, cat si pe
manipularea tabelelor care stau in spatele cosului de cumparaturi.
    Implementarea acestuia este facuta in scala, drept prilej de invatare.


# Cum functioneaza?

    Primim codul de bare pe care il vom decodifica. Initial avem niste bare (cele negre codifica
biti de 1, iar cele albe biti de 0). Mai multe bare de aceeasi culoare, la un loc, determina o
bara mai groasa. Secventa de 59 de bare a codului contine 95 de biti, astfel impartiti:

1. 3 biti 101 pentru a marca startul
2. 42 biti (7 pentru fiecare cifra) pentru cifrele de pe pozitiile 2-7 (prima cifra este
codificata indirect)
3. 5 biti 01010 pentru a marca centrul
4. 42 biti (7 pentru fiecare cifra) pentru cifrele de pe pozitiile 8-13
5. 3 biti 101 pentru a marca sfarsitul

    Prima cifra se numeste cifra de paritate. Spunem despre codificarea unei cifre că e de paritate
pară dacă conține un număr par de biți de 1. Altfel e impară.
    Pentru fiecare cifră din primul grup (pozițiile 2→7) există două codificări posibile, una cu un
număr impar de biți de 1 (codificare L, paritate impară) și una cu un numar par de biți de 1
(codificare G, paritate pară).
    Pentru fiecare cifră din al doilea grup (pozițiile 8→13) există o singură codificare posibilă
(codificare R).

    Ultima cifră a unui cod de bare EAN-13 se numeste cifră de control. Este folosită pentru a
confirma citirea corectă a unui cod. Fiecare cifră din codul de bare (fară cea de control), are o
pondere de 1 sau 3 în funcție de poziție la calculul cifrei de control (cele de pe poziții impare au
pondere 1, iar cele de pe poziții pare au pondere 3, vezi tabelul de mai jos). Aceasta se calculeaza
pe baza unei formule.

    Dupa aceasta parte implementat un tabel care sa reprezinte fie un cos de cumparaturi, fie
inventarul magazinului. Am adaugat cateva functionalitati precum: stergerea unui rand, inserarea unui
rand, sortarea pe baza unei coloane.

    Apoi am dezvoltat un limbaj de interogare, care va servi ca API pentru o gama variata de
transformari de tabelem anterior implementate sub forma de functii. Acest limbaj de query va permite
secvente sau combinatii ale acestor transformari.

    In final, am simulat operatii elementare pe care le-am folosi la o casa de marcat de tip self
checkout.

# Testare

    Pentru a testa functionalitatea codului, puteti scrie in terminal "sbt test" pentru a rula
cateva teste prestabilite.