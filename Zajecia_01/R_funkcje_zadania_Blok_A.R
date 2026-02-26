#BLOK A ----
#Zadanie 1.	 Wartość przyszła inwestycji (procent składany)
#Scenariusz: Pracujesz w banku i musisz obliczyć, ile zarobi klient, inwestując pieniądze na lokatę.
#Stwórz funkcję wartosc_przyszla(kapital, stopa, lata), która oblicza wartość przyszłą inwestycji.
#Wzór: FV = PV × (1 + r)^n
#PV = kapitał początkowy
#r = stopa procentowa (zapisana jako ułamek, np. 0.05 dla 5%)
#n = liczba lat

wartosc_przyszla = function (kapital, stopa, lata){
  wartosc = kapital * (1 + stopa)^lata
  return (wartosc)
}

wartosc_przyszla(5000, 0.05, 1)




