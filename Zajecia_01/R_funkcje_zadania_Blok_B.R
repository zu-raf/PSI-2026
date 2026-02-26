#BLOK B ----
#Zadanie 6.	Ocena zdolności kredytowej
#Scenariusz: Pracujesz w banku i potrzebujesz funkcji, która automatycznie sprawdzi, czy klient może otrzymać kredyt.
#Stwórz funkcję ocena_kredytowa(dochod, zadluzenie), która zwraca:

#"KREDYT PRZYZNANY" - jeśli zadłużenie < 30% dochodu
#"WYMAGA WERYFIKACJI" - jeśli zadłużenie 30-50% dochodu
#"KREDYT ODRZUCONY" - jeśli zadłużenie > 50% dochodu

#Przetestuj funkcję dla następujących klientów:
#Dochód 10000, zadłużenie 2000
#Dochód 10000, zadłużenie 4000
#Dochód 10000, zadłużenie 6000

ocena_kredytowa = function (dochod, zadluzenie){
  stopa = zadluzenie/dochod
  if (stopa < 0.3){
    czykredyt = "KREDYT PRZYZNANY"
  } else if (stopa > 0.3 && stopa < 0.5){
    czykredyt = "WYMAGA WERYFIKACJI"}
  else if (stopa > 0.5 && stopa < 1){
    czykredyt = "KREDYT ODRZUCONY"
  }
  return (czykredyt)
}

ocena_kredytowa (10000, 2000)
ocena_kredytowa (10000, 4000)
ocena_kredytowa (10000, 6000)