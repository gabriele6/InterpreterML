i test possono essere lanciati spostandosi nella cartella del progetto e dando il comando ocaml < test.ml

"interpreter.ml" contiene l'interprete "pulito"
"test.ml" contiene l'interprete + i test, quindi va lanciato per testare le funzioni tramite i vari test già scritti

le funzioni implementate fanno quanto richiesto dalle specifiche, tranne la For.
la For prende un identificatore, una tupla ed una espressione, assegna all'identificatore il valore di ciascun elemento della tupla (uno alla volta), e valuta l'espressione. nel caso in cui la funzione non fosse applicabile ad uno o più parametri della tupla, la funzione restituisce un errore.
l'output della funzione For è dato da una tupla contenente tutti i risultati ottenuti dall'applicazione della funzione agli elementi della tupla ricevuta in ingresso.

FUNZIONI
le funzioni possono essere dichiarate tramite la funBinder, che prende in ingresso una exp, un ambiente delle espressioni, ed un ambiente delle funzioni. la funBinder controlla che il tipo passato sia Function e fa un bind nell'ambiente delle funzioni, inserendo il nome della funzione e la tripla data da parametro formale, body ed ambiente delle espressioni. il bind verrà effettuato restituendo il vecchio ambiente delle funzioni con un cons (nome, parametri), ossia (nome, (parametro_formale, body, env))::fenv

Punto 4)
se l'interprete dovesse avere scoping dinamico, basterebbe non salvare l'ambiente al momento della dichiarazione della funzione (funBinder).
in questo modo la funzione non verrebbe eseguita nell'env in cui è stata dichiarata, ma in quello in cui è invocata.