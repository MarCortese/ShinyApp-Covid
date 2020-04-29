# Covid-19
Questo progetto , per la sezione Italia, è stato ispirato dalla condivisione dei dati da parte di OnData https://github.com/ondata/covid19italia sul Covid-19 in Italia, mentre per la sezione Mondo è stato ispirato dai dati reperiti al seguente link https://github.com/CSSEGISandData/COVID-19 sul Covid-19 nel mondo.



È stata realizzata una shiny app per analizzare i dati giornalieri pubblicati dalla protezione civile per la situazione di "Pandemia" che stiamo vivendo in questo periodo.

La Shiny app è  suddivisa in 6 sezioni:
    

* Generiche
* Analisi Nazionale
* Analisi Regionale
* Nel Mondo
* Trend Nazioni
* Informazioni



#### N.B. Questa app rappresenta i dati nella loro interezza, non è una finestra temporale giornaliera, perciò prevede il caricamento ogni volta di una quantità di dati maggiore, il tempo di attesa è cresciuto, l'app impiega circa 40 secondi per mostrare tutto e un messaggio ci informerà sul'effettivo caricamento dei dati. 

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/home_dati.jpg)

## Generiche

Questa tab prevede la possibilità di osservare su mappa lo sviluppo dei casi positivi sia su base regionale che su base provinciale, dando la possibilità di osservare il fenomeno nei diversi giorni.

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/home_1.jpg)


Vi è la possibilità di vedere, attraverso pie chart, la ripartizione dei casi positivi seguendo la seguente suddivisione:
Totale Ospedalizzati=Ricoverati con sintomi+Terapia Intensiva
Attualmente Positivi=Totale Ospedalizzati+Isolamento Domiciliare
Totale Casi=Attualmente Positivi+Dimessi Guariti+Decessi

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/home_2.jpg)

Tabella

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/home_3.jpg)



## Analisi Nazionale 

Questa tab consente di osservare il fenomeno su base nazionale nella sua interezza, sono presenti 2 line plot differenti nel quale si analizzano i numeri assoluti delle variabili prese in esame e l'andamento dei tassi di guarigione e di mortalità

Trend nazionale

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/nazione_trend.jpg)

Trend tassi

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/nazione_tassi.jpg)



Il successivo grafico consente di valutare l'andamento giornaliero delle variabili

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/nazione_variabile.jpg)


## Analisi Regionale 

Questa tab consente di osservare il fenomeno su base regionale.
Vi è la possibilità di scegliere una regione e valutare l'andamento giornaliero della regione e delle sue province.
Si parte con un pie chart che mostra la ripartizione dei casi nella regione

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/regione_pie.jpg)

Vi è la possibilità di scegliere una regione e valutare l'andamento giornaliero della regione e delle sue province.

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/regioni_trend.jpg)

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/province_trend.jpg)


## Nel mondo 

Nel mondo consente di osservare l'andamneto del Covid-19 nel mondo, con le informazioni sui casi confermati, i  casi attualmente positivi, i dimessi guaritio e i decessi.

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/mondo_1.jpg)

Tabella

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/mondo_2.jpg)

## Trend Nazioni

Dopo avere osseervato la situazione attuale delle diverse nazioni del mondo vi è la possibilità di valutare il trend dall'inizio dell'epidemia fino all'ultimo giorno di valutazione per ogni nazione.


![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/nazioni_trend.jpg)


## Informazioni

Informazioni, invece, racchiude l’insieme dei canali utili in questo periodo e i link per le dirette del Presidente del Consiglio Giuseppe Conte e della Protezione Civile.

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/info_1.jpg)

![alt text](https://raw.githubusercontent.com/MarCortese/Covid-19/master/Corona/Screen/info_2.jpg)


###### Link

App deployata su un account free perciò limitata nel tempo e se ancora disponibile può essere raggiunta al seguente link:

https://marcocortese.shinyapps.io/Covid-19Italia/



Se il lavoro è di vostro gradimento lasciate una stella sul repository ed eventualmente citate questo progetto.
```
@misc{Covid-19,
  author =       {Marco Cortese},
  title =        {Covid-19 Italia},
  howpublished = {\url{https://github.com/MarCortese/Covid-19}},
  year =         {2020}
}
```
