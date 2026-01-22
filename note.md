# Note

## Risposta mail prof

La proposta è ragionevole, ma avrei qualche commento. Suggerirei, nel documento, di evidenziare in modo esplicito le problematiche di sincronizzazione e comunicazione, come indicato sul sito del corso.
Dato che siete in quattro, mi aspetto qualcosa di un po' più corposo. Propongo di sostituire SpringBoot con una semplice applicazione Java (bastano servlet e JSP) con deployment su Tomcat o su Glassfish. Occorre chiarire quale tipo di informazioni si devono scambiare il modulo web e la parte Erlang, e come.
Un aspetto critico è la sincronizzazione dei timer su ciascun client web che partecipa all'asta: proponete una soluzione che garantisca una adeguata risoluzione delle corse su offerte concorrenti - a ogni offerta dovrà essere associato un timestamp "globale" che permetta di risolvere le corse, anche in caso di latenze diverse nelle comunicazioni tra client web e server.
Potete iniziare a lavorare sul progetto: attendo comunque una seconda versione delle specifiche, che tenga conto dei vari commenti.
