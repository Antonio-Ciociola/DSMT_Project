# Todo

- [ ] reject websocket connection of users not connected to auction
- [ ] update remaining time or duration when a bid is done
- [ ] remove users connected to auction when auction ends
- [ ] post to java when auction ends
  - /api/finish-auction
- [ ] all checks when adding to db, for example 2 times post on user connecting to auction
- [ ] chiave webtoken
- [ ] timer, in questo momento un timer su ogni nodo, probabilmente non consistente
- [ ] post to java, each node does it
- [ ] consistenza delle bid, forse guardano solo il db locale e non totale

- [ ] nodo master che sa gli slave cosa fanno e quelli liberi
- [ ] risponde a create auction con url del ws dello slave
- [ ] quando join auction viene aggiunto a mysql dell'utente
