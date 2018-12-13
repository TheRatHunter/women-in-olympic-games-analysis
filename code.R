#récupération d'un data frame avec une seule fois chaque participant
test=cbind(athlete_events$ID,athlete_events$Year,athlete_events$Sex)
dataUnique=test[!duplicated(test[,1]),]

#



