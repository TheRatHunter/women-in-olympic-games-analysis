#récupération d'un data frame avec une seule fois chaque participant par jeux

dataUnique=unique.data.frame(cbind.data.frame(athlete_events$ID,athlete_events$Year, athlete_events$Sex))



