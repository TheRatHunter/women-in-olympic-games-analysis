#récupération d'un data frame avec une seule fois chaque participant par jeux

dataUnique=unique.data.frame(cbind.data.frame(athlete_events$ID,Year=athlete_events$Year, athlete_events$Sex))

dataUniqueOrdonne=dataUnique[order(dataUnique$Year),]

ddply(dataUniqueOrdonne,c(athlete_events$Sex,dataUniqueOrdonne$Year),summarize,nb_femme=sum(Sex=='F'),nb_homme=sum(Sex=='M'))

ggplot(dataUniqueOrdonne)
#retrouver  le nombre de femmes et d'hommes par an -> ne marche pas
test2=dataUniqueOrdonne %>% group_by(dataUniqueOrdonne$Year) %>% 
summarize(nbHommes=sum(dataUniqueOrdonne$`athlete_events$Sex`=='M'),
          nbFemmes=sum(dataUniqueOrdonne$`athlete_events$Sex`=='F'))

#retrouver le nombre de femmes et d'hommes au JO par an -> marche
test3= dataUniqueOrdonne %>% group_by(dataUniqueOrdonne$Year,Gender=dataUniqueOrdonne$`athlete_events$Sex`) %>% summarize(gender_count=n())
ggplot(test3,aes(x=test3$`dataUniqueOrdonne$Year`,y=test3$gender_count,fill=test3$Gender))+geom_bar(stat="identity",position="dodge")

