#r?cup?ration d'un data frame avec une seule fois chaque participant par jeux
athlete_events = read.csv(file="./athlete_events.csv", header=TRUE, sep=",")

dataUnique=unique.data.frame(cbind.data.frame(ID=athlete_events$ID,Year=athlete_events$Year, Sex=athlete_events$Sex))

#dataUniqueOrdonne=dataUnique[order(dataUnique$Year),]

plot1 = ddply(dataUnique, .(Sex, Year), summarize, Nb=sum(Sex==Sex))

ggplot(data=plot1, aes(x=Year, y=Nb, fill=Sex)) +
  geom_bar(stat="identity", position=position_dodge())