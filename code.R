library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

### 1 - Nb d'hommes et de femmes dans les JO par an

athlete_events = read.csv(file="./athlete_events.csv", header=TRUE, sep=",")

# Compter les athletes seulement une fois
year_sex_unique = ddply(athlete_events, .(Sex, Year), summarize, ID=unique(ID))

# Prepare la donnee a afficher
plot1 = ddply(year_sex_unique, .(Sex, Year), summarize, Nb=sum(Sex==Sex))

ggplot(data=plot1, aes(x=Year, y=Nb, fill=Sex)) +
  geom_bar(stat="identity", position=position_dodge())

### 2 - Medailles feminines par habitant en 2008, comparees avec l'IDSH des pays 

idsh = read.csv(file="./idsh.csv", header=TRUE, sep=",")

# Medailles feminines avec pays en 2006 et 2008
medals_per_year_f = ddply(athlete_events, .(NOC, Year), summarize, Medals=sum(!is.na(Medal) & (Sex=="F")))
medals_per_year_f_2006_2008 = medals_per_year_f[medals_per_year_f$Year==2006 | medals_per_year_f$Year==2008,]

# Medailles par habitants et isdh pour les 40 pays selectionnes et pour les 2 annees selectionnees
temp = merge(idsh, medals_per_year_f_2006_2008, by.x="NOC", by.y="NOC")
plot2 = ddply(temp, .(NOC, idsh2006, Year), summarize, ratio_medals_per_habitants=(Medals/habitants2006))
plot2$Year = factor(plot2$Year)

ggplot(plot2, aes(x=idsh2006, y=ratio_medals_per_habitants, color=Year)) +
  geom_point() +
  scale_color_manual(values=c("#0000ff", "#ff0000"))
       