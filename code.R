#Pas universel, modification selon path et version
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

### 1 - Nb d'hommes et de femmes dans les JO par an selon path

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

###3 - 10 sports avec le moins de femmes aux JO été et aux JO hiver

women_only=athlete_events[athlete_events$Sex=='F',]

#nb de femmes par sport
women_by_sports= ddply(women_only,.(Sport,Year),summarize,Nb_femme=sum((Sport==Sport)))

#nb de femmes par sport été et hiver sur les memes annees que l'idsh
women_by_sports_2006_2008=women_by_sports[women_by_sports$Year==2006 | women_by_sports$Year ==2008,]

#Ordonner les valeurs
women_by_sports_ordered=women_by_sports_2006_2008[order(women_by_sports_2006_2008$Nb_femme),]
tentative=head(women_by_sports_ordered,10)

women_by_sports_ordered$Year=factor(women_by_sports_ordered$Year)

# affichage -- on voit bien les sports pour les femmes, je n'arrive pas à mettre les hommes aussi
ggplot(head(women_by_sports_ordered[women_by_sports_ordered$Year==2006 | women_by_sports_ordered$Year==2008,],10),aes(x = Sport,y= Nb_femme, fill=Year))+geom_bar(stat="identity", position="dodge")


######tentative avec les hommes en +
bottom_des_sports=ddply(athlete_events,.(Sport,Year,Sex), summarize,Nb=sum(Sex==Sex))
bottom_charts_2006_2008=bottom_des_sports[bottom_des_sports$Year==2006 | bottom_des_sports$Year==2008,]

bottom_ordered=bottom_charts_2006_2008[order(bottom_charts_2006_2008$Nb),]     

test= bottom_charts_2006_2008[(bottom_charts_2006_2008$Sex=='M'),] # | (bottom_des_sports$Sport==women_by_sports_ordered$Sport),]
test1=bottom_charts_2006_2008[(bottom_charts_2006_2008$Sport== tentative$Sport[1] | bottom_charts_2006_2008$Sport==tentative$Sport[2] | bottom_charts_2006_2008$Sport==tentative$Sport[3] |
bottom_charts_2006_2008$Sport== tentative$Sport[4] | bottom_charts_2006_2008$Sport== tentative$Sport[5] | bottom_charts_2006_2008$Sport== tentative$Sport[6] | bottom_charts_2006_2008$Sport== tentative$Sport[7] | bottom_charts_2006_2008$Sport== tentative$Sport[8] | bottom_charts_2006_2008$Sport== tentative$Sport[9] | bottom_charts_2006_2008$Sport== tentative$Sport[10] ),]

ggplot(test1, aes(x=Sport,y=Nb, fill=Sex))+ geom_bar(stat="identity", position="stack")


# question 4 pour la gymnastique
# on récupère une seule fois chaque athlète (ils sont toujours dans la même catégorie de sport)
plot4 = ddply(athlete_events, .(Sex, Year,Sport,Weight), summarize, ID=unique(ID) )
plot4bis=na.omit(plot4)


plot4ter=plot4bis[plot4bis$Sport=="Gymnastics"& plot4bis$Year>=1936,] 


plot4quat=ddply(plot4ter,.(Sex,Year),summarize, moy=mean(Weight))

ggplot(plot4quat, aes(x=Year, y=moy, color=Sex))+geom_line()

# question 4 pour le basketball
plot4terb=plot4bis[plot4bis$Sport=="Basketball",] #& plot4bis$Year>=1936,] 


plot4quatb=ddply(plot4terb,.(Sex,Year),summarize, moyb=mean(Weight))

ggplot(plot4quatb, aes(x=Year, y=moyb, color=Sex))+geom_line()

#question 4 pour le handball pas vraiment de recherche de ressembler aux hommes
plot4terh=plot4bis[plot4bis$Sport=="Handball" & plot4bis$Year>=1976,] 


plot4quath=ddply(plot4terh,.(Sex,Year),summarize, moyh=mean(Weight))

ggplot(plot4quath, aes(x=Year, y=moyh, color=Sex))+geom_line()

#question 4 pour la  très peu de temps et en plus des catégories donc pas parlant
plot4terbo=plot4bis[plot4bis$Sport=="Boxing" & plot4bis$Year>=2012,] 


plot4quatbo=ddply(plot4terbo,.(Sex,Year),summarize, moybo=mean(Weight))

ggplot(plot4quatbo, aes(x=Year, y=moybo, color=Sex))+geom_line()

#tentative pour le bobsleigh - legere augmentation

Bob=plot4bis[plot4bis$Sport=="Bobsleigh" & plot4bis$Year>=2002,] 


Bob2=ddply(Bob,.(Sex,Year),summarize, moybob=mean(Weight))

ggplot(Bob2, aes(x=Year, y=moybob, color=Sex))+geom_line()

#tentative pour la natation

Nat=plot4bis[plot4bis$Sport=="Swimming" & plot4bis$Year>=1924,] 

Nat2=ddply(Nat,.(Sex,Year),summarize, moynat=mean(Weight))

ggplot(Nat2, aes(x=Year, y=moynat, color=Sex))+geom_line()
ggplot(Nat,aes(x=Year,y=Weight,color=Sex))+geom_point(position = "jitter")

### Choix du sport par différence la plus faible et différence la plus élevé avec les hommes sur la moyenne des poids
women_only=athlete_events[athlete_events$Sex=='F',]
mean_sport=ddply(women_only,.(Sex,Sport),summarize,mean=mean(Weight,na.rm = TRUE))

test=ddply(plot4bis,.(Year,Sex,Sport),summarize, moyenne=mean(Weight))

test1=test[test$Year==2016 | test$Year==2014,]


test3=test1[test1$Sex=='F',]
test3bis=test3[order(test3$Sport),]

test4=test1[test1$Sex=='M',]
test4bis=test4[order(test4$Sport),]

test5=0
test6=0
for (i in 1:46) {
  if(test3bis[[3]][i]== test4bis[[3]][i] ){
    test6[i]=test3bis[[3]][i]
    test5[i]=test4bis[[4]][i]-test3bis[[4]][i]
    
  }
}
##difference moyenne par sport
test7=data.frame(diff_moy_weight=test5,sport=test6)
test8= test7[which.max(test7$diff_moy_weight),] #Handball poids homme- femme les plus éloignés
test9 =test7[which.min(test7$diff_moy_weight),] #Boxing poids homme-femme les plus proches


#### Choix des sports par moyenne la plus lourde, moyenne la plus légère
plus_lourde=mean_sport[which.max(mean_sport$mean),] #Gymnastics

plus_legere=mean_sport[which.min(mean_sport$mean),] #Basketball


