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

### 3 - CLassement des sports selon le ratio hommes femmes

# Nb de femmes et hommes par sport et ratio
gender_by_sports= ddply(athlete_events, .(Sport, Sex), summarize, Nb=sum(Sex==Sex))

# Pour cibler une année :
# gender_by_sports= ddply(athlete_events[which(athlete_events$Year==2008),], .(Sport, Sex), summarize, Nb=sum(Sex==Sex))

# Calcul du nombre total de participants par sport pour calculer le ratio
totalPeopleInThisSport = function(mySport){return(sum(gender_by_sports[which(gender_by_sports$Sport==mySport),3]))}
gender_by_sports$Total = as.integer(lapply(gender_by_sports[,1], totalPeopleInThisSport))
gender_by_sports$Ratio = (gender_by_sports$Nb/gender_by_sports$Total)*100

# Rajout du ratio feminin sur toutes les lignes pour ordonner
femaleRatio = function(index){
  if (gender_by_sports[index,2]=="F"){
    return(gender_by_sports[index, 5])
  } else {
    return(100-gender_by_sports[index, 5])
  }
}
gender_by_sports$FemaleRatio = as.double(lapply(1:nrow(gender_by_sports), femaleRatio))

# Ordonner les valeurs selon ratio feminin
gender_by_sports_ordered=gender_by_sports[order(gender_by_sports$FemaleRatio),]

# Definir ordre des labels comme etant celui de FemaleRatio
gender_by_sports_ordered$Sport <- factor(gender_by_sports_ordered$Sport, levels = unique(gender_by_sports_ordered$Sport[order(gender_by_sports_ordered$FemaleRatio)]))

ggplot(gender_by_sports_ordered, aes(x=Sport,y=Ratio, fill=Sex)) + 
  geom_bar(stat="identity", position="stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# question 4 pour la gymnastique
# on r?cup?re une seule fois chaque athl?te (ils sont toujours dans la m?me cat?gorie de sport)
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

#question 4 pour la  tr?s peu de temps et en plus des cat?gories donc pas parlant
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

### Choix du sport par diff?rence la plus faible et diff?rence la plus ?lev? avec les hommes sur la moyenne des poids
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
test8= test7[which.max(test7$diff_moy_weight),] #Handball poids homme- femme les plus ?loign?s
test9 =test7[which.min(test7$diff_moy_weight),] #Boxing poids homme-femme les plus proches


#### Choix des sports par moyenne la plus lourde, moyenne la plus l?g?re
plus_lourde=mean_sport[which.max(mean_sport$mean),] #Gymnastics

plus_legere=mean_sport[which.min(mean_sport$mean),] #Basketball


