# Pas universel, modification selon path et version
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("cowplot", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

### 1 - Nb d'hommes et de femmes dans les JO par an selon path

athlete_events = read.csv(file="./athlete_events.csv", header=TRUE, sep=",")

# Compter les athletes seulement une fois
year_sex_unique = ddply(athlete_events, .(Sex, Year), summarize, ID=unique(ID))

# Prepare la donnee a afficher
plot1 = ddply(year_sex_unique, .(Sex, Year), summarize, Nb=sum(Sex==Sex))

ggplot(data=plot1, aes(x=Year, y=Nb, fill=Sex)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#228b22", "#E69F00"))

### 2 - Medailles feminines par habitant en 2008, comparees avec l'IDSH des pays 

idsh = read.csv(file="./idsh.csv", header=TRUE, sep=",")

# Medailles feminines avec pays en 2006 et 2008
medals_per_year_f = ddply(athlete_events, .(NOC, Year), summarize, Medals=sum(!is.na(Medal) & (Sex=="F")))
medals_per_year_f_2006_2008 = medals_per_year_f[medals_per_year_f$Year==2006 | medals_per_year_f$Year==2008,]

# Medailles par habitants et isdh pour les 40 pays selectionnes et pour les 2 annees selectionnees
temp = merge(idsh, medals_per_year_f_2006_2008, by.x="NOC", by.y="NOC")
plot2 = ddply(temp, .(NOC, idsh2006, Year), summarize, ratio_women_medals_per_habitants=(Medals/habitants2006))
plot2$Year = factor(plot2$Year)

ggplot(plot2, aes(x=idsh2006, y=ratio_women_medals_per_habitants, color=Year, shape=Year, size=Year)) +
  geom_point() +
  scale_shape_manual(values=c(15, 19)) +
  scale_color_manual(values=c("#228b22", "#E69F00")) +
  scale_size_manual(values=c(4,4))

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
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values=c("#228b22", "#E69F00"))

### 4 - Evolution de la taille et du poids des athletes masculins et feminins dans différents sports

# On recupere une seule fois chaque athlete (ils sont toujours dans la meme categorie de sport)
plot4 = ddply(athlete_events, .(Sex, Year,Sport,Weight), summarize, ID=unique(ID) )
# Ne pas prendre en compte les NA
plot4=na.omit(plot4)
# Isoler la gymnastique, apres 1930
plot4gym=plot4[plot4$Sport=="Gymnastics"& plot4$Year>=1930,] 
# Calculer moyenne des poids par sexe et annee
plot4gym=ddply(plot4gym,.(Sex,Year),summarize, weight_mean=mean(Weight))
p4_1 = ggplot(plot4gym, aes(x=Year, y=weight_mean, color=Sex))+geom_line() +
  scale_color_manual(values=c("#228b22", "#E69F00"))

# Idem pour le basketball
plot4basketball=plot4[plot4$Sport=="Basketball"& plot4$Year>=1970,] #& plot4bis$Year>=1936,]
plot4basketball=ddply(plot4basketball,.(Sex,Year) ,summarize, weight_mean=mean(Weight))
p4_2 = ggplot(plot4basketball, aes(x=Year, y=weight_mean, color=Sex))+geom_line() +
  scale_color_manual(values=c("#228b22", "#E69F00"))

# Idem pour le handball
plot4handball=plot4[plot4$Sport=="Handball"& plot4$Year>=1970,] 
plot4handball=ddply(plot4handball,.(Sex,Year),summarize, weight_mean=mean(Weight))
p4_3 = ggplot(plot4handball, aes(x=Year, y=weight_mean, color=Sex))+geom_line() +
  scale_color_manual(values=c("#228b22", "#E69F00"))

# Idem pour le triathlon
plot4archery=plot4[plot4$Sport=="Archery",] 
plot4archery=ddply(plot4archery,.(Sex,Year),summarize, weight_mean=mean(Weight))
p4_4 = ggplot(plot4archery, aes(x=Year, y=weight_mean, color=Sex))+geom_line() +
  scale_color_manual(values=c("#228b22", "#E69F00"))

# Idem pour le bobsleigh
plot4bobsleigh=plot4[plot4$Sport=="Bobsleigh"& plot4$Year>=2000,] 
plot4bobsleigh=ddply(plot4bobsleigh,.(Sex,Year),summarize, weight_mean=mean(Weight))
p4_5 = ggplot(plot4bobsleigh, aes(x=Year, y=weight_mean, color=Sex))+geom_line() +
  scale_color_manual(values=c("#228b22", "#E69F00"))

# Idem pour la natation
plot4swimming=plot4[plot4$Sport=="Swimming"& plot4$Year>=1924,] 
plot4swimming=ddply(plot4swimming,.(Sex,Year),summarize, weight_mean=mean(Weight))
p4_6 = ggplot(plot4swimming, aes(x=Year, y=weight_mean, color=Sex))+geom_line() +
  scale_color_manual(values=c("#228b22", "#E69F00"))


plot_grid(p4_1, p4_2, p4_3, p4_4, p4_5, p4_6, 
          labels=c("Gymnastics", "Basket", "Handball", "Archery", "Bobsleigh", "Swimming"), 
          label_size = 12, ncol = 3, nrow = 2)

### 5 - Etude de l'age de derniere participation des hommes et des femmes

# On calcule l'age de derniere participation de tous les participants (long donc dans une autre variable pour ne pas relancer a chaque fois)
plot5preparation = ddply(athlete_events, .(Sex, Year, Age, ID), summarize, LastParticipation=max(Age) )
# Ne pas prendre en compte les NA
plot5=na.omit(plot5preparation)
# Calculer la moyenne de l'age de derniere participation par annee et par sexe
plot5 = ddply(plot5, .(Sex, Year), summarize, last_participation_age_mean=mean(LastParticipation))

# Avant 1920 il y a trop peu de femmes pour que ce soit représentatif
plot5 = plot5[which(plot5$Year>1950),]

ggplot(plot5,aes(x=Year,y=last_participation_age_mean,color=Sex)) +
  scale_shape_manual(values=c(3, 3)) +
  scale_color_manual(values=c("#228b22", "#E69F00")) +
  geom_point(position = "jitter", aes(shape=Sex, color=Sex)) +
  geom_smooth()



