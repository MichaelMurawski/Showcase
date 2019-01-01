###Start der Hausarbeit

#von Michael Murawski 6356208

############################Aufgabe 1

setwd("C:/Users/Michael/Desktop/Uni R")
getwd
read.table("leistungstest.dat")

MyData<-read.table("leistungstest.dat",header=TRUE)

MyData[order(MyData$schule,MyData$klasse),] #Sortieren nach Schulen und Klassen


#levels(MyData$geschl)<-c("male","female")
#levels(MyData$geschl)
################# Wenn man Geschlecht doch nicht als Zahlen haben möchte


MyData$geschl <- factor(MyData$geschl) #Umwandlung in Faktor
str(MyData$geschl)
MyData$geschl


install.packages("psych")
library(psych)

summary(MyData)
#Motivation bei 95 Schülern nicht bekannt


#############################Aufgabe 2
MyTable<-table(MyData$schule)
MyTable
which(MyTable==max(MyTable)) #Möglichkeit 1
MyTable==max(MyTable)        #Möglichkeit 2
       
#Schule 3 hat die meisten Teilnehmer
round(prop.table(MyTable),3)
#Teilnehmerprozentsätze der einzelnen Schulen

str(MyData$schule)
#Schule ist nominales Skalenniveau mit 5 Ausprägungen ohne Rangfolge oder sinnvoll-interpretierbare Abstände

summary(MyData)


############################## Aufgabe 3


ftable(geschl~klasse,data=MyData)
#Tabelle mit Darstellung der Geschlechter und Klassenstufen, Absolute Häufigkeiten
prop.table(ftable(geschl~klasse,data=MyData))
#Relative Häufigkeiten

counts <- table(MyData$geschl,MyData$klasse)
counts

############################ Absolute Häufigkeiten

barplot(counts, main="Verteilung der Jungen und Mädchen in der Mittelstufe",
        xlab="Verteilung nach Geschlecht und Klassen",ylim = c(0,250), col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

############################ Relative Häufigkeiten


counts<-table(MyData$geschl,MyData$klasse)
prop.table(counts)
barplot(prop.table(counts), main="Verteilung der Jungen und Mädchen in der Mittelstufe",
        xlab="Verteilung nach Geschlecht und Klassen (7,8,9)",ylim = c(0,0.2), col=c("blue","green"),
        legend = rownames(counts), beside=TRUE)

############################ Aufgabe 4

mean(MyData$deutsch)
median(MyData$deutsch)
#Median und arith. Mittel aller Schulen in Deutsch

describeBy(Deutsch,Schule)

########################## Alle Schulen in einem Boxplot
boxplot(Deutsch ~ Schule,horizontal=TRUE,las=1,ylim=c(0,50),col=rainbow(5),xlab="Punktergebnis des Deutschtests",ylab="Schule")
########################## Schule 1
Var1<-subset(MyData$deutsch,MyData$schule==1)
boxplot(Var1,horizontal = T,col=rainbow(1),ylim=c(0,50))
abline(v=mean(Var1),col='tomato')
########################## Schule 2
Var2<-subset(MyData$deutsch,MyData$schule==2)
boxplot(Var2,horizontal = T,col="darkblue",ylim=c(0,50))
abline(v=mean(Var2),col='tomato')
########################## Schule 3
Var3<-subset(MyData$deutsch,MyData$schule==3)
boxplot(Var3,horizontal = T,col="blue",ylim=c(0,50))
abline(v=mean(Var3),col='tomato')
########################## Schule 4
Var4<-subset(MyData$deutsch,MyData$schule==4)
boxplot(Var4,horizontal = T,col="green",ylim=c(0,50))
abline(v=mean(Var4),col='tomato')
########################## Schule 5
Var5<-subset(MyData$deutsch,MyData$schule==5)
boxplot(Var5,horizontal = T,col="blue",ylim=c(0,50))
abline(v=mean(Var5),col='tomato')




#################################### 4.1 & 4.2

subset(MyData,schule==3 & Deutsch==47)
MyData$deutsch[MyData$deutsch==47] <- NA 
summary(MyData$deutsch)
#1.Schüler identifizieren 2.Deutsch-Note entfernen 3. Überprüfen,ob wirklich nur der eine Wert entfernt wurde
#altes arithm. Mittel=19,79 und alter Median=20

mean(MyData$deutsch,na.rm=TRUE)
median(MyData$deutsch,na.rm=TRUE)


#Der Median ist robust gegenüber Außreißern .... arithm. Mittel nicht daher verändert sich das arithm. Mittel auch minimal nach unten - Konstruktionsgemäß -

#


##################################### Aufgabe 5
MyData<- cbind ( MyData , "Schull"= MyData$deutsch + MyData$mathe + MyData$englisch)
describe(MyData$Schull,na.rm=TRUE)
#ODER einzeln zB.
#max(MyData$Schull,na.rm=TRUE)
#min(MyData$Schull,na.rm=TRUE)
hist(MyData$Schull,main="Schulleistung der Schüler",xlab="Anzahl der erreichten Punkte",ylab="Anzahl der Schüler",xlim=c(10,140),ylim=c(0,400),border="blue", col=terrain.colors(15),las=1)

##################################### Aufgabe 6
#Spearman-Korrelation anwenden,da erreichte Mathepunkte (metrisch) mit Motivation (ordinal) zu vergleichen ist. (Spearman oder Kendall Methode)
cor(MyData$motivation,MyData$mathe,method="spearman",use ="complete.obs")
cor(MyData$motivation,MyData$mathe,method="spearman",use ="pairwise.complete.obs")
#Korrelation zwischen der motivation der Schüler und ihren matheergebnissen ist nur leicht positiv linear, vermutlich nur schwacher zusammenhang
plot(MyData$mathe~MyData$motivation, ylab='Mathenote', xlab='Motivation') 

##################################### Aufgabe 7
read.csv("C:/Users/Michael/Desktop/Uni R", header=T)
?read.csv
#Es hat nur einmal funktioniert und seitdem kommt eine Meldung mit "Permission denied" und ich verstehe nicht wieso? Ich habe seitdem nichts geändert...

