###Assignment

#by Michael Murawski

############################ Task 1

setwd("C:/Users/Michael/Desktop/Uni R") #please adjust
getwd
read.table("leistungstest.dat")

MyData<-read.table("leistungstest.dat",header=TRUE)

MyData[order(MyData$schule,MyData$klasse),] #Sort by schools and classes


#levels(MyData$geschl)<-c("male","female")
#levels(MyData$geschl)
################# Transform prior gender (0,1) with male,female


MyData$geschl <- factor(MyData$geschl) #Transformation into factor
str(MyData$geschl)
MyData$geschl


install.packages("psych")
library(psych)

summary(MyData)
#Motivation data is missing for 95 students


############################# Task 2
MyTable<-table(MyData$schule)
MyTable
which(MyTable==max(MyTable)) #Opportunity  1
MyTable==max(MyTable)        #Opportunity  2
       
#Note: school 3 has the most participants
round(prop.table(MyTable),3)
# participation in percent for each school 

str(MyData$schule)
#Note: school is measured with a nominal scale, has 5 outcomes, without any ranking or order and no meaningful interval interpretation 


summary(MyData)


############################## Task 3


ftable(geschl~klasse,data=MyData)

#Table showing the sexes and class levels in absolute frequency
prop.table(ftable(geschl~klasse,data=MyData))

#in relative frecuency

counts <- table(MyData$geschl,MyData$klasse)
counts

############################ absolute frequency

barplot(counts, main="Verteilung der Jungen und Mädchen in der Mittelstufe",
        xlab="Verteilung nach Geschlecht und Klassen",ylim = c(0,250), col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

############################ relative frequency 


counts<-table(MyData$geschl,MyData$klasse)
prop.table(counts)
barplot(prop.table(counts), main="Verteilung der Jungen und Mädchen in der Mittelstufe",
        xlab="Verteilung nach Geschlecht und Klassen (7,8,9)",ylim = c(0,0.2), col=c("blue","green"),
        legend = rownames(counts), beside=TRUE)

############################ Task 4

mean(MyData$deutsch)
median(MyData$deutsch)
#Median and arith. mean of all schools in German

describeBy(Deutsch,Schule)

########################## Boxplot diagrams for every school
boxplot(Deutsch ~ Schule,horizontal=TRUE,las=1,ylim=c(0,50),col=rainbow(5),xlab="Punktergebnis des Deutschtests",ylab="Schule")
########################## School 1
Var1<-subset(MyData$deutsch,MyData$schule==1)
boxplot(Var1,horizontal = T,col=rainbow(1),ylim=c(0,50))
abline(v=mean(Var1),col='tomato')
########################## School 2
Var2<-subset(MyData$deutsch,MyData$schule==2)
boxplot(Var2,horizontal = T,col="darkblue",ylim=c(0,50))
abline(v=mean(Var2),col='tomato')
########################## School 3
Var3<-subset(MyData$deutsch,MyData$schule==3)
boxplot(Var3,horizontal = T,col="blue",ylim=c(0,50))
abline(v=mean(Var3),col='tomato')
########################## School 4
Var4<-subset(MyData$deutsch,MyData$schule==4)
boxplot(Var4,horizontal = T,col="green",ylim=c(0,50))
abline(v=mean(Var4),col='tomato')
########################## School 5
Var5<-subset(MyData$deutsch,MyData$schule==5)
boxplot(Var5,horizontal = T,col="blue",ylim=c(0,50))
abline(v=mean(Var5),col='tomato')




#################################### 4.1 & 4.2

subset(MyData,schule==3 & Deutsch==47)
MyData$deutsch[MyData$deutsch==47] <- NA 
summary(MyData$deutsch)
#1.identify the students you are looking for 
#2.remove german grade 
#3. check if really only one value has been removed
#Old arithmetic. mean=19,79 and old median=20

mean(MyData$deutsch,na.rm=TRUE)
median(MyData$deutsch,na.rm=TRUE)


#The median is robust against outliers ....The arithmetic mean is not. Therefore the arithmetic mean changes minimally downwards

##################################### Task 5
MyData<- cbind ( MyData , "Schull"= MyData$deutsch + MyData$mathe + MyData$englisch)
describe(MyData$Schull,na.rm=TRUE)
#or for each single school with:
#max(MyData$Schull,na.rm=TRUE)
#min(MyData$Schull,na.rm=TRUE)

hist(MyData$Schull,main="Schulleistung der Schüler",xlab="Anzahl der erreichten Punkte",ylab="Anzahl der Schüler",xlim=c(10,140),ylim=c(0,400),border="blue", col=terrain.colors(15),las=1)

##################################### Task 6
#Use Spearman correlation, since achieved math points (metric) shall be compared with motivation (ordinal). (Spearman or Kendall method)
cor(MyData$motivation,MyData$mathe,method="spearman",use ="complete.obs")
cor(MyData$motivation,MyData$mathe,method="spearman",use ="pairwise.complete.obs")
#correlation between students' motivation and their maths results is only slightly positive linear, probably only weak relationship
plot(MyData$mathe~MyData$motivation, ylab='Mathenote', xlab='Motivation') 

##################################### Task 7
my_data3 <- read_excel(file.choose())
my_data3 <- read_excel("", sheet = "data")
#both methods usable to integrate 
