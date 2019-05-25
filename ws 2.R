

rm(list=ls())

getwd()
setwd("C:/Users/Michael/Downloads") 
mydata <- read.table("Umfrage_mit_missings.dat", sep=",", header=T)


### 

### "Please analyse the structure of the dataset"

str(mydata)
dim(mydata) ; nrow(mydata) ; ncol(mydata)
names(mydata)
head(mydata,3)
tail(mydata,3)
summary(is.na(mydata))

### Dataset Information: Interviewee,Division of University, Semester, Sex/Gender, hours spent on studying, hours spent on job, hours used for nightlife activities, Income last sem., income in current sem., satisfaction with studying, satisfaction with nightlife activities 

#"How can we gain an overview of the dataset/dataset structure?" 
y <- rnorm(100,15,4) #example dataset
hist(y,freq=F,density=20,las=1,main="",xlab="",breaks=8)
curve(dnorm(x, mean=mean(y), sd=sd(y)), 
      col="blue", lwd=5, add=TRUE)
#use different plotting methods and comparing them with target distributions

# Normal distributions in Q-Q Plot 
qqnorm(y)

# Random Comparison to skew distribution in Q-Q Plot (F-Distribution) 
z <- rf(100,4,20)
hist(z)
qqnorm(z)

#Task: Find out whether the time for the part-time job is at the expense of the time for university or not?
cor(mydata$h_job, mydata$h_stud, use = "pairwise.complete.obs", method="pearson") 
plot(mydata$h_job, mydata$h_stud)

#cut out outliers
tail(sort(mydata$h_job))  #Identify outliers for job -> h_job=100
tail(sort(mydata$h_stud)) #Identify outliers for study -> h_stud=500 & 100
mydata$h_job[mydata$h_job==100] <- NA
mydata$h_stud[mydata$h_stud==100] <- NA
mydata$h_stud[mydata$h_stud==500] <- NA

plot(mydata$h_job, mydata$h_stud) #Increased "zoom" once outliers are gone
cor.test(mydata$h_job, mydata$h_stud, alternative="less", method="pearson") #corr increased 

#Task: "Is there a connection/correlation between high income and high study satisfaction? Please use the 5-step approach"


#1.
# Income->metric
# Satisfaction->ordinal

# 2.
# H1: rho>0 
# H0: rho<=0

# 3.
# alpha = .05 

# 4.
cor(mydata$eink_akt,mydata$zuf_stud,method="spearman",
    use="pairwise.complete.obs")
plot(mydata$eink_akt,mydata$zuf_stud)
plot(mydata$eink_akt,jitter(mydata$zuf_stud)) #  "Use Jitter for discrete Variables with big sample size"
boxplot(mydata$eink_akt ~ mydata$zuf_stud)
hist(mydata$eink_akt)
hist(x,freq=F,density=20,las=1,main="",xlab="",breaks=12)


qqnorm(mydata$eink_akt)


# 5.
cor.test(mydata$eink_akt,mydata$zuf_stud,method="spearman",
         use="pairwise.complete.obs",alternative="greater")
cor.test(mydata$eink_akt,mydata$zuf_stud,method="kendall",
         use="pairwise.complete.obs",alternative="greater")




### Regression ###
#Task: "Can the current income of students be predicted by their income in the last semester?"

#Descriptive
plot(mydata$eink_lfs, mydata$eink_akt)
cor(mydata$eink_lfs, mydata$eink_akt, use = "pairwise.complete.obs", method="pearson") 
hist(mydata$eink_lfs)
hist(mydata$eink_akt)

#Regression
myreg <- lm(mydata$eink_akt ~ mydata$eink_lfs)

str(myreg)                   # structure
summary(myreg)               # overview
coefficients(myreg)          # coefficient
fitted.values(myreg)         # predicted Y-values
predict(myreg)               # Y prediction
residuals(myreg)             # difference realisation vs expectation

mycoef<-coefficients(myreg)
####str(a)
mycoef[1]+mycoef[2]*1000     # predicted value for x=1000


#Plot to easily check for normal distribution of residual values and homoscedacity
plot(myreg)

#show regression results
plot(mydata$eink_akt, mydata$eink_lfs)
abline(myreg, col="orchid", lwd=2)
points(mydata$eink_akt[857], mydata$eink_lfs[857], col="red", pch=16)   #point out outliers
points(mydata$eink_akt[974], mydata$eink_lfs[974], col="blue1", pch=16) #point out outliers

#Check for correlation of residual errors & predicted values
plot(residuals(myreg),fitted(myreg))
cor(residuals(myreg),fitted(myreg))    
options(scipen=18)                    
cor(residuals(myreg),fitted(myreg))    



# task 2

# 1. 
#Time spent for night life in hours (metric)
#predictor: Income in Euro (metric)

# 2. 
# Test of model significance:     H0: F=1 H1: F!=1
# coefficient test:               H0: beta.1=0 H1: beta.1!=0
# two sided test

# 3. alpha = .05

# 4. 
cor(mydata$eink_akt,mydata$h_nachtl,
    use="complete.obs")
plot(mydata$eink_akt,mydata$h_nachtl)

tail(sort(mydata$h_nachtl))
mydata$h_nachtl[mydata$h_nachtl==100] <- NA

cor(mydata$eink_akt,mydata$h_nachtl,
    use="complete.obs")
plot(mydata$eink_akt,mydata$h_nachtl)

# 5. 
reg <- lm(mydata$h_nachtl ~ mydata$eink_akt)
summary(reg)

# plot(...) -> 
abline(reg,col="red")





### Multiple Regression Example - 

### "Which influence is stronger on temperature?"

mydata2<-airquality
str(airquality)
multregair<-lm(airquality$Temp ~ airquality$Wind + airquality$Ozone)
summary(multregair)
### You can see that Windspeed has a very low significance ("." significance rating)


### Standardization of all variables

#none
(multreg<-lm(airquality$Temp ~ airquality$Wind + airquality$Ozone)) 

#Standardization
(standreg<-lm(scale(airquality$Temp) ~ 
                scale(airquality$Wind) + scale(airquality$Ozone)))


#par(mfrow=c(1,2))
plot(airquality$Ozone,airquality$Temp);abline(lm(airquality$Temp ~ airquality$Ozone,data=mydata2),col="red")
plot(airquality$Wind,airquality$Temp);abline(lm(airquality$Temp ~ airquality$Wind,data=mydata2),col="red")




##### t-Test #####

#Does the average temperatur at LaGuardia Airport differ from average German summer temperature?
AvgTempGermany <- 69.8
hist(airquality$Temp)
abline(v=mean(airquality$Temp, na.rm=T), col="red")
abline(v=AvgTempGermany, lty=2, col="red")
t.test(airquality$Temp, mu=AvgTempGermany, conf.level=.95)


##### ANOVA #####

### Beschreibendes Streudiagramm 
plot(jitter(mydata$fb),mydata$eink_akt,xlim=c(1.5,5.5),axes=F,xlab="Department",
     ylab="Income in Euro",col="grey")
axis(1,at=2:5,labels=c("02","03","04","05"))
axis(2,las=1)

abline(h=mean(mydata$eink_akt, na.rm=T), lty=2, col="darkblue")
mi <- tapply(mydata$eink_akt,mydata$fb,mean,na.rm=T)
se <- tapply(mydata$eink_akt,mydata$fb,sd,  na.rm=T)
mip <- mi+se; mim <- mi-se
for(i in 1:length(se)){
  segments(i+1,mip[i],i+1,mim[i])
  segments(i+1-.1,mim[i],i+1+.1,mim[i])
  segments(i+1-.1,mip[i],i+1+.1,mip[i])
}
points(2:5,mi,pch=16,col="blue",cex=2)

mydata$fb <- factor(mydata$fb)

mean(mydata$eink_akt,na.rm=T)
round(tapply(mydata$eink_akt, mydata$fb, mean, na.rm=T),2)
round(tapply(mydata$eink_akt, mydata$fb, sd,   na.rm=T),2)

boxplot(mydata$eink_akt ~ mydata$fb)

# Analysis of Variance
mod <- aov(eink_akt ~ fb, data=mydata)
summary(mod)
model.tables(mod)

# strength of effect eta^2 ("explained variance")
anova.tabelle <- summary(mod)[[1]]
anova.tabelle[1,2]     # SS.between
anova.tabelle[2,2]     # SS.within
sum(anova.tabelle[,2]) # SS.total

(eta2 <- anova.tabelle[1,2] / sum(anova.tabelle[,2]))

# Teacher: Basic rule
# >.26 big effect
# >.13 medium effect
# >.02 small effect


