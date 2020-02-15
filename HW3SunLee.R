#bring readxl libray
library(readxl)
#setdirectory
setwd("/Users/seonmin/Desktop/stathw3")
#read sediment data and put it 
sed = read.table("sediment.data",header=T)
#attach sediment data
attach(sed)
fit<-lm(Ni ~ Site, data = sed)
anova(fit)
#qqnorm plot
qqnorm(fit$residuals)

#qqline (Q1~Q3)
qqline(fit$residuals)
#In this plot, there is a big outlier.
#Except that outlier, most of the points follow normality.
#



#Make box plot to compare 3 Groups
boxplot(Cu ~ Site, data = sed)
#Show points to Clafify
points(Cu ~ Site, data = sed)

#fit the analysis of variance
fit1<-lm(Cu ~ Site, data = sed)
#anova function (analysis of variance)
summary(fit1)


#
require(graphics)

#fitted model from anova
fit2<-aov(sed$Cu ~ sed$Site)
#check for pairwise differences(Turkey test)
TukeyHSD(fit2, "sed$Site")

#1-d 
# Tukey test is different from Anova test.
# It is because Anova test is used to find significant differecne
# between three or more group means and find which mean is significant
# different. However, Tukey test is designed to perform a pairwise 
# comparison of the means to see significant 
# difference lies
library(readr)
#Question 2
setwd("/Users/seonmin/Desktop/stathw3")
drp<-read.table("/Users/seonmin/Desktop/stathw3/hummingbird.txt", header =TRUE)
drp
boxplot(length ~ variety, data = drp)
#Hypothesis
#H0 : There is no big difference among these  group.
#H1 : Three groups are different in size
#kruskal test
kruskal.test(length ~ variety, data= drp)
#Anova Test
#fitted 
fit3<-lm(length~variety, data = drp)
#summarize fit3
summary(fit3)
#output the anova table
anova(fit3)


#pairwise t test p-value adj = none
pairwise.t.test(drp$length,drp$variety, p.adj="none")
#pairwise t test p-value adj = bonferroni
pairwise.t.test(drp$length,drp$variety, p.adj="bon")
#Tukey multiple comparison
summary(aov(drp$length~drp$variety))
#
#fitted model from anova
fit5<-aov(drp$length~drp$variety)
TukeyHSD(fit5)

#cfheickt assumption
qqnorm(fit3$residuals)
qqline(fit3$residuals)

#Conclusion(Summarize result)
# The p-value rejected the hypothesis that there was no differece in
# the length of th groups

#Question 3
drp$length = log(drp$length)
#assumption
qqnorm(drp$length)
qqline(drp$length)

#These are not closer to what i want for a valid analysis
#
#
#Question4
poet <- read.table("/Users/seonmin/Desktop/stathw3/poets.txt", header =T)
poet
#install.packages("agricolae")
#bring agricolae from library
library(agricolae)

#4-d

fit6<-aov(poet$Age ~ poet$Type)
#LSD comparison summary table
(LSD.test(fit6, "poet$Type", alpha = 0.05, p.adj="none"))
(LSD.test(fit6, "poet$Type", alpha = 0.05,group =FALSE, p.adj="bonferroni"))
#LSD plot
plot(LSD.test(fit6, "poet$Type", alpha = 0.05, p.adj="bonferroni"))
plot(LSD.test(fit6, "poet$Type", alpha = 0.05, p.adj="none"))

fit7<-aov(poet$Age ~ poet$writer)
#LSD comparison summary table
(LSD.test(fit7, "poet$Type", alpha = 0.05, p.adj="none"))
(LSD.test(fit7, "poet$Type", alpha = 0.05,group =FALSE, p.adj="bonferroni"))
#LSD plot
plot(LSD.test(fit7, "poet$writer", alpha = 0.05, p.adj="bonferroni"))
plot(LSD.test(fit7, "poet$writer", alpha = 0.05, p.adj="none"))


#Question5
# aaaa = 1151.498
# bbbb = 4.5314
# cccc = 0.1044
# dddd = 20.534
# eeee = 245822
# sum of squares total = 873930
# observation 74 data set
