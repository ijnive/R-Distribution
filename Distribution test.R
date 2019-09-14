#http://blog.pulipuli.info/2018/01/rkruskalwalliswelchs-anova-non.html
#http://www3.nccu.edu.tw/~99354011/R%20commands(11.09.13).pdf
#skewness()http://www.math.nsysu.edu.tw/~lomn/homepage/R/R_testing.htm
#https://officeguide.cc/r-normality-test-tutorial/
#http://statisticprojct.weebly.com/
#http://rpubs.com/Mentors_Ubiqum/removing_outliers  # remove outliers
#install.packages("plyr")
#install.packages("moments") # to check skewness and Kurtosis
library(plyr)
library(moments)
test <- read.table("C:/Users/Eugene/Desktop/5000.CSV", quote="\"", comment.char="") # the path should be modified
test = as.vector(as.matrix(test))
hist(test,breaks="Freedman-Diaconis") #default is using Sturges method or can set breaks = seq(from = ?, to = ?, by = ?)
hist(test)
shapiro.test(test[1:5000]) #to test it is a normal distribution with data size <5000
t.test(test, y=NULL, alternative=c("two.sided", "less", "greater"), mu=0, paired=FALSE, var.equal=FALSE, conf.level=0.95)
chisq.test(test, y = NULL, correct = TRUE,p = rep(1/length(test), length(test)),simulate.p.value = FALSE, B = 4000)
mean(test)
median(test)
sd(test)
var(test)
range(test)
quantile(test)
IQR(test)
summary(test)
plot(density(test)) #plot(density(test),xlim=c(500,3500))
count = table(round_any(test,100,floor)) # count numbers of every gap, which divied by 100.
skewness(test)
kurtosis(test)
# mode is not a built-in function, so need to creat
mode<-function(x){
       ux<-sort(unique(x))  # find all unique numbers
       tab<-tabulate(match(x,ux))  #put all numbers' count
       ux[tab==max(tab)]
  }
mode(test)
# test2 is a subset of test, test2 has removed the outlier and compared with N-Distribution.
v <- boxplot(test)$out # plot & list outlier
test2 <- test[-which(test %in% v)] # assign test2 with data of removing outliers
plot(density(test2))
curve(dnorm(x,mean = mean(test2), sd=sd(test2)), col="green", add=T)
