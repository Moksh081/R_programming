#Q1.

#a
data = read.csv(file.choose())

str(data)

#b
nrow(data)
head(data,10)

ncol(data)

dim(data)

#c
m <- mean(data$ Wall.Thickness)
hist(data$ Wall.Thickness)

#d
abline(v=m, col="lightblue")

#1.a
n <- 1000
S = c()
for(i in 1:n){
  S[i] = mean(sample(data$ Wall.Thickness,10,replace = TRUE))
}
hist(S)

abline(v=mean(S),col="black")

#1.b
n = 1000
s1 = c()
s2 = c()
s3 = c()
for(i in 1:n){
  s1[i] = mean(sample(data$Wall.Thickness,50,replace = TRUE))
  s2[i] = mean(sample(data$Wall.Thickness,500,replace = TRUE))
  s3[i] = mean(sample(data$Wall.Thickness,9000,replace = TRUE))
}
par(mfrow = c(1,3))
hist(s1 , col = "green")
abline(v=mean(s1),col = "black")
hist(s2 , col = "red")
abline(v=mean(s2),col = "black")
hist(s3 , col = "blue")
abline(v=mean(s3),col = "black")
par(mfrow = c(1,1))


##Q2.

x = c(58,69,43,39,63,52,47,31,74,36)
y = c(189,235,193,177,154,191,213,165,198,181)

reg = lm(y~x)
plot(x,y,pch=16,abline(reg))

predict(reg,data.frame(x=60))

##Q3.
#level of significance + level of conf. = 1

b = c(145,173,158,141,167,159,154,167,145,153)
a = c(155,167,156,149,168,162,158,169,157,161)

t.test(b,a,alternative="greater",mu=0,paired=TRUE,conf.level=0.95)

#if greater ie b<a, and if result is +v : b<a , if -v : false assumption therefore b>a