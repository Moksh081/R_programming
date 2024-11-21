####ASS 5

#Q1. Consider that X is the time (in minutes) that a person has to wait in order to take a flight. 
#If each flight takes off each hour X ~ U(0, 60). Find the probability that  
#(a) waiting time is more than 45 minutes, and 
#(b) waiting time lies between 20 and 30 minutes. 

a <- 0
b <- 60

#a. 
punif(45,a,b,lower.tail = FALSE)

#b.
punif(30,a,b)-punif(20,a,b)

#Q2. The time (in hours) required to repair a machine is an exponential distributed random 
#variable with parameter λ = 1/2. 
#(a) Find the value of density function at x = 3. 
#(b) Plot the graph of exponential probability distribution for 0 ≤ x ≤ 5. 
#(c) Find the probability that a repair time takes at most 3 hours. 
#(d) Plot the graph of cumulative exponential probabilities for 0 ≤ x ≤ 5. 
#(e) Simulate 1000 exponential distributed random numbers with λ = ½ and plot the 
#simulated data. 

t <- 1/2

#a.
dexp(3,t)

#b.
x <- seq(0,5,by=0.01)
pdf = dexp(x,t)
plot(x,pdf)

#c.
pexp(3,t)

#d.
x<- seq(0,5,by=0.01)
cdf = pexp(x,t)
plot(x,cdf)

#e.
set.seed(5)
r=rexp(1000,t)
hist(r,breaks = 20)


#Q3. The lifetime of certain equipment is described by a random variable X that follows 
#Gamma distribution with parameters α = 2 and β = 1/3. 
#(a) Find the probability that the lifetime of equipment is (i) 3 units of time, and (ii) 
#at least 1 unit of time. 
#(b) What is the value of c, if P(X ≤ c) ≥ 0.70? (Hint: try quantile function qgamma()) 

a <- 2
b <- 1/3

#a.
dgamma(3,a,b)

pgamma(1,a,b,lower.tail = FALSE)

#b.
qgamma(0.7,a,b,)

#############ASS6.

install.packages("pracma")
library("pracma")

#Q1. (i) check that it is a joint density function or not? (Use integral2())
#(ii) nd marginal distribution g(x) at x = 1.
#(iii) nd the marginal distribution h(y) at y = 0
#(iv) nd the expected value of g(xy) = xy.

f <- function(x,y)(2*(2*x+3*y)/5)

#a.
integral2(f,0,1,0,1)

#b.
g <- function(y)f(1,y)
integrate(g,0,1)

#c.
h <- function(x)f(x,0)
integrate(h,0,1)

#d.
X <- function(x,y)x*y*f(x,y)
integral2(X,0,1,0,1)


##Q2.f(x,y) = (x+y)/30; x =0,1,2,3; y =0,1,2
# (i) display the joint mass function in rectangular (matrix) form.
#(ii) check that it is joint mass function or not? (use: Sum())
#(iii) find the marginal distribution g(x) for x = 0123. (Use:apply())
#(iv) find the marginal distribution h(y) for y = 012. (Use:apply())
#(v) find the conditional probability at x = 0 given y = 1.
#(vi) find E(x)E(y)E(xy)Var(x)Var(y)Cov(xy) and its correlation coe cient.


f <- function(x,y)((x+y)/30)

#a.
m <- matrix(c(f(0:3,0),f(0:3,1),f(0:3,2)),nrow= 4,ncol= 3,byrow=FALSE)
m

#b.
sum(m)

#c.
g_x<-apply(m,1,sum)

#d.
h_y<-apply(m,2,sum)

#e.
m[1,2]/h_y[2]

#f.
x = c(0:3)
y = c(0:2)

E_x <- sum(x*g_x)
E_x

E_y <- sum(y*h_y)
E_y

l <- function(x,y)x*y*f(x,y)
M2 <- matrix(c(l(0:3,0),l(0:3,1),l(0:3,2)),nrow = 4,ncol = 3,byrow=FALSE)
E_xy <- sum(M2)
E_xy

var_x <- sum(x^2*g_x) - (E_x)^2
var_x

var_y <- sum(y^2*h_y) - (E_y)^2
var_y

cov_xy <- E_xy - E_x*E_y
cov_xy


######Ass.7

#Q1. Use the rt(n,df) function in r to investigate the t-distribution for n = 100 and df = n 1 and plot
#the histogram for the same.

n = 100
df = n-1
y = rt(n,df)
hist(y)

#Q2. Use the rchisq(n,df) function in r to investigate the chi-square distribution with n = 100 and
#df = 2,10,25.

n = 100
df = 2,10,25
x <- rchisq(100,2)
t <- rchisq(100,10)
z <- rchisq(100,25)
x
t
z

par(mfrow = c(1,3))
hist(x , col = "green")
hist(t , col = "red")
hist(z , col = "blue")
par(mfrow = c(1,1))

#Q3. Generate a vector of 100 values between-6 and 6. Use the dt() function in r to nd the values of a
#t-distribution given a random variable x and degrees of freedom 1,4,10,30. Using these values plot
#the density function for students t-distribution with degrees of freedom 30. Also shows a comparison
#of probability density functions having di erent degrees of freedom (1,4,10,30).

x = seq(-6,6,length=100)
df = c(1,4,10,3)
cl = c("yellow","red","green","blue")
plot(x,dt(x,df[4]),col=cl[4])
for(i in 1:3){
  lines(x,dt(x,df[i]),col=cl[i])
}

#Q4.  (i) To find the 95th percentile of the F-distribution with (10,20) degrees of freedom.
#(ii) To calculate the area under the curve for the interval [0,1.5] and the interval [1.5,+ ) of
#a F-curve with v1 = 10 and v2 = 20 (USE pf()).
#(iii) To calculate the quantile for a given area (= probability) under the curve for a F-curve
#with v1 = 10 and v2 = 20 that corresponds to q = 02505075 and 0999. (use the qf())
#(iv) To generate 1000 random values from the F-distribution with v1 = 10 and v2 = 20 (userf())and plot a histogram

#a.
qf(0.95,10,20)

#b.
s1 <- pf(1.5,10,20)
s2 <- 1 - pf(1.5,10,20)
s1 + s2

#c.
qf(0.25,10,20)
qf(0.5,10,20)
qf(0.75,10,20)
qf(0.999,10,20)

#d.
t<- rf(1000,10,20)
hist(t)

#######ASS. 8


#(a) Import the csv data file in R.
#(b) Validate data for correctness by counting number of rows and viewing the top
#ten rows of the dataset.
#(c) Calculate the population mean and plot the observations by making a histogram.
#(d) Mark the mean computed in last step by using the function abline.


#a.
data = read.csv(file.choose())

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

# (a) Draw sufficient samples of size 10, calculate their means, and plot them in R
#by making histogram. Do you get a normal distribution.
#(b) Now repeat the same with sample size 50, 500 and 9000. Can you comment on
#what you observe

#a.
n<- 1000
s = c()
for(i in 1:n){
  s[i] = mean(sample(data$Wall.Thickness,10,replace=TRUE))
}

hist(s)

abline(v=mean(s),col="black")

#b.
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
