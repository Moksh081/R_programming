##exp 3
#q1
n <- 12
p <- 1/6
prob_7 <- dbinom(7, size=n, prob=p)
prob_8 <- dbinom(8, size=n, prob=p)
prob_9 <- dbinom(9, size=n, prob=p)
total_prob <- prob_7+prob_8+prob_9

probp_6 <- pbinom(6, size=n, prob=p)
probp_9 <- pbinom(9, size=n, prob=p)
new_total <- probp_9-probp_6

#q2
1-pnorm(84,mean=72,sd=15.2) #or
pnorm(84,mean=72,sd=15.2,lower.tail = FALSE, log.p = FALSE)

#q3
dpois(x=0,lambda = 5)
a <- ppois(q=47,lambda = 50,log = FALSE)
b <- ppois(q=50,lambda = 50, log = FALSE)
b-a
#or if using + wala then need to use dpois, if subtraction approach then p wala

#q4
dhyper(x=3, m=17, n=233 , k= 5, log = FALSE)
#x = x, m = k, n = N-k, K= k
#here N = 250,m=k=17 

#q5
x <- 0:31
pmf <- dbinom(x,size=31,prob=0.447)
plot(x,pmf)

x<-0:31
cmf <- pbinom(x,size=31,prob=0.447,lower.tail=TRUE)
plot(x,cmf)

#n=31, p=0.447, variance(sigm^2)= npq, std_dev(sigma) = (npq)^0.5
n=31
p=0.447
q=1-p
var = n*p*q
std = (n*p*q)^0.5
