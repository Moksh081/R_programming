#Q1
A=0
B=60
X=45
punif(X,A,B,lower.tail = FALSE)
a=punif(20,0,60)
b=punif(30,0,60)
b-a
#Q2
#a
dexp(3,1/2)
#b
x=seq(0,5,by=0.01)
pdf=dexp(x,1/2)
plot(x,pdf)
#c
pexp(3,1/2)
#d
cdf=pexp(x,1/2)
plot(x,cdf)
#e
set.seed(5)#command to generate the same output every time
r=rexp(1000,1/2)
hist(r,breaks = 20)
#Q3
#a
a1=2
b1=1/3
dgamma(3,a1,b1)
pgamma(1,a1,b1,lower.tail = FALSE)
#b  use qgamma
qgamma(0.7,a1,b1)
