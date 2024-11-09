##ass 7

#Q1.
n = 100
df = n-1
y = rt(n,df)

hist(y)

#Q2.
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

#Q3.
X = seq(-6,6,length=100)
df = c(1,4,10,30)
cl = c("yellow","red","green","blue")
plot(X,dt(X,df[4]),col = cl[4])
for (i in 1:3) {
  lines(X,dt(X,df[i]),col=cl[i])
}

#NOTE
# p : need to calculate cumulative value (ar. under curve)
# d : prob at particular pt
# r : to find random var
# q : quantile/percentile 

#Q4.
qf(0.95,10,20)

s1 <- pf(1.5,10,20)
s2 <- 1 - pf(1.5,10,20)
s1 + s2

qf(0.25,10,20)
qf(0.5,10,20)
qf(0.75,10,20)
qf(0.999,10,20)

rf(1000,10,20)
