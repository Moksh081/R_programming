install.packages("pracma")
library("pracma")

##Q1. density fn (continuous data) : integration

##1.1  check that it is a joint density function or not? (Use integral2())
f <- function(x,y)(2*(2*x+3*y)/5)
integral2(f,0,1,0,1)

##1.2  find marginal distribution g(x) at x = 1
g <- function(y)f(1,y)
integrate(g,0,1)

##1.3  find the marginal distribution h(y) at y = 0
h <- function(x)f(x,0)
integrate(h,0,1)

##1.4  find the expected value of g(xy) = xy.
t <- function(x,y)x*y*f(x,y)
integral2(t,0,1,0,1)

##Q2 mass fn (discrete data) : summation

##2.1  display the joint mass function in rectangular (matrix) form.
m <- function(x,y)((x+y)/30)
M1 <- matrix(c(m(0:3,0),m(0:3,1),m(0:3,2)),nrow = 4,ncol = 3,byrow=FALSE)
M1

##2.2  check that it is joint mass function or not? (use: Sum())
sum(M1)

##2.3  find the marginal distribution g(x) for x = 0123. (Use:apply())
g_x <- apply(M1,1,sum)  #1 : for row wise
g_x

##2.4  find the marginal distribution h(y) for y = 012. (Use:apply())
h_y <- apply(M1,2,sum)  #2 : for col wise

##2.5  find the conditional probability at x = 0 given y = 1. ie P(x=0|y=1)
#position wise : 1,2,... not from 0 jo R read krega
M1[1,2]/h_y[2]

##2.6  find E(x)E(y)E(xy)Var(x)Var(y)Cov(xy) and its correlation coeficient.
x = c(0:3)
y = c(0:2)

E_x <- sum(x*g_x)
E_x

E_y <- sum(y*h_y)
E_y

l <- function(x,y)x*y*m(x,y)
M2 <- matrix(c(l(0:3,0),l(0:3,1),l(0:3,2)),nrow = 4,ncol = 3,byrow=FALSE)
E_xy <- sum(M2)
E_xy

var_x <- sum(x^2*g_x) - (E_x)^2
var_x

var_y <- sum(y^2*h_y) - (E_y)^2
var_y

cov_xy <- E_xy - E_x*E_y
cov_xy
