##exp 1

#q1 creating vector and finding max and min element
a <- c(5,10,15,20,25,30)
max_value <- max(c)
print(max_value)
min_value <- min(c)
print(min_value)
#print(paste("Minimum value:", min_value))

#q2 factorial

factorial_function <- function(n){
  if(n<0){
    return("Error: Negative number, factorial not defined.")
  }else if(n==0||n==1){
    return(1)
  }else{
    return(n * factorial_function(n - 1))
  }
}
n <- as.integer(readline(prompt="Enter a number: "))
result <- factorial_function(n)
print(result)

#q3 fibonacci

fibonacci_function <- function(n) {
  a <- 0
  b <- 1
  
  if (n <= 0) {
    return("Error: Fibonacci sequence is not defined for n <= 0.")
  } else if (n == 1) {
    return(a)
  } else if (n == 2) {
    return(c(a, b))
  } else {
    seq <- numeric(n)
    seq[1] <- a
    seq[2] <- b
    for (i in 3:n) {
      seq[i] <- seq[i-1] + seq[i-2]
    }
    return(seq)
  }
}

n <- as.integer(readline(prompt = "Enter a number: "))
result <- fibonacci_function(n)
print(result)


#q4 calculator
calculator_function <- function(){
  a <- as.integer(readline(prompt = "Enter no. a: "))
  b <- as.integer(readline(prompt = "Enter no. b: "))
  x <- readline(prompt = "Enter operator: ")
  if(x=='+'){
    return(a+b)
  }else if(x=='-'){
    return(a-b)
  }else if(x=='*'){
    return(a*b)
  }else if(x=='/' && b!=0){
    return(a/b)
  }else{
    print(paste("INVALID x"))
  }
}
result <- calculator_function()
print(result)

#q5 plots

data <- c(2, 3, 5, 7, 8)

#bar plot
barplot(data, width = 1,col = "blue", border = par("fg"),
        main = "Bar Plot",xlab = "X", ylab = "Y")
#pie  
pie(data, labels = names(data), edges = 200, radius = 1,
    col=rainbow(length(data)), border = NULL,main = "Pie Plot")

#simple line plot
plot(data, type="o", col="red", main="Line Plot", xlab="X-axis", ylab="Y-axis")

#######################EXP 2

#q1 a. coins to be select = 10 , in chest : S = 30, G = 20, B = 50 sample space?
coins = c(rep('G',20),rep('S',30),rep('B',50))
s1 = sample(x=coins,size=10)

#q1 b. chance of succ 90%, fail 10% prob for next 10 chances ?
sur = c('succ','fail')
p = c(0.9,0.1)
s1=sample(x=sur,size=10,replace=TRUE,prob=p)
print(s1)

#q2

f2 <- function(n) {
  prob_no_match <- 1
  for (i in 1:(n-1)) {
    prob_no_match <- prob_no_match * (365 - i) / 365
  }
  return(prob_no_match)  # Probability of no match
}

f2_match <- function(n) {
  return(1 - f2(n))  # 1 - probability of no match
}

# Test with n = 2
n <- 2
x5 <- f2_match(n)
print(x5)

###or
  f2=function(n){
    a=factorial(365)/(365**n * factorial(365-n))
    return(a)
  }


x5=f2(2)


##q3 cloudy : 40% & rain: 20%, clouds : 85% when rainy 
# outside cloudy , rain ?? use conditional prob
f1=function(A,B,A_B){
  B_A = (A_B*B)/A
  return (B_A)
}
x=0.4
y=0.2
z=0.85
t=f1(x,y,z)
print(t)

#q4 iris for 150 flowers and 5 diff. species
data=iris
# (a) Print first few rows of this dataset.
head(data)
#  (b) Find the structure of this dataset.
str(data)
# (c) Find the range of the data regarding the sepal length of flowers.
range(data$Sepal.Length)
# (d) Find the mean of the sepal length.
mean(data$Sepal.Length)
#  (e) Find the median of the sepal length.
median(data$Sepal.Length)
# (f) Find the first and the third quartiles and hence the interquartile range.
x1=quantile(data$Sepal.Length,0.25)
x3=quantile(data$Sepal.Length,0.75)

IQR=x3-x1
x1
x3
IQR

# (g) Find the standard deviation and variance.
sd(data$Sepal.Length)
var(data$Sepal.Length)

# (h) Use the built-in function summary on the dataset Iris.
summary(data$Sepal.Length)

##q5 find mode

find_mode <- function(x){
  uniq_x <- unique(x)
  mode_value <- uniq_x[which.max(tabulate(match(x, uniq_x)))]
  return(mode_value)
}

sample_data <- c(1, 2, 2, 3, 4, 4, 4, 5)
mode_result <- find_mode(sample_data)
mode_result


####################### exp3

#q1 no. of 6 : 7,6,9 if 12 dice rolled ?
n <- 12
p <- 1/6
prob_6 <- pbinom(6,size = n,prob=p)
prob_9 <- pbinom(9,size = n,prob=p)
a <- prob_9 - prob_6

## or

prob_7 <- dbinom(7, size=n, prob=p)
prob_8 <- dbinom(8, size=n, prob=p)
prob_9 <- dbinom(9, size=n, prob=p)
total <- prob_7+prob_8+prob_9

#q2 mean score = 72, sd = 15.2, %student score >=84
1-pnorm(84,mean=72,sd=15.2)
#or
pnorm(84,mean=72,sd=15.2,lower.tail = FALSE, log.p = FALSE)

#q3 
a <- ppois(q=47,lambda = 50,log = FALSE)
b <- ppois(q=50,lambda = 50, log = FALSE)
b-a
#or
a <- dpois(48, lambda = 50)
b <- dpois(49, lambda = 50)
c <- dpois(50, lambda = 50)

result <- a + b + c
result

#q4 n = 250, 17 defective : randomly select 5 , X : no of def P(X=3)?

dhyper(x=3,m=17,n=233,k=5,log=FALSE)

#q5 44.7% use wikepedia in atleast 1 paper, n = 31, X random student

#  (a) How is X distributed?
x <- 0:31

#  (b) Sketch the probability mass function
pmf <- dbinom(x,31,0.447)
plot(x,pmf)

#  (c) Sketch the cumulative distribution function.
cmf <- pbinom(x,31,0.447)
plot(x,cmf)

#  (d) Find mean, variance and standard deviation of X.
n = 31
p = 0.447
q = 1-p
var <- n*p*q
std <- (n*p*q)^0.5


####################### exp 4

#q1 average number of imperfections per 10 meters
x = c(0,1,2,3,4)
p = c(0.41,0.37,0.16,0.05,0.01)
sum(x*p)/sum(x)

or
t = weighted.mean(x,p)
t/sum(x)

c(x%*%p)

#q2 
f = function(t){
  t*(0.1)*exp(-0.1*t)
}
integrate(f,0,Inf)$value

#q3
x = c(0,1,2,3)
p = c(0.1,0.2,0.2,0.5)
y = 12*x + 2*(x-3) - 18
weighted.mean(y,p)

#q4
t = function(x){
  x*0.5*exp(-abs(x))
}
t1 = integrate(t,1,10)$value #mean

t3 = function(x){
  x*x*0.5*exp(-abs(x))
}
t2 = integrate(t3,1,10)$value

var = t2 - t1*t1
var

#q5
f = function(y){
  0.75*(0.25)^(sqrt(y)-1)
}
x = c(1,2,3,4,5)
y = x^2
p = f(y)
m1 = weighted.mean(y,p)
m2 = weighted.mean(y^2 , p)

#or

m1 = sum(y*p)
m2 = sum(y^2*p)

var = m2-m1*m1
var


