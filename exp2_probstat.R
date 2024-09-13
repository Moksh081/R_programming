##q1
coins=c(rep('G',20),rep('S',30),rep('B',50))
s1=sample(x=coins,size=10)

oper=c('succ','fail')
p1=c(0.9,0.1)
s2=sample(x=oper,size=10,replace=TRUE,prob=p1)

##q2

f2=function(n){
  a=factorial(365)/(365**n * factorial(365-n))
  return(a)
}


x5=f2(2)

##q3
f1=function(A,B,A_B){
  B_A = (A_B*B)/A
  return (B_A)
}
x=0.4
y=0.2
z=0.85
t=f1(x,y,z)

##q4
data=iris
head(data)


str(data)


range(data$Sepal.Length)


mean(data$Sepal.Length)


median(data$Sepal.Length)

x1=quantile(data$Sepal.Length,0.25)
x3=quantile(data$Sepal.Length,0.75)

IQR=x3-x1
x1
x3
IQR


sd(data$Sepal.Length)
var(data$Sepal.Length)



summary(data$Sepal.Length)
summary(data$Sepal.Width)
summary(data$Petal.Length)
summary(data$Petal.Width)


summary(data)

##q5
find_mode <- function(x) {
  uniq_x <- unique(x)
  mode_value <- uniq_x[which.max(tabulate(match(x, uniq_x)))]
  return(mode_value)
}

# Example usage:
sample_data <- c(1, 2, 2, 3, 4, 4, 4, 5)
mode_result <- find_mode(sample_data)
print(paste("Mode of the dataset:", mode_result))


