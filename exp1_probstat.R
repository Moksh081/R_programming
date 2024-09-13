##exp 1

##q1
c <- c(5, 10, 15, 20, 25, 30)
max_value <- max(c)
min_value <- min(c)
print(max_value)
print(paste("Minimum value:", min_value))

##q2
factorial_function <- function(n) {
  if (n < 0) {
    return("Error: Negative number, factorial not defined.")
  } else {
    return(factorial(n))
  }
}

n <- as.integer(readline(prompt="Enter a number: "))
result <- factorial_function(n)
print(result)

##q3
fibonacci_function <- function(n){
  a = 0
  b = 1
  if(n==1){
    return 0
  }else if(n==2){
    return (c(a,b))
  }else if(n>2){
    seq <- numeric(n)
    seq[1] <- a
    seq[2] <- b
    for(i in 3:n){
      seq[i] <- seq[i-1] + seq[i-2]
    }
    return (seq)
  }
}

n <- as.integer(readline(prompt="Enter a number: "))
result <- fibonacci_function(n)
print(result)

##q4
calculator <- function() {
  num1 <- as.numeric(readline(prompt="Enter first number: "))
  operator <- readline(prompt="Enter operator (+, -, *, /): ")
  num2 <- as.numeric(readline(prompt="Enter second number: "))
  
  result <- switch(operator,
                   "+" = num1 + num2,
                   "-" = num1 - num2,
                   "*" = num1 * num2,
                   "/" = ifelse(num2 != 0, num1 / num2, "Error: Division by zero"),
                   "Invalid operator")
  
  return(result)
}

result <- calculator()
print(result)

##q5
# Example: Simple plots
data <- c(2, 3, 5, 7, 8)

# Bar Plot
barplot(data, main="Bar Plot", xlab="X-axis", ylab="Y-axis", col="blue")

# Pie Chart
pie(data, main="Pie Chart", col=rainbow(length(data)))

# Simple Line Plot
plot(data, type="o", col="red", main="Line Plot", xlab="X-axis", ylab="Y-axis")

