# Comparing a sample with a mean of x and a sd of y to a population with a mean of 0 and an unknown sd
# Constant sample size
# Point represents test statistic

mean1= 1 #x
sd1= 5 #y

# Calculating the test statistic

t <- mean1/(sd1/(sqrt(100)))

# Calculating the critical value

critical_value <- qt(p=.05, df=99, lower.tail=FALSE)

# Defining upper and lower boundaries

upper_x_possible <- 5

if(t > upper_x_possible){
  upper_x <- t+1
} else {
  upper_x <- 5
}

# calculating p value to generate as title

p_value <- (1-pt(abs(t), 99))
p_value_rounded <- signif(p_value, digits=3)
p_string <- paste("p value = ", p_value_rounded, sep = " ")

# Plotting the distribution

x <- seq(-5, upper_x, length=200)
y <- dnorm(x, mean=0, sd=1)

plot(x, y, type="l", lwd=2, col="slateblue3", frame=FALSE, ylab="Probability Density", xlab="", main=p_string)

# Defining & shading the rejection region

polygon_x <- seq(critical_value, upper_x, length=300)
polygon_y <- dnorm(polygon_x, mean=0, sd=1)
polygon(c(critical_value,polygon_x,upper_x), c(0,polygon_y,0), col="slateblue3")

# Adding a vertical line for the test statistic

abline(v=t, lty="dotted")

# Plotting the test statistic (green if not rejecting the null; red if rejecting)

if(t < critical_value){
  points(x=t, y=dnorm(t, mean=0, sd=1), pch=21, col="black", bg="green", cex=1.5)
} else {
  points(x=t, y=dnorm(t, mean=0, sd=1), pch=21, col="black", bg="red", cex=1.5)
}