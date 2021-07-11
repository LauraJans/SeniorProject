if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2)

# code should work as long as standard deviation is positive; mean can be positive or negative

mean1 <- 3
sd1 <- 6
sample_size <- 100 #constant

# Calculating test statistic

t <- mean1/(sd1/(sqrt(sample_size)))

# Calculating critical value

critical_value <- qt(p=.05/2, df=sample_size-1, lower.tail=FALSE)

# defining upper and lower boundaries

upper_x_possible <- mean1+3*sd1

if(upper_x_possible > 2){
  upper_x <- upper_x_possible
} else {
  upper_x <- 2
}

lower_x_possible <- mean1-3*sd1

if(lower_x_possible < -2) {
  lower_x <- lower_x_possible
} else {
  lower_x <- -2
}

# creating the distribution

dis_1 <- ggplot(data.frame(x=c(lower_x,upper_x)),aes(x=x))
dis_2 <- dis_1 + stat_function(fun=dnorm, args=list(mean=mean1, sd=sd1), col="black")
dis_3 <- dis_2 + theme_classic()

# calculating p value to generate as title

p_value <- 2*(1-pt(abs(t), 99))
p_value_rounded <- signif(p_value, digits=3)
p_string <- paste("p value = ", p_value_rounded, sep = " ")

# Shading (green if the null is not rejected; red if the null is rejected)  

ShadeRegion <- function(x) {
  y <- dnorm(x, mean=mean1, sd=sd1)
  return(y)
}

if(abs(t) >= critical_value) {
  dis_4 <- dis_3 + stat_function(fun=ShadeRegion, geom= "area", fill="red", alpha=0.1)
  dis_5 <- dis_4 + xlab("") + ylab("Probability Density") + ggtitle(p_string) + theme(plot.title = element_text(hjust = 0.5))
  dis_6 <- dis_5 + geom_vline(xintercept=0)
  dis_6
} else {
  dis_4 <- dis_3 + stat_function(fun=ShadeRegion, geom= "area", fill="green", alpha=0.1)
  dis_5 <- dis_4 + xlab("") + ylab("Probability Density") + ggtitle(p_string) + theme(plot.title = element_text(hjust = 0.5))
  dis_6 <- dis_5 + geom_vline(xintercept=0)
  dis_6
}