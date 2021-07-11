if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2)

# code should work as long as standard deviation is positive; means can be positive or negative
# assumption is that each group has 100 subjects
# two tailed test, unequal variances assumed 

mean1 <- 2.21
sd1 <- .89

mean2 <- 1.56
sd2 <- .69

# defining upper and lower boundaries for x axis

upper_x_possible_1 <- mean1+3*sd1
upper_x_possible_2 <- mean2+3*sd2

if(upper_x_possible_1 > upper_x_possible_2){
  upper_x <- upper_x_possible_1
} else {
  upper_x <- upper_x_possible_2
}

lower_x_possible_1 <- mean1-3*sd1
lower_x_possible_2 <- mean2-3*sd2 

if(lower_x_possible_1 < lower_x_possible_2) {
  lower_x <- lower_x_possible_1
} else {
  lower_x <- lower_x_possible_2
}

# creating the distributions

dis_1 <- ggplot(data.frame(x=c(lower_x,upper_x)),aes(x=x))
dis_2 <- dis_1 + stat_function(fun=dnorm, size=1.2, args=list(mean=mean1, sd=sd1), aes(col="Distribution 1"))
dis_3 <- dis_2 + stat_function(fun=dnorm, size=1.2, args=list(mean=mean2, sd=sd2), aes(col="Distribution 2"))
dis_4 <- dis_3 + theme_classic()
dis_5 <- dis_4 + scale_colour_manual("Distributions", values= c("lightblue2","slateblue3"))

# calculating t value and critical value

variance_1 <- sd1^2
variance_2 <- sd2^2

mean_dif <- mean1-mean2
sample_error_sum <- sqrt(variance_1/100 + variance_2/100)
t_value <- mean_dif/sample_error_sum
critical_value <- qt(p=.05/2, df=198, lower.tail=FALSE)

# calculating p value to generate as title

p_value <- 2*(1-pt(abs(t_value), 198))
p_value_rounded <- signif(p_value, digits=3)
p_string <- paste("p value = ", p_value_rounded, sep = " ")

# creating functions to shade regions 

ShadeRegion1 <- function(x) {
  y <- dnorm(x, mean=mean1, sd=sd1)
  return(y)
}

ShadeRegion2 <- function(x) {
  y <- dnorm(x, mean=mean2, sd=sd2)
  return(y)
}

# shading regions and fixing labels

dis_6 <- dis_5 + stat_function(fun=ShadeRegion1, geom= "area", fill="lightblue2", alpha=0.1)
dis_7 <- dis_6 + stat_function(fun=ShadeRegion2, geom= "area", fill="slateblue3", alpha=0.1)
dis_8 <- dis_7 + xlab("") + ylab("Probability Density") + ggtitle(p_string) + theme(plot.title = element_text(hjust = 0.5))

# vertical lines represent the means; will turn red if you can reject the null and green if you cannot 

if(abs(t_value)>=critical_value) {
  dis_9 <- dis_8 + geom_vline(xintercept=mean1, lty="dotted", col="red", size=1)
  dis_10 <- dis_9 + geom_vline(xintercept=mean2, lty="dotted", col="red", size=1)
  dis_10
} else {
  dis_9 <- dis_8 + geom_vline(xintercept=mean1, lty="dotted", col="green", size=1)
  dis_10 <- dis_9 + geom_vline(xintercept=mean2, lty="dotted", col="green", size=1)
  dis_10
}