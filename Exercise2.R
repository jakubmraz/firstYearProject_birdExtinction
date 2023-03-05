#load the dataset
data <- read.csv("data.csv")

#fit a regression model
model <- lm(Time ~ Pairs, data=data)

#get list of residuals 
res <- resid(model)

#produce residual vs. fitted plot
my_plot <- plot(fitted(model), res, xlab="Fitted plot", ylab="Residual plot")
# my_plot <- my + xlab('Fitted plot') + ylab('Residual plot')

#add a horizontal line at 0 
abline(0,0)


# //////// Determining whether the residuals follow normal distribution

#create Q-Q plot for residuals
# qqnorm(res)

#add a straight diagonal line to the plot, the residuals tend to stray from the line, which could indicate that they’re not normally distributed
# qqline(res) 

#Create density plot of residuals, does not strictly follow the bell curve, thus might not be normal distribution
# plot(density(res))