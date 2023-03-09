library(ggplot2)
library(dplyr)
library(tidyr)

#load the dataset
data <- read.csv("data.csv")

#fit a regression model
#model <- lm(Time ~ Pairs, data=data)

#get list of residuals 
#res <- resid(model)

#produce residual vs. fitted plot
#my_plot <- plot(fitted(model), res, xlab="Fitted plot", ylab="Residual plot")
# my_plot <- my + xlab('Fitted plot') + ylab('Residual plot')

#add a horizontal line at 0 
#abline(0,0)


# //////// Determining whether the residuals follow normal distribution

#create Q-Q plot for residuals
# qqnorm(res)

#add a straight diagonal line to the plot, the residuals tend to stray from the line, which could indicate that they’re not normally distributed
# qqline(res) 

#Create density plot of residuals, does not strictly follow the bell curve, thus might not be normal distribution
# plot(density(res))

# Fit the full model
full_model <- lm(Time ~ Pairs * Size * Status, data = data)

# Create a residual plot
residual_plot <- ggplot(data, aes(x = Pairs, y = resid(full_model))) +
  geom_point() +
  facet_wrap(~ Size + Status, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Pairs") +
  ylab("Residuals") +
  ggtitle("Residual Plot for Full Model")

print(residual_plot)