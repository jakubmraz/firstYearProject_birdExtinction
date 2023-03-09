library(ggplot2)

#load the dataset
data <- read.csv("data.csv")

#fit a regression model
model <- lm(Time ~ Pairs, data=data)


# Scatterplot with scale_x_log10() and scale_y_log10()
log_plot <- ggplot(data = data, aes(x=Pairs, y=Time)) +
  geom_point() + scale_y_log10()
print(log_plot)

#sqrt(time)
sqrt_plot <- ggplot(data = data, aes(x=Pairs, y=Time)) +
  geom_point() + scale_y_sqrt()
print(sqrt_plot)

#1/time g
inverse_plot <- ggplot(data = data, aes(x=Pairs, y=1/Time)) +
  geom_point()
print(inverse_plot)