library(ggplot2)

#load the dataset
data <- read.csv("data.csv")

# Create the models with different transformations
full_model_log <- lm(log(Time) ~ Pairs * Size * Status, data = data)

full_model_sqrt <- lm(sqrt(Time) ~ Pairs * Size * Status, data = data)

full_model_div <- lm(1/Time ~ Pairs * Size * Status, data = data)

# Create a residual plot for log
residual_plot_log <- ggplot(data, aes(x = Pairs, y = resid(full_model_log))) +
  geom_point() +
  facet_wrap(~ Size + Status, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Pairs") +
  ylab("Residuals") +
  ggtitle("Residual Plot for log")
full_model <- lm(1/(Time) ~ Pairs * Size * Status, data = data)

# Create a residual plot for sqrt
residual_plot_sqrt <- ggplot(data, aes(x = Pairs, y = resid(full_model_sqrt))) +
  geom_point() +
  facet_wrap(~ Size + Status, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Pairs") +
  ylab("Residuals") +
  ggtitle("Residual Plot for sqrt")

# Create a residual plot
residual_plot_div <- ggplot(data, aes(x = Pairs, y = resid(full_model_div))) +
  geom_point() +
  facet_wrap(~ Size + Status, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Pairs") +
  ylab("Residuals") +
  ggtitle("Residual Plot for 1/Time")

print(residual_plot_log)
print(residual_plot_sqrt)
print(residual_plot_div)