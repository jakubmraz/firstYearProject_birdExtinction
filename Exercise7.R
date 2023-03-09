#Based on our findings from the previous tasks, we can create a reduced model by using the log transformation for the response variable and including only the significant interaction term between Pairs and Size. Therefore, the reduced model would be:

library(ggplot2)

#load the dataset
data <- read.csv("data.csv")

reduced_model <- lm(log(Time) ~ Pairs:Size, data = data)

print(summary(reduced_model)$r.squared)

# Create scatterplot with regression line
scatterplot <- ggplot(data, aes(x = Pairs, y = log(Time), color = factor(Size), shape = factor(Status))) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  xlab("Number of Pairs") +
  ylab("Log(Time)") +
  ggtitle("Scatterplot of Log(Time) vs Pairs") +
  scale_color_manual(values = c("#FF6666", "#66CCCC"), name = "Size") +
  scale_shape_manual(values = c(1, 19), name = "Status")

#print(scatterplot)

residual_plot <- ggplot(data, aes(x = Pairs, y = resid(reduced_model))) +
  geom_point() +
  facet_wrap(~ Size + Status, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Pairs") +
  ylab("Residuals") +
  ggtitle("Residual Plot for Reduced Model")

#print(residual_plot)
#This model takes into account the interaction between Pairs and Size, which was found to be significant in Task 4, and uses the log transformation for the response variable, which was found to be the best transformation in Task 3.