library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("data.csv")

# Full model
full_model <- lm(log(Time) ~ Pairs * Size * Status, data = data)

# Model without interaction between Pairs and Size
model_1 <- lm(log(Time) ~ Pairs + Size + Status, data = data)

# Model without interaction between Pairs and Status
model_2 <- lm(log(Time) ~ Pairs + Size * Status, data = data)

# Model without interaction between Size and Status
model_3 <- lm(log(Time) ~ Pairs * Size + Status, data = data)

# Model without interaction between Pairs, Size, and Status
model_4 <- lm(log(Time) ~ Pairs + Size + Status, data = data)

reduced_model <- lm(log(Time) ~ Pairs:Size, data = data)

# Compare models using ANOVA
print(anova(model_1, model_2, model_3, model_4, full_model, reduced_model))