library(ggplot2)
library(dplyr)
library(tidyr)
library(performance)
library(see)

data <- read.csv("data.csv")

# Full model
full_model <- lm(log(Time) ~ Pairs * Size * Status, data = data)
full_model2 <- lm(1/(Time) ~ log(Pairs) * Size * Status, data = data)


full_model_plus <- lm(1/(Time) ~ log(Pairs) + Size + Status, data = data)

# Model without interaction between Pairs and Size
model_1 <- lm(log(Time) ~ Pairs + Size + Status, data = data)

# Model without interaction between Pairs and Status
model_2 <- lm(log(Time) ~ Pairs + Size * Status, data = data)

# Model without interaction between Size and Status
model_3 <- lm(log(Time) ~ Pairs * Size + Status, data = data)

# Model without interaction between Pairs, Size, and Status
model_4 <- lm(log(Time) ~ Pairs + Size + Status, data = data)

# Model with weak interaction between Pairs and Size
model_5 <- lm(log(Time) ~ Pairs:Size + Status, data = data)

#Model with weak interaction between Size and Status
model_6 <- lm(log(Time) ~ Pairs + Size:Status, data = data)

#Model with interaction between Pairs and Size and weak interaction with Status
model_7 <- lm(log(Time) ~ Pairs * Size:Status, data = data)

#Model with weak interaction between Pairs and Size and interaction with Status
model_8 <- lm(log(Time) ~ Pairs : Size * Status, data = data)

model_9 <- lm(log(Time) ~ Pairs + Size, data = data)

model_10 <- lm(log(Time) ~ Pairs * Size, data = data)

reduced_model <- lm(log(Time) ~ Pairs:Size, data = data)

# Compare models using ANOVA
# print(anova(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10, full_model, reduced_model))

print(summary(model_1))

# print(check_model(full_model))