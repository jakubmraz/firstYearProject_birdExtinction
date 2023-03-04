library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("data.csv")

# Create the scatterplot
my_plot <- ggplot(data, aes(x=Pairs, y=Time)) +
  geom_point() +
  facet_wrap(~ Size + Status, nrow=2) +
  geom_smooth(method="lm", se=FALSE)

# Add axis labels and a title
my_plot + xlab("Number of Pairs") + ylab("Years Until Extinction") +
  ggtitle("Relationship between Pairs and Time by Size and Status")

print(my_plot)

# Calculate R-squared for each regression line
r_squared <- data %>% 
  group_by(Size, Status) %>% 
  do(model = lm(Time ~ Pairs, data = .)) %>% 
  summarise(
    intercept = coef(model)[1],
    slope = coef(model)[2],
    adj_r_squared = summary(model)$adj.r.squared,
    combination = paste(Size, Status, sep = "_")
  )

# Print R-squared values
print(r_squared)