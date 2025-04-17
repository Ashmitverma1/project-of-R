install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)
set.seed(123)  # for reproducibility

# Create a simulated customer dataset
customers <- data.frame(
  customer_id = 1:100,
  age = sample(18:60, 100, replace = TRUE),
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  purchase_frequency = sample(1:10, 100, replace = TRUE),
  average_spend = round(runif(100, 20, 200), 2)  # random between $20 and $200
)

# Add total spend column
customers$total_spend <- customers$purchase_frequency * customers$average_spend

head(customers)
# Summary
summary(customers)

# Group by gender and calculate average total spend
customers %>%
  group_by(gender) %>%
  summarise(avg_total_spend = mean(total_spend))
# Histogram of total spend
ggplot(customers, aes(x = total_spend)) +
  geom_histogram(fill = "skyblue", bins = 20, color = "black") +
  labs(title = "Distribution of Total Spend", x = "Total Spend", y = "Number of Customers")

# Boxplot by gender
ggplot(customers, aes(x = gender, y = total_spend, fill = gender)) +
  geom_boxplot() +
  labs(title = "Total Spend by Gender", x = "Gender", y = "Total Spend")

# Scatter plot of age vs total spend
ggplot(customers, aes(x = age, y = total_spend)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Age vs Total Spend", x = "Age", y = "Total Spend")
# Create a new segment variable
customers$segment <- ifelse(customers$total_spend > 800, "High Spender", "Regular")

# Count in each segment
table(customers$segment)