# Load necessary libraries
library(dplyr)  # for data manipulation

# Load the CSV file
data <- StatsProject

# View the first few rows of the data
head(data)

# Check for missing values
summary(data)

# Ensure the columns for the t-test are numeric
data$Yards.Game.Before.Transfer <- as.numeric(as.character(data$Yards.Game.Before.Transfer))
data$Yards.Game.After.Transfer <- as.numeric(as.character(data$Yards.Game.After.Transfer))

# Remove rows with missing values (if necessary)
data <- na.omit(data)

# Calculate the difference
data$Difference <- data$Yards.Game.After.Transfer - data$Yards.Game.Before.Transfer

# Descriptive statistics for better understanding
summary(data$Yards.Game.Before.Transfer)
summary(data$Yards.Game.After.Transfer)
summary(data$Difference)

# Plot histograms
hist(data$Yards.Game.Before.Transfer, main="Histogram of Yards/Game Before Transfer", xlab="Yards/Game Before Transfer")
hist(data$Yards.Game.After.Transfer, main="Histogram of Yards/Game After Transfer", xlab="Yards/Game After Transfer")
hist(data$Difference, main="Histogram of Difference (After - Before)", xlab="Difference (After - Before)")

# Plot boxplots
boxplot(data$Yards.Game.Before.Transfer, data$Yards.Game.After.Transfer, data$Difference,
        names = c("Before Transfer", "After Transfer", "Difference"),
        main = "Boxplot of Yards/Game Before, After Transfer, and Difference")

# Check normality of the differences
shapiro.test(data$Difference)
qqnorm(data$Difference)
qqline(data$Difference)

# Perform a paired t-test
t_test_result <- t.test(data$Yards.Game.Before.Transfer, data$Yards.Game.After.Transfer, paired = TRUE)

# Print the results
print(t_test_result)

