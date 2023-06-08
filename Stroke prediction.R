#Logistic regression model
# Load the stroke dataset
stroke_data <- read.csv("C:/Users/HP/Downloads/archive (1)/stroke_data.csv")


# Set the seed for reproducibility
set.seed(123)

# Fit logistic regression model
model <- glm(stroke ~ ., data = stroke_data, family = binomial)


# Print model summary
summary(model)

# Split the data into training and testing sets
#install.packages("caTools")
library(caTools)
split <- sample.split(stroke_data$stroke, SplitRatio = 0.7)
train_data <- subset(stroke_data, split == TRUE)
test_data <- subset(stroke_data, split == FALSE)

# Convert bmi to a factor with levels that are present in the training data
levels <- unique(train_data$bmi)
test_data$bmi <- factor(test_data$bmi, levels = levels)

# Perform logistic regression on the training data
model <- glm(stroke ~ ., data = train_data, family = binomial)

# Make predictions on the testing data
predictions <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the accuracy of the predictions
library(caret)
confusionMatrix(table(binary_predictions, test_data$stroke))
print(confusionMatrix(factor(binary_predictions), factor(test_data$stroke)))

# Generate ROC curve
#install.packages("pROC")
library(pROC)
roc_data <- roc(test_data$stroke, predictions)
plot(roc_data)



############################################################################
#Kruskal_Walis_test
#finding the datatype of bmi
class(stroke_data$bmi)
#converting the type of bmi
stroke_data$bmi <- as.numeric(stroke_data$bmi)

# Create a new variable to group BMI into three categories
stroke_data$BMI_group <- cut(stroke_data$bmi, breaks = c(0, 18.5, 25, Inf), labels = c("Underweight/Normal", "Overweight", "Obese"))

# View the new variable
head(stroke_data$BMI_group)



# Extracting avg_glucose_level values corresponding to each BMI group
underweight_normal <- stroke_data[stroke_data$BMI_group == "Underweight/Normal",]$avg_glucose_level
overweight <- stroke_data[stroke_data$BMI_group == "Overweight",]$avg_glucose_level
obese <- stroke_data[stroke_data$BMI_group == "Obese",]$avg_glucose_level


#plot boxplot to visualize the normality
help("boxplot")
boxplot(underweight_normal, main = "Boxplot of Avg Glucose Level for Underweight/Normal Individuals", xlab = "BMI Group", ylab = "Average Glucose Level")
boxplot(overweight,main = "Boxplot of Avg Glucose Level for overweight Individuals", xlab = "BMI Group", ylab = "Average Glucose Level"))
boxplot(obese , main = "Boxplot of Avg Glucose Level for obese Individuals", xlab = "BMI Group", ylab = "Average Glucose Level")   )

# Perform Shapiro-Wilk test for normality on avg_glucose_level
shapiro_test <- shapiro.test(underweight_normal)
shapiro_test <- shapiro.test(overweight)
shapiro_test <- shapiro.test(obese)

#Result & conclusion :since the p value of all the group are 2.2e-16< p value ,
#we reject the null hypothesis



#since the data doesn't follow a normal distribution, we will use kruskal-walis test
#Ho: there is no difference between the avg_glucose_level among the 3 BMI groups
#H1: there is a difference between the avg_glucose_level among the 3 BMI groups
kruskal.test(avg_glucose_level~BMI_group, data=stroke_data)

# Decision: Since p-value < 0.05, we reject H0.

# Conclusion: 
# We conclude that at least two group means are significantly different at 0.05 significance level.



# We can use pairwise.wilcox.test() to calculate pairwise comparisons between 
# group levels with corrections for multiple testing.

# Visualizing the distribution of average glucose level for each BMI group
library(ggplot2)

ggplot(stroke_data, aes(x = bmi, y = avg_glucose_level, color = BMI_group)) +
  geom_point() +
  labs(title = "Relationship between BMI and Average Glucose Level",
       x = "BMI",
       y = "Average Glucose Level",
       color = "BMI Group") +
  theme_minimal()



help(pairwise.wilcox.test)

# Run pairwise Wilcoxon rank-sum test with Benjamini-Hochberg adjustment
pairwise.wilcox.test(stroke_data$avg_glucose_level, stroke_data$BMI_group, p.adjust.method = "BH")

# Result:
#H0: There is no significant difference between obese and underweight/Normal
# The p-value is 3.3e-05  (< 0.05) . Therefore we reject the null hypothesis
# We can conclude that there is a significant difference between obese and underweight/Normal at 0.05 level.


#H0: There is no significant difference between obese and overweight
# The p-value is 3.0e-07    (< 0.05) . Therefore we reject the null hypothesis

# We can conclude that there is a significant difference between obese and overweight at 0.05 level.


#########################################################################
#Additional
#visualize the relationship between average glucose level and
#smoking status using stacked barplot



# Create a new variable to group avg_glucose_level into three categories
stroke_data$glucose_group <- cut(stroke_data$avg_glucose_level,
                                 breaks = c(0, 100, 200, Inf), labels = c("Low", "Medium", "High"))

# Count the frequency of each smoking status within each glucose level group
smoking_freq <- with(stroke_data, table(glucose_group, smoking_status))

# Convert the table into a data frame
smoking_freq_df <- as.data.frame(smoking_freq)

# Rename the columns
colnames(smoking_freq_df) <- c("Glucose Level", "Smoking Status", "Frequency")

# Load the ggplot2 package
library(ggplot2)

# Create a stacked bar plot
ggplot(smoking_freq_df, aes(x = `Glucose Level`, y = Frequency, fill = `Smoking Status`)) + 
  geom_bar(stat = "identity") +
  xlab("Glucose Level") +
  ylab("Frequency") +
  ggtitle("Frequency of Smoking Status by Glucose Level")