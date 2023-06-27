

# Video 2 code 3-125
# Load required Libraries ----
library(readr)
library(dplyr)
library(janitor)

# Load in data ----
data <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
employees <- select(data, -EmployeeCount, -EmployeeNumber, -Over18, -StandardHours)
employees <- clean_names(employees)

# Exploration of Data ----
glimpse(employees)

employees %>% 
  glimpse() %>% 
  select(where(is.character))

emps <- employees %>% 
  select(where(is.character))
glimpse(emps)

employees <- employees %>% 
  mutate(across(where(is.character), as.factor))

glimpse(employees)

# Counts & Proportions
table(employees$attrition)
proportions(table(employees$attrition))


# Finding Outliers
boxplot(employees[, sapply(employees, is.numeric)], outline = TRUE)

#Tidyverse Standardized Boxplot
#Load the necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Standardize the Numeric Variables
standardized_employees <- employees %>%
  mutate(across(where(is.numeric), scale))

# gather the standardized data to long format
standardized_employees_long <- standardized_employees %>%
  pivot_longer(cols = where(is.numeric), names_to = "Variable", values_to = "Value")

# Create a boxplot using the selected variables
ggplot(standardized_employees_long, aes(x = Variable, y = Value)) +
  geom_boxplot() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(y = "Value", 
       title = "Numeric Variables Boxplot", 
       x = NULL) 

# Create a boxplot using the selected variables with a horizontal orientation
ggplot(standardized_employees_long, aes(x = Value, y = Variable)) +
  geom_boxplot() + 
  theme_bw() + 
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) + 
  labs(x = "Value", 
       title = "Numeric Variables Boxplot", 
       y = NULL)


# Create an Outlier List
library(purrr)

# Calculate the outliers using the IQR method
outliers <- standardized_employees %>%
  select(where(is.numeric)) %>%
  map(~ {
    q <- quantile(.x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- IQR(.x, na.rm = TRUE)
    upper_threshold <- q[2] + 1.5 * iqr
    lower_threshold <- q[1] - 1.5 * iqr
    .x[.x > upper_threshold | .x < lower_threshold]
  })

# Identify the variables with outliers
variables_with_outliers <- outliers %>%
  keep(~ length(.x) > 0) %>%
  names()

# Create a data frame with variable names and outlier counts
outliers_table <- tibble(
  Variable = variables_with_outliers, 
  Outlier_Count = map_int(outliers[variables_with_outliers], ~ length(.x))
)

# Sort the table in descending order of Outlier_Count
outliers_table <- arrange(outliers_table, desc(Outlier_Count))

# Print the table
print(outliers_table)

# Create the final boxplot with outlier counts
library(forcats)


# Select the variables with outliers from the 'standardized_employees' dataframe
selected_variables <- standardized_employees %>%
  select(any_of(variables_with_outliers)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = fct_reorder(Variable, Value, .fun = max))  # Order variables by max value

# Join with 'outliers_table' to get the 'Outlier_Count'
selected_variables <- selected_variables %>%
  left_join(outliers_table, by = "Variable")

# Create a boxplot using the selected variables with a horizontal orientation
ggplot(selected_variables, aes(x = Value, y = Variable)) +
  geom_boxplot() +
  geom_text(aes(label = paste("n =", Outlier_Count), x = max(Value)),
            position = position_nudge(x = 0.5), size = 3, check_overlap = TRUE) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Value", y = NULL,
       title = "Scaled Variables with Outliers Boxplot")

#End of Video 2 code
#____________________________________________________________

# Video 3 130-283
# Multivariate Normality Test (1)

numeric_employees <- employees %>% select_if(is.numeric)

# Multivariate tests for normality
# All provide p-values <.05 - reject null hypothesis that data 
# is multivariate normal
library(QuantPsyc)
mult.norm(numeric_employees)$mult.test
library(energy)
mvnorm.etest(numeric_employees, R=100)
library(mvnormtest)
mvnormtest::mshapiro.test(t(numeric_employees))


# MVN provides more runtime options
library(MVN) 
result <- mvn(data = numeric_employees, subset = NULL, mvnTest = "hz",
              desc = TRUE, transform = "none", univariateTest = "SW")

print(result)
high_skew <- subset(result$Descriptives, Skew > 1 | Skew < -1, select = c(Skew))
View(high_skew)
print(high_skew)


# Convert row names to column
library(tibble)
high_skew <- rownames_to_column(high_skew, var = "Variable")
# Full join of the two data frames
merged_df <- merge(high_skew, outliers_table, by = "Variable", all = TRUE)
print(merged_df)

library(tidyverse)
# Get the variable names from the high_skew dataframe
vars_to_plot <- high_skew$Variable
# Check which variables from vars_to_plot exist in numeric_employees
matching_vars <- vars_to_plot[vars_to_plot %in% names(numeric_employees)]
numeric_employees_filtered <- numeric_employees[, matching_vars]
# Convert the data from wide format to long format
numeric_employees_long <- pivot_longer(numeric_employees_filtered, everything(), names_to = "Variable", values_to = "Value")
print(numeric_employees_long)

# Create density plots and histograms
ggplot(numeric_employees_long, aes(x = Value)) +
  geom_histogram(aes(y = after_stat(density)), fill = "skyblue", alpha = 0.5, bins = 20) +
  geom_density(fill = "steelblue", alpha = 0.3) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title="Density Plots and Histograms",
       x="Value",
       y="Density")


# Multivariate Outliers (1)
mahalanobis_values <- mahalanobis(x = numeric_employees, 
                                  center = colMeans(numeric_employees), 
                                  cov = cov(numeric_employees))
outliers <- mahalanobis_values > qchisq(1 - 0.001, df = ncol(numeric_employees))
data_no_outliers <- numeric_employees[!outliers, ]
data_outliers <- numeric_employees[outliers, ]


# Comparison of means across outliers and all employees
library(tidyverse)
# Calculate column means for data_outliers
outliers_col_means <- round(colMeans(data_outliers), 1)
# Calculate column means for numeric_employees
employees_col_means <- round(colMeans(numeric_employees), 1)
# Create a tibble to store the results
summary_df <- tibble(
  Variable = names(numeric_employees),  # Variable names from numeric_employees
  Outliers_Mean = format(outliers_col_means, scientific = FALSE),  # Column means from data_outliers without scientific notation
  Employees_Mean = format(employees_col_means, scientific = FALSE),  # Column means from numeric_employees without scientific notation
  Difference = outliers_col_means - employees_col_means  # Difference between the two means
)
# Convert Difference column to numeric
summary_df <- summary_df %>% mutate(Difference = as.numeric(Difference))
# Sort the tibble by the Difference column in descending order
summary_df <- summary_df %>% arrange(desc(Difference))

# Isolation Forests for multivariate Outliers (2)
library(solitude)
# Fit the model
iso_forest <- isolationForest$new()
iso_forest$fit(numeric_employees)
# Get the anomaly scores
anomaly_scores <- iso_forest$predict(numeric_employees)
# Identify the outliers - points with anomaly score greater than some threshold, say 0.6
outliers_anon <- numeric_employees[anomaly_scores$anomaly_score > 0.63, ]

# # Calculate column means for data_outliers using ISO Forest*****
library(tidyverse)
# Calculate column means for data_outliers
outliers_col_means <- round(colMeans(outliers_anon), 1)
# Calculate column means for numeric_employees
employees_col_means <- round(colMeans(numeric_employees), 1)
# Create a tibble to store the results
summary_df_anon <- tibble(
  Variable = names(numeric_employees),  # Variable names from numeric_employees
  Outliers_Mean = format(outliers_col_means, scientific = FALSE),  # Column means from data_outliers without scientific notation
  Employees_Mean = format(employees_col_means, scientific = FALSE),  # Column means from numeric_employees without scientific notation
  Difference = outliers_col_means - employees_col_means  # Difference between the two means
)
# Convert Difference column to numeric
summary_df_anon <- summary_df_anon %>% mutate(Difference = as.numeric(Difference))
# Sort the tibble by the Difference column in descending order
summary_df_anon <- summary_df_anon %>% arrange(desc(Difference))

# Add "employee_number" back in as a variable
data_1 <- clean_names(data)
employees_id <- dplyr::select(data_1, -employee_count, -over18, -standard_hours)
numeric_employees_id <- employees_id %>% select_if(is.numeric)

# Isolation Forests for multivariate Outliers with "employee_number"
library(solitude)
# Fit the model
iso_forest <- isolationForest$new()
iso_forest$fit(numeric_employees_id)
# Get the anomaly scores
anomaly_scores <- iso_forest$predict(numeric_employees_id)
# Identify the outliers - points with anomaly score greater than some threshold, say 0.6
outliers_anon <- numeric_employees_id[anomaly_scores$anomaly_score > 0.63, ]

# Merge the "data_outliers" and "employees" dataframes based on the common identifier column
data_outliers_updated <- merge(outliers_anon, data_1[c("employee_number", "attrition", "job_role")], by = "employee_number")

table(data_outliers_updated$attrition)
table(data_outliers_updated$job_role)
outlier_attrition <- table(data_outliers_updated$attrition)

# Insights into outlier attrition table
print_insights <- function() {
  # Calculate percentages
  percentage_no_attrition <- round(((outlier_attrition[[1]] / sum(outlier_attrition[[1]], outlier_attrition[[2]])) * 100),1)
  percentage_attrition <- round((outlier_attrition[[2]] / sum(outlier_attrition[[1]], outlier_attrition[[2]])) * 100,1)
  
  # Calculate odds
  odds_attrition <- outlier_attrition[[2]] / outlier_attrition[[1]]
  
  # Print the calculated results
  cat("Percentage of attrition (No):", paste0(percentage_no_attrition, "%\n"))
  cat("Percentage of attrition (Yes):", paste0(percentage_attrition, "%\n"))
  cat("Odds of attrition:", odds_attrition, "\n")
  
  # Provide insights
  cat("Insights:\n")
  cat("The percentage of attrition (No) is", paste0(percentage_no_attrition, "%, indicating that approximately ", paste(percentage_no_attrition, "of the observed cases resulted in no attrition.\n")))
  cat("The percentage of attrition (Yes) is", paste0(percentage_attrition, "%, indicating that approximately ", paste(percentage_attrition, "of the observed cases resulted in attrition.\n")))
  cat("The odds of attrition are", odds_attrition, ", suggesting that the odds of experiencing attrition are relatively lower compared to the odds of not experiencing attrition.\n")
  cat("The odds of attrition are", odds_attrition, ", suggesting that the odds of experiencing attrition are approximately", paste0(odds_attrition * 100, "%.\n"))
}

print_insights()

#________________________________________________
