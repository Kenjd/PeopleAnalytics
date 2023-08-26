

#This is all the code used in videos 1-5 of the Employees Decision Tree series of videos.

# Video 2 code 3-125 ----
# Load required Libraries ----
library(readr)
library(dplyr)
library(janitor)

# Load in data ----
data <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
employees <- dplyr::select(data, -EmployeeCount, -EmployeeNumber, -Over18, -StandardHours)
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

# Video 3 ----
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


# Multivariate Outliers
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
summary_df 

# Isolation Forests for multivariate Outliers
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

# End of video 3
#______________________________________________________

# Start of Video 4 ----

# First: Find features that are highly correlated

library(Hmisc)
library(dplyr)

employees_lab <- employees
# Convert attrition to numeric: "Yes" to 1 and "No" to 0
employees_lab$attrition <- ifelse(employees_lab$attrition == "Yes", 1, 0)

# Filter employees with job_role "Laboratory Technician"
employees_lab <- employees_lab %>% filter(job_role == "Laboratory Technician" &
                                            attrition == 0)
#employees_lab <- employees_lab %>% filter(job_role == "Laboratory Technician")


# Select only numeric variables from employees_lab
employees_lab <- employees_lab %>% select_if(is.numeric)

# Compute the correlation
correlation_result <- rcorr(as.matrix(employees_lab), type = "spearman")

# Access the correlation matrix and p-value matrix
correlation_matrix <- correlation_result$r
p_value_matrix <- correlation_result$P

# Create a matrix of the same dimensions, but indicating if correlation is significant
significant_correlation <- p_value_matrix < 0.05

# Create a matrix with the same dimensions, but indicating if the correlation value is above 0.7
strong_correlation <- abs(correlation_matrix) > 0.70

# Filter the correlation matrix
filtered_correlation <- correlation_matrix * significant_correlation * strong_correlation

# Visualize the Correlation matrix
library(igraph)
graph <- graph_from_adjacency_matrix(filtered_correlation, mode = "undirected", 
                                     weighted = TRUE, diag = FALSE)

plot(graph, vertex.size=12, vertex.label.cex=0.80)


# Extract edges and their weights
edge_list <- as.data.frame(get.edgelist(graph))
weights <- E(graph)$weight
weights[is.nan(weights)] <- 0

# Filter out edges with weights equal to 0
non_zero_indices <- which(weights != 0)

# Subset the edge list and weights based on non-zero indices
edge_list <- edge_list[non_zero_indices, ]
weights <- weights[non_zero_indices]

#Frame for only attrition =Yes for Lab Tech position
df_all <- data.frame(
  from = edge_list$V1,
  to = edge_list$V2,
  all_weight = weights
)

print(df_all)

# Merge df_all with df_no
merged_df <- merge(df_all, df_no, by = c("from", "to"), all = TRUE)
# Merge the result with df_yes
merged_df <- merge(merged_df, df_yes, by = c("from", "to"), all = TRUE)
# Replace NA values with 0
merged_df[is.na(merged_df)] <- 0
# Print the combined dataframe
print(merged_df)

# Not Used in video
# Singular insights
# Extract edges and their weights
edges <- as.data.frame(get.edgelist(graph))
weights <- E(graph)$weight
weights[is.nan(weights)] <- 0


# Convert to meaningful text
for (i in 1:nrow(edges)) {
  
  # Skip correlations equal to 0
  if (weights[i] == 0) {
    next
  }
  
  # Determine the direction of the correlation
  if (weights[i] > 0) {
    direction <- "positive"
  } else {
    direction <- "negative"
  }
  
  # Print the correlation information
  cat(paste("There is a", direction, "correlation of", round(abs(weights[i]),2), 
            "between", edges[i, 1], "and", edges[i, 2], ".\n"))
}



# Odds Ratio
employees_odds <- employees

# Subset the data for only Lab Technicians and Attrition status
labtech_data <- subset(employees_odds, job_role == "Laboratory Technician", select = "attrition")
employees$is_labtech <- employees_odds$job_role == "Laboratory Technician"

# Create a table
contingency_table <- table(employees_odds$attrition, employees$is_labtech)

# Using Fisher's Exact Test:
test_result <- fisher.test(contingency_table)
odds_ratio <- test_result$estimate
print(odds_ratio)

# Odds Ratio -
# Using Logistic Regression:
# Convert Attrition to a binary numeric variable (0 for No, 1 for Yes)
employees_odds$attrition_binary <- as.numeric(employees_odds$attrition == "Yes")

# Perform logistic regression
model <- glm(attrition_binary ~ job_role, data = employees_odds, family = "binomial")

# Extract odds ratio for Lab Technician
odds_ratio_logit <- exp(coef(model)["job_roleLaboratory Technician"])
print(odds_ratio_logit)

levels(employees_odds$job_role)

# Control what is the reference group
# Set "Sales Executive" as the reference group
employees_odds$job_role <- relevel(employees_odds$job_role, ref = "Healthcare Representative")

# Perform logistic regression
model <- glm(attrition_binary ~ job_role, data = employees_odds, family = "binomial")

# Extract odds ratio for Lab Technician compared to Sales Executive
odds_ratio_logit <- exp(coef(model)["job_roleLaboratory Technician"])
print(odds_ratio_logit)


# To create an odds ration on more than 1 dimension
# 1. Create a new binary variable for the specific group:
employees_odds$labtech_and_manager <- as.numeric(employees_odds$job_role == "Laboratory Technician" & employees$years_with_curr_manager > 5)
# 2. Perform logistic regression using this new variable:
model <- glm(attrition_binary ~ labtech_and_manager, data = employees_odds, family = "binomial")
# 3. Extract the odds ratio:
odds_ratio_logit <- exp(coef(model)["labtech_and_manager"])
print(odds_ratio_logit)

# To determine the "tipping point" where a Laboratory Technician is more likely
# to leave based on the values of "years_at_company" and
# "years_in_current_role", you can use logistic regression. In this context,
# logistic regression will model the probability of attrition (leaving) as a
# function of "years_at_company" and "years_in_current_role".

# 1. Subset the Data for Laboratory Technicians with Attrition = Yes:
labtech_data <- subset(employees_odds, job_role == "Laboratory Technician" & attrition == "Yes")
# 2. Perform Logistic Regression:
model <- glm(attrition_binary ~ years_at_company + years_in_current_role, data = labtech_data, family = "binomial")
# 3. Predict the Probability of Leaving:
new_data <- data.frame(years_at_company = c(15), years_in_current_role = c(15))
predicted_prob <- predict(model, newdata = new_data, type = "response")
# 4. Compute the Odds:
odds <- predicted_prob / (1 - predicted_prob)
# 5. Compute the Odds Ratio:
# Compute average odds for all Laboratory Technicians
average_prob <- mean(predict(model, type = "response"))
average_odds <- average_prob / (1 - average_prob)

odds_ratio <- odds / average_odds
print(odds_ratio)

# explore the combination of "years_at_company" and "years_in_current_role" to
# identify which combinations are associated with higher or lower odds of
# attrition

# 1. Fit the Logistic Regression Model:
labtech_data <- subset(employees_odds, job_role == "Laboratory Technician")
model <- glm(attrition_binary ~ years_at_company + years_in_current_role, data = labtech_data, family = "binomial")
# 2. Predict Odds Across a Range of Values:
# Create a grid of values
grid <- expand.grid(years_at_company = seq(0, max(labtech_data$years_at_company), by=1),
                    years_in_current_role = seq(0, max(labtech_data$years_in_current_role), by=1))

# Predict probabilities
grid$predicted_prob <- predict(model, newdata = grid, type = "response")
# Compute odds
grid$odds <- grid$predicted_prob / (1 - grid$predicted_prob)

# 3. Compare to Average Odds:
# Compute average odds
average_prob <- mean(predict(model, type = "response"))
average_odds <- average_prob / (1 - average_prob)
# Compute odds ratios for each combination
grid$odds_ratio <- grid$odds / average_odds

# 4. Identify Distinguishing Combinations:
# For example, combinations with odds ratio > 1.5 or < 0.5
# This will give you combinations of "years_at_company" and
# "years_in_current_role" that are associated with either 50% higher odds or 50%
# lower odds of attrition compared to the average Laboratory Technician.
distinguishing_combinations <- subset(grid, odds_ratio > 1.5 | odds_ratio < 0.5)

# 5. extract rows from the grid data frame where the odds_ratio > 1:
# 5.1 Filter the Data:
filtered_combinations <- subset(grid, 
                                odds_ratio > 1 & 
                                  years_at_company >= years_in_current_role & 
                                  years_at_company > 0 &
                                  years_in_current_role > 0)
# 5.2 Sort the Results:
sorted_combinations <- filtered_combinations[order(-filtered_combinations$odds_ratio), ]
# 5.3. View the Results:
print(sorted_combinations)


employees_odds <- employees
# a model that includes only the predictors that were found to be most
# statistically significant in predicting attrition for Laboratory Technicians.
# Subset the data
labtech_data <- subset(employees_odds, job_role == "Laboratory Technician") # & attrition == "Yes")
# Convert Attrition to a binary numeric variable (0 for No, 1 for Yes)
labtech_data$attrition_binary <- as.numeric(labtech_data$attrition == "Yes")
# Remove columns with only one unique value
labtech_data <- labtech_data[, sapply(labtech_data, function(col) length(unique(col)) > 1)]
# Fit a full model with all predictors
full_model <- glm(attrition_binary ~ ., data = labtech_data, family = "binomial", control = list(maxit = 50))
# Use stepwise regression for variable selection
selected_model <- step(full_model, direction = "both")
summary(selected_model)

# End of video 4
#____________________________________________________________

# Video 5 ----

# Subset the data for only lab techs
library(dplyr)
lab_tech_data <- employees %>% 
  filter(job_role == "Laboratory Technician")

# Create an object of only factor variables extracted from the lab_tech_data
lab_tech_data_chr <- lab_tech_data %>% 
  dplyr::select(which(sapply(., is.character)), -department, -job_role)

table(lab_tech_data_chr$attrition)

# individually compute the chi-squared table
library(vcd)
chi1 <- table(lab_tech_data_chr$attrition, lab_tech_data_chr$gender)
assoc <- assocstats(chi1)
print(assoc)

# Create a mosaic plot for attrition and gender variables
chi2 <- xtabs(~attrition + gender, data = lab_tech_data_chr)
mosaic(chi2, gp= shading_Friendly(),
       split_vertical = TRUE,
       main = "Attrition by Gender")

# Automate the chi-squared table creation
# Load in the required library
library(lsr)

# Initialize matrix to store results
associations_matrix <- matrix(0, ncol(lab_tech_data_chr), ncol(lab_tech_data_chr))
colnames(associations_matrix) <- names(lab_tech_data_chr)
rownames(associations_matrix) <- names(lab_tech_data_chr)

# Compute associations
for (i in 1:ncol(lab_tech_data_chr)) {
  for (j in 1:ncol(lab_tech_data_chr)) {
    associations_matrix[i, j] <- cramersV(lab_tech_data_chr[[i]], lab_tech_data_chr[[j]])
  }
}

# View the results
print(associations_matrix)
hc <- hclust(as.dist(1 - associations_matrix))
plot(hc, main = "Hierarchical Clustering of Cramer's V Associations",
     xlab = "", sub = "")

# Feature Selection
library(caret)
# Select only numeric variables for scaling
numeric_vars <- lab_tech_data %>% 
  select_if(is.numeric) %>% 
  names()
# Scale the numeric variables
scaled_lab_tech_data <- lab_tech_data %>% 
  dplyr::select(all_of(numeric_vars)) %>% 
  scale() %>% 
  as.data.frame()
# Combine scaled numeric variables with other non-numeric variables
lab_tech_data_scaled <- bind_cols(lab_tech_data %>% dplyr::select(-all_of(numeric_vars)),
                                  scaled_lab_tech_data)
lab_tech_data_scaled <- dplyr::select(lab_tech_data_scaled, -"department", -"job_role")

# feature selection of lab-tech_data_scaled
library(caret)
# Split the data into predictors (X) and targets (y)
X <- lab_tech_data_scaled %>% dplyr::select(-attrition)
y <- factor(lab_tech_data_scaled$attrition)
# Perform feature selection using random forests with caret
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
result1 <- rfe(X, y, sizes = c(1:length(X)), rfeControl = ctrl)
# Print the results
print(result1)

# test with the five variables suggested in the result
X_important <- lab_tech_data_scaled %>% 
  dplyr::select(over_time, work_life_balance, age, years_at_company, years_with_curr_manager)
#Perform selection again with random forest using caret
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
result2 <- rfe(X_important, y, sizes = c(1:length(X_important)), rfeControl = ctrl)
#print the result
print(result2)


if ("optVariables" %in% names(result1)) {
  optimal_variables <- result1$optVariables
  print(optimal_variables)
} else {
  print("optVariables attribute not found")
}


# Decision tree for lab_tech_data
library(rpart)
tree_model <- rpart(attrition ~ over_time + work_life_balance + age + years_at_company + years_with_curr_manager,
                    data = lab_tech_data, method = "class")
# Visualize the tree
library(rpart.plot)
rpart.plot(tree_model, nn = TRUE, extra = 104)

#______________________________________________________
# End of video 5