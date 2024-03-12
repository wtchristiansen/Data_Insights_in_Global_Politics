# Chapter 3 - Sanctions and Trade
# Data Insights in World Politics
# William Christiansen, Ph.D.
# 3/9/24

# load packages
install.packages("readxl")
install.packages("tidyverse")
library(readxl)
library(tidyverse) # should already be installed, if not, add an install.packages() command with "tidyverse" in the parentheses

# load data
# https://sanctions.web.unc.edu/wp-content/uploads/sites/18834/2021/04/tiesusersmanualv4.pdf
# https://correlatesofwar.org/data-sets/bilateral-trade/

ties_data <- read_excel("ties.xls")
export_data <- read.csv("National_COW_4.0.csv")

# rename ties columns to match export columns for merging

ties_data$year <- ties_data$startyear
ties_data$ccode <- ties_data$targetstate

# merge

merged_data <- merge(export_data, ties_data, by = c("ccode", "year"))

# inspect column names

colnames(merged_data)

head(merged_data)

# opens up viewer window
View(merged_data)

# start exploring and thinking of variables to compare
# for our example, we will depart with the question:
# are states with higher exports more likely to receive a warning or threat than states with lower exports?
# Hypothesis: In a comparison of states, those receiving threats or warnings will have higher levels of exports than those that do not.
# Null Hypothesis: States receiving threats or warnings will have equivalent OR lower levels of exports than those that do not

# Summary statistics for exports, by threat status
merged_data %>%
  group_by(threat) %>%
  summarise(mean_export = mean(exports, na.rm = TRUE),
            sd_export = sd(exports, na.rm = TRUE),
            n = n())

# Boxplot to visualize export levels by threat status
ggplot(merged_data, aes(x = as.factor(threat), y = exports)) +
  geom_boxplot() +
  labs(x = "Threat (0 = No, 1 = Yes)", y = "Exports", title = "Export Levels by Threat Status") +
  theme_minimal()

# notice the boxplot values are driven down by a bunch of very small values
# we need to transform exports

# Creating export categories based on quantiles
merged_data$export_cat <- cut(merged_data$export, 
                              breaks = quantile(merged_data$export, probs = c(0, .33, .67, 1), na.rm = TRUE),
                              labels = c("Low", "Medium", "High"),
                              include.lowest = TRUE)


# Barplot with categorized exports
ggplot(merged_data, aes(x = as.factor(threat), fill = export_cat)) +
  geom_bar(position = "dodge") +
  labs(x = "Threat (0 = No, 1 = Yes)", y = "Count", fill = "Export Level", title = "Export Levels by Threat Status") +
  theme_minimal()

# Boxplot with categorized exports
ggplot(merged_data, aes(x = as.factor(threat), y = exports, fill = export_cat)) +
  geom_boxplot(position = "dodge") +
  labs(x = "Threat (0 = No, 1 = Yes)", y = "Count", fill = "Export Level", title = "Export Levels by Threat Status") +
  theme_minimal()


# Boxplot with log scale for y-axis through ggplot
ggplot(merged_data, aes(x = as.factor(threat), y = exports)) +
  geom_boxplot() +
  scale_y_log10() + # This transforms the y-axis to a log scale
  labs(x = "Threat (0 = No, 1 = Yes)", y = "Exports (Log Scale)", title = "Export Levels by Threat Status (Log Scale)") +
  theme_minimal()


# Activity
# so what about imports?
# recreate the analysis above but with the variable 'imports' instead




# Appendix:
### Advanced Activity: Logistic regression

# Logistic regression
model <- glm(threat ~ log(exports) + log(imports), data = merged_data, family = binomial())

# View model summary
summary(model)

# Remove rows where 'exports' is NA or infinite
cleaned_data <- merged_data %>%
  filter(!is.na(exports) & is.finite(exports))

# Create a range of values for imports
import_values <- seq(from = min(cleaned_data$imports, na.rm = TRUE), 
                     to = max(cleaned_data$imports, na.rm = TRUE), 
                     length.out = 10) # Adjust length.out as needed

# Expand new_data to include all combinations of exports and the range of imports
new_data <- expand.grid(exports = cleaned_data$exports, imports = import_values)


# Predict probabilities
new_data$predicted_prob <- predict(model, newdata = new_data, type = "response")

# Plot
ggplot(new_data, aes(x = exports, y = predicted_prob)) +
  geom_line() +
  scale_x_log10() +  # Apply log scale to x-axis if exports were log-transformed
  labs(x = "Exports (Log Scale)", y = "Predicted Probability of Receiving a Threat",
       title = "Probability of Receiving a Threat by Export Levels") +
  theme_minimal()


# Add confidence intervals
new_data$conf_high <- predict(model, newdata = new_data, type = "response", se.fit = TRUE)$fit + 1.96 * predict(model, newdata = new_data, type = "response", se.fit = TRUE)$se.fit
new_data$conf_low <- predict(model, newdata = new_data, type = "response", se.fit = TRUE)$fit - 1.96 * predict(model, newdata = new_data, type = "response", se.fit = TRUE)$se.fit

# Plot with confidence intervals
ggplot(new_data, aes(x = exports)) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), fill = "lightblue") +
  scale_x_log10() +
  labs(x = "Exports (Log Scale)", y = "Predicted Probability of Receiving a Threat",
       title = "Probability of Receiving a Threat by Export Levels with Confidence Intervals") +
  theme_minimal()

# Use broom package to tidy the model and get coefficients
if(!requireNamespace("broom", quietly = TRUE)) install.packages("broom")
library(broom)

tidied_model <- tidy(model)

# Plot
ggplot(tidied_model, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error * 1.96, xmax = estimate + std.error * 1.96)) +
  labs(x = "Coefficient Estimate", y = "Variable",
       title = "Coefficient Estimates from Logistic Regression") +
  theme_minimal()

# Define scenarios for imports
scenarios <- c("low" = min(cleaned_data$imports, na.rm = TRUE), 
               "medium" = mean(cleaned_data$imports, na.rm = TRUE), 
               "high" = max(cleaned_data$imports, na.rm = TRUE))

# Repeat prediction for each scenario
results <- list()
for (name in names(scenarios)) {
  new_data$imports <- scenarios[[name]]
  predicted_probs <- predict(model, newdata = new_data, type = "response")
  results[[name]] <- data.frame(exports = new_data$exports, predicted_prob = predicted_probs, scenario = name)
}

# Combine results from all scenarios
all_results <- do.call(rbind, results)

ggplot(all_results, aes(x = exports, y = predicted_prob, color = scenario)) +
  geom_line() +
  labs(x = "Exports", y = "Predicted Probability of Receiving a Threat",
       title = "Effect of Exports on Threat Probability Across Import Scenarios") +
  theme_minimal()


# final graph

# Assuming you've already created new_data with different scenarios of imports as shown previously

results <- list()
for (name in names(scenarios)) {
  new_data$imports <- scenarios[[name]]
  
  # Predict probabilities with standard errors
  preds <- predict(model, newdata = new_data, type = "response", se.fit = TRUE)
  
  # Calculate 95% confidence intervals
  ci_lower <- preds$fit - 1.96 * preds$se.fit
  ci_upper <- preds$fit + 1.96 * preds$se.fit
  
  # Store results including confidence intervals
  results[[name]] <- data.frame(exports = new_data$exports, 
                                predicted_prob = preds$fit, 
                                ci_lower = ci_lower, 
                                ci_upper = ci_upper, 
                                scenario = name)
}

# Combine results from all scenarios
all_results <- do.call(rbind, results)

ggplot(all_results, aes(x = exports)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = scenario), alpha = 0.2) +
  geom_line(aes(y = predicted_prob, color = scenario)) +
  labs(x = "Exports", y = "Predicted Probability of Receiving a Threat",
       title = "Effect of Exports on Threat Probability Across Import Scenarios") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +  # Adjust color palette as desired
  scale_color_brewer(palette = "Dark2")


