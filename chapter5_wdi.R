# Chapter 5: International Development: Revealing Correlations in Foreign Aid and WDI Data
# Data Insights in Global Politics
# William Christiansen, Ph.D.
# 3/11/24

# Install packages
if(!require(WDI)) install.packages("WDI")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidyverse)) install.packages("broom")
library(broom)
library(WDI)
library(tidyverse)

# we will use water access but here are some other indicators you can use for your own purposes

# Fetch WDI water access data
water_access <- WDI(indicator = 'SH.H2O.BASW.Q2.ZS', country = "all", start = 1960, end = 2020)

# Fetch Foreign Aid data
aid_data <- WDI(indicator = 'DT.ODA.ALLD.GD.ZS', country = "all", start = 1960, end = 2020)

# Merge datasets by country and year

all_data <- inner_join(water_access, aid_data, by = c("year", "country"))

# select variables
# Selecting only the necessary variables
all_data <- all_data %>%
  select('country', 'year', 'DT.ODA.ALLD.GD.ZS', 'SH.H2O.BASW.Q2.ZS')


# rename development assistance to ODA for simplicity

all_data$ODA <- all_data$DT.ODA.ALLD.GD.ZS
all_data$water_access <- all_data$SH.H2O.BASW.Q2.ZS

# select x and y for final data frame
all_data <- all_data %>%
  select(country, year, ODA, water_access)


# inspect
head(all_data)

# cross tabs

# Example of creating a summarized table (adjust as needed)
summary_table <- all_data %>%
  group_by(country) %>%
  summarize(mean_water_access = mean(water_access, na.rm = TRUE),
            total_aid = sum(ODA, na.rm = TRUE))

head(summary_table)

# lag ODA 10 years to give aid time to take affect

all_data$lagged_ODA <- lag(all_data$ODA, 10)


# Fit a linear model with the log of the lagged ODA
model <- lm(water_access ~ log(lagged_ODA), data = all_data)


# Summary of the model
model_summary <- summary(model)
model_summary

# Exportable table using stargazer
if(!require(stargazer)) install.packages("stargazer")
library(stargazer)
stargazer(model, type = "latex")

# extract beta (slope of fitted line through data)
beta_value <- tidy(model)$estimate[2] # Getting the beta coefficient for the aid variable

# Scatter plot with regression line, CI, and beta label
ggplot(all_data, aes(x = water_access, y = log(lagged_ODA))) +
  geom_point(color = "lightblue") +
  geom_smooth(method = "lm", se = TRUE) +
  geom_label(x = Inf, y = Inf, label = paste("Beta =", round(beta_value, 3)), hjust = 1.1, vjust = 1.1) +
  labs(x = "Clean Water Access", y = "log(ODA) w/10 year lag", title = "Foreign Aid and Clean Water Access - Linear Model") +
  theme_minimal()



