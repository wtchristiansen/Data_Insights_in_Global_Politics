# Chapter 2: International Conflict and COW Data
# William Christiansen
# Mount St. Mary's University
# 3/7/24

# install packages
# packages make your life easier by simplifying more complex operations
# the tidyverse is a collection of packages for data science
# it employs 'pipes' %>% that easily allow you to connect parts of your code into single chunks

install.packages("tidyverse")
library(tidyverse)

# load data with read.csv, 
# make sure the dataset is in the same working directory as this script
# 1. Save the data and .R file in the same folder as a .Rproj (create new R project)
# 2. OR: save the data in a folder for this book, open it in the file viewer (bottom right),
     # and set it as the working directory by clicking the blue gear/circle drop down and
     # clicking Set as Working Directory

cow_data <- read.csv("cow_data.csv")

# Viewing the first few rows of the dataset
head(cow_data)

# Summary of the dataset to get an overview of the variables
summary(cow_data)

# Structure of the dataset to see the variable types
str(cow_data)

# Descriptive statistics for a numeric variable
cow_data %>%
  summarise(min_year = min(StartYear1, na.rm = TRUE),
            max_year = max(StartYear1, na.rm = TRUE))

# Frequency counts for a categorical variable, state name
cow_data %>%
  count(StateName) %>%
  arrange(desc(n))

plot(cow_data$StartYear1,cow_data$BatDeath)

# Exploring a relationship between time and casualties
ggplot(cow_data, aes(x = StartYear1, y = BatDeath)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Conflict Casualties Over Time",
       x = "Year",
       y = "Casualties Incurred by State in War")


# Identify the 3 highest values for casualties
top_three <- cow_data %>%
  arrange(desc(BatDeath)) %>%
  slice(1:3)

# Create a new plot
ggplot(cow_data, aes(x = StartYear1, y = BatDeath)) +
  geom_point() +
  # Add labels for the two highest values
  geom_text(data = top_three, aes(label = paste(StateName, WarName, sep = ": "), y = BatDeath + 0.05 * max(BatDeath)), vjust = 0) +
  theme_minimal() +
  labs(title = "Conflict Casualties Over Time",
       x = "Year",
       y = "Casualties Incurred by State in War")



