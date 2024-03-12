# Chapter 4: Global Trade and Sanctions: Describing the TIES Data
# Data Insights in Global Politics
# William Christiansen, Ph.D.
# 3/11/24

# load packages

# load packages
install.packages("tidyverse")
library(tidyverse)

# load data
igo_dat <- read.csv("igo_year_formatv3.csv")


# inspect data
head(igo_dat)
summary(igo_dat)
colnames(igo_dat)
dim(igo_dat)


# make sure org type dummmies are factors (1=yes 0=no).
igo_dat$political <- as.factor(igo_dat$political)
igo_dat$economic <- as.factor(igo_dat$economic)
igo_dat$social <- as.factor(igo_dat$social)


# summarize parts of data we are analyzing
# igo is your data set, you are overwriting it with a new variable, duration produced by the start data and end date

# the if_else command takes three arguments
# if: if dead date is missing
# if outcome: put the current year in numeric format
# else outcome: if not missing, leave current value

igo_dat <- igo_dat %>%
  mutate(
    deaddate = ifelse(is.na(deaddate), as.numeric(format(Sys.Date(), "%Y")), deaddate),
    duration = deaddate - sdate
  )

# inspect new variable: select with $
summary(igo_dat$duration)


# visualize data
# questions for analysis:
# Has the prominence of IGOs changed over time?
# Is there a relationship between IGO type and duration?
# What organizations does the US participate in?

# time and igos

# Generating an arbitrary index for each row to use as the y-axis value
igo_dat <- igo_dat %>%
  mutate(index = row_number())

# to avoid an overcrowded plot, randomly sample 10 observations to plot
set.seed(123) # Setting seed for reproducibility
sampled_data <- igo_dat %>% sample_n(15)

# Plotting
ggplot(sampled_data, aes(x = duration, y = index)) +
  geom_text(aes(label = ioname), color = "steelblue") +  # Add labels slightly above points
  labs(x = "Duration (Years)", y = "Index", title = "Random Sample of Organizations Over Time") +
  theme_minimal() +
  theme(axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), 
        axis.title.y=element_blank())





# duration and type

igo_dat_unique <- unique(igo_dat[,c("duration","political")])


ggplot(igo_dat_unique, aes(x = duration, y = political)) +
  geom_jitter(aes(color = political)) + # Apply color to jittered points
  labs(x = "Duration (Years)", y = "Political Organization?", title = "Lifespan of Organizations by Political Dummy") +
  theme_minimal()

# Calculating mean duration and CIs
igo_summary <- igo_dat %>%
  group_by(political) %>%
  summarize(
    mean_duration = mean(duration),
    lower_ci = mean_duration - qt(0.975, df=n()-1) * sd(duration) / sqrt(n()),
    upper_ci = mean_duration + qt(0.975, df=n()-1) * sd(duration) / sqrt(n())
  )

# Plotting

ggplot(igo_summary, aes(x = political, y = mean_duration)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Political? (0=No) (1=Yes)", y = "Average Duration (Years)", title = "Average Duration by Organization Type with 95% CI") +
  theme_minimal()



# density plot
ggplot(igo_dat, aes(x = duration, fill = political)) +
  geom_density(alpha = 0.7) +
  labs(x = "Duration (Years)", title = "Density of Organization Lifespans by Type") +
  theme_minimal()


# exercise: do this for economic and social variables





# us membership: what are the longest lasting US orgs

# Filter for US membership and ensure each organization is represented once
# Then, calculate the maximum duration for each org to handle duplicates
us_member_orgs_unique <- igo_dat %>%
  filter(usa == 1) %>%
  group_by(ioname) %>%
  summarize(duration = max(duration, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(duration)) %>%
  top_n(10, duration)

# Plotting the top ten longest-lasting organizations the US is a member of
ggplot(us_member_orgs_unique, aes(x = reorder(ioname, duration), y = duration)) +
  geom_point(color = "navy") +
  geom_text(aes(label = ioname), hjust = 0.3, vjust = 1.5, check_overlap = TRUE, angle = 45) +
  labs(x = "Organization Name", y = "Duration (Years)", title = "Top Ten Longest-Lasting Organizations the US is a Member of") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.ticks.x = element_blank())



