#### Additional Exercises
# Loading/Visualizing Historical Data
# Data Insights in Global Politics
# William Christiansen, Ph.D.
# 3/17/24


# transatlantic tsd trade data

tsd_dat <- read.csv("tsd_data.csv")
colnames(tsd_dat)
tsd_dat$Voyage.itinerary.imputed.principal.port.of.tsd.disembarkation..mjslptimp..place

str(tsd_dat)


# subset for charleston
charleston_dat <- subset(tsd_dat, tsd_dat$Voyage.itinerary.imputed.principal.port.of.tsd.disembarkation..mjslptimp..place == "Charleston")

charleston_dat <- na.omit(charleston_dat)

# total captives arrived at Charleston (missingness removed, this is a bottom end estimate)
sum(charleston_dat$Captives.arrived.at.1st.port)
mean(charleston_dat$Captives.arrived.at.1st.port)

# what's the amount if we impute the mean?
charleston_dat_impute <- subset(tsd_dat, tsd_dat$Voyage.itinerary.imputed.principal.port.of.tsd.disembarkation..mjslptimp..place == "Charleston")

# Assuming charleston_dat_impute is your dataframe and 
# Captives.arrived.at.1st.port is the column with missing values you wish to impute

# Replace NA values with 185.25 in the specific column
charleston_dat_impute$Captives.arrived.at.1st.port[is.na(charleston_dat_impute$Captives.arrived.at.1st.port)] <- 185.25


# notice that the mean barely moves because we replaced the values with the mean, the sum estimate gets much higher and is likely more accurate than just deleting missing values altogether
sum(charleston_dat_impute$Captives.arrived.at.1st.port)
mean(charleston_dat_impute$Captives.arrived.at.1st.port)

# Generate plots to visualize
ggplot(data = charleston_dat, aes(x = Year.of.arrival.at.port.of.disembarkation, y = Captives.arrived.at.1st.port)) +
  geom_point(color = "darkblue") + # Points in dark blue to evoke the Atlantic
  theme_minimal() + # Minimalist theme for clarity
  theme(plot.background = element_rect(fill = "lightblue", color = "lightblue"), # Light blue background like the ocean
        panel.background = element_rect(fill = "lightblue", color = "lightblue"),
        plot.title = element_text(color = "darkblue", size = 14, face = "bold"), # Dark blue, bold title
        axis.title = element_text(color = "darkblue"), # Dark blue axis titles
        axis.text = element_text(color = "navy"), # Navy color for axis text for better readability
        panel.grid.major = element_line(color = "white"), # White major grid lines
        panel.grid.minor = element_line(color = "white", linetype = "dotted")) + # White, dotted minor grid lines for a subtle nod to nautical charts
  labs(x = "Year of Arrival at Port of Disembarkation", 
       y = "Captives Arrived at 1st Port",
       title = "Charleston Slave Trade over Time")



ggplot(data = charleston_dat, aes(x = Year.of.arrival.at.port.of.disembarkation, y = Captives.arrived.at.1st.port)) +
  geom_line(color = "darkblue") + # Points in dark blue to evoke the Atlantic
  theme_minimal() + # Minimalist theme for clarity
  theme(plot.background = element_rect(fill = "lightblue", color = "lightblue"), # Light blue background like the ocean
        panel.background = element_rect(fill = "lightblue", color = "lightblue"),
        plot.title = element_text(color = "darkblue", size = 14, face = "bold"), # Dark blue, bold title
        axis.title = element_text(color = "darkblue"), # Dark blue axis titles
        axis.text = element_text(color = "navy"), # Navy color for axis text for better readability
        panel.grid.major = element_line(color = "white"), # White major grid lines
        panel.grid.minor = element_line(color = "white", linetype = "dotted")) + # White, dotted minor grid lines for a subtle nod to nautical charts
  labs(x = "Year of Arrival at Port of Disembarkation", 
       y = "Captives Arrived at 1st Port",
       title = "Charleston Slave Trade over Time")


ggplot(data = charleston_dat, aes(x = Captives.arrived.at.1st.port)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(x = "Captives Arrived", y = "Frequency", title = "Distribution of Captives Arrived - Charleston") +
  theme_bw()

ggplot(data = charleston_dat, aes(x = Captives.arrived.at.1st.port)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Captives Arrived", y = "Density", title = "Density of Captives Arrived - Charleston") +
  theme_classic()

ggplot(data = charleston_dat, aes(x = as.factor(Year.of.arrival.at.port.of.disembarkation), y = Captives.arrived.at.1st.port)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Year of Arrival", y = "Captives Arrived", title = "Distribution of Captives Arrived by Year (Charleston Disembarkation)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 75, hjust = 0.5))



               