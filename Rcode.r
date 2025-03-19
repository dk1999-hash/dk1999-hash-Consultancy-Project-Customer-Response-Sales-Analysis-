# Question 1: Scores on update and age
install.packages("readxl")
library(readxl)
getwd()
setwd("~/Desktop")
file.exists("MAST7200_assesment2.xlsx")
dataset <- read_excel("MAST7200_assesment2.xlsx")
# Remove columns with all NA values
library(dplyr)
dataset <- dataset[, colSums(is.na(dataset)) < nrow(dataset)]

# Verify the data
head(dataset)
str(dataset)

# Mann-Whitney U test for Update (Q1 should be numeric here)
wilcox.test(Q1 ~ Update, data = dataset)
# p-value = 5.109e-05 (0.00005109): This is a tiny number,
# much smaller than 0.05. In plain terms, it’s the chance that the difference in Q1 scores between before and after the update is just a random accident. 
# Since it’s less than 0.05, it’s like saying 
# there’s less than a 0.005% chance it’s random—so the difference is likely real.

# Kruskal-Wallis test for Age
kruskal.test(Q1 ~ Age, data = dataset)
# p-value is greater than 0.05 so there is no effect on the recommendation score by age

# Calculate NPS (Q1 must be numeric for cut)
dataset$NPS_Category <- cut(dataset$Q1, breaks = c(-1, 6, 8, 10),
                            labels = c("Detractors", "Passives", "Promoters"),
                            ordered = TRUE)

# NPS by Update
nps_update <- dataset %>%
  group_by(Update) %>%
  summarise(Promoters = mean(NPS_Category == "Promoters") * 100,
            Detractors = mean(NPS_Category == "Detractors") * 100) %>%
  mutate(NPS = Promoters - Detractors)

print(nps_update)

# NPS by Age
nps_age <- dataset %>%
  group_by(Age) %>%
  summarise(Promoters = mean(NPS_Category == "Promoters") * 100,
            Detractors = mean(NPS_Category == "Detractors") * 100) %>%
  mutate(NPS = Promoters - Detractors)

print(nps_age)

# Bar chart for NPS by Update
barplot(nps_update$NPS, names.arg = nps_update$Update,
        main = "Net Promoter Score by Update Status",
        ylab = "NPS (%)",
        col = "lightblue")

# Bar chart for NPS by Age
barplot(nps_age$NPS, names.arg = nps_age$Age,
        main = "Net Promoter Score by Age Group",
        ylab = "NPS (%)",
        col = "lightgreen")

# Install and load likert package (now after NPS)
install.packages("likert")
library(likert)

# Convert Q1 to an ordered factor for likert
dataset$Q1 <- factor(dataset$Q1, levels = 0:10, ordered = TRUE)
dataset$Update <- factor(dataset$Update, levels = c(0, 1), labels = c("Before Update", "After Update"))
dataset$Age <- factor(dataset$Age, levels = c(1, 2, 3), labels = c("18-29", "30-59", "60+"))

# Create likert objects
likert_update <- likert(items = data.frame(Q1 = dataset$Q1), grouping = dataset$Update)
summary(likert_update)

likert_age <- likert(items = data.frame(Q1 = dataset$Q1), grouping = dataset$Age)
summary(likert_age)

# Plot for Update
plot(likert_update, type = "bar", centred = TRUE, centre = 5,  # British spelling
     plot.percents = TRUE, legend.position = "right",
     main = "Recommendation Scores by Update Status",
     xlab = "Percentage (%)")

# Plot for Age
plot(likert_age, type = "bar", centred = TRUE, centre = 5,  # British spelling
     plot.percents = TRUE, legend.position = "right",
     main = "Recommendation Scores by Age Group",
     xlab = "Percentage (%)")

# Question 2: Map of the UK with pie chart showing sales by region
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)
colnames(dataset)

# Load world map data
# Inspect the dataset to debug column names
print(colnames(dataset))  # Check the exact column names
head(dataset)  # Look at the first few rows

# If column names are lowercase or different, rename them to match expected names
# For example, if they are "longitude" and "latitude", rename to "Longitude" and "Latitude"
dataset <- dataset %>%
  rename(Longitude = longitude, Latitude = latitude, Average_Monthly_Profit = Average_Monthly_Profit)

# Load world map data
mapdata <- map_data("world")

library(readxl)

# Load the correct sheet
dataset <- read_excel("MAST7200_assesment2.xlsx", sheet = "Sales")

# Check the first few rows
head(dataset)
install.packages("plotly")
library(plotly)
library(ggplot2)
library(dplyr)

# Create profit range categories
dataset <- dataset %>%
  mutate(Profit_Range = cut(Average_Monthly_Profit,
                            breaks = c(0, 10000, 15000, 20000, 25000, 30000, Inf),
                            labels = c("<10K", "10K-15K", "15K-20K", "20K-25K", "25K-30K", "30K+")))

# Create the static ggplot map
world_map <- ggplot() +
  geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), fill = "lightgrey", colour = "black") +  # British spelling
  geom_point(data = dataset, aes(x = Longitude, y = Latitude, size = Average_Monthly_Profit, 
                                 colour = Profit_Range, text = paste("ID:", Restaurant_ID, "<br>Profit: $", Average_Monthly_Profit))) +  # British spelling
  labs(title = "Restaurant Locations and Average Monthly Profit (sales performance)",
       x = "Longitude", y = "Latitude", size = "Profit ($)", colour = "Profit Range") +  # British spelling
  theme_minimal() +
  coord_cartesian(xlim = c(-5, 5), ylim = c(45, 60))  # Keep UK region zoom

# Convert to interactive plot
interactive_map <- ggplotly(world_map, tooltip = "text")

# Display the interactive map
interactive_map

