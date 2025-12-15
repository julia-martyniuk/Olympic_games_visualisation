print("We are starting the project!")

# Libraries
library(dplyr)
library(stringr)
library(tidyverse)

data_w <- read.csv('data/winter.csv')
data_s <- read.csv('data/summer.csv')
data_general <- read.csv('data/dictionary.csv')

# Check structure of the datasets and data types
str(data_w)
str(data_s)
str(data_general)

# check unique values 
# Summer Olympic games
data_s %>%
  summarise(across(everything(), n_distinct))
unique(data_s[["Year"]])
unique(data_s[["City"]])
unique(data_s[["Sport"]])
unique(data_s[["Discipline"]])  
unique(data_s[["Medal"]])

# Winter Olympic games
data_w %>%
  summarise(across(everything(), n_distinct))
unique(data_w[["Year"]])
unique(data_w[["City"]])
unique(data_w[["Sport"]])
unique(data_w[["Discipline"]])  
unique(data_w[["Event"]])  
  
# Edit the name of the participants 
data_w <- data_w %>%
  separate(Athlete, into = c("Last_Name", "First_Name"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(Last_Name = str_to_title(Last_Name),
         First_Name = str_to_title(First_Name)) %>%
  mutate(First_Name = str_trim(First_Name))  %>%
  unite(col = "Athlete_name",                          
        c("Last_Name", "First_Name"),                
        sep = ", ",                                  
        remove = TRUE)                              

data_s <- data_s %>%
  separate(Athlete, into = c("Last_Name", "First_Name"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(Last_Name = str_to_title(Last_Name),
         First_Name = str_to_title(First_Name)) %>%
  mutate(First_Name = str_trim(First_Name))  %>%
  unite(col = "Athlete_name",                          
        c("Last_Name", "First_Name"),                
        sep = ", ",                                  
        remove = TRUE)

# Create new summary table 
summary_winter <- data_w %>%
  group_by(Country, Year) %>%
  summarise(
    n_women_winners = n_distinct(Athlete_name[Gender == "Women"]), 
    n_men_winners   = n_distinct(Athlete_name[Gender == "Men"]),
    
    # Count Medals 
    n_gold   = sum(Medal == "Gold", na.rm = TRUE),
    n_silver = sum(Medal == "Silver", na.rm = TRUE),
    n_bronze = sum(Medal == "Bronze", na.rm = TRUE),
    
    # Count Unique Disciplines by Gender
    n_disc_women = n_distinct(Discipline[Gender == "Women"]),
    n_disc_men   = n_distinct(Discipline[Gender == "Men"]),
    
    # Total Medals 
    total_medals = n() 
  ) %>%
  ungroup() 

head(summary_winter)

summary_summer <- data_s %>%
  group_by(Country, Year) %>%
  summarise(
    n_women_winners = n_distinct(Athlete_name[Gender == "Women"]), 
    n_men_winners   = n_distinct(Athlete_name[Gender == "Men"]),
    
    # Count Medals 
    n_gold   = sum(Medal == "Gold", na.rm = TRUE),
    n_silver = sum(Medal == "Silver", na.rm = TRUE),
    n_bronze = sum(Medal == "Bronze", na.rm = TRUE),
    
    # Count Unique Disciplines by Gender
    n_disc_women = n_distinct(Discipline[Gender == "Women"]),
    n_disc_men   = n_distinct(Discipline[Gender == "Men"]),
    
    # Total Medals 
    total_medals = n() 
  ) %>%
  ungroup() 

head




# test plot 
library(tidyverse)

global_gender_trends <- data_w %>%
  group_by(Year) %>%
  summarise(
    # Count unique disciplines available globally for each gender
    Women = n_distinct(Discipline[Gender == "Women"]),
    Men   = n_distinct(Discipline[Gender == "Men"])
  ) %>%
  ungroup()


global_trends_long <- global_gender_trends %>%
  pivot_longer(
    cols = c("Women", "Men"),
    names_to = "Gender",      
    values_to = "Count"       
  )

# Look at the transformed data structure
head(global_trends_long)

library(ggplot2)

ggplot(global_trends_long, aes(x = Year, y = Count, color = Gender)) +

  geom_line(size = 1.2) +
  geom_point(size = 3, alpha = 0.7) +
  

  scale_color_manual(values = c("Men" = "#2c7bb6", "Women" = "#d7191c")) +
  
  scale_x_continuous(breaks = seq(1924, 2014, by = 8)) + # Show every 8 years to avoid crowding
  scale_y_continuous(limits = c(0, NA)) + # Ensure Y axis starts at 0
  labs(
    title = "The Closing Gap: Winter Olympic Disciplines by Gender (1924-2014)",
    subtitle = "Number of unique disciplines in which Men vs. Women competed globally",
    y = "Number of Unique Disciplines",
    x = "Olympic Year",
    caption = "Source: Winter Olympics Dataset"
  ) +
  

  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top", # Move legend to top to save width
    panel.grid.minor.x = element_blank() # Remove extra vertical grid lines
  )