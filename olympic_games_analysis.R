print("We are starting the project!")
################################################################################
################################################################################
# Libraries
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel) 
# install.packages('gifski')
# install.packages('gganimate')
library(gganimate)
library(jpeg)
library(patchwork)
################################################################################
################################################################################
######## Data preprocessing ########
data_w <- read.csv('data/winter.csv')
data_s <- read.csv('data/summer.csv')
data_general <- read.csv('data/dictionary.csv')

# Add season and create one common dataset 
data_w <- data_w %>% mutate(Season = "Winter")
data_s <- data_s %>% mutate(Season = "Summer")
olympics <- bind_rows(data_w, data_s)

# Check structure of the datasets and data types
str(olympics)
str(data_general)

# check unique values 
olympics %>%
  summarise(across(everything(), n_distinct))
unique(olympics[["Year"]])
unique(olympics[["City"]])
unique(olympics[["Sport"]])
unique(olympics[["Discipline"]])  
unique(olympics[["Medal"]])
unique(olympics[["Country"]])

  
# Edit the name of the participants 
olympics <- olympics %>%
  separate(Athlete, into = c("Last_Name", "First_Name"), sep = ",", extra = "merge", fill = "right") %>%
  mutate(Last_Name = str_to_title(Last_Name),
         First_Name = str_to_title(First_Name)) %>%
  mutate(First_Name = str_trim(First_Name))  %>%
  unite(col = "Athlete_name",                          
        c("Last_Name", "First_Name"),                
        sep = ", ",                                  
        remove = TRUE)                              

# Due to the historical and political events, some countries were reunited or even
# no more exist. So, for our project, we want to adjust this issue 
# just a mote:  ZZX - Mixed teams, SCG - Serbia and Montenegro (from 2006 SRB and MNE)
# YUG - Yugoslavia, IOP - Independent Olympic Participants (1992 for athletes from Yugoslavia and the Republic of Macedonia)
olympics <- olympics %>%
  mutate(Country_Link = case_when(
    # Russia legacy 
    # Combines: USSR, Russian Empire, Unified Team
    Country %in% c("URS","RUS", "EUN", "RU1") ~ "RUS_L",
    
    # Germany legacy
    # Combines: West, East, Unified Team, Modern Germany
    Country %in% c("GER", "GDR", "FRG", "EUA") ~ "GER_L",
    
    # Czech legacy
    # Combines: Bohemia, Czechoslovakia, Czech Republic
    Country %in% c("TCH", "CZE", "BOH") ~ "CZE_L",
    TRUE ~ Country
  ))

# Add IS_Host column that indicate whether it was hosted country
olympics <- olympics %>%
  mutate(
    # Define the Host Country Code for each City
    Host_NOC = case_when(
      # Summer games
      City == "Athens"      ~ "GRE",
      City == "Paris"       ~ "FRA",
      City == "St Louis"    ~ "USA", 
      City == "London"      ~ "GBR",
      City == "Stockholm"   ~ "SWE",
      City == "Antwerp"     ~ "BEL",
      City == "Amsterdam"   ~ "NED",
      City == "Los Angeles" ~ "USA",
      City == "Berlin"      ~ "GER",
      City == "Helsinki"    ~ "FIN",
      City == "Melbourne / Stockholm" ~ "AUS", # Handle "SWE" separately below
      City == "Rome"        ~ "ITA",
      City == "Tokyo"       ~ "JPN",
      City == "Mexico"      ~ "MEX",
      City == "Munich"      ~ "FRG", # West Germany hosted in 1972
      City == "Montreal"    ~ "CAN",
      City == "Moscow"      ~ "URS", # Soviet Union hosted in 1980
      City == "Seoul"       ~ "KOR",
      City == "Barcelona"   ~ "ESP",
      City == "Atlanta"     ~ "USA",
      City == "Sydney"      ~ "AUS",
      City == "Beijing"     ~ "CHN",
      
      # Winter games
      City == "Chamonix"    ~ "FRA",
      City == "St.Moritz"   ~ "SUI",
      City == "Lake Placid" ~ "USA",
      City == "Garmisch Partenkirchen" ~ "GER",
      City == "Oslo"        ~ "NOR",
      City == "Cortina d'Ampezzo" ~ "ITA",
      City == "Squaw Valley" ~ "USA",
      City == "Innsbruck"   ~ "AUT",
      City == "Grenoble"    ~ "FRA",
      City == "Sapporo"     ~ "JPN",
      City == "Sarajevo"    ~ "YUG", # Yugoslavia hosted in 1984
      City == "Calgary"     ~ "CAN",
      City == "Albertville" ~ "FRA",
      City == "Lillehammer" ~ "NOR",
      City == "Nagano"      ~ "JPN",
      City == "Salt Lake City" ~ "USA",
      City == "Turin"       ~ "ITA",
      City == "Vancouver"   ~ "CAN",
      City == "Sochi"       ~ "RUS",
      TRUE ~ NA_character_
    ),
    # Create the Binary Column (1 = Yes, 0 = No)
    is_host = case_when(
      # The country matches the host map
      Country == Host_NOC ~ 1,
      # SPECIAL CASE: 1956 Equestrian Events in Stockholm (while main games were in AUS)
      City == "Melbourne / Stockholm" & Country == "SWE" ~ 1,
      # Otherwise, not the host
      TRUE ~ 0
    )
  )

# Paste Olympic games logo on the plots 
getwd()
img_path <- 'olympic_emblem_2.jpg'
################################################################################
################################################################################
# Olympic games by number of events and number of countries participates per years 
growth_n_events <- olympics %>%
  # Group by both Year and Season now
  group_by(Year, Season) %>%
  summarise(
    Countries = n_distinct(Country_Link),
    Events = n_distinct(Event)
  ) %>%
  ungroup() %>%
  # Reshape to Long format
  pivot_longer(cols = c("Countries", "Events"), 
               names_to = "Metric", 
               values_to = "Count")


n_events_growth_pl <-  
  ggplot(growth_n_events, aes(x = Year, y = Count, color = Season, linetype = Metric)) +
        geom_line(size = 1.2) +
        geom_point(aes(shape = Metric), size = 3) + 
        scale_color_manual(values = c("Summer" = "#d7191c", "Winter" = "#2c7bb6")) +
        scale_linetype_manual(values = c("Events" = "solid", "Countries" = "dashed")) +
       # Scales
        scale_x_continuous(breaks = seq(1896, 2016, by = 8)) +    
       # Labels
        labs(
        title = "Evolution of the Olympic games",
        subtitle = "Growth in participating Nations vs. Events (Summer & Winter)",
        x = "Year",
        y = "Count",
        caption = "Source: Olympics dataset",
        color = "Season",
        linetype = "Metric",
        shape = "Metric"
         ) +
        # Theme
        theme_minimal() +
        theme(
        legend.position = "bottom",
        legend.background = element_rect(color='black', fill=NA),
        legend.box = "horizontal", 
        plot.title = element_text(color= "black",face = "bold", size = 20),
        axis.title = element_text(color = "grey31"),
        panel.grid.minor = element_blank() 
        )

logo <- readJPEG(img_path, native = TRUE)

img_n_events_growth_pl <- n_events_growth_pl +                  
  inset_element(p = logo,
                left = 0.02,
                bottom = 0.80,
                right = 0.20,
                top = 0.98, 
                align_to = "panel")
img_n_events_growth_pl

################################################################################
################################################################################
# Olympic Disciplines by Gender 

global_gender_trends <- olympics %>%
  group_by(Year, Season) %>%
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


disc_by_gndr <-  ggplot(global_trends_long, aes(x = Year, y = Count, color = Gender)) +
                    geom_line(size = 1) +
                    geom_point(size = 2) +
                    facet_wrap(~ Season) + 
                    scale_color_manual(values = c("Men" = "#1f77b4", "Women" = "#e377c2")) +
                    theme_minimal() +
                    theme(
                      plot.title = element_text(color= "black",face = "bold", size = 20),
                      axis.title = element_text(color = "grey31"),
                      legend.background = element_rect(color='black', fill=NA),
                      legend.box = "vertical",
                    ) +
                    labs(
                      title = "Growth of Olympic disciplines by gender",
                      subtitle = "Comparison of unique disciplines available for Men vs. Women over time",
                      x = "Year",
                      y = "Number of Disciplines",
                      color = "Gender"
                    )

img_disc_by_gndr <- disc_by_gndr +                  
  inset_element(p = logo,
                left = 0.02, # to align to right 0.80
                bottom = 0.80, # 0.85
                right = 0.20, # 1
                top = 0.98, # 1
                align_to = "plot")
img_disc_by_gndr
