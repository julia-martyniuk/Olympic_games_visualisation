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

################################################################################
################################################################################
# Share of women in the teams

# Top 15 Countries by distinct ATHLETES sent
top_big_teams_15_countries <- olympics %>%
  group_by(Country_Link) %>%
  summarise(Total_Unique_Athletes = n_distinct(Athlete_name)) %>% 
  arrange(desc(Total_Unique_Athletes)) %>%
  slice_head(n = 15) %>%
  pull(Country_Link)

# Calculate Team Composition per Year/Season
yearly_team_composition <- olympics %>%
  filter(Country_Link %in% top_big_teams_15_countries) %>%
  group_by(Year, Season, Country_Link) %>%
  summarise(
    Total_Athletes = n_distinct(Athlete_name),
    Women_Count = n_distinct(Athlete_name[Gender == "Women"]),
    .groups = "drop"
  ) %>%
  mutate(
    Share_Women = Women_Count / Total_Athletes
  )


share_w_in_team_pl <- ggplot(yearly_team_composition, aes(x = Total_Athletes, y = Share_Women, 
                                   size = Total_Athletes, color = Country_Link)) +
  # Use points for the bubbles
  geom_point(alpha = 0.7) +
  # Add a reference line for 50% parity
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  annotate("text", x = max(yearly_team_composition$Total_Athletes), y = 0.52, 
           label = "Parity (50%)", hjust = 1, color = "gray50") +
  scale_size(range = c(3, 15), name = "Team Size") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) +
  scale_x_log10() + 
  facet_wrap(~Season, scales = "free_x") +
  
  theme_minimal() +
  labs(
    x = "Team Size (Log Scale)",
    y = "Percentage of Women in Team",
    caption = "Source: Olympics Dataset"
  )

# Animate
anim_share_w_in_team <- share_w_in_team_pl + 
  labs(title = 'Year: {frame_time}') + 
  transition_time(Year) +   # drives the time change
  ease_aes('linear')   # Smooths the transition between years

# Render the animation
animate(anim_share_w_in_team, fps = 10, duration = 20, width = 800, height = 500, renderer = gifski_renderer())

################################################################################
################################################################################
# Most summer and winter country winners (by number of medals) 
# This plot illustrates whether country tends to win mostly during summer or winter games

seasonality_cntr <- olympics %>%
  group_by(Country_Link, Season) %>%
  summarise(Total_Medals = n(), .groups = "drop") %>%
  pivot_wider(names_from = Season, values_from = Total_Medals, values_fill = 0) %>%
  filter(Summer > 0, Winter > 0) %>%
  mutate(Type = case_when(
    # rules for the winter and summer winners
    Winter > Summer * 0.4 ~ "Winter power",  
    Summer > Winter * 10 ~ "Summer power",
    TRUE ~ "General"
  ))

winter_giants <- seasonality_cntr %>% 
  filter(Type == "Winter power")

# Highlighted Region: 'Winter Powers' (Polygon created via Convex Hull)
hull_indices <- chull(winter_giants$Summer, winter_giants$Winter) # returns the row numbers of the points that make up the outer shape
hull_data <- winter_giants[hull_indices, ]

seasonality_cntr_pl <- ggplot(seasonality_cntr, aes(x = Summer, y = Winter)) +
  geom_polygon(data = hull_data, fill = "deepskyblue", alpha = 0.2) +
  geom_point(aes(color = Type), size = 3, alpha = 0.7) +
  # Labels
  geom_text_repel(aes(label = Country_Link), 
                  size = 3, 
                  max.overlaps = 15,
                  box.padding = 0.4) +
  # Scales
  scale_x_log10() + 
  scale_y_log10() +
  # Formatting
  scale_color_manual(values = c("General" = "gray", "Summer power" = "orange", "Winter power" = "deepskyblue")) +
  theme_bw() + 
  theme(
    plot.title = element_text(color= "black",face = "bold", size = 20),
    axis.title = element_text(color = "grey31"),
    legend.background = element_rect(color='black', fill=NA),
    legend.box = "vertical",
  ) +
  labs(
    title = "Geography is destiny: summer vs. winter winners",
    x = "Total summer medals (log scale)",
    y = "Total winter medals (log scale)",
    caption = "Source: Olympics Dataset"
  )

img_seasonality_cntr_pl <- seasonality_cntr_pl +                  
  inset_element(p = logo,
                left = 0.02, 
                bottom = 0.82,
                right = 0.15,
                top = 0.98, 
                align_to = "panel")
img_seasonality_cntr_pl

################################################################################
################################################################################
# Show top 10 countries that received the most medals per year
annual_medal_counts <- olympics %>%
  filter(!is.na(Medal)) %>%
  mutate(Country_Name = if("Country_Link" %in% names(.)) Country_Link else Country) %>%
  group_by(Year, Country_Name) %>%
  summarise(Yearly_Medals = n(), .groups = "drop")

# Calculate racing for the countries by years
racing_cntr_data <- annual_medal_counts %>%
  complete(Year = unique(Year), Country_Name, fill = list(Yearly_Medals = 0)) %>% # fill 0 if countries missed games
  group_by(Country_Name) %>%
  arrange(Year) %>%
  mutate(Cumulative_Total = cumsum(Yearly_Medals)) %>%
  ungroup() %>%
  group_by(Year) %>%   # Calculate Rank for EACH Year
  mutate(
    Rank = rank(-Cumulative_Total, ties.method = "first"),
    Value_Rel = Cumulative_Total / max(Cumulative_Total)
  ) %>%
  ungroup() %>%
  # Filter for only the Top 10 at any given moment
  filter(Rank <= 10) %>%
  filter(Cumulative_Total > 0)

# Create the Static Plot
racing_cntr_pl <- ggplot(racing_cntr_data, aes(x = Rank, y = Cumulative_Total, fill = Country_Name, group = Country_Name)) +
  geom_col(width = 0.9, alpha = 0.8) + # bars
  geom_text(aes(y = 0, label = paste(Country_Name, " ")), vjust = 0.2, hjust = 1, size = 5, fontface = "bold") +
  geom_text(aes(y = Cumulative_Total, label = as.character(Cumulative_Total)), hjust = -0.1, size = 5) +
  coord_flip(clip = "off", expand = FALSE) +  # Swap Axes: We want horizontal bars (Rank on Y, Count on X)
  scale_x_reverse() +   # Invert Rank so #1 is at the top
  scale_fill_viridis_d(option = "turbo", guide = "none") + # "turbo" gives distinct vibrant colors
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(), # Hide rank numbers
    axis.title = element_blank(),
    plot.margin = margin(1, 4, 1, 6, "cm"), # Extra margin for labels
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "grey50")
  )

# Add Animation
anim_racing_cntr_pl <- racing_cntr_pl + 
  transition_time(Year) + 
  view_follow(fixed_y = TRUE) + # X-axis moves, Y-axis (Rank 1-10) stays
  labs(title = 'All-time medal leaders in {frame_time}')
animate(anim_racing_cntr_pl, fps = 20, duration = 25, width = 800, height = 600, renderer = gifski_renderer())

################################################################################
################################################################################
# 

cntr_medal_counts <- olympics %>%
  filter(!is.na(Medal)) %>%
  group_by(Country_Link, Medal) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Medal = factor(Medal, levels = c("Gold", "Silver", "Bronze")))

# Top 10 Countries by total medals
top_10_cntr_medals <- cntr_medal_counts %>%
  group_by(Country_Link) %>%
  summarise(Total = sum(Count)) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10) %>%
  pull(Country_Link)


top_10_cntr_medals_pl_data <- cntr_medal_counts %>%
  filter(Country_Link %in% top_10_cntr_medals)

top_10_cntr_medals_pl <- ggplot(top_10_cntr_medals_pl_data, aes(x = reorder(Country_Link, -Count), y = Count, fill = Medal)) +
                            geom_col(position = "dodge", width = 0.7) +
                            geom_text(aes(label = Count), 
                                      position = position_dodge(width = 0.7), 
                                      vjust = -0.5, size = 3, fontface = "bold") +
                            scale_fill_manual(values = c("Gold" = "#FFD700",   
                                                         "Silver" = "#C0C0C0", 
                                                         "Bronze" = "#CD7F32")) + 
                            theme_minimal() +
                            labs(
                              title = "Top 10 Olympic countries: medal breakdown",
                              x = NULL,
                              y = "Number of medals",
                              fill = "Medal type"
                            ) +
                            theme(
                              legend.position = "top",
                              axis.text.x = element_text(angle = 45, hjust = 1, size = 11, face = "bold"),
                              panel.grid.major.x = element_blank(),
                              plot.title = element_text(color= "black",face = "bold", size = 20),
                              axis.title = element_text(color = "grey31")
                            )
 
img_top_10_cntr_medals_pl <-  top_10_cntr_medals_pl +                  
                              inset_element(p = logo,
                                            left = 0.80, 
                                            bottom = 0.85,
                                            right = 1,
                                            top = 1, 
                                            align_to = "panel")
img_top_10_cntr_medals_pl

