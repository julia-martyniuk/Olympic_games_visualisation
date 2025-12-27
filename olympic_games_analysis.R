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
# install.packages('jpeg') 
# install.packages("patchwork")  
# install.packages("magick")
#install.packages("ggalluvial")
library(ggalluvial)
library(gganimate)
library(jpeg)
library(patchwork)
library(magick)
library(shiny)
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
  # WWI (1914-1918)
  annotate("rect", xmin = 1914, xmax = 1918, ymin = -Inf, ymax = Inf,
           fill = "#ffb6c1", alpha = 0.3) +
  
  # WWII (1939-1945)
  annotate("rect", xmin = 1939, xmax = 1945, ymin = -Inf, ymax = Inf,
           fill = "#ffb6c1", alpha = 0.3) +
  
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
# Medals breakdown per top 10 countries

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
################################################################################
################################################################################
# Performance of the countries estimated in the number of medals by years 
# Static plot (country and season should be typped manually)
target_country <- "GBR"
target_season <- "Summer"

country_path <- olympics %>%
  filter(Country_Link == target_country) %>% 
  filter(Season == target_season) %>% 
  group_by(Year, City, is_host) %>%
  summarise(
    Total_Medals = sum(!is.na(Medal)),        
    Team_Size = n_distinct(Athlete_name),    
    .groups = "drop"
  ) %>%
  mutate(Label = paste0(City)) # Alternatively could (City," ('", substr(Year, 3, 4), ")") 

country_path_pl <- ggplot(country_path, aes(x = Year, y = Total_Medals)) +
                      geom_smooth(method = "loess", span = 0.5, color = "gray60",  fill = "gray80", alpha = 0.2) + 
                      geom_line(color = "gray80", size = 1) +
                      # Bubbles
                      geom_point(aes(size = Team_Size, fill = factor(is_host)), shape = 21, color = "white", stroke = 0.5) +
                      # Labels
                      geom_text_repel(aes(label = Label), 
                                      box.padding = 0.5, 
                                      point.padding = 0.5,
                                      size = 3, 
                                      color = "black",
                                      max.overlaps = 50) + 
                      scale_size_continuous(range = c(3, 12), name = "Team Size") +
                      scale_x_continuous(breaks = seq(1896, 2016, 8)) +
                      
                      scale_fill_manual(
                        name = "Status",
                        values = c("0" = "#2c7bb6", "1" = "#d7191c"), 
                        labels = c("0" = "Visitor", "1" = "Host")     
                      ) +
                      
                      guides(
                        fill = guide_legend(order = 1,, override.aes = list(size = 5) ), 
                        size = guide_legend(order = 2, override.aes = list(fill = "gray50")) 
                      ) +
                      
                      theme_minimal() +
                      theme(
                        legend.position = "top",
                        legend.box = "horizontal",      
                        legend.box.just = "center",     
                        legend.margin = margin(t = 5, b = 5), 
                        legend.spacing.x = unit(0.5, 'cm'), 
                        legend.title = element_text(size = 12, face = "bold"),
                        legend.text = element_text(size = 10),
                        plot.title = element_text(face = "bold", size = 16),
                        panel.grid.minor = element_blank(),
                        axis.title = element_text(color = "grey31")
                      ) +
                      
                      labs(
                        title = paste("Performance history of", target_country),
                        x = "Year",
                        y = "Total medals won"
                      )

img_country_path_pl <-  country_path_pl +                  
  inset_element(p = logo,
                left = 0.80, 
                bottom = 0.85,
                right = 1,
                top = 1, 
                align_to = "plot")
img_country_path_pl



################################################################################
################################################################################
# Number of wins when the country is a host
# Version 1
annual_medals <- olympics %>%
  filter(!is.na(Medal)) %>%
  group_by(Year, Country_Link, is_host) %>% 
  summarise(Total_Medals = n(), .groups = "drop")

dumbbell_data <- annual_medals %>%
  group_by(Country_Link) %>%
  filter(sum(is_host) > 0) %>%
  group_by(Country_Link, is_host) %>%
  summarise(Avg_Medals = mean(Total_Medals), .groups = "drop") %>%
  pivot_wider(names_from = is_host, 
              values_from = Avg_Medals, 
              names_prefix = "Status_") %>%
  rename(Visiting = Status_0, Hosting = Status_1) %>%
  mutate(Gap = Hosting - Visiting) 

hosting_adv_pl <-  ggplot(dumbbell_data, aes(y = reorder(Country_Link, Gap))) + 
                # Segment (grey line)
                geom_segment(aes(x = Visiting, xend = Hosting, y = Country_Link, yend = Country_Link), 
                             color = "#b2b2b2", size = 1) +
                geom_point(aes(x = Visiting, color = "Visiting"), size = 3) +
                geom_point(aes(x = Hosting, color = "Hosting"), size = 4) +
                scale_color_manual(
                  name = "Status", 
                  values = c("Visiting" = "#95a5a6", "Hosting" = "#d7191c"),
                  breaks = c("Visiting", "Hosting") 
                ) +
                
                theme_minimal() +
                labs(
                  title = "Do countries win more when they host?",
                  x = "Average medals won",
                  y = NULL,
                  caption = "Ordered by the magnitude of the 'home advantage' gap"
                ) +
                theme(
                  legend.position = "top",
                  legend.box = "horizontal",      
                  legend.box.just = "center", 
                  panel.grid.major.y = element_blank(), 
                  axis.text.y = element_text(face = "bold", size = 10),
                  plot.title = element_text(face = "bold", size = 16),
                )

img_hosting_adv_pl <-  hosting_adv_pl +                  
  inset_element(p = logo,
                left = 0.80, 
                bottom = 0.90,
                right = 1,
                top = 1, 
                align_to = "plot")
img_hosting_adv_pl

################################################################################
################################################################################
# Number of wins when the country is a host
# Version 2

home_advantage <- annual_medals %>%
  group_by(Country_Link) %>%
  # We only care about countries that have actually hosted
  filter(sum(is_host) > 0) %>%
  summarise(
    Hosting = mean(Total_Medals[is_host == 1]),
    Visiting = mean(Total_Medals[is_host == 0])
  ) %>%
  # Keep only countries with significant data  > 10 medals avg to remove noise
  filter(Visiting > 10) %>%
  ungroup()

plot_data <- home_advantage %>%
  pivot_longer(cols = c("Hosting", "Visiting"), 
               names_to = "Context", 
               values_to = "Avg_Medals")

hosting_adv_pl_2 <- ggplot(plot_data, aes(x = reorder(Country_Link, Avg_Medals), y = Avg_Medals, fill = Context)) +
                      geom_col(position = "dodge", width = 0.7) +
                      coord_flip() +
                      
                      scale_fill_manual(values = c("Hosting" = "#d7191c", "Visiting" = "#2c7bb6")) +
                      
                      theme_minimal() +
                      labs(
                        title = "The Home Advantage: Hosting vs. Visiting Performance",
                        subtitle = "Comparing average medal counts when hosting the Games vs. competing abroad",
                        x = NULL,
                        y = "Average medals per games",
                        fill = "Context"
                      ) +
                      theme(
                        legend.position = "top",
                        axis.text.y = element_text(face = "bold")
                      )

img_hosting_adv_pl_2 <-  hosting_adv_pl_2 +                  
  inset_element(p = logo,
                left = 0.80, 
                bottom = 0.02,
                right = 1,
                top = 0.2, 
                align_to = "panel")
img_hosting_adv_pl_2

################################################################################
################################################################################
# Total medals per country and season
medals_per_country <- olympics %>%
  filter(!is.na(Medal)) %>%                # keep only medal-winning entries
  group_by(Country_Link, Season) %>%
  summarise(
    Total_Medals = n(),
    .groups = "drop"
  )

dist_medals_country_pl <- ggplot(
  medals_per_country,
  aes(x = Total_Medals)
) +
  # Histogram
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = "gray70",
    color = "white",
    alpha = 0.7
  ) +
  
  # Density curve
  geom_density(
    color = "#d7191c",
    linewidth = 1
  ) +
  
  # Facet by season
  facet_wrap(~ Season, scales = "free_x") +
  
  # Labels
  labs(
    title = "Distribution of total medals per country",
    subtitle = "Histogram and density of medal counts by season",
    x = "Total medals won by country",
    y = "Density",
    caption = "Source: Olympics dataset"
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(color = "grey30"),
    panel.grid.minor = element_blank()
  )

img_dist_medals_country_pl <- dist_medals_country_pl +
  inset_element(
    p = logo,
    left = 0.02,
    bottom = 0.80,
    right = 0.18,
    top = 0.98,
    align_to = "panel"
  )

img_dist_medals_country_pl


###################################

library(dplyr)
library(ggplot2)
library(scales)
library(gganimate)


# ---- Lorenz data builder ----
lorenz_data <- function(df) {
  df <- df %>%
    filter(!is.na(Medal)) %>%
    group_by(Country_Link) %>%
    summarise(Medals = n(), .groups = "drop") %>%
    arrange(Medals)
  
  if (nrow(df) == 0) return(NULL)
  
  df %>%
    mutate(
      Country_Share = row_number() / n(),
      Medal_Share   = cumsum(Medals) / sum(Medals)
    )
}

lorenz_by_season <- olympics %>%
  filter(!is.na(Medal)) %>%
  group_by(Season, Country_Link) %>%
  summarise(Medals = n(), .groups = "drop") %>%
  arrange(Season, Medals) %>%
  group_by(Season) %>%
  mutate(
    Country_Share = row_number() / n(),
    Medal_Share   = cumsum(Medals) / sum(Medals),
    Gini = gini_coef(Medals)
  ) %>%
  ungroup()

lorenz_alltime_pl <- ggplot(lorenz_by_season, aes(x = Country_Share, y = Medal_Share, color = Season)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 1.2) +
  facet_wrap(~Season) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Lorenz curve of medal distribution across countries",
    subtitle = "More bowed curve = more concentration of medals in few countries",
    x = "Cumulative share of countries",
    y = "Cumulative share of medals",
    caption = "Source: Olympics dataset"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

lorenz_alltime_pl

target_season <- "Summer"   

lorenz_yearly <- olympics %>%
  filter(!is.na(Medal), Season == target_season) %>%
  group_by(Year, Country_Link) %>%
  summarise(Medals = n(), .groups = "drop") %>%
  group_by(Year) %>%
  arrange(Medals, .by_group = TRUE) %>%
  mutate(
    Country_Share = row_number() / n(),
    Medal_Share   = cumsum(Medals) / sum(Medals)
  ) %>%
  ungroup()

lorenz_anim_pl <- ggplot(lorenz_yearly, aes(Country_Share, Medal_Share, group = Year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 1.2, color = "#d7191c") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = paste0("Lorenz curve over time (", target_season, ") — Year: {frame_time}"),
    subtitle = "Medal concentration across countries (more bowed = more unequal)",
    x = "Cumulative share of countries",
    y = "Cumulative share of medals"
  ) +
  theme_minimal() +
  transition_time(Year)

animate(lorenz_anim_pl, fps = 10, duration = 15, width = 800, height = 500, renderer = gifski_renderer())

# ---- Lorenz animation for WINTER ----

target_season <- "Winter"

lorenz_yearly_winter <- olympics %>%
  filter(!is.na(Medal), Season == target_season) %>%
  group_by(Year, Country_Link) %>%
  summarise(Medals = n(), .groups = "drop") %>%
  group_by(Year) %>%
  arrange(Medals, .by_group = TRUE) %>%
  mutate(
    Country_Share = row_number() / n(),
    Medal_Share   = cumsum(Medals) / sum(Medals)
  ) %>%
  ungroup()

lorenz_anim_winter_pl <- ggplot(lorenz_yearly_winter,
                                aes(x = Country_Share, y = Medal_Share, group = Year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 1.2, color = "#2c7bb6") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Lorenz curve over time (Winter) — Year: {frame_time}",
    subtitle = "Medal concentration across countries (more bowed = more unequal)",
    x = "Cumulative share of countries",
    y = "Cumulative share of medals"
  ) +
  theme_minimal() +
  transition_time(Year)

animate(lorenz_anim_winter_pl,
        fps = 10, duration = 15, width = 800, height = 500,
        renderer = gifski_renderer())


########################################



# Top 6 countries PER SEASON
top_countries_by_season <- olympics %>%
  filter(!is.na(Medal)) %>%
  count(Season, Country_Link, sort = TRUE) %>%
  group_by(Season) %>%
  slice_head(n = 6) %>%
  ungroup()

# Top 6 sports PER SEASON
top_sports_by_season <- olympics %>%
  filter(!is.na(Medal)) %>%
  count(Season, Sport, sort = TRUE) %>%
  group_by(Season) %>%
  slice_head(n = 6) %>%
  ungroup()

# Build alluvial data using season-specific top lists
alluvial_data_season <- olympics %>%
  filter(!is.na(Medal)) %>%
  inner_join(top_countries_by_season, by = c("Season", "Country_Link")) %>%
  inner_join(top_sports_by_season,   by = c("Season", "Sport")) %>%
  count(Season, Country_Link, Sport, name = "Medals")

#Plot (with nicer labels + free y scaling)
alluvial_pl <- ggplot(alluvial_data_season,
                      aes(axis1 = Country_Link, axis2 = Sport, y = Medals)) +
  geom_alluvium(aes(fill = Sport), alpha = 0.5, width = 1/18) +
  geom_stratum(width = 1/12, fill = "gray92", color = "gray50") +
  geom_text(stat = "stratum",
            aes(label = str_trunc(after_stat(stratum), 16)),
            size = 3) +
  facet_wrap(~Season, scales = "free_y") +
  scale_x_discrete(limits = c("Country", "Sport"), expand = c(.05, .05)) +
  labs(
    title = "Where medals come from: country → sport flows",
    subtitle = "Top 6 countries and top 6 sports (chosen within each season)",
    y = "Number of medals",
    caption = "Source: Olympics dataset"
  ) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

alluvial_pl

################################################################################
################################################################################

top_n <- 5
target_season <- "Summer"

top_countries <- olympics %>%
  filter(!is.na(Medal), Season == target_season) %>%
  count(Country_Link, sort = TRUE) %>%
  slice_head(n = top_n) %>%
  pull(Country_Link)

ranked <- olympics %>%
  filter(!is.na(Medal),
         Season == target_season,
         Country_Link %in% top_countries) %>%
  count(Year, Country_Link, name = "Medals") %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Medals, ties.method = "first")) %>%
  ungroup()

library(ggrepel)

end_labels <- ranked %>%
  group_by(Country_Link) %>%
  filter(Year == max(Year)) %>%
  ungroup()

bump_pl <- ggplot(
  ranked,
  aes(Year, Rank, group = Country_Link, color = Country_Link)
) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 2.4) +
  scale_y_reverse(breaks = 1:top_n) +
  scale_x_continuous(breaks = sort(unique(ranked$Year))) +
  scale_color_viridis_d(option = "turbo", end = 0.95) +
  geom_text_repel(
    data = end_labels,
    aes(label = Country_Link),
    nudge_x = 2,
    direction = "y",
    hjust = 0,
    size = 3.8,
    show.legend = FALSE,
    min.segment.length = 0
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0("Bump chart: Top ", top_n, " countries by total medals (", target_season, ")"),
    subtitle = "Lower rank is better (Rank 1 at the top)",
    x = "Year",
    y = "Rank",
    caption = "Source: Olympics dataset"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
    plot.margin = margin(10, 60, 10, 10)
  )

print(bump_pl)


# ---- Settings ----
target_season <- "Winter"
top_n <- 5

# ---- Top 5 countries overall in Winter ----
top_countries <- olympics %>%
  filter(!is.na(Medal), Season == target_season) %>%
  count(Country_Link, sort = TRUE) %>%
  slice_head(n = top_n) %>%
  pull(Country_Link)

# ---- Medals per year for those countries + ranking per year ----
ranked_winter <- olympics %>%
  filter(!is.na(Medal),
         Season == target_season,
         Country_Link %in% top_countries) %>%
  count(Year, Country_Link, name = "Medals") %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Medals, ties.method = "first")) %>%
  ungroup()

# ---- End labels (last year each country appears) ----
end_labels_winter <- ranked_winter %>%
  group_by(Country_Link) %>%
  filter(Year == max(Year)) %>%
  ungroup()

# ---- Plot ----
bump_pl_winter <- ggplot(
  ranked_winter,
  aes(Year, Rank, group = Country_Link, color = Country_Link)
) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 2.4) +
  scale_y_reverse(breaks = 1:top_n) +
  scale_x_continuous(breaks = sort(unique(ranked_winter$Year))) +
  scale_color_viridis_d(option = "turbo", end = 0.95) +
  geom_text_repel(
    data = end_labels_winter,
    aes(label = Country_Link),
    nudge_x = 2,
    direction = "y",
    hjust = 0,
    size = 3.8,
    show.legend = FALSE,
    min.segment.length = 0
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0("Bump chart: Top ", top_n, " countries by total medals (Winter)"),
    subtitle = "Lower rank is better (Rank 1 at the top)",
    x = "Year",
    y = "Rank",
    caption = "Source: Olympics dataset"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
    plot.margin = margin(10, 60, 10, 10)
  )

print(bump_pl_winter)


################################################################################
################################################################################



################################################################################
################################################################################
# Performance of the countries estimated in the number of medals by years 
# Dynamic plot 
ui <- fluidPage(
  titlePanel("Olympic performance"),
  sidebarLayout(
    sidebarPanel(
      h4("Settings"),
      selectInput(inputId = "selected_country", 
                  label = "Choose a country:", 
                  choices = sort(unique(olympics$Country_Link)),
                  selected = "GBR"), 
      selectInput(inputId = "selected_season", 
                  label = "Choose season:", 
                  choices = c("Summer", "Winter"), 
                  selected = "Summer"),
      hr()
    ),
    
    mainPanel(
      plotOutput("coolPlot", height = "600px")
    )
  )
)
server <- function(input, output) {
  output$coolPlot <- renderPlot({
    target_country <- input$selected_country
    target_season  <- input$selected_season
    
    # data for the plot
    country_path <- olympics %>%
      filter(Country_Link == target_country) %>% 
      filter(Season == target_season) %>% 
      group_by(Year, City, is_host) %>% 
      summarise(
        Total_Medals = sum(!is.na(Medal)),        
        Team_Size = n_distinct(Athlete_name),     
        .groups = "drop"
      ) %>%
      mutate(Label = City)
    
    #  prevent errors for countries with no medals
    if(nrow(country_path) == 0) return(NULL)
    
    # plot
    country_path_pl <- ggplot(country_path, aes(x = Year, y = Total_Medals)) +
      geom_smooth(method = "loess", span = 0.5, color = "gray60",  fill = "gray80", alpha = 0.2) + 
      geom_line(color = "gray80", size = 1) +
      # Bubbles
      geom_point(aes(size = Team_Size, fill = factor(is_host)), shape = 21, color = "white", stroke = 0.5) +
      # Labels
      geom_text_repel(aes(label = Label), 
                      box.padding = 0.5, 
                      point.padding = 0.5,
                      size = 3, 
                      color = "black",
                      max.overlaps = 50) + 
      scale_size_continuous(range = c(3, 12), name = "Team Size") +
      scale_x_continuous(breaks = seq(1896, 2016, 8)) +
      
      scale_fill_manual(
        name = "Status",
        values = c("0" = "#2c7bb6", "1" = "#d7191c"), 
        labels = c("0" = "Visitor", "1" = "Host")     
      ) +
      
      guides(
        fill = guide_legend(order = 1,, override.aes = list(size = 5) ), 
        size = guide_legend(order = 2, override.aes = list(fill = "gray50")) 
      ) +
      
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.box = "horizontal",      
        legend.box.just = "center",     
        legend.margin = margin(t = 5, b = 5), 
        legend.spacing.x = unit(0.5, 'cm'), 
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "grey31")
      ) +
      
      labs(
        title = paste("Performance:", target_country, "(",target_season,")"),
        x = "Year",
        y = "Total Medals Won"
      )
    country_path_pl + inset_element(p = logo, left = 0.85, bottom = 0.80, right = 1, top = 1, align_to = "plot")
  })
}
# Run the app
shinyApp(ui = ui, server = server)