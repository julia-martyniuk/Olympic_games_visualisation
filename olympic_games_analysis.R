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
library(magick)
library(shiny)
library(forcats)
library(maps)
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
# national dominance by sport categories
#  map specific disciplines to groups
olympics_categorized <- olympics %>%
  filter(!is.na(Medal)) %>%
  mutate(Category = case_when(
    Sport %in% c("Judo", "Boxing", "Wrestling", "Taekwondo", "Fencing") ~ "Combat",
    
    Sport %in% c("Basketball", "Football", "Volleyball", "Handball", "Hockey", 
                      "Ice Hockey", "Rugby", "Baseball", "Softball", "Lacrosse", 
                      "Cricket", "Polo", "Tug of War") ~ "Team Sports",
    
    Sport %in% c("Tennis", "Table Tennis", "Badminton", "Rackets", 
                      "Jeu de paume", "Basque Pelota") ~ "Racquet",
    
    Sport %in% c("Aquatics", "Swimming", "Diving", "Rowing", "Canoe", 
                      "Canoe / Kayak", "Sailing", "Water Motorsports") ~ "Water Sports",
    
    Sport %in% c("Shooting", "Archery", "Golf", "Curling", "Croquet", "Roque") ~ "Precision",
    
    Sport %in% c("Athletics", "Gymnastics", "Weightlifting", "Triathlon", 
                      "Modern Pentathlon") ~ "Athletics & Gym",
    
    Sport %in% c("Skiing", "Skating", "Biathlon", "Bobsleigh", "Luge") ~ "Winter Sports",
    
    Sport %in% c("Equestrian", "Cycling") ~ "Cycling & Horse",
    
    TRUE ~ "Other" # Catch-all for anything missed
  ))


# keep top 10 Countries
top_10_countries <- olympics_categorized %>%
  count(Country_Link, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Country_Link)

heatmap_data <- olympics_categorized %>%
  filter(Country_Link %in% top_10_countries) %>%
  # filter out "Other" or tiny categories 
  filter(Category != "Other") %>%
  group_by(Country_Link, Category, Discipline) %>%
  summarise(Medals = n(), .groups = "drop")


ggplot(heatmap_data, aes(x = Discipline, y = Country_Link, fill = Medals)) +
  geom_tile(color = "white", size = 0.2) +
  # Faceting
  facet_grid(~Category, scales = "free_x", space = "free_x") +
  # Scales
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Medals") +
  # Guides (Legend Size)
  guides(fill = guide_colorbar(
    barwidth = unit(4, "cm"), 
    barheight = unit(0.3, "cm"),
    title.position = "top", 
    title.hjust = 0.5
  )) +
  
  # Formatting
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = 9,face = "bold"), 
    axis.text.y = element_text(face = "bold"),
    
    # Facet Headers
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    
    # Floating Legend Positioning
    legend.position = c(1, 1.12), 
    legend.justification = "right",
    legend.direction = "horizontal",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    
    # to fit the legend and title
    plot.margin = margin(t = 60, r = 10, b = 10, l = 10) 
  ) +
  
  labs(
    title = "National Dominance by Sport Categories",
    subtitle = "Which nations specialize more?",
    x = NULL,
    y = NULL
  )



################################################################################
################################################################################
# Olympic Tarzan

# Johnny's Weissmuller data
tarzan_data <- olympics %>%
  filter(grepl("Weissmuller", Athlete_name)) %>%
  filter(!is.na(Medal)) %>%
  mutate(
    Event_Label = gsub("Swimming Men's ", "", Event),
    Event_Label = gsub("Water Polo Men's ", "", Event_Label)
  )

ggplot(tarzan_data, aes(x = Year, y = Event_Label)) +

  geom_segment(aes(x = 1922, xend = 1930, y = Event_Label, yend = Event_Label), 
               color = "#a6cee3", size = 6, alpha = 0.5) + 
  # Medals
  geom_point(aes(fill = Medal), shape = 21, color = "white", size = 8, stroke = 1) +
  geom_text(aes(label = substr(Medal, 1, 1)), color = "white", fontface = "bold", size = 3) +
  # Tarzan Annotation
  annotate("curve", x = 1929, y = 3, xend = 1932, yend = 3, 
           curvature = -0.2, arrow = arrow(length = unit(0.3, "cm")), 
           color = "#006400", size = 1) + 
  
  annotate("text", x = 1932.2, y = 3, 
           label = "1932: Becomes Tarzan\n(First of 12 movies)", 
           hjust = 0, color = "#006400", fontface = "bold", size = 4) +
  # Scales
  scale_fill_manual(values = c("Gold" = "#FFD700", "Bronze" = "#cd7f32")) +
  scale_x_continuous(limits = c(1922, 1938), breaks = c(1924, 1928)) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#f0fff0", color = NA),  
    panel.background = element_rect(fill = "#f0fff0", color = NA), 
    
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 18, hjust = 0, color = "#004d00"), 
    plot.subtitle = element_text(size = 12, color = "#2e8b57", hjust = 0), 
    
    axis.text.y = element_text(face = "bold", size = 11, color = "#0047AB"), 
    axis.text.x = element_text(face = "bold", size = 11, color = "#006400")
  ) +
  
  labs(
    title = "Johnny Weissmuller: From the Pool to the Jungle",
    subtitle = "Sport before his Hollywood career",
    x = NULL,
    y = NULL
  )



################################################################################
################################################################################
# Map, footprint of the Olympic games
host_cities_list <- olympics %>%
  filter(is_host == 1) %>%
  distinct(City, Year, Season) %>%
  arrange(City)

rio_row <- data.frame(City = "Rio de Janeiro", Year = 2016, Season = "Summer")
host_cities_list <- rbind(host_cities_list, rio_row)

# Add countries names (to match them on the map)
city_country_map <- data.frame(
  City = c("Albertville", "Amsterdam", "Antwerp", "Athens", "Atlanta", "Barcelona", 
           "Beijing", "Berlin", "Calgary", "Chamonix", "Cortina d'Ampezzo", 
           "Garmisch Partenkirchen", "Grenoble", "Helsinki", "Innsbruck", 
           "Lake Placid", "Lillehammer", "London", "Los Angeles", 
           "Melbourne / Stockholm", "Mexico", "Montreal", "Moscow", "Munich", 
           "Nagano", "Oslo", "Paris", "Rome", "Salt Lake City", "Sapporo", 
           "Sarajevo", "Seoul", "Sochi", "Squaw Valley", "St Louis", "St.Moritz", 
           "Stockholm", "Sydney", "Tokyo", "Turin", "Vancouver", "Rio de Janeiro"),
  # These must match map_data("world") region names exactly
  Region = c("France", "Netherlands", "Belgium", "Greece", "USA", "Spain", 
             "China", "Germany", "Canada", "France", "Italy", 
             "Germany", "France", "Finland", "Austria", 
             "USA", "Norway", "UK", "USA", 
             "Australia", "Mexico", "Canada", "Russia", "Germany", 
             "Japan", "Norway", "France", "Italy", "USA", "Japan", 
             "Bosnia and Herzegovina", "South Korea", "Russia", "USA", "USA", "Switzerland", 
             "Sweden", "Australia", "Japan", "Italy", "Canada", "Brazil")
)

host_cities_list <- left_join(host_cities_list, city_country_map, by = "City")


# Add coordinates
city_coords <- data.frame(
  City_Name = c("Albertville", "Amsterdam", "Antwerp", "Athens", "Atlanta", "Barcelona", 
                "Beijing", "Berlin", "Calgary", "Chamonix", "Cortina d'Ampezzo", 
                "Garmisch Partenkirchen", "Grenoble", "Helsinki", "Innsbruck", 
                "Lake Placid", "Lillehammer", "London", "Los Angeles", 
                "Melbourne / Stockholm", "Mexico", "Montreal", "Moscow", "Munich", 
                "Nagano", "Oslo", "Paris", "Rome", "Salt Lake City", "Sapporo", 
                "Sarajevo", "Seoul", "Sochi", "Squaw Valley", "St Louis", "St.Moritz", 
                "Stockholm", "Sydney", "Tokyo", "Turin", "Vancouver",
                "Rio de Janeiro"),
  Lat = c(45.67, 52.36, 51.21, 37.98, 33.74, 41.38, 39.90, 52.52, 51.04, 45.92, 
          46.54, 47.49, 45.18, 60.16, 47.26, 44.27, 61.11, 51.50, 34.05, -37.81, 
          19.43, 45.50, 55.75, 48.13, 36.64, 59.91, 48.85, 41.90, 40.76, 43.06, 
          43.85, 37.56, 43.60, 39.19, 38.62, 46.49, 59.32, -33.86, 35.67, 45.07, 49.28,
          -22.90),
  Long = c(6.39, 4.90, 4.40, 23.72, -84.38, 2.17, 116.40, 13.40, -114.07, 6.86, 
           12.13, 11.07, 5.72, 24.93, 11.40, -73.97, 10.46, -0.12, -118.24, 144.96, 
           -99.13, -73.56, 37.61, 11.58, 138.18, 10.75, 2.35, 12.49, -111.89, 141.35, 
           18.41, 126.97, 39.73, -120.24, -90.19, 9.83, 18.06, 151.20, 139.65, 7.68, -123.12,
           -43.17)
)

plot_data <- host_cities_list %>%
  group_by(City, Season) %>%
  summarise(Host_Count = n(), Region = first(Region), .groups = "drop") %>%
  left_join(city_coords, by = c("City" = "City_Name")) %>%
  mutate(Label_Text = ifelse(City == "Rio de Janeiro", "Rio 2016", City))

# map layers
world_map <- map_data("world")

# filter the world map to get ONLY the host countries
host_countries_map <- world_map %>%
  filter(region %in% plot_data$Region)

map_pl <- ggplot() +
                  # base World map layer
                  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                             fill = "#f0f0f0", color = "white", size = 0.2) +
                
                  # second layer highlighted green host countries territories 
                  geom_polygon(data = host_countries_map, aes(x = long, y = lat, group = group),
                               fill = "#c7e9c0", color = "white", size = 0.2) + # "Pale Green"
                  
                  # bubbles for number of times city hosted
                  geom_point(data = plot_data, 
                             aes(x = Long, y = Lat, size = Host_Count, fill = Season), 
                             shape = 21, color = "black", stroke = 0.5, alpha = 0.9) +
                  
                  # labels
                  geom_text_repel(data = plot_data, aes(x = Long, y = Lat, label = Label_Text),
                                  size = 3, fontface = "bold", 
                                  box.padding = 0.4, point.padding = 0.3,
                                  max.overlaps = 15, seed = 42) +
                  
                  # Scales andlegends
                  scale_fill_manual(values = c("Summer" = "#d7191c", "Winter" = "#2c7bb6")) +
                  scale_size_continuous(range = c(4, 10), breaks = c(1, 2, 3), name = "Times Hosted") +
                  
                  guides(
                    fill = guide_legend(override.aes = list(size = 8), order = 1), 
                    size = guide_legend(override.aes = list(fill = "gray60"), order = 2)
                  ) +
                  
                  theme_void() +
                  theme(
                    legend.position = "bottom",
                    legend.box = "horizontal",
                    legend.box.just = "center",
                    legend.margin = margin(t = 10),
                    legend.title = element_text(face = "bold", size = 10),
                    legend.text = element_text(size = 9),
                    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5, color = "grey50", size = 12),
                    plot.background = element_rect(fill = "white", color = NA)
                  ) +
                  
                  labs(
                    title = "Global Footprint of the Olympics",
                    fill = "Season"
                  )

img_map_pl <- map_pl +                  
  inset_element(p = logo,
                left = 0.88,   
                bottom = 0.02, 
                right = 1,    
                top = 0.10,    
                align_to = "plot")

img_map_pl
