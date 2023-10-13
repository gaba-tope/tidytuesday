# Import library-----------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(plyr)
library(dplyr)
library(showtext)
library(ggtext)
library(svglite)
library(usmap)
library(readxl)

# Fonts--------------------------------------------------
font_add_google("Oswald", "oswald")
sysfonts::font_add(family = "Font Awesome 6 Brands", # Social media icon fonts
                   regular = "C:/R/Rwd/fonts/Font-Awesome-6-Brands-Regular-400.otf")
showtext_auto()

# Data Import---------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2023-10-10')
haunted_places <- tuesdata$haunted_places
state_area <- read.csv(file="./us_state_area.csv") # https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_area
state_popul <- read.csv(file="./us_state_popul.csv") # https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population 

## Data Manipulation-------------------------------------
# State-Value dataframe
state_value <- as.data.frame(table(haunted_places$state_abbrev))
colnames(state_value) <- c("state", "Freq")

state_abb_full <- dplyr::select(haunted_places, state, state_abbrev)
state_abb_full <- dplyr::distinct(state_abb_full, state, .keep_all = TRUE)
state_abb_full

# State-Value-Area- V/A dataframe
state_area <- dplyr::select(state_area, "Land..water.and.total.area.by.U.S..state..district.or.territory..sortable", 
              "X.2", "X.5") 
colnames(state_area) <- c("state", "total_area", "land_area")
state_area <- state_area[-1:-2,] |> as_tibble(state_area)
head(state_area) # One space at the start of state column. Need to remove them.
state_area$state <- str_trim(state_area$state, side = "left") # space trimming.
state_val_area <- merge(state_area, state_abb_full, by = "state")
colnames(state_val_area) <- c("state_full", "total_area", "land_area", "state")
state_val_area <- merge(state_val_area, state_value, by = "state") |> as_tibble()
state_val_area$land_area <- readr::parse_number(state_val_area$land_area) |>as.integer() #Remove non-numeric character.

state_val_area_per <- ddply(state_val_area, .(state), mutate, # normalize land area
                            land_area_norm = (land_area/min(state_val_area_per$land_area))) 
state_val_area_per <- ddply(state_val_area_per, .(state), mutate, # normalize number of haunted place
                            Freq_norm = (Freq/min(state_val_area_per$Freq))) 
state_val_area_per <- ddply(state_val_area_per, .(state), mutate, # Get Ratio
                            place_area_ratio = (Freq_norm/land_area_norm)*100) 

# State-Value-Popul-V/P dataframe
names(state_popul)
state_popul <- dplyr::select(state_popul, "State.or.territory", "X.1" ) 
colnames(state_popul) <- c("state", "popul")
state_popul <- state_popul[-1:-3 ,] 
state_popul$state <- str_trim(state_popul$state, side = "left") # space trimming.
state_val_popul <- merge(state_popul, state_abb_full, by = "state") |>as_tibble()
colnames(state_val_popul) <- c("state_full", "popul", "state")
state_val_popul <- merge(state_val_popul, state_value, by = "state") |> as_tibble()
state_val_popul$popul <- readr::parse_number(state_val_popul$popul) |>as.integer() #Remove non-numeric character.

state_val_popul_per <- ddply(state_val_popul, .(state), mutate, # normalize population
                             popul_norm = (popul/min(state_val_popul_per$popul))
                             )
state_val_popul_per <- ddply(state_val_popul_per, .(state), mutate, # normalize Freq
                             Freq_norm = (Freq/min(state_val_popul_per$Freq))
)
state_val_popul_per <- ddply(state_val_popul_per, .(state), mutate, # Get Ratio
                             place_popul_ratio = (Freq_norm/popul_norm)*100
)

# Labeling state abbr
centroid_labels <- usmapdata::centroid_labels("states")
colnames(centroid_labels)[4] <- "state"
state_labels <- merge(state_value, centroid_labels, by = "state")
state_labels_area <- merge(state_val_area_per, centroid_labels, by= "state")
state_labels_pop <- merge(state_val_popul_per, centroid_labels, by = "state")


state_labels_low <- state_labels |> filter(Freq < 500) # Black color
state_labels_high <- state_labels |> filter(Freq >= 500) # White color

state_labels_area_low <- state_labels_area |> filter(place_area_ratio < 150)
state_labels_area_high <- state_labels_area |> filter(place_area_ratio >=150)

state_labels_popul_low <- state_labels_pop |> filter(place_popul_ratio < 150)
state_labels_popul_high <- state_labels_pop |> filter(place_popul_ratio >= 150)

## Colors----------------------------------------------
bg_col <- "#eeeeee" # fafafa
text_col <- "grey10"
map_col <- "#bf0000" # "#0576b6" blue

## Texts------------------------------------------------
# Plot2 - Texts
title2 =  "Ghostly Geographies: Haunted Places Across the U.S."
subtitle2 = "Haunted House Density Index"
caption2 = paste0("'Haunted House Density Index' is a comparative metric devised
to assess the relative haunted house density of each U.S. state in relation to 
the smallest state. It is calculated by dividing the 'haunted place ratio' by the
'area ratio' which is then multiplied by 100. 'Haunted place ratio' is calculated by 
dividing the number of haunted place of a given state by the number of the haunted place 
of the smallest state.  'Area ratio' is calculated by the land area of
each U.S. state divided by the smallest state area. This index provides a dimensionless measure indicating 
the number of haunted house per state land area, accounting for the relative land size of each state.
<br>**Data**: Haunted Places Dataset (Timothy Renner)<br>**Graphic**: Tope ", social_caption
)
#Plot3 - Texts
title3 = "Ghostly Geographies: Haunted Places Across the U.S."
subtitle3 = "Per Capita Haunted House Index"
caption3 = paste0(
  "\'Per Capita Haunted House Index\' is a comparative metric devised
to assess relative number of haunted house per person in each U.S. state, 
normalized to the state with the smallest number of haunted house. 
It is calculated by dividing the \'haunted place ratio\' by the \'population ratio\' which is then multiplied by 100. ",
  "\'Haunted place ratio\' is calculated by dividing the number of haunted place of a given state by the number of the haunted place of the smallest state. 
                  \'Population ratio\' is calculated by population of each U.S state divided by the population of the smallest state. 
This index provides a dimensionless measure indicating the number of haunted house per capita, accounting for the relative population size of each state. <br>",
  "**Data**: Haunted Places Dataset (Timothy Renner)<br>**Graphic**: Tope ", social_caption
)

# Social
github_icon <- "&#xf09b" #unicode f09b, used for HTML so &#x added.
github_username <- "gaba-tope"
twit_icon <- "&#xf099"
twit_username <- "@tope_ezia"
social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #E30B5C'>{github_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{twit_icon};</span>
  <span style='color: #E30B5C'>{twit_username}</span>"
)
# Plot1: Haunted_places(2023-10-10).R 

# Plot2: Haunted House Density Index: per state area
plot_haunted_area <-  plot_usmap(data = state_val_area_per, values = "place_area_ratio", color = map_col)+
  scale_fill_continuous(
    low = "white", high = map_col, name = "Haunted House Density Index",
    label = scales::comma  ) +
  geom_text(data = state_labels_area_low, aes(x=x, y=y, label= state),
            color= "black", size= 5)+
  geom_text(data = state_labels_area_high, aes(x=x, y=y, label= state),
            color= "white", size= 5)+
  labs(title = title2,
       subtitle = subtitle2,
       caption = caption2)+
  theme_void(base_size = 30, base_family = "oswald")+ # for png, base: 30. svg: 20
  theme(legend.position = "right",
        plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
        plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
        plot.title = element_textbox_simple(
          colour= text_col,
          face = "bold",
          family = "oswald",
          lineheight = 0.5,
          size = 50, # for png, size = 35. svg: 25
          margin = margin(b = 2, t = 5, r = 5, l = 5) # plot title 기본 위치를 기준으로 변경.
        ),
        plot.subtitle = element_textbox_simple(
          colour = text_col,
          family = "oswald",
          margin = margin(b = 5, t = 1, l = 5),
          size = 30
        ),
        plot.caption = element_textbox_simple(
          colour= text_col,
          lineheight = 0.5,
          family = "oswald",
          margin = margin(t = 5, b = 10, l = 5), # plot caption 기본 위치를 기준으로 변경.
          size = 20), 
        plot.background = element_rect(fill = bg_col, colour = bg_col),
  )
ggsave(file="haunted_density.png", plot = plot_haunted_area, width = 2400 ,height= 1600, 
       units = 'px')

# Plot3: per state population
plot_haunted_popul <-  plot_usmap(data = state_val_popul_per, values = "place_popul_ratio", color = map_col)+
  scale_fill_continuous(
    low = "white", high = map_col, name = "Per Capita Haunted House Index",
    label = scales::comma  ) +
  geom_text(data = state_labels_popul_low , aes(x=x, y=y, label= state),
            color= "black", size= 5)+
  geom_text(data = state_labels_popul_high, aes(x=x, y=y, label= state),
            color= "white", size= 5)+
  labs(title = title3,
       subtitle = subtitle3,
       caption = caption3)+
  theme_void(base_size = 30, base_family = "oswald")+ # for png, base: 30. svg: 20
  theme(legend.position = "right",
        plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
        plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
        plot.title = element_textbox_simple(
          colour= text_col,
          face = "bold",
          family = "oswald",
          lineheight = 0.5,
          size = 50, # for png, size = 35. svg: 25
          margin = margin(b = 2, t = 5, r = 5, l = 5) # plot title 기본 위치를 기준으로 변경.
        ),
        plot.subtitle = element_textbox_simple(
          colour = text_col,
          family = "oswald",
          margin = margin(b = 5, t = 1, l = 5),
          size = 30
        ),
        plot.caption = element_textbox_simple(
          colour= text_col,
          lineheight = 0.5,
          family = "oswald",
          margin = margin(t = 13, b = 5, l = 5), # plot caption 기본 위치를 기준으로 변경.
          size = 20), 
        plot.background = element_rect(fill = bg_col, colour = bg_col),
  )
ggsave(file="haunted_per_capita.png", plot = plot_haunted_popul, width = 2400 ,height= 1600, 
       units = 'px')
