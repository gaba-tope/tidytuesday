# Import library
library(tidytuesdayR)
library(tidyverse)
library(plyr)
library(showtext)
library(ggtext)
library(svglite)
library(usmap)
# Fonts
font_add_google("Oswald", "oswald")
sysfonts::font_add(family = "Font Awesome 6 Brands", # Social media icon fonts
                   regular = "C:/R/Rwd/fonts/Font-Awesome-6-Brands-Regular-400.otf")
showtext_auto()

# Data Import
tuesdata <- tidytuesdayR::tt_load('2023-10-10')
haunted_places <- tuesdata$haunted_places
head(haunted_places)

## Data Manipulation 
# State-Value dataframe
state_value <- as.data.frame(table(haunted_places$state_abbrev))
colnames(state_value) <- c("state", "Freq")

# Labeling state abbr
centroid_labels <- usmapdata::centroid_labels("states")
colnames(centroid_labels)[4] <- "state"
state_labels <- merge(state_value, centroid_labels, by = "state")
state_labels_low <- state_labels |> filter(Freq < 500) # Black color
state_labels_high <- state_labels |> filter(Freq >= 500) # White color

## Colors
bg_col <- "#eeeeee" # fafafa
text_col <- "grey10"
map_col <- "#bf0000" # "#0576b6" blue

## Texts
# Plot1 - Texts
title1 = "Ghostly Geographies: Haunted Places Across the U.S."
subtitle1 = "Number of Haunted Places per State"
caption1 = paste0(
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


## Plot1: # of Haunted Place per state
plot_haunted <- plot_usmap(data = state_value, values = "Freq", color = map_col)+
  scale_fill_continuous(
    low = "white", high = map_col, name = "# of Haunted Place",
    label = scales::comma  ) +
  geom_text(data = state_labels_low, aes(x=x, y=y, label= state),
            color= "black", size= 5)+
  geom_text(data = state_labels_high, aes(x=x, y=y, label= state),
            color= "white", size= 5)+
  labs(title = title1,
       subtitle = subtitle1,
       caption = caption1)+
  theme_void(base_size = 30, base_family = "oswald")+ # for png, base: 30. svg: 20
  theme(legend.position = "right",
        plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
        plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
        plot.title = element_textbox_simple(
          colour= text_col,
          face = "bold",
          family = "oswald",
          lineheight = 0.5,
          size = 40, # for png, size = 35. svg: 25
          margin = margin(b = 2, t = 5, r = 5, l = 5) # plot title 기본 위치를 기준으로 변경.
        ),
        plot.subtitle = element_textbox_simple(
          colour = text_col,
          family = "oswald",
          margin = margin(b = 5, t = 1, l = 5),
          size = 30),
        plot.caption = element_textbox_simple(
          colour= text_col,
          lineheight = 0.5,
          family = "oswald",
          margin = margin(t = 13, b = 5, l = 5), # plot caption 기본 위치를 기준으로 변경.
          size = 30), 
        plot.background = element_rect(fill = bg_col, colour = bg_col),
        )

#ggsave(file="haunted.svg", plot = plot_haunted, width = 2400 ,height= 1600, 
#       units = 'px')

ggsave(file="haunted.png", plot = plot_haunted, width = 2400 ,height= 1600, 
       units = 'px')
