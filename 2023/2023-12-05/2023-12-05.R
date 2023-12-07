## Import library-----------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(plyr)
library(showtext)
library(ggtext)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(maps)
library(countrycode)
library(plotly)
options(scipen = 999) # Disable scientific notation.

## Fonts---------------------------------------------------
font_add_google(name = "Oswald", family = "oswald")
font_add_google(name = "Titillium Web", family = "titil")
sysfonts::font_add(family = "Font Awesome 6 Brands", # Social media icon fonts
                   regular = "C:/R/Rwd/fonts/Font-Awesome-6-Brands-Regular-400.otf")
showtext_auto()

main_font <- "oswald"

## Colors -------------------------------------------------
bg_col <- "#eeeeee"
text_col <- "grey10"
major_grid_col <- "#bebebe"
minor_grid_col <- "#d6d6d6"
orange <- "#E69F00" # Okabe-Ito Palette
lightblue <- "#56B4E9" # Okabe-Ito Palette
red <- "#D55E00"# Okabe-Ito Palette
map_label_col <- c("black", "white", "black", "black", "black", "black")
## Data Import---------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2023-12-05')
life_expectancy <- tuesdata$life_expectancy
life_expectancy_different_ages <- tuesdata$life_expectancy_different_ages
life_expectancy_female_male <- tuesdata$life_expectancy_female_male

## Data Wrangling -----------------------------------------
# Designed Map
life_expectancy_FM_2021 <- filter(life_expectancy_female_male, Year == 2021)
world <- map_data("world") # dataframe from map (outline) 
world$Code <- countrycode(world$region, "country.name", "iso3c") # Code column added.
life_expectancy_FM_2021_mapped <- inner_join(life_expectancy_FM_2021, world, by = "Code")
Country_ExpDiffFM <- life_expectancy_FM_2021_mapped |> select(c(Entity, Code, LifeExpectancyDiffFM)) |> distinct(LifeExpectancyDiffFM, .keep_all = T) |> arrange(desc(LifeExpectancyDiffFM))
Top_bottom_three <- data.frame(Country = c("Armenia (10.8)", "Russia (10.6)", "Belarus (10.4)", "Maldives (1.9)", "Togo (1.5)", "Nigeria (0.8)"),
                               lat = c(40.1, 55.7, 53.9, 4.1, 6.1, 9.07),
                               long = c(44.5, 37.6, 27.5, 73.5, 1.2, 7.39)) 


# Time-series Line Plot
life_expectancy_KOR <- filter(life_expectancy_different_ages, Code == "KOR") |> select(Year, LifeExpectancy0)
life_expectancy_KOR_25 <- filter(life_expectancy_different_ages, Code == "KOR") |> select(Year, LifeExpectancy25)
life_expectancy_FM_KOR <- filter(life_expectancy_female_male, Code == "KOR") |> select(Year, LifeExpectancyDiffFM)
life_expectancy_KOR_joined <- inner_join(life_expectancy_KOR, life_expectancy_KOR_25, by = "Year") |> inner_join(life_expectancy_FM_KOR, by = "Year") 

## Texts --------------------------------------------------
# Social Info
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
# Map Plot Text
title_map <- "Global Life Expectancy Trends 2021: Unveiling the Female-Male Divide"
subtitle_map <- "Exploring life expectancy at birth in the year 2021."
cap_map <- paste0(
  "**Data**: Life Expectancy Dataset <br> (from 'Our World in Data Life Expectancy Report')<br>**Graphic**: Tope ", social_caption
)
legend_map <- "Life Expectancy Difference\n(Female - Male)"
# Time-series Line Plot Text
title_line <- "Life Expectancy Trends in Korea"
subtitle_line <- "Life Expectancy at Birth, from 1950 to 2021"
cap_line <- paste0(
  "**Data**: Life Expectancy Dataset <br> (from 'Our World in Data Life Expectancy Report')<br>**Graphic**: Tope ", social_caption
)
# Time-series Line Plot_at age 25 Text
title_line_25 <- "Life Expectancy Trends in Korea"
subtitle_line_25 <- "Life Expectancy at Age 25, from 1950 to 2021"
cap_line_25 <- paste0(
  "**Data**: Life Expectancy Dataset <br> (from 'Our World in Data Life Expectancy Report')<br>**Graphic**: Tope ", social_caption
)

## Themes -------------------------------------------------
# Map Theme
map_theme <- theme(
  plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
  plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_textbox_simple(
    colour= text_col,
    face = "bold",
    family = main_font,
    lineheight = 0.5,
    size = 40,
    margin = margin(b = 2, t = 2) 
  ),
  plot.subtitle = element_textbox_simple(
    colour = text_col,
    family = main_font,
    size = 30,
    margin = margin(b = 10)
  ),
  plot.caption = element_textbox_simple(
    colour= text_col,
    lineheight = 0.5,
    family = main_font,
    margin = margin(t = 13, b = 5, l = 5),
    size = 30
  ),
  axis.text.x = element_blank(), # No axis text and title.
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_blank(),
  legend.text = element_text(colour = text_col,
                             family = main_font,
                             size = 30
                             ),#
  legend.title = element_text(colour = text_col,
                              family = main_font,
                              size = 30,
                              lineheight = 0.3
                              ), #face = "bold"
  legend.background = element_rect(fill = bg_col)
)

# Time-series Line Plot Theme
line_theme <- theme(
  plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
  plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.grid.major = element_line(colour = major_grid_col),
  panel.grid.minor = element_line(colour = minor_grid_col),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_textbox_simple(
    colour= text_col,
    face = "bold",
    family = "oswald",
    lineheight = 0.5,
    size = 40,
    margin = margin(b = 2, t = 2) 
  ),
  plot.subtitle = element_textbox_simple(
    colour = text_col,
    family = "oswald",
    size = 30,
    margin = margin(b = 10)
  ),
  plot.caption = element_textbox_simple(
    colour= text_col,
    lineheight = 0.5,
    family = "oswald",
    margin = margin(t = 13, b = 5, l = 5),
    size = 25
  ),
  axis.text= element_text(colour = text_col,
                             family = "oswald", size = 20),
  axis.title.x = element_text(colour = text_col,
                              family = "oswald", size = 30),
  axis.title.y = element_text(colour = text_col,
                              family = "oswald", size = 30,
                              lineheight = 0.5),
  axis.line = element_blank()
)

## Plots --------------------------------------------------
# Designed Map w/ {ggplot2} and {maps}
worldplot <- ggplot()+ 
  geom_polygon(data = life_expectancy_FM_2021_mapped,
               aes(x = long, y = lat, group = group, fill = LifeExpectancyDiffFM),
               linewidth = 0.1, color = text_col) +
  geom_point(data = Top_bottom_three, aes(x = long, y = lat), colour = red, size = 0.2, alpha = 0.8)+
  geom_text_repel(data = Top_bottom_three, aes(x = long, y = lat, label = Country),
                  size = 7, colour = map_label_col, 
                  nudge_x = c(-70.5, 0, -25.5, 0, -5.5, -40.5),
                  nudge_y = c(5, 0, +20.5, -5, -10.5, -1),
                  segment.size = 0.2, segment.color = text_col, family = main_font)+
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(title = title_map,
       subtitle = subtitle_map,
       caption = cap_map,
       fill = legend_map)+
  coord_fixed(1.3)+
  map_theme
  
worldplot
ggsave(file="world_plot_FM_2021_oswald.png", plot = worldplot, width = 2500, height= 1400, 
       units = 'px', dpi = 300)

# Time-series Line Plot
Korplot_exp <- ggplot(data = life_expectancy_KOR_joined) +
  geom_line(aes(x = Year, y = LifeExpectancy0), color = orange, size = 1)+
  geom_point(aes(x = Year, y = LifeExpectancy0), color = bg_col, size = 1.5, shape = 21, fill = orange) +
  scale_x_continuous(
    breaks = seq(1950, 2025, 10)
  )+
  scale_y_continuous(
    limits = c(20,90),
    breaks = seq(20, 90, 20),
    name = "Life Expectancy"
  )+
  labs(title = title_line,
       subtitle = subtitle_line)+
  line_theme

Korplot_expFM <- ggplot(data = life_expectancy_KOR_joined)+
  geom_line(aes(x = Year, y = LifeExpectancyDiffFM), color = lightblue, size = 1)+
  geom_point(aes(x = Year, y = LifeExpectancyDiffFM), color = bg_col, size = 1.5, shape = 21, fill = lightblue) +
  scale_x_continuous(
    breaks = seq(1950, 2025, 10)
  )+
  scale_y_continuous(
    limits = c(0,20),
    breaks = seq(0, 20, 5),
    name = "Life Expectancy Difference\n(Female - Male)"
  )+
  labs(caption = cap_line)+
  line_theme

composite_plot <- plot_grid(Korplot_exp, Korplot_expFM, align = 'v', ncol = 1, labels = " ")

ggsave(file="composite_exp.png", plot = composite_plot, width = 2500, height= 1400, 
       units = 'px', dpi = 300)

# Time-series Line Plot - at Age 25
Korplot_exp_25 <- ggplot(data = life_expectancy_KOR_joined) +
  geom_line(aes(x = Year, y = LifeExpectancy25), color = orange, size = 1)+
  geom_point(aes(x = Year, y = LifeExpectancy25), color = bg_col, size = 1.5, shape = 21, fill = orange) +
  scale_x_continuous(
    breaks = seq(1950, 2025, 10)
  )+
  scale_y_continuous(
    limits = c(40,90),
    breaks = seq(40, 90, 10),
    name = "Life Expectancy"
  )+
  labs(title = title_line_25,
       subtitle = subtitle_line_25,
       caption = cap_line_25)+
  line_theme

Korplot_exp_25
ggsave(file="exp_25.png", plot = Korplot_exp_25, width = 2500, height= 1400, 
       units = 'px', dpi = 300)
ggplotly(worldplot)
