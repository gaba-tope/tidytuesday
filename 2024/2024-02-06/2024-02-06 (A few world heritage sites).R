## Import library-----------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(plyr)
library(showtext)
library(ggtext)
library(ggplot2)
library(forcats)
library(socialcap)
library(ggchicklet) # remotes::install_github("hrbrmstr/ggchicklet"), for rounded corner
options(scipen = 999) # Disable scientific notation.


## Data Import---------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2024-02-06')

## Fonts---------------------------------------------------
font_add_google(name = "Oswald", family = "oswald")
showtext_auto()

main_font <- "oswald"

## Colors -------------------------------------------------
bg_col <- "#eeeeee"
text_col <- "grey10"
major_grid_col <- "#bebebe"
minor_grid_col <- "#d6d6d6"
orange <- "#E69F00" 
blue <- "#436f82" 
red <- "#ae4544"
purple <- "#7c5981"
pal <- RColorBrewer::brewer.pal(n = 6,"Set1")
pal_SDN <- pal[c(1, 3, 2)]

## Data Wrangling -----------------------------------------
heritage <- tuesdata[["heritage"]] |> mutate(pct_change = paste0("+", round(((`2022` - `2004`) / `2004`) * 100, digits = 2), "%"))
heritage_long <- pivot_longer(heritage, cols = -c(country, pct_change), names_to = "year", values_to = "number") 
heritage_long$country <- factor(heritage_long$country, ordered = TRUE, levels = c("Sweden", "Denmark", "Norway"))

heritage_long |> dplyr::arrange(country)
## Texts --------------------------------------------------
social_caption <- socialcap(gitname = "gaba-tope", twitname = "@tope_ezia")
title <- "World Heritage Sites in Sweden, Denmark, and Norway:"
subtitle <- "Contrast between 2004 and 2022"
caption <- paste0("Data : UNESCO World Heritage Sites <br>Graphic : ", social_caption)

## Themes -------------------------------------------------
main_theme <- theme(
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
    size = 27,
    margin = margin(b = 2, t = 2) 
  ),
  plot.subtitle = element_textbox_simple(
    colour= text_col,
    family = main_font,
    lineheight = 0.5,
    size = 20,
    margin = margin(b = 2, t = 2) 
  ),
  plot.caption = element_textbox_simple(
    colour= text_col,
    lineheight = 0.5,
    family = main_font,
    margin = margin(t = 13, b = 5, l = 5),
    size = 15
  ),
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(family = main_font,
                             size = 15,
                             colour = text_col)
)

## Plots --------------------------------------------------
main_plot <- 
  ggplot(data = heritage_long) +
  #geom_col(aes(x = fct_rev(as.factor(year)), y = number, fill = fct_rev(country)),
  #         width = 0.3) +
  geom_chicklet(aes(x = fct_rev(as.factor(year)), y = number, fill = country), width = 0.3) +
  scale_fill_brewer(palette = "Set1")+
  #scale_fill_manual(values = c(blue, purple, red)) +
  guides(fill = F) +
  geom_text(data = filter(heritage_long, year == "2022"),
            aes(x = 0.5, y = number, label = country),
            nudge_x = 0.25,
            nudge_y = c(19.1, 7.5, -13), # Norway, Denmark, Sweden
            family = main_font,
            size = 8) +
  geom_text(data = filter(heritage_long, year == "2022") |> arrange(desc(number)),
            aes(x = 0.5, y = number, label = number),
            nudge_x = 0.5,
            nudge_y = c(-14, 6, 17.8),
            family = main_font,
            colour = bg_col,
            size = 7) +
  geom_text(data = filter(heritage_long, year == "2004") |> arrange(desc(number)),
            aes(x = 1.5, y = number, label = number),
            nudge_x = 0.5,
            nudge_y = c(-12, 12.7, 9.7),
            family = main_font,
            colour = bg_col,
            size = 7) +
  geom_text(data = filter(heritage_long, year == "2004") |> arrange(desc(number)),
            aes(x = 1.0, y = number, label = pct_change),
            nudge_x = 0.5,
            nudge_y = c(-6, 19, 13),
            family = main_font,
            colour = pal_SDN,
            size = 6) +
  geom_rect(aes(xmin = 1.43, xmax = 1.57, ymin = 4.75, ymax = 9.3),
            colour = pal_SDN[1], alpha = 0) +
  geom_rect(aes(xmin = 1.43, xmax = 1.57, ymin = 22.4, ymax = 25.8),
            colour = pal_SDN[2], alpha = 0) +
  geom_rect(aes(xmin = 1.43, xmax = 1.57, ymin = 15.2, ymax = 18.8),
            colour = pal_SDN[3], alpha = 0) +
  geom_line(data = data.frame(x = c(1.14, 1.86), y = c(0.05, 0.05)),
            aes(x = x, y = y),
            colour = pal_SDN[1], linewidth = 0.6) +
  geom_line(data = data.frame(x = c(1.14, 1.86), y = c(15.0, 12.95)),
            aes(x = x, y = y),
            colour = pal_SDN[1], linewidth = 0.6) +
  geom_line(data = data.frame(x = c(1.14, 1.86), y = c(25.0, 16.9)),
            aes(x = x, y = y),
            colour = pal_SDN[3], linewidth = 0.6) +
  geom_line(data = data.frame(x = c(1.14, 1.86), y = c(32.9, 21.9)),
            aes(x = x, y = y),
            colour = pal_SDN[2], linewidth = 0.6) +
  labs(title = title,
       subtitle = subtitle,
    caption = caption) +
  coord_flip() +
  main_theme
  
main_plot
  
ggsave(file = "Bar_plot.png", plot = main_plot, 
         width = 254, height = 190, units = "mm",
         dpi = 100)
