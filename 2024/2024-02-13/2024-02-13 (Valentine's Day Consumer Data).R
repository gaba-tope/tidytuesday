## Import library-----------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(plyr)
library(showtext)
library(ggtext)
library(ggplot2)
library(cowplot)
library(patchwork)
library(waffle)   #remotes::install_github("hrbrmstr/waffle")
library(socialcap)

## Data Import---------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2024-02-13')

# Not part of Tidytuesday. From original dataset from Kaggle (https://www.kaggle.com/datasets/infinator/happy-valentines-day-2022?resource=download)
point <- readxl::read_xlsx("dataset_Kaggle/Valentines Day.xlsx")

## Fonts---------------------------------------------------
font_add_google(name = "Oswald", family = "oswald")
sysfonts::font_add(family = "font-awesome", regular = "Font Awesome 6 Free-Solid-900.otf")
showtext_auto()

main_font <- "oswald"

## Colors -------------------------------------------------
bg_col <- "#eeeeee"
text_col <- "grey10"
major_grid_col <- "#bebebe"
minor_grid_col <- "#d6d6d6"
red <- "#ae4544"
orange <- "#E69F00" 

## Data Wrangling -----------------------------------------
historical <- tuesdata$historical_spending
gifts_age <- tuesdata$gifts_age
gifts_gender <- tuesdata$gifts_gender

historical$Year <- as.factor(historical$Year)
historical_longer <- pivot_longer(historical, cols = -Year, names_to = "Group", values_to = "Amount")
historical_longer

point_longer <- pivot_longer(point, cols = -Point, names_to = "Gender", values_to = "Pct") |> 
  mutate(Pct_100 = Pct * 100)


## Texts --------------------------------------------------
social_caption <- socialcap(gitname = "gaba-tope", twitname = "@tope_ezia")
title_type_line <- "Average Valentine's Day Spending Across Gift Types"

title_spend_line <- "Average Spending on Valentine's Day Gift"

title_propor_line <- "The Percentage of People Celebrating Valentine's Day (2010-2022)"

title_point_bar <- "What is the point of valentine's day?"
caption <- paste0("Data : Valentine's Day Consumer Data (U.S. National Retail Federation, organiazed by Suraj Das for a Kaggle dataset)<br>Graphic : ", social_caption)

## Themes -------------------------------------------------
main_theme <- theme(
  plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
  plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = major_grid_col),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_textbox_simple(
    colour= text_col,
    face = "bold",
    family = main_font,
    lineheight = 0.5,
    size = 27,
    margin = margin(b = 5, t = 2) 
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
  axis.title.x = element_text(family = main_font,
                            size = 20,
                            colour = text_col),
  axis.title.y = element_text(family = main_font,
                              size = 20,
                              colour = text_col,
                              margin = margin(r = 3)),
  axis.text.x = element_text(family = main_font,
                             size = 15,
                             colour = text_col,
                             angle = 45,
                             margin = margin(t = 2)),
  axis.text.y = element_text(family = main_font,
                             size = 15,
                             colour = text_col),
  legend.background = element_rect(fill = bg_col),
  legend.key = element_rect(fill = bg_col,
                            size = 20),
  legend.text = element_text(family = main_font,
                             size = 20),
  legend.title = element_text(family = main_font, 
                              size = 20)
)

patch_theme <- theme( 
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  plot.title = element_textbox_simple(
    colour = text_col,
    fill = bg_col,
    face = "bold",
    family = main_font,
    lineheight = 0.5,
    size = 30,
    margin = margin(b = 5, t = 2) 
  ),
  plot.caption = element_textbox_simple(
    colour = text_col,
    fill = bg_col,
    lineheight = 0.5,
    family = main_font,
    margin = margin(t = 13, b = 5, l = 5),
    size = 20
  )
)

# Plot 1: Historical Trend -Expenditure per Type of Gifts ---------------
historical_type_line <- ggplot() + 
  geom_line(data = filter(historical_longer, Group != "PerPerson" & Group != "PercentCelebrating"),
            aes(x = Year, y = Amount, group = Group, color = Group),
            stat = "identity", linewidth = 1) +
  scale_color_brewer(palette = "Dark2",
                     labels = c("Candy", "Clothing", "Evening Out", "Flowers",
                                "Gift Cards", "Greeting Cards", "Jewelry")
                     ) +
  guides() +
  labs(title = title_type_line,
       caption = caption,
       y = "Spending ($)") +
  main_theme

historical_type_line
ggsave("historical_type_line.png", plot = historical_type_line, 
       width = 270, height = 190, units = "mm",
       dpi = 100)

# Plot 2: Historical Trend - How total spending change over time --------------
historical_spend_line <- ggplot() + 
  geom_line(data = filter(historical_longer, Group == "PerPerson"),
            aes(x = Year, y = Amount, group = Group),
            color = orange, linewidth = 1.5) +
  geom_point(data = filter(historical_longer, Group == "PerPerson"),
             aes(x = Year, y = Amount), 
             shape = 22, fill = text_col) +
  geom_label(data = filter(historical_longer, Group == "PerPerson"),
             aes(x = Year, y = Amount, label = paste0("$", Amount)), nudge_y = 5) +
  guides(color = F) +
  labs(title = title_spend_line,
       caption = caption, y = "Spending ($)") +
  main_theme

historical_spend_line
ggsave("historical_spend_line.png", plot = historical_spend_line, 
       width = 270, height = 190, units = "mm",
       dpi = 100)

# Plot 3: Historical Trend - Proportion of those celebrating VD --------------
historical_propor_line <-  ggplot() + 
  geom_line(data = filter(historical_longer, Group == "PercentCelebrating"),
            aes(x = Year, y = Amount, group = Group),
            color = red, linewidth = 1.5) +
  geom_point(data = filter(historical_longer, Group == "PercentCelebrating"),
             aes(x = Year, y = Amount), 
             shape = 22, fill = text_col) +
  geom_label(data = filter(historical_longer, Group == "PercentCelebrating"),
             aes(x = Year, y = Amount, label = paste0(Amount, "%")), nudge_y = 0.5) +
  guides(color = F) +
  labs(title = title_propor_line,
       caption = caption,
       y = "Percentage (%)") +
  main_theme

historical_propor_line
ggsave("historical_propor_line.png", plot = historical_propor_line, 
       width = 270, height = 190, units = "mm",
       dpi = 100)

# Plot 4: Point of Valentine's Day - Bar Plot ------------
point_m <- filter(point_longer, Gender == "Men") |> select(c(Point, Gender, Pct_100)) 
point_m$Point <- ordered(point_m$Point, levels = c("Show how much you care", "Sex", "To go out for dinner",
                                                   "To impress someone", "To receive gifts from someone",
                                                       "To treat yourself", "To buy someone gifts" ))
point_m <- point_m |> mutate(Pct_pct = paste0(Pct_100, "%")) |> dplyr::arrange(Point) 

point_w <- filter(point_longer, Gender == "Women") |> select(c(Point, Gender, Pct_100))
point_w$Point <- ordered(point_w$Point, levels = c("Show how much you care", "To treat yourself",
                                                   "To buy someone gifts", "Sex", "To go out for dinner",
                                                    "To receive gifts from someone","To impress someone"))
point_w <- point_w |> mutate(Pct_pct = paste0(Pct_100, "%")) |> dplyr::arrange(Point)

point_bar_m <- ggplot() +
  geom_bar(data = point_m,
           aes(x = Gender, y = Pct_100, fill = Point),
           position = "dodge",
           stat = "identity") + 
  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(0, 50, by = 10)) +
  scale_fill_manual(values = c("#d96e87", "#990000", "#56B4E9", "#ff9933", "#009E73", "#694732", "#D55E00")
                    ) +
  geom_label(data = point_m[1:3, ],
             aes(x = Gender, y = Pct_100, label = paste0(Pct_100, "%")),
             nudge_x = c(-0.385, -0.255, -0.125),
             nudge_y = 2
             ) +
  guides(fill = F) +
  labs(x = "", y = "Percentage (%)") +
  main_theme

point_bar_w <- ggplot() +
  geom_bar(data = point_w,
           aes(x = Gender, y = Pct_100, fill = Point),
           position = "dodge",
           stat = "identity") +
  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(0, 50, by = 10)) +
  scale_fill_manual(values = c("#d96e87", "#694732", "#D55E00", "#990000","#56B4E9","#009E73", "#ff9933" )) +
  geom_label(data = point_w[1:3, ],
             aes(x = Gender, y = Pct_100, label = paste0(Pct_100, "%")),
             nudge_x = c(-0.385, -0.255, -0.125),
             nudge_y = 2
  ) +
  labs(x = "", y = "") +
  main_theme

point_bar <- point_bar_m + point_bar_w +
  plot_annotation(title = title_point_bar,
                  caption = caption,
                  theme = patch_theme)
point_bar

ggsave("Point_bar.png", plot = point_bar, 
       width = 300, height = 200, units = "mm",
       dpi = 120)


# Plot 5: Point of Valentine's Day - Waffle plot --------------------
point_waffle <- ggplot(data = point_longer, aes(fill = Point, values = Pct_100)) +
  geom_waffle(n_rows = 10,
              size = 0.33,
              colour = "white",
              filp = T) +
  facet_wrap(~Gender, ncol = 1) +
  scale_fill_brewer(palette = "Set1") +
  coord_equal()

point_m <- filter(point_longer, Gender == "Men") |> select(c(Point, Pct_100)) 
point_m <- point_m |> dplyr::arrange(desc(Pct_100))
point_m_five <- rbind(point_m[1:4, ],
                      data.frame(Point = "Others", Pct_100 = sum(point_m[5:7, 2]))
)

point_m_five$Point <- ordered(point_m_five$Point, levels = c("Others", "To impress someone", "To go out for dinner",
                                                             "Sex", "Show how much you care"))

point_w <- filter(point_longer, Gender == "Women") |> select(c(Point, Pct_100))

point_w <- point_w |> dplyr::arrange(desc(Pct_100))
point_w_five <- rbind(point_w[1:4, ],
                      data.frame(Point = "Others", Pct_100 = sum(point_w[5:7, 2]))
)
point_w_five$Point <- ordered(point_w_five$Point, levels = c("Others", "Sex", "To buy someone gifts", 
                                                              "To treat yourself", "Show how much you care")
                              )

point_m_waffle <- ggplot(data = point_m_five, aes(label = Point, values = Pct_100)) +
  geom_pictogram(n_rows = 15,
                 aes(colour = Point),
                 family = "font-awesome",
                 make_proportional = TRUE,
                 flip = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c( "pink","red", "skyblue", "black", "grey"),
    labels = c("Show how much you care","Sex", "To go out for dinner",
               "To impress someone", "Others")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("heart", "fire", "utensils", "hand-holding-heart", "asterisk"),
    labels = c("Show how much you care","Sex", "To go out for dinner",
               "To impress someone", "Others")
  ) +
  coord_equal() +
  theme_void()

point_m_waffle

point_w_waffle <- ggplot(data = point_w_five, aes(label = Point, values = Pct_100)) +
  geom_pictogram(n_rows = 15,
                 aes(colour = Point),
                 family = "font-awesome",
                 make_proportional = TRUE,
                 flip = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c("pink","brown", "black","red", "grey"),
    labels = c("Show how much you care", "To treat yourself", "To buy someone gifts", 
               "Sex", "Others")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("heart", "mug-hot", "gift", "fire", "asterisk"),
    labels = c("Show how much you care", "To treat yourself", "To buy someone gifts", 
               "Sex", "Others")
  ) +
  coord_equal() +
  theme_void()

point_w_waffle

