# Import library-----------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(plyr)
library(showtext)
library(ggtext)
library(ggcorrplot)
library(colourvalues)
library(ggplot2)
library(reshape2)

# Fonts--------------------------------------------------
font_add_google("Oswald", "oswald")
sysfonts::font_add(family = "Font Awesome 6 Brands", # Social media icon fonts
                   regular = "C:/R/Rwd/fonts/Font-Awesome-6-Brands-Regular-400.otf")
showtext_auto()

# Colors -------------------------------------------------
bg_col <- "#eeeeee"
text_col <- "grey10"
# Data Import---------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2023-10-17')
taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

# Data Manipulation----------------------------------------
attributes <- dplyr::select(taylor_all_songs, danceability, energy, loudness, speechiness, 
                            acousticness, instrumentalness, liveness, valence, tempo)
danceability_noNA <- attributes$danceability |>as_tibble()
danceability_noNA <- danceability_noNA[complete.cases(danceability_noNA),]
colnames(danceability_noNA) <- "danceability"
## Texts-----------------------------------------------------
title1 = "Danceability Distribution in Taylor Swift's Songs"
caption1 = paste0("The Spotify Danceability Score is an index that measures how suitable a track is for dancing. 
                  The scores are provided by Spotify.<br>",
                  "**Data**: Taylor Swift Song Dataset (Curated by W. Jake Thompson)<br>**Graphic**: Tope ",
                  social_caption)

title2 = "Exploring Attribute Links in Taylor's Melodies"
subtitle2 = "Correlation Matrix between Attributes in Taylor Swift's Songs"
caption2 = paste0("Spearman's rank correlation coefficient is used above.<br>",
  "**Data**: Taylor Swift Song Dataset (Curated by W. Jake Thompson)<br>**Graphic**: Tope ", social_caption
)
#social
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

## Plot -----------------------------------------------------
# Plot1: Danceability Distribution
taylor_dance_dist <- ggplot(data = danceability_noNA) +
  geom_histogram(aes(x=danceability), 
                 fill = "#56B4E9", color = "#56B4E9", alpha = 0.6, binwidth = 0.02)+
  labs(title = title1,
       caption = caption1,
       x= "Spotify Danceability Score",
       y = "Number of Songs")+
  theme_void(base_size = 30, base_family = "oswald")+
  theme(
    plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
    plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.grid.major = element_line(color = 'grey'),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      colour= text_col,
      face = "bold",
      family = "oswald",
      lineheight = 0.5,
      size = 50,
      margin = margin(b = 10, t = 2, l = 20) # plot title 기본 위치를 기준으로 변경.
    ),
    plot.caption = element_textbox_simple(
      colour= text_col,
      lineheight = 0.5,
      family = "oswald",
      margin = margin(t = 13, b = 5, l = 20), # plot caption 기본 위치를 기준으로 변경.
      size = 30
    ),
    axis.text.x = element_text(face="bold", size = 20),
    axis.text.y = element_text(face="bold",size = 20),
    axis.title.x = element_text(size = base_size + 15 ,margin= margin(t=6)),
    axis.title.y = element_text(size = base_size + 15, angle = 90, margin = margin(r = 10))
  )
ggsave(file="Taylor_dance_dist(2023-10-16).png", plot = taylor_dance_dist, width = 2400 ,height= 1600, 
       units = 'px')


# Plot2: Correlation Matrix
corr_matrix <- cor(attributes, use="complete.obs", # "complete.obs": discard the entire row when NA is present.
                   method = "spearman")            # Nonparametric correlation analysis

taylor_corr_full <- ggcorrplot(corr_matrix, method="circle", type= "upper",
                          hc.order = T,
                          tl.col = "black",
                          outline.color='black',
                          colors=c("#4A6FE3","#E2E2E2","#D33F6A"),
                          ) +
guides(fill = guide_legend(title = "Correlation Coefficient"))+
labs(title = title2, subtitle = subtitle2, caption = caption2, x = "Attributes", y = "Attributes") +
theme_void(base_size = 30, base_family = "oswald") +
theme(
  axis.text.x = element_text(margin=margin(-2,0,0,0), family = "oswald", angle = 45),
  axis.text.y = element_text(margin=margin(l = 5, r =  5), family = "oswald"),
  plot.title = element_textbox_simple(
                          colour= text_col,
                          face = "bold",
                          family = "oswald",
                          lineheight = 0.5,
                          size = 40, # for png, size = 35. svg: 25
                          margin = margin(b = 2, t = 3, r = 5, l = 5) # plot title 기본 위치를 기준으로 변경.
                        ),
  plot.subtitle = element_textbox_simple(
                colour = text_col,
                family = "oswald",
                size = 30,
                margin = margin(b = 2, t = 5, r = 5, l = 5)
              ),
  plot.caption = element_textbox_simple(
                colour= text_col,
                lineheight = 0.5,
                family = "oswald",
                margin = margin(t = 13, b = 5, l = 5), # plot caption 기본 위치를 기준으로 변경.
                size = 25),
  panel.grid.major = element_line(color = 'white'),
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  legend.margin = margin(r = 5)
              )

ggsave(file="Taylor_corr(2023-10-16).png", plot = taylor_corr_full, width = 1800, height= 1800, 
       units = 'px')
