## Import library-----------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(plyr)
library(showtext)
library(ggtext)
library(ggplot2)
library(reshape2)
library(wordcloud2) #devtools::install_github("lchiffon/wordcloud2")
library(tm) 
library(webshot)
library(htmlwidgets)

## Data Import---------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2023-11-28')
drwho_episodes <- tuesdata$drwho_episodes
drwho_directors <- tuesdata$drwho_directors
drwho_writers <- tuesdata$drwho_writers

## Data Wrangling -----------------------------------------
drwho_target <- drwho_episodes[, c(5, 6, 8, 10, 11)] # episode_number, episode_title, first_aired, uk_viewers, rating
# by rating
drwho_rating <- drwho_episodes[,c(6, 11)] |> filter(rating >= 80) |> arrange(desc(rating))
# by viewers
drwho_viewer <- drwho_episodes[,c(6, 10)]  |> arrange(desc(uk_viewers))
viewer_percent <- quantile(drwho_viewer$uk_viewers, probs = c(.25, .5, .75, .9))

drwho_viewer_90th <- filter(drwho_viewer, 
                                    uk_viewers >= viewer_percent[[4]]) # >= 90th percentile (Upper 10%)
drwho_viewer_90th_sorted <- drwho_viewer_90th |> arrange(desc(uk_viewers))

# Word Cloud with Episode Titles
Ep_title <- drwho_viewer
title_corpus <- Corpus(VectorSource(Ep_title))

title_corpus <- title_corpus |> tm_map(removeNumbers) |> tm_map(removePunctuation) |> tm_map(stripWhitespace)
title_corpus <- tm_map(title_corpus, content_transformer(tolower)) # Convert all letter to lowercase 
title_corpus <- tm_map(title_corpus, removeWords, stopwords("english")) # Remove stopwords (e.g. a, the, is)
                       
term_matrix <- TermDocumentMatrix(title_corpus) 
matrix <- as.matrix(term_matrix) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_episode <- data.frame(word = names(words),freq=words)                     
                       
## Fonts---------------------------------------------------
font_add_google("Oswald", "oswald")
sysfonts::font_add(family = "Font Awesome 6 Brands", # Social media icon fonts
                   regular = "C:/R/Rwd/fonts/Font-Awesome-6-Brands-Regular-400.otf")
showtext_auto()

## Colors -------------------------------------------------
bg_col <- "#eeeeee"
text_col <- "grey10"
blue <- "#1c64d7"
tardis_palette <- c("#00060b", "#00203c", "#003b6f", "#6f8ea9", "#a6b8c7") # "TARDIS - Doctor Who Color Palette" from "https://www.color-hex.com/color-palette/16374"
tardis_bar_palette <- rev(c("#00060b", "#00203c", "#003b6f", "#6f8ea9", rep("#a6b8c7",14)))
cloud_bg <- "#f6f7f9"

## Texts --------------------------------------------------
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

title <- "Exploring Episode Highlights with TARDIS"
subtitle <- "The Most-Watched Episodes in the Top 10th Percentile"

cap_drwho <- paste0(
    "**Data**: Doctor Who Episodes Dataset <br>(Compiled by Jonathan Kitt from Wikipedia)<br>**Graphic**: Tope ", social_caption
  )

## Plots --------------------------------------------------
base_size <- 30
# Plot by viewers
drwho_viewer_90th_sorted$episode_title <- as.factor(drwho_viewer_90th_sorted$episode_title)  
drwho_viewer_90th_sorted_factored <- drwho_viewer_90th_sorted |> arrange(uk_viewers)|>
  mutate(episode_title = factor(episode_title, levels = unique(episode_title)))

drwho_view_plot <- ggplot(data = drwho_viewer_90th_sorted_factored)+
  geom_bar(aes(x = uk_viewers, y = episode_title, fill = uk_viewers), stat = 'identity', width = 1) +
  #scale_fill_manual(values = tardis_bar_palette, guide = FALSE)+
  scale_fill_gradient(high = "#003b6f", low = "#a6b8c7", guide = FALSE)+
  scale_x_continuous(
    limits = c(0, 15),
    breaks = seq(0, 14, by = 2.5), 
  ) +
  labs(title = title,
       subtitle = subtitle,
       x = "Viewers in U.K. (millions)",
       y = "Episodes", 
       caption = cap_drwho) +
  geom_text(
    data = drwho_viewer_90th_sorted_factored[10:18, ],
    aes(0, y = episode_title, label = episode_title),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "oswald",
    size = 8
  )+
  geom_text(
    data = drwho_viewer_90th_sorted_factored[1:9, ],
    aes(0, y = episode_title, label = episode_title),
    hjust = 0,
    nudge_x = 0.3,
    colour = "#00060b",
    family = "oswald",
    size = 8
  )+
  theme_void(base_size = base_size, base_family = "oswald")+
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
      size = 40,
      margin = margin(b = 2, t = 2) # plot title 기본 위치를 기준으로 변경.
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      family = "oswald",
      size = 25,
      margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(
      colour= text_col,
      lineheight = 0.5,
      family = "oswald",
      margin = margin(t = 13, b = 5, l = 5), # plot caption 기본 위치를 기준으로 변경.
      size = base_size - 10
    ),
    axis.text.x = element_text(size = 15),
    #axis.text.y = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = base_size, margin = margin(t=6)),
    axis.title.y = element_text(size = base_size, angle = 90, margin = margin(r = 10))
  )
ggsave(file="Episode_viewer.png", plot = drwho_view_plot, width = 2400, height= 1600, 
       units = 'px')

# Word Cloud
# webshot:: install_phantomjs()
episode_cloud <- wordcloud2(data = df_episode, size= 0.7, color = tardis_palette,
                            backgroundColor = cloud_bg, fontFamily = "oswald", widgetsize = c(800, 800))

episode_cloud_tardis <- wordcloud2(data = df_episode, size = 0.7, figPath = "tardis.png",
                                   color = "#003b6f", backgroundColor = cloud_bg, fontFamily = "oswald")
# Unable to export the 'episode_cloud_tardis' with {webshot}.
# Instead, 'episode_cloud_tardis' shown in 'Viewer'tab of RStudio was captured and named "episode_cloud_original.png"
