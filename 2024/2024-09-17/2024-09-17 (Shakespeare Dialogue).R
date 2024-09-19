## Load Packages ---------------------------
library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(showtext)
library(ggrepel)
library(cowplot)
library(camcorder)


## Font and Colors ---------------------------
font_add_google("Oswald", "Oswald")
main_font <- "Oswald"
showtext_auto()

bg_col <- "white" # "#eeeeee"
text_col <- "grey10"
major_grid_col <- "#bebebe"
minor_grid_col <- "#d6d6d6"

ham_col <- c(RColorBrewer::brewer.pal(n = 6, "Set1"), "#bebebe")
mac_col <- c(RColorBrewer::brewer.pal(n = 5, "Set2"), "#bebebe")
romeo_col <- c(RColorBrewer::brewer.pal(n = 5, "Set3"), "#bebebe")

caption <- paste(socialcap::socialcap(gitname = "gaba-tope", twitname = "tope_ezia", textfont = "Oswald"),
                 "<br><br> Shapekspeare Dialogue Dataset from shakespeare.mit.edu, curated by Nicola Rennie.")

## Import Data ---------------------------
tuesdata <- tidytuesdayR::tt_load('2024-09-17')
hamlet <- tuesdata$hamlet
macbeth <- tuesdata$macbeth
romeo_juliet <- tuesdata$romeo_juliet

## Data Wrangling ---------------------------
hamlet <- hamlet |>  
  mutate_at("character", as.factor)
macbeth <- macbeth |> 
  mutate_at("character", as.factor)
romeo_juliet <- romeo_juliet |> 
  mutate_at("character", as.factor)
# hamlet ----
hamlet_df <- hamlet |> 
  count(character) |> 
  mutate(prop = prop.table(n))

ham_count <- hamlet_df |> 
  top_n(prop, n = 5) |> 
  mutate(label = paste(character, ":", formatC(prop * 100, digits = 2, format = "f"), "%"),
         rad = 4) |> 
  arrange(desc(n))

ham_uncount <- hamlet_df |>
  mutate(label = paste(character, ":", formatC(prop * 100, digits = 2, format = "f"), "%"),
         rad = 4) |> 
  arrange(desc(n)) |> 
  dplyr::filter(!(character %in% ham_count$character))

ham_etc <- tibble(character = as.factor("others"),
                  n = sum(ham_uncount$n),
                  prop = sum(ham_uncount$prop),
                  label = paste("Others:", formatC(sum(ham_uncount$prop) * 100, digits = 2, format = "f"),
                                "%"),
                  rad = 4)

ham_order <- ham_count |> 
  select(character) |> 
  mutate_at("character", as.character) |> 
  unlist() 
ham_order[length(ham_order) + 1] <- "others"

ham_count_whole <- rbind(ham_count, ham_etc) |> 
  mutate(ord = c(1:7)) |> 
  arrange(desc(ord)) |> 
  mutate(text_y = cumsum(prop)-prop/2) # position of label


# Macbeth ---
mac_df <- macbeth |> 
  count(character) |> 
  mutate(prop = prop.table(n))

mac_count <- mac_df |> 
  top_n(prop, n = 5) |> 
  mutate(label = paste(character, ":", formatC(prop * 100, digits = 2, format = "f"), "%"),
         rad = 4) |>
  arrange(desc(n))

mac_uncount <- mac_df |>
  mutate(label = paste(character, ":", formatC(prop * 100, digits = 2, format = "f"), "%"),
         rad = 4) |> 
  arrange(desc(n)) |> 
  dplyr::filter(!(character %in% mac_count$character))

mac_etc <- tibble(character = as.factor("others"),
                  n = sum(mac_uncount$n),
                  prop = sum(mac_uncount$prop),
                  label = "Others",
                  rad = 4)

mac_order <- mac_count |> 
  select(character) |> 
  mutate_at("character", as.character) |> 
  unlist()
mac_order[length(mac_order) + 1] <- "others"

mac_count_whole <- rbind(mac_count, mac_etc) |> 
  mutate(ord = c(1:6)) |> 
  arrange(desc(ord)) |> 
  mutate(text_y = cumsum(prop)-prop/2) # position of label

romeo_df <- romeo_juliet |> 
  count(character) |>
  mutate(prop = prop.table(n))

romeo_count <- romeo_df |> 
  top_n(prop, n = 5) |> 
  mutate(label = paste(character, ":", formatC(prop * 100, digits = 2, format = "f"), "%"),
         rad = 4) |>
  arrange(desc(n))

romeo_uncount <- romeo_df |>
  mutate(label = paste(character, ":", formatC(prop * 100, digits = 2, format = "f"), "%"),
         rad = 4) |> 
  arrange(desc(n)) |> 
  dplyr::filter(!(character %in% romeo_count$character))

romeo_etc <- tibble(character = as.factor("others"),
                  n = sum(romeo_uncount$n),
                  prop = sum(romeo_uncount$prop),
                  label = "Others",
                  rad = 4)

romeo_order <- romeo_count |> 
  select(character) |> 
  mutate_at("character", as.character) |> 
  unlist()
romeo_order[length(romeo_order) + 1] <- "others"

romeo_count_whole <- rbind(romeo_count, romeo_etc) |> 
  mutate(ord = c(1:6)) |> 
  arrange(desc(ord)) |> 
  mutate(text_y = cumsum(prop)-prop/2) # position of label

## Plot Themes ---------------------------
main_theme <- theme(
  plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
  plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_textbox_simple(
    colour = text_col,
    face = "bold",
    family = main_font,
    lineheight = 0.5,
    size = 15, halign = 0.5,
    margin = margin(b = 10, t = 2) 
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
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  legend.background = element_rect(fill = bg_col, colour = bg_col),
  legend.title = element_text(family = main_font,
                              size = 20,
                              colour = text_col),
  legend.text = element_text(family = main_font,
                             size = 15,
                             colour = text_col)
  
  
)

## Plots ---------------------------
gg_record(
  dir = file.path("recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 1000, # width of saved image
  height = 1000, # height of saved image
  units = "px", # units for width and height
  dpi = 200 # dpi to use when saving image
)

ham_count_plot <- ggplot() +
  geom_col(data = ham_count_whole,
           aes(x = rad, y = prop, fill = fct_relevel(character, ham_order)),
           color = "black"
           ) +
  geom_label_repel(data = ham_count_whole, aes(x = rad, y = text_y, label = label),
                   force_pull = 100, nudge_x = 0.3, family = main_font
                   ) +
  scale_fill_manual(values = ham_col) +
  scale_x_continuous(limits = c(3.0, 4.5)) +
  guides(fill = "none") +
  coord_polar(theta = "y") +
  labs(title = "Hamlet") +
  main_theme

ham_count_plot  

mac_count_plot <- ggplot() +
  geom_col(data = mac_count_whole,
           aes(x = rad, y = prop, fill = fct_relevel(character, mac_order)),
           color = "black"
  ) +
  geom_label_repel(data = mac_count_whole, aes(x = rad, y = text_y, label = label),
                   force_pull = 100, nudge_x = 0.3, family = main_font
  ) +
  scale_fill_manual(values = mac_col) +
  scale_x_continuous(limits = c(3.0, 4.5)) +
  guides(fill = "none") +
  coord_polar(theta = "y") +
  labs(title = "Macbeth") +
  main_theme

mac_count_plot

romeo_count_plot <- ggplot() +
  geom_col(data = romeo_count_whole,
           aes(x = rad, y = prop, fill = fct_relevel(character, romeo_order)),
           color = "black"
  ) +
  geom_label_repel(data = romeo_count_whole, aes(x = rad, y = text_y, label = label),
                   force_pull = 100, nudge_x = 0.3, family = main_font
  ) +
  scale_fill_manual(values = romeo_col) +
  scale_x_continuous(limits = c(3.0, 4.5)) +
  guides(fill = "none") +
  coord_polar(theta = "y") +
  labs(title = "Romeo and Juliet") +
  main_theme

romeo_count_plot

# Total Plot ---
title_gg <- ggplot() + 
  labs(title = "Who spoke the most?", subtitle = "Dialogue Analysis of Shakespeare's Three Works") +
  theme(plot.margin = margin(0, 0, 0, 7),
        plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        plot.title = element_textbox_simple(
          colour = text_col,
          face = "bold",
          family = main_font,
          lineheight = 0.5,
          size = 20, hjust = 0.5,
          margin = margin(b = 10, t = 2) 
        ),
        plot.subtitle = element_textbox_simple(
          colour = text_col,
          family = main_font,
          lineheight = 0.5,
          size = 15,
          margin = margin(b = 2, t = 2) 
        )
  )

caption_gg <- ggplot() +
  labs(caption = caption) + 
  theme(plot.caption = element_textbox_simple(
          colour= text_col,
          lineheight = 0.5,
          family = main_font,
          margin = margin(t = 13, b = 5, l = 5),
          size = 10
        ))

gg_resize_film(
  height = 1000,
  width = 1800,
  units = "px",
  dpi = 200
)

gridded_plot <- plot_grid(ham_count_plot, mac_count_plot, romeo_count_plot, nrow = 1)

total_plot <- plot_grid(title_gg,  gridded_plot, caption_gg, ncol = 1,
                        rel_heights = c(0.2, 1, 0.2))

total_plot

# save gif
gg_playback(
  name = file.path("20240917_longer.gif"),
  first_image_duration = 5,
  last_image_duration = 20,
  image_resize = 800,
  frame_duration = 1,
  background = "white"
)

## Save Plots ----
ggsave(filename = "Ham_plot.svg", plot = ham_count_plot, 
       device = "svg", width = 1000, height = 1000, units = "px", dpi = 200)

ggsave(filename = "Mac_plot.svg", plot = mac_count_plot, 
       device = "svg", width = 1000, height = 1000, units = "px", dpi = 200)
ggsave(filename = "romeo_plot.svg", plot = romeo_count_plot,
       device = "svg", width = 1000, height = 1000, units = "px", dpi = 200)
ggsave(filename = "total_plot.svg", plot = total_plot,
       device = "svg", width = 3600, height = 1500, units = "px", dpi = 300)

rsvg::rsvg_png("total_plot.svg", "total_plot_png", width = 3500, height = 1800)
