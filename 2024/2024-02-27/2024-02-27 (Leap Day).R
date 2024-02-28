## Import library-----------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(ggplot2)
library(ggrepel)
library(rcartocolor)
library(plotly)
library(socialcap) #remotes::install_github("gaba-tope/socialcap")


## Data Import---------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2024-02-27')


## Fonts & Colors ---------------------------------------------------
font_add_google(name = "Oswald", family = "oswald")
showtext_auto()

main_font <- "oswald"
bg_col <- "#eeeeee"
text_col <- "grey10"
major_grid_col <- "#bebebe"
minor_grid_col <- "#d6d6d6"
cols_vec <- carto_pal(n = 5, name = "Purp")
cols_vec2 <- carto_pal(n = 9, name = "Purp")
cols_vec_ly <- c(cols_vec2[1:3], cols_vec2[7], cols_vec2[9])


## Data Wrangling -----------------------------------------
events <- tuesdata$events
births <- tuesdata$births
deaths <- tuesdata$deaths

events_data <- events |>
  group_by(year) |> 
  mutate(num = row_number())

events_data <- events_data |> 
  rowwise() |> 
  mutate(year_int = cut(year, breaks = seq(870, 2020, by = 25), labels = FALSE, include.lowest = TRUE) |> 
           as.factor()
         ) |> 
  filter(year >= 1900)


events_data <- events_data |> 
  rowwise() |> 
  mutate(tooltip = glue::glue("<b>In {year}:</b><br>{event}") |> 
           strwrap(width = 50) |> 
           paste(collapse = "<br>")) |> 
  mutate(year_ano = as.character(year))

#strwrap(events_data$tooltip[[1]], width = 50) |> paste(collapse = "<br>")

  
## Texts --------------------------------------------------
social_caption <- socialcap(gitname = "gaba-tope", twitname = "@tope_ezia")
title_event <-  "What Happened in Februrary 29?"
title_event_ly <- "<b>The Leap Day Timeline: </b>"
subtitle_event <- "from 1900 to 2020"
subtitle_event_ly <- "What Happened on Februrary 29 (1900 ~ 2020)"
caption <- paste0("Data : February 29 (Wikipedia)<br>Graphic : ", social_caption)
caption_ly <- paste0("Data : February 29 (Wikipedia)<br>Graphic : Tope (@tope_ezia)")

## Themes -------------------------------------------------
main_theme <- theme(
  plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
  plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.grid.major.x = element_line(color = major_grid_col),
  panel.grid.major.y = element_blank(),
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
  axis.title.y = element_blank(),
  axis.text.x = element_text(family = main_font,
                             size = 15,
                             colour = text_col,
                             angle = 45,
                             margin = margin(t = 2)),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)



# Plot1 : Event, time-series plot -----------------------------------------
main_data <- events_data
main_data

event_plot <- ggplot() + 
  geom_point(data = main_data,
             aes(x = year, y = num, fill = year_int),
             shape = 21, size = 5
             ) +
  scale_fill_manual(values = cols_vec) +
  guides(fill = "none") + 
  geom_abline(slope = 0, intercept = 1) +
  geom_text(data = filter(main_data, num == 1),
            aes(x = year, y = num, label = year),
            angle = 90, nudge_y = -0.3,
            family = main_font) +
  scale_x_continuous(limits = c(1895, 2030),
                     breaks = seq(1900, 2030, by = 25)) +
  scale_y_continuous(limits = c(0, 4)) +
  labs(title = title_event,
       subtitle = subtitle_event,
       cap = caption,
       x = "Year") +
  main_theme

event_plot
ggsave(plot = event_plot, 
       filename = "event_plot.svg",
       dpi = 300,
       width = 2500, height = 2000, unit = "px" )

rsvg::rsvg_png(
  "event_plot.svg", "event_plot.png",
  width = 2500, height = 2000
)


# Plot2 : Plotly Interactive Plot --------------
main_data <- events_data

# To add horizontal line in plotly
hline <- function(y = 1, color = text_col) {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}


event_plotly <- plot_ly() |> 
  add_markers(data = main_data,
            type = "scatter", mode = "markers",
            x = ~year,
            y = ~num,
            text = ~tooltip,
            marker = list(size = 20,
                          line = list(
                            color = "black",
                            width = 1
                          )),
            color = ~year_int,
            colors = cols_vec_ly,
            hoverinfo = "text",
            showlegend = F,
            hoverlabel = list(bgcolor = "black",
                              bordercolor = "transparent",
                              font = list(family = "oswald",
                                          size = 15,
                                          color = "white"
                                          ),
                              align = "left"
                              )
  ) |> 
  add_annotations(data = filter(main_data, num == 1),
                  x = ~year,
                  y = ~(num - 0.2),
                  text = ~year,
                  font = list(family = main_font),
                  textangle = -90,
                  showarrow = F,
                  textposition = "bottom center"
  ) |> 
  add_annotations(xref = "paper",
                  yref = "paper",
                  x = -0.07, y = 1.09,
                  showarrow = FALSE,
                  text = subtitle_event_ly,
                  font = list(family = main_font,
                              color = text_col,
                              size = 20
                              )
  ) |> 
  add_annotations(xref = "paper",
                  yref = "paper",
                  x = -0.07, y = -0.2,
                  showarrow = FALSE,
                  text = caption_ly,
                  align = "left",
                  font = list(family = main_font,
                              color = text_col,
                              size = 15
                              )
  ) |> 
  layout(plot_bgcolor = bg_col,
         paper_bgcolor = bg_col,
         title = list(text = title_event_ly, 
                      x = 0.02, y = 0.97,
                      font = list(family = main_font,
                                  color = text_col,
                                  size = 30)),
         xaxis = list(
            zerolinecolor = text_col,
            zerolinewidth = 3,
            nticks = 6,
            range = c(1895, 2025),
            tick0 = 1900, 
            dtick = 25, 
            tickmode = "linear",
            gridcolor = major_grid_col,
            title = list(text = "<b>Year</b>", 
                         font = list(family = main_font,
                                     color = text_col,
                                     size = 15)),
            tickfont = list(family = main_font,
                            color = text_col)
            ),
         yaxis = list(
           title = "",
           zeroline = FALSE,
           showline = FALSE,
           showticklabels = FALSE,
           showgrid = FALSE,
           range = c(-0.03, 3.5)
         ),
         shapes = list(hline(y = 1)),
         autosize = TRUE,
         margin = list(b = 80, t = 70, r = 10)
         )
event_plotly

htmlwidgets::saveWidget(event_plotly, file = "event_plotly.html") 

event_plotly |> class()
