## Import library-----------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(ggplot2)
library(ggsci)
library(socialcap) #remotes::install_github("gaba-tope/socialcap")

## Data Import---------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2024-05-21')


## Fonts & Colors ---------------------------------------------------
font_add_google(name = "Oswald", family = "oswald")
showtext_auto()
main_font <- "oswald"
# Font dependency for plotly
oswald_file <- showtextdb::google_fonts("Oswald")$regular_url 
font_css <- paste0(
  "<style type = 'text/css'>",
  "@font-face { ",
  "font-family: 'oswald'; ", 
  "src: url('", oswald_file, "'); ", 
  "}",
  "</style>")

bg_col <- "#eeeeee"
text_col <- "grey10"
major_grid_col <- "#bebebe"
minor_grid_col <- "#d6d6d6"
cols_vec_ly <- pal_npg("nrc")(9)


## Data Wrangling -----------------------------------------
emissions <- tuesdata$emissions

total_emissions <- emissions |> 
  select(commodity, total_emissions_MtCO2e, year) |> 
  group_by(year, commodity) |> 
  mutate(total_e_year = sum(total_emissions_MtCO2e))

# total_emissions$commodity <- total_emissions$commodity |> as.factor()

ordered_com_list <- total_emissions |>
  filter(year == 2022) |> 
  select(total_e_year) |> 
  unique() |> 
  arrange(total_e_year)
  
ordered_com_vec <- ordered_com_list[["commodity"]]

# emission data: text column added for plotly
total_emissions_ly <- total_emissions |> 
    select(-total_emissions_MtCO2e) |> 
    mutate(tooltip = glue::glue("Year: {year}, Commodity: {commodity}<br>Total CO2 Emission: {total_e_year}")) |>
  group_by(year, commodity) |> 
  summarise(total_e_year = sum(total_e_year, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = commodity, values_from = total_e_year, values_fill = 0)



## Themes ---------------------------
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
  axis.title = element_text(family = main_font,
                              size = 15,
                              colour = text_col,
                              margin = margin(t = 5)),
  axis.text = element_text(family = main_font,
                             size = 10,
                             colour = text_col,
                             angle = 45,
                             margin = margin(t = 2)),
  legend.background = element_rect(fill = bg_col, colour = bg_col),
  legend.title = element_text(family = main_font,
                              size = 20,
                              colour = text_col),
  legend.text = element_text(family = main_font,
                             size = 15,
                             colour = text_col)
  
  
)


## Texts ---------------------------------------------------
title <- "Global CO2 Emission by commodities"
social <- socialcap(gitname = "gaba-tope", twitname = "@tope_ezia")
expl <- "Carbon Majors is a database of historical production data from 122 of the world’s largest oil,
  gas, coal, and cement producers, dating back to 1854. This data is used to quantify the
  direct operational emissions and emissions from the combustion of marketed products."
caption <- paste0("Data: Carbon Majors Emissions Data", "<br>Graphic: ", social, "<br><br>", expl)

caption_ly <- paste0("Data: Carbon Majors Emissions Data", "<br>Graphic: ", social)

## Plot
total_emission_plot <- ggplot(data = total_emissions) +
  geom_line(aes(x = year, y = total_e_year, color = fct_relevel(commodity, ordered_com_vec)),
            size = 0.7) +
  scale_x_continuous(breaks = seq(1860, 2020, by = 50)) +
  scale_color_npg() +
  guides(color = guide_legend(reverse = TRUE, title = "Commodities")) +
  labs(title = title, caption = caption,
       y = "Total CO2 Emission (MtCO2e)") +
  main_theme

total_emission_plot

ggsave(plot = total_emission_plot, 
       filename = "total_emission_plot.svg",
       dpi = 300,
       width = 2500, height = 2000, unit = "px" )

rsvg::rsvg_png(
  "total_emission_plot.svg", "total_emission_plot.png",
  width = 2500, height = 2000
)


## Plotly
library(plotly)
ordered_com_vec

total_e_ly <- plot_ly(data = total_emissions_ly,
        type = "scatter", mode = "lines",
        x = ~year, y = ~`Oil & NGL`, name = "Oil & NGL",
        line = list(color = cols_vec_ly[[1]], width = 2),
        hovertemplate = paste("<b>Year: </b>%{x}",
                              "<br><b>Emission: </b>%{y:,.1f}"),
        hoverlabel = list(bgcolor = "black",
                          bordercolor = "transparent",
                          font = list(family = "oswald",
                                      size = 15,
                                      color = "white"
                          ),
                          align= "left")
        ) |> 
  add_trace(y = ~`Bituminous Coal`, name = "Bituminous Coal",
            line = list(color = cols_vec_ly[[2]], width = 2)) |>
  add_trace(y = ~`Natural Gas`, name = "Natural Gas",
            line = list(color = cols_vec_ly[[3]], width = 2)) |> 
  add_trace(y = ~`Metallurgical Coal`, name = "Metallurgical Coal",
            line = list(color = cols_vec_ly[[4]], width = 2)) |> 
  add_trace(y = ~`Sub-Bituminous Coal`, name = "Sub-Bituminous Coal",
            line = list(color = cols_vec_ly[[5]], width = 2)) |> 
  add_trace(y = ~`Anthracite Coal`, name = "Anthracite Coal",
            line = list(color = cols_vec_ly[[6]], width = 2)) |> 
  add_trace(y = ~`Cement`, name = "Cement",
            line = list(color = cols_vec_ly[[7]], width = 2)) |> 
  add_trace(y = ~`Thermal Coal`, name = "Thermal Coal",
            line = list(color = cols_vec_ly[[8]], width = 2)) |> 
  add_trace(y = ~`Lignite Coal`, name = "Lignite Coal",
            line = list(color = cols_vec_ly[[9]], width = 2)) |> 
  # add_annotations(axref = "x", ayref = "y",
  #                 x = 1, y = 1,
  #                 showarrow = FALSE,
  #                 text = caption_ly,
  #                 align = "left",
  #                 font = list(size = 15)
  #                 ) |> 
  config(scrollZoom = TRUE,
         responsive = TRUE) |> 
  layout(font = list(family = main_font,
                     color = text_col),
         plot_bgcolor = bg_col, # plot area
         paper_bgcolor = bg_col, # outside plot area
         title = list(text = "<span style='font-size:40px;'><b>Global CO2 Emission by Commodities</b></span>",
                      xanchor = "center", yanchor = "top",
                      font = list(size = 20), y = 0.95
                      ),
         xaxis = list(title = list(text = "<span style='font-size:20px;'><b>Year</b></span>"),
                      ticks = "outside",
                      dtick = 25,
                      font = list(size = 18)
                      ),
         yaxis = list(title = list(text = "<span style='font-size:20px;'><b>Total CO2 Emissions</b> (MtCO2e)</span>"),
                      font = list(size = 18)
                      ),
         legend = list(title = list(text = "<b>Commodities</b>",
                                    align = "center",
                                    font = list(size = 20)),
                       font = list(size = 17), y = 0.85
                       )
         #hovermode = "x unified"
         )
 

total_e_ly$dependencies <- c(
  total_e_ly$dependencies,
  list(
    htmltools::htmlDependency(
      name = "oswald",
      version = "0",  
      src = "",
      head = font_css 
    )
  )
)

total_e_ly
htmlwidgets::saveWidget(total_e_ly, file = "total_e_ly_post.html") 


