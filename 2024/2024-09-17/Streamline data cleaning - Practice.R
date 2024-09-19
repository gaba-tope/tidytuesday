## Streamline data cleaning for multiple data with the same structure

## Load Packages ---------------------------
library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(showtext)

## Import Data ---------------------------
tuesdata <- tidytuesdayR::tt_load('2024-09-17')
hamlet <- tuesdata$hamlet
macbeth <- tuesdata$macbeth
romeo_juliet <- tuesdata$romeo_juliet

## Data Wrangling ---------------------------
analyze_data <- function(play_data, play_name) {
  # Convert char to factor
  play_data <- play_data |> 
    mutate(character = as.factor(character))
  
  # Count characters and calculate proportions
  df <- play_data |> 
    count(character) |> 
    mutate(prop = prop.table(n))
  
  # Get top 5 character who spoke the most
  top_df <- df |> 
    top_n(prop, n = 5) |> 
    mutate(label = paste(character, ":", formatC(prop * 100, digits = 2, format = "f"), "%"),
           rad = 4) |> 
    arrange(desc(prop))
  # Get rest characters
  rest_df <- df |> 
    mutate(label = paste(character, ":", formatC(prop *100, digits = 2, format = "f"), "%"),
           rad = 4) |> 
    arrange(desc(prop)) |> 
    dplyr::filter(!(character %in% top_df$character))
  # Create 'others' category by adding the rest_df up
  others <- tibble(
    character = as.factor("Others"),
    n = sum(rest_df$n),
    prop = sum(rest_df$n),
    label = paste(character, ":", formatC(prop *100, digits = 2, format = "f"), "%"),
    rad = 4
  )
  
  # Combine data
  order <- c(as.character(top_df$character), "Others")
  whole_df <- rbind(top_df, others) |> 
    mutate(ord = seq_len(n())) |> 
    arrange(desc(ord)) |> 
    mutate(text_y = cumsum(prop) - prop / 2)
  
  list(
    play_name = play_name,
    data = whole_df,
    order = order
  )
}

hamlet_analysis <- analyze_data(hamlet, "Hamlet")
macbeth_analysis <- analyze_data(macbeth, "Macbeth")
romeo_analysis <- analyze_data(romeo_juliet, "Romeo and Juliet")

hamlet_analysis
