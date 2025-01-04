library(shiny)
library(rgbif)
library(rinat)
library(ggplot2)
library(dplyr)
library(lubridate)
library(maps)
library(plotly) 

california_bb <- c(32.5, -124.5, 42, -114)

data <- get_inat_obs(
  query = "Danaus plexippus",
  quality = "research",
  bounds = california_bb
)

counts_df <- data %>%
  dplyr::mutate(date = as.Date(datetime)) %>%  # Convert to Date first
  dplyr::mutate(group_date = lubridate::floor_date(date, unit = "month")) %>%
  count(group_date) %>%
  dplyr::mutate(label = format(group_date, "%B %Y")) %>%
  arrange(group_date)

plot_ly(counts_df, 
        x = ~group_date, 
        y = ~n, 
        type = "bar",
        marker = list(color = "orange"),
        text = ~paste("Date:", label,
                      "<br>Count:", n),
        hoverinfo = "text") %>% 
  layout(
    title = paste("Observations of Monarch Butterflies in California",
    xaxis = list(title = "Date", tickangle = 45),
    yaxis = list(title = "Number of Observations"),
    hoverlabel = list(bgcolor = "white")
  )
)
