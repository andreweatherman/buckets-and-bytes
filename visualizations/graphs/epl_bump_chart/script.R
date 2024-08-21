### You might need to adjust font sizes on your own after running + saving

# Prepare Session ---------------------------------------------------------

library(tidyverse)
library(worldfootballR)
library(ggbump)
library(ggtext)
library(showtext)

# Data --------------------------------------------------------------------

## takes a few minutes
data <- map_dfr(1:38, \(week) tm_matchday_table("England",
  start_year = "2023",
  matchday = week
))

# Plot --------------------------------------------------------------------

## add oswald font from google fonts // needed if on Windows, not Mac
font_add_google("Oswald", "oswald")
showtext_auto()

plot <- data %>%
  select(team = squad, week = matchday, place = rk) %>%
  ## add fill colors and alpha levels based on whether teams are man city/arsenal or not
  mutate(
    fill_color = case_when(
      team == "Man City" ~ "#6CABDD",
      team == "Arsenal" ~ "#EF0107",
      .default = "grey50"
    ),
    alpha = ifelse(team %in% c("Man City", "Arsenal"), 1, 0.5)
  ) %>%
  ggplot(aes(week, place, color = fill_color, group = team)) +
  geom_point(size = 1.7, aes(alpha = alpha)) +
  ## geom_bump from ggbump
  geom_bump(aes(alpha = alpha), linewidth = 0.7) +
  scale_color_identity() +
  ## customize our weeks lables
  scale_x_continuous(
    breaks = c(1, seq(10, 30, 10), 38),
    labels = c("1st week", "10th", "20th", "30th", "Final week")
  ) +
  ## reverse the y-axis so 1st is plotted at the top and customize the labels
  scale_y_reverse(breaks = seq(1, 20, 1), labels = c("1st", seq(2, 19, 1), "Last")) +
  theme_void() +
  theme(
    legend.position = "none", # remove legend
    plot.title.position = "plot",
    ## add the cairo font and change some sizes for saving
    plot.title = element_text(family = "cairo", hjust = 0.5, size = 16, face = "bold", vjust = 0),
    plot.subtitle = element_markdown(family = "cairo", hjust = 0.5, size = 10),
    plot.caption.position = "plot",
    plot.caption = element_text(family = "cairo", hjust = 0.5, vjust = -2, size = 8),
    axis.text = element_text(family = "cairo", size = 8),
    axis.text.x = element_text(vjust = -1),
    ## add light dotted major gridlines for viewing
    panel.grid.major.x = element_line(color = "grey70", linetype = "dotted"),
    panel.grid.major.y = element_line(color = "grey70", linetype = "dotted"),
    ## set margins
    plot.margin = unit(c(0.25, 0.5, 0.5, 0.5), "cm"),
    ## make background off-white
    plot.background = element_rect(fill = "#F6F7F2")
  ) +
  labs(
    title = "2023-24 English Premier League Table Flow",
    ## with ggtext, we can use minor HTML to change color and weight of our subtitle
    subtitle = "<p>The fight between <span style='color:#6CABDD;'><b>Manchester City</b></span> and <span style='color:#EF0107;'><b>Arsenal</b></span> throughout the 2023-24 campaign</p>",
    caption = "Data by worldfootballR || Viz. by @andreweatherman"
  )


# Saving ------------------------------------------------------------------

path <- here("visualizations", "graphs", "epl_bump_chart", "plot.png")
ggsave(plot = plot, path, w = 6.16, h = 4.81, device = grDevices::png)
