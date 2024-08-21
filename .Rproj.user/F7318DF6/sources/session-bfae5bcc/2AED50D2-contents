### You might need to adjust font sizes on your own after running + saving

# Prepare Session ---------------------------------------------------------

library(rvest)
library(tidyverse)
library(cbbplotR)
library(cbbdata)
library(showtext)


# Data --------------------------------------------------------------------

teams <- cbd_teams()

## Looping function for sports reference ===

get_players <- function(team) {
  slug <- filter(teams, common_team == team)$sr_link

  ## grab color of team for bar fill
  color <- filter(teams, common_team == team)$color

  read_html(paste0("https://www.basketball-reference.com/friv/colleges.fcgi?college=", slug)) %>%
    html_nodes("#nba_aba_players") %>%
    html_table() %>%
    pluck(1) %>%
    janitor::row_to_names(1) %>%
    janitor::clean_names() %>%
    select(player, pts) %>%
    mutate(pts = as.numeric(pts)) %>%
    filter(!is.na(pts)) %>%
    arrange(desc(pts)) %>%
    mutate(
      player = trimws(gsub("\\*", "", player)), ## remove * from some names
      player = factor(player, levels = player), ## set an ordering factor for the bars
      team = team,
      fill = color,
      ## only include names of first 15 players, truncate them to 15 chars.
      label = ifelse(row_number() <= 15, str_trunc(as.character(player), 15), "")
    )
}

data <- map_dfr(
  .x = c("Duke", "North Carolina", "Kansas", "Kentucky", "UCLA"),
  \(team) get_players(team)
)

# Plot --------------------------------------------------------------------

## add oswald font from google fonts // needed if on Windows
font_add_google("Oswald", "oswald")
showtext_auto()

plot <- data %>%
  ## min. 1000 points scored
  filter(pts >= 1000) %>%
  ## order teams by total points scored
  ggplot(aes(x = fct_reorder(team, -pts, sum), y = pts, fill = fill)) +
  ## stack bars and decrease line width
  geom_bar(position = "stack", stat = "identity", color = "#F6F7F2", linewidth = 0.25) +
  ## label for players
  geom_text(aes(label = toupper(label), size = pts),
    position = position_stack(vjust = 0.5),
    color = "white", family = "oswald"
  ) +
  ## calc. total points scored and include label (nudge label up)
  geom_text(
    data = ~ summarize(.x, total = sum(pts), .by = team),
    aes(label = scales::label_comma()(total), x = team, y = total),
    size = 3.5, fontface = "bold", inherit.aes = FALSE,
    family = "oswald", nudge_y = 11000
  ) +
  ## line above logos
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_size(range = c(0.5, 3)) +
  scale_fill_identity() +
  ## decrease space b/w logos and plot // set limit to above the highest
  ## total points scored
  scale_y_continuous(expand = c(0.01, 0), limits = c(0, 450000)) +
  theme_void() +
  theme(
    axis.text.x = element_cbb_teams(size = 1),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      hjust = 0.5, vjust = 3, face = "bold", size = 15,
      family = "oswald"
    ),
    plot.subtitle = element_text(
      hjust = 0.5, vjust = 6, size = 10,
      family = "oswald"
    ),
    plot.caption = element_text(
      hjust = 0.5, family = "oswald",
      size = 8
    ),
    ## set margins
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    ## make background off-white
    plot.background = element_rect(fill = "#F6F7F2")
  ) +
  labs(
    title = "Total points scored in the NBA by Blue Blood alumni",
    subtitle = "In all career games from players in the NBA or ABA (min. 1,000 pts. scored)",
    caption = "Data by Sports Reference || Viz. by @andreweatherman"
  )

# Saving ------------------------------------------------------------------

path <- here("visualizations", "graphs", "nba_points_stacked_bar", "plot.png")
ggsave(plot = plot, path, w = 5, h = 6, device = grDevices::png)
