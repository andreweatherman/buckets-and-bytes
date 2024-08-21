
# Prepare Session ---------------------------------------------------------

library(cbbdata)
library(cbbplotR) # update pkg is given a `select` error
library(tidyverse)
library(gt)
library(gtExtras)
library(magick)
library(glue)

# Data --------------------------------------------------------------------

away_ratings <- cbd_torvik_team_factors(year = 2024, venue = 'away')

best_win <- cbd_torvik_game_factors(year = 2024, location = 'A') %>%
  filter(result == 'W') %>%
  left_join(cbd_torvik_ratings(year=2024) %>% select(team, barthag), join_by('opp' == 'team')) %>%
  left_join(cbd_teams() %>% select(opp = common_team, opp_logo = logo)) %>%
  slice_max(barthag, n = 1, by = team) %>%
  select(team, opp_logo)

quad_records <- cbd_torvik_team_schedule(year = 2024, location = 'A') %>%
  filter(date < Sys.Date()) %>%
  cbd_add_net_quad() %>%
  left_join(cbd_torvik_game_box(year = 2024) %>% select(date, team, opp, result),
            by = c('date', 'team', 'opp')) %>%
  summarize(
    record = paste0(sum(result == 'W'), '-', sum(result == 'L')),
    .by = c(team, quad)
  ) %>%
  pivot_wider(names_from = quad, values_from = record) %>%
  mutate(across(-team, ~ifelse(.x == 'NA-NA' | is.na(.x), '0-0', .x))) %>%
  select(team, q1 = `Quadrant 1`, q2 = `Quadrant 2`, q3 = `Quadrant 3`, q4 = `Quadrant 4`)

pred_avg <- cbd_all_metrics() %>%
  summarize(
    avg = (trank_rank + kp_rank + bpi_rank + net_rank) / 4,
    .by = team
  )

data <- list(away_ratings, quad_records, best_win, pred_avg) %>%
  reduce(left_join, by = 'team') %>%
  slice_max(barthag, n = 10) %>%
  left_join(cbd_teams() %>% select(team = common_team, logo = logo)) %>%
  mutate(team = glue("<img src='{logo}' style='height: 20px; width: auto; vertical-align: -25%;'>&nbsp; {team}")) %>%
  mutate(record = paste0(wins, '-', losses),
         team = paste0(team, ' (', record, ')'),
         avg = paste0('#', round(avg, 0))) %>%
  select(avg, team, adj_o, adj_d, barthag, starts_with('q'), best = opp_logo)



# Table -------------------------------------------------------------------

table <- data %>%
  gt(id = "table") %>%
  gt_theme_nytimes() %>%
  fmt_markdown(team) %>%
  fmt_image(best, width = 20, height = 20) %>%
  fmt_number(columns = adj_o:adj_d, decimals = 1) %>%
  fmt_percent(columns = barthag, decimals = 1) %>%
  cols_align(columns = avg, "right") %>%
  cols_align(columns = -c(team, avg), "center") %>%
  tab_style(locations = cells_body(columns = barthag), style = cell_text(weight = "bold")) %>%
  tab_spanner(columns = q1:q4, label = "Quadrant Records") %>%
  tab_spanner(columns = adj_o:barthag, label = "Road Efficiency") %>%
  gt_add_divider(team, include_labels = FALSE, color = "black", weight = px(1.5)) %>%
  gt_add_divider(barthag, include_labels = FALSE, color = "black", weight = px(1.5)) %>%
  gt_add_divider(q4, include_labels = FALSE, color = "black", weight = px(1.5)) %>%
  tab_footnote(
    locations = cells_column_labels(columns = best),
    footnote = "Highest current opponent T-Rank in a road win"
  ) %>%
  tab_footnote(
    locations = cells_column_labels(columns = barthag),
    footnote = "T-Rank Rating: WP% vs. average team on neutral floor based on road performance"
  ) %>%
  tab_footnote(
    locations = cells_column_labels(columns = avg),
    footnote = "Predictive Average: KenPom, T-Rank, BPI, and NET (rounded to nearest integer)"
  ) %>%
  cols_label(
    team = "Team (record)",
    adj_o = "Adj. O",
    adj_d = "Adj. D",
    barthag = "T-Rank",
    best = "Best"
  ) %>%
  opt_row_striping() %>%
  tab_options(
    data_row.padding = 4,
    footnotes.font.size = 11,
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    table.border.bottom.style = "none",
    source_notes.font.size = 10,
    source_notes.border.lr.style = "none"
  ) %>%
  tab_header(
    title = "Who are the best road teams in college basketball?",
    subtitle = "10 highest T-Rank ratings in true road performances (D-1 vs. D-1)"
  ) %>%
  tab_source_note(
    md("Data by cbbdata + cbbplotR through 2023-24<br>Table + Analysis by @andreweatherman")
  ) %>%
  opt_css(
    "#table .gt_footnote {
        padding-top: 2px !important;
        padding-bottom: 2px !important;
        line-height: 1;
      }
    #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
      }
      #table .gt_sourcenote {
         text-align: right
        }
    "
  )


# Saving ------------------------------------------------------------------

crop_gt <- function(file, whitespace) {
  image_read(file) %>%
    image_trim() %>%
    image_border("white", glue('{whitespace}x{whitespace}')) %>%
    image_write(file)
}

path <- here("visualizations", "tables", "road_performance", "table.png")
gtsave_extra(table, path, zoom = 5)
crop_gt(path, 80)

