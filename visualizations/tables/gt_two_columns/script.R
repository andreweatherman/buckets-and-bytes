# Prepare Session ---------------------------------------------------------

library(tidyverse)
library(cbbdata)
library(cbbplotR)
library(gt)
library(gtExtras)
library(glue)
library(magick)
library(here)

# Data --------------------------------------------------------------------

data <- cbd_torvik_game_stats() %>%
  filter(year >= 2020) %>%
  left_join(cbd_torvik_ratings_archive() %>% select(opp = team, date, barthag, rank)) %>%
  filter(rank <= 100) %>%
  summarize(
    mean_score = mean(game_score),
    games = n(),
    .by = team
  ) %>%
  filter(games >= 25) %>%
  slice_max(mean_score, n = 100) %>%
  arrange(desc(mean_score)) %>%
  left_join(cbd_teams() %>% select(team = common_team, logo)) %>%
  mutate(
    rank = row_number(),
    group = rep(1:5, each = 20),
    mean_score = round(mean_score, 1),
    mean_score = ifelse(nchar(mean_score) == 2, glue("{mean_score}.0"), mean_score),
    formatted_score = glue(
      "<div style='display: flex; align-items: center;'>
      {rank}. &nbsp;
      <img src='{logo}' alt='Logo' style='height:25px; margin-right: 8px;'>
      <span style='font-weight: bold; font-size: 1.2em; color: black; vertical-align: middle;'>{mean_score}</span>
      <span style='font-size: 0.8em; color: gray; vertical-align: middle;'>&nbsp; ({games})</span>
    </div>"
    )
  ) %>%
  group_split(group) %>%
  map_dfc(~ select(.x, contains("formatted_score")))


# Table -------------------------------------------------------------------

table <- data %>%
  gt(id = "table") %>%
  gt_theme_pl() %>%
  fmt_markdown(contains("formatted_score")) %>%
  cols_label(everything() ~ "") %>%
  cols_align(columns = everything(), "center") %>%
  gt_add_divider(columns = -last_col(), color = "black", weight = px(1.5), include_labels = FALSE) %>%
  gt_set_font("Barlow") %>%
  tab_options(data_row.padding = 1) %>%
  tab_style(locations = cells_body(), cell_text(color = "black")) %>%
  tab_footnote(locations = cells_column_labels(), footnote = md("Opponent rank is determined by Barttorvik T-Rank on game date. Game score is game-level Barthag performance (the<br>probability that you beat an average team on a neutral floor). Total games played is next to mean score.")) %>%
  tab_header(
    title = "Performance against top 100 teams over the past five seasons",
    subtitle = "Average T-Rank game score vs. then-top 100 opponents from 2020-2024; min. total 25 games"
  ) %>%
  tab_source_note("Data by Barttorvik + cbbdata || Viz. + Analysis by @andreweatherman") %>%
  opt_css(
    "
    #table .gt_column_spanner {
      border-bottom-style: none !important;
      display: none !important;
    }
    #table .gt_subtitle {
      line-height: 1.2;
      padding-top: 0px;
      padding-bottom: 0px;
    }
    #table .gt_footnote {
      border-bottom-style: solid;
      border-bottom-width: 1px;
      border-bottom-color: #000;
      font-size: 12px;
    }
    #table .gt_footnote_marks {
      display: none !important;
    }
    #table .gt_sourcenote {
      text-align: right;
    }
    #table .gt_row {
      border-top-color: black;
    }
    "
  )


# Saving ------------------------------------------------------------------

crop_gt <- function(file, whitespace) {
  image_read(file) %>%
    image_trim() %>%
    image_border("white", glue("{whitespace}x{whitespace}")) %>%
    image_write(file)
}

path <- here("visualizations", "tables", "gt_two_columns", "table.png")
gtsave_extra(table, path, zoom = 7)
crop_gt(path, 200)
