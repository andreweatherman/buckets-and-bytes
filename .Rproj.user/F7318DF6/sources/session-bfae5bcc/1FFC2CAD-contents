
# Prepare Session ---------------------------------------------------------

library(cfbfastR)
library(gt)
library(gtExtras)
library(glue)
library(tidyverse)
library(cbbdata)
library(rlang)
library(nflreadr)
library(magick)

# Data --------------------------------------------------------------------

b1g <- cfbd_team_info() %>% filter(conference == "Big Ten") %>% pull("school")

schedule <- espn_cfb_schedule(year = 2024, limit = 1000) %>%
  filter(type != "postseason") %>% # filter out 2024 CFP
  select(home_team = home_team_location, away_team = away_team_location, date = game_date) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%dT%H:%MZ"))

weeks <- espn_cfb_calendar(year = 2024) %>%
  select(week, start_date, end_date) %>%
  mutate(across(-week, ~as.Date(.x, format = "%Y-%m-%dT%H:%MZ")))

schedule <- left_join(
  schedule,
  weeks,
  join_by(between(date, start_date, end_date))
) %>%
  select(home_team, away_team, week) %>%
  nflreadr::clean_homeaway() %>%
  filter(team %in% b1g)

## Team Logos ===

logos <- cbd_teams() %>% select(team = espn_location, logo)
logos <- logos %>% pull(logo) %>% set_names(logos$team)

plot_data <- schedule %>%
  mutate(opponent = glue("<img src='{logos[opponent]}' alt={location} style='height:25px; vertical-align:middle;'>")) %>%
  pivot_wider(id_cols = team, names_from = week, values_from = opponent) %>%
  arrange(team) %>%
  mutate(team = logos[team])



# Plotting Functions for CSS ----------------------------------------------

generate_css <- function(indices, css_id, color) {
  map2_chr(
    .x = indices[, 1],
    .y = indices[, 2],
    .f = ~glue("#{css_id} tbody tr:nth-child({.x}) td:nth-child({.y}) {{ background-color: {color}; }}")
  )
}

home_css <- arrayInd(which(str_detect(as.matrix(plot_data), 'alt=home')), .dim = dim(plot_data)) %>%
  generate_css('table', '#cce7f5')

bye_css <- arrayInd(which(is.na(as.matrix(plot_data))), .dim = dim(plot_data)) %>%
  generate_css('table', '#d9d9d9')


## Header + CSS ===

html_content <- '
<div style="text-align: center;">
  <h1 style="margin: 0; font-size: 20px;">Big Ten Football Schedule | 2024</h1>
  <div style="display: flex; justify-content: center; align-items: center; margin-top: 5px;">
    <div style="border: 1.5px solid black; padding: 2px 10px; text-align: center; background-color: #cce7f5; font-size: 10px; margin-right: 5px;">Home</div>
    <div style="border: 1.5px solid black; padding: 2px 10px; text-align: center; font-size: 10px; margin-right: 5px;">Away</div>
    <div style="border: 1.5px solid black; padding: 2px 10px; text-align: center; background-color: #d9d9d9; font-size: 10px;">Bye</div>
  </div>
</div>
'

additional_css <- "

  #table .gt_sourcenote {
    line-height: 1.3;
  }

"


# Table -------------------------------------------------------------------

table <- plot_data %>%
  gt(id = 'table') %>%
  gt_theme_538() %>%
  fmt_image(team, height = 25) %>%
  fmt_markdown(-team) %>%
  # use sub_missing to replace na with empty text string
  sub_missing(-team, missing_text = '') %>%
  cols_align(columns = everything(), 'center') %>%
  cols_label(team = '') %>%
  # bold col. headers
  tab_style(locations = cells_column_labels(), style = cell_text(weight = 'bold')) %>%
  # add dividers
  gt_add_divider(columns = -team, sides = 'all', include_labels = FALSE, color = 'black', weight = px(1.5)) %>%
  tab_header(html(html_content)) %>%
  tab_source_note(md("Data by cfbfastR<br>Viz. by @andreweatherman (h/t to @cobrastats)")) %>%
  tab_options(data_row.padding = 1) %>%
  # apply above css
  opt_css(c(home_css, bye_css, additional_css))

# Saving ------------------------------------------------------------------

crop_gt <- function(file, whitespace) {
  image_read(file) %>%
    image_trim() %>%
    image_border("white", glue('{whitespace}x{whitespace}')) %>%
    image_write(file)
}

path <- here("visualizations", "tables", "schedule_matrix", "table.png")
gtsave_extra(table, path, zoom = 5)
crop_gt(path, 80)
