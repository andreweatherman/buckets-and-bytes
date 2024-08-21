# Prepare Session ---------------------------------------------------------

library(tidyverse)
library(rvest)
library(cbbdata)
library(gt)
library(gtExtras)
library(glue)
library(janitor)
library(glue)
library(magick)
library(here)


# Utility Functions -------------------------------------------------------

get_coaching_changes <- function(year) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent = "Not Windows")
    read_html(glue("https://barttorvik.com/coaching_moves.php?year={year}")) %>%
      html_table() %>%
      pluck(1) %>%
      clean_names() %>%
      mutate(year = year) %>%
      select(team, year, new_coach)
  })
}

grab_schedules <- function(team, year) {
  Sys.sleep(3) # sleep for 501 // don't really need for 10 requests though

  slug <- filter(sr_ids, team == !!team)$sr_link
  url <- glue("https://www.sports-reference.com/cbb/schools/{slug}/men/{year}-schedule.html")

  read_html(url) %>%
    html_nodes("#schedule") %>%
    html_table() %>%
    pluck(1) %>%
    clean_names() %>%
    slice_tail(n = 1) %>%
    select("type") %>%
    mutate(team = team, year = year)
}

gt_cbb_stack <- function(data, upper_text1, upper_text2, lower_text1, lower_text2, lower_text3, logo) {
  data %>%
    mutate(stack = glue(
      "<div style='display: flex; align-items: center;'>
           <img src='{eval(expr({{logo}}))}' style='height: auto; width: 20px; padding-right: 5px;'>
           <div>
             <div style='line-height:14px;'><span style='font-weight:bold;color:black;font-size:14px'>{eval(expr({{upper_text1}}))}, {eval(expr({{upper_text2}}))}</span></div>
             <div style='line-height:10px;'><span style='font-weight:plain;color:grey;font-size:10px'>{eval(expr({{lower_text1}}))} --  #{eval(expr({{lower_text2}}))}, {eval(expr({{lower_text3}}))}</span></div>
           </div>
         </div>"
    ))
}

gt_column_subheaders <- function(gt_table, ...) {
  subheaders <- list(...)
  all_col_names <- colnames(gt_table[["_data"]])

  for (col_name in all_col_names) {
    subtitle_info <- subheaders[[col_name]] %||% list(subtitle = "&nbsp;", heading = col_name)
    subtitle <- subtitle_info$subtitle
    new_header_title <- subtitle_info$heading

    label_html <- htmltools::HTML(glue(
      "<div style='line-height: 1.05; margin-bottom: -2px;'>
    <span style='font-size: 14px; font-weight: bold; color: black;'>{new_header_title}</span>
    <br>
    <span style='font-size: 10px; font-weight: normal; color: #808080;'>{subtitle}</span>
  </div>"
    ))

    gt_table <- gt_table %>%
      cols_label(!!sym(col_name) := label_html)
  }

  gt_table
}


# Data --------------------------------------------------------------------

all_changes <- map_dfr(2012:2024, \(year) get_coaching_changes(year))

archive <- cbd_kenpom_ratings_archive() %>%
  filter(year >= 2008) %>%
  summarize(
    start_em = adj_em[which.min(date)],
    end_em = adj_em[which.max(date)],
    final_rank = adj_em_rk[which.max(date)],
    .by = c(team, year)
  ) %>%
  mutate(diff = end_em - start_em)

team_records <- cbd_torvik_game_box() %>%
  summarize(
    record = glue("{sum(result == 'W')}-{sum(result == 'L')}"),
    .by = c(team, year)
  )

data <- list(all_changes, archive, team_records) %>%
  reduce(left_join, by = c("team", "year")) %>%
  slice_max(diff, n = 10)

sr_ids <- cbd_teams() %>% select(team = common_team, sr_link)

postseason <- map2_dfr(data$team, data$year, \(team, year) grab_schedules(team, year))

data <- left_join(data, postseason, by = c("team", "year")) %>%
  mutate(type = ifelse(type == "CTOURN", "---", type)) %>%
  left_join(cbd_teams() %>% select(team = common_team, espn_nickname, logo)) %>%
  gt_cbb_stack(new_coach, year, espn_nickname, final_rank, record, logo)

## If you do not have a KenPom account, you can grab the `data` object in the repo

# data <- read_csv(here("visualizations", "tables", "coaching_improvements", "data.csv"))


# Table -------------------------------------------------------------------

table <- data %>%
  select(stack, type, start_em, end_em, diff) %>%
  gt(id = "table") %>%
  gt_theme_nytimes() %>%
  fmt_markdown(stack) %>%
  cols_move_to_start(stack) %>%
  cols_align(columns = stack, "left") %>%
  cols_align(columns = -stack, "center") %>%
  gt_column_subheaders(
    stack = list(
      heading = "Coach and Year",
      subtitle = "Team, Final Rank, and Record"
    ),
    type = list(
      heading = "Post SZN",
      subtitle = "Tournament"
    ),
    start_em = list(
      heading = "Pre",
      subtitle = "Rating"
    ),
    end_em = list(
      heading = "End",
      subtitle = "Rating"
    ),
    diff = list(
      heading = "Jump",
      subtitle = "End - Start"
    )
  ) %>%
  tab_style(locations = cells_body(columns = c(type, ends_with("em"))), style = cell_borders()) %>%
  tab_style(locations = cells_body(columns = -ends_with("em")), style = cell_borders(sides = "bottom")) %>%
  tab_style(locations = cells_body(rows = 1), style = cell_borders(sides = "top", weight = px(2))) %>%
  tab_style(locations = cells_body(columns = diff), style = cell_text(weight = "bold")) %>%
  tab_options(
    data_row.padding = 3.5,
    source_notes.border.bottom.style = "solid",
    source_notes.border.bottom.color = "white",
    source_notes.font.size = 10
  ) %>%
  tab_header(
    title = "New coaches beating KenPom expectations",
    subtitle = md("The largest pre-season vs. year-end KenPom rating improvements<br>by new head coaches since 2012")
  ) %>%
  tab_source_note(md("Data by cbbdata + Sports Reference<br>Viz. + Analysis by @andreweatherman")) %>%
  opt_css(
    "
    #table .gt_heading {
      padding-top: 6px;
      padding-bottom: 0px;
    }
    #table .gt_subtitle {
      padding-top: 2px;
      padding-bottom: 6px;
    }
    #table tbody tr:last-child {
    border-bottom: 2px solid #000000;
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

path <- here("visualizations", "tables", "coaching_improvements", "table.png")
gtsave_extra(table, path, zoom = 5)
crop_gt(path, 80)
