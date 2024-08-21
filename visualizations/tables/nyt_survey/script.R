# Prepare Session ---------------------------------------------------------

library(tidyverse)
library(janitor)
library(data.table)
library(gt)
library(gtExtras)
library(glue)
library(magick)
library(here)

# Data --------------------------------------------------------------------

data <- read_csv("https://gist.github.com/andreweatherman/8dde2049840d0a27a4ef2a3bef4bdd8f/raw/74e70fdaea6777463521c433f179d6962a93eca5/juror_news.csv")

data <- data %>%
  transpose(keep.names = "news") %>%
  janitor::row_to_names(1) %>%
  as_tibble() %>%
  rename("source" = "juror") %>%
  mutate(
    across(-source, ~ {
      display_number <- ifelse(row_number() == 1,
        ifelse(as.numeric(cur_column()) > 12, as.numeric(cur_column()) - 12, cur_column()),
        NA
      )

      color <- ifelse(is.na(.x), "#EEEEEE", "#FCCF10")

      glue("<span style='display:inline-block; width:20px; height:20px; line-height:20px; background-color: {color}; vertical-align:middle; margin:4px 1px; font-size: 12px; font-weight: bold; text-align:center;'>{ifelse(!is.na(display_number), display_number, '')}</span>")
    }),
    blank = "", .after = 13
  )


# Table -------------------------------------------------------------------

table <- data %>%
  gt(id = "table") %>%
  gt_theme_nytimes() %>%
  fmt_markdown(-c(source, blank)) %>%
  cols_label(blank = "") %>%
  cols_width(blank ~ px(15)) %>%
  tab_spanner(columns = 1, label = "Source") %>%
  tab_spanner(columns = 2:13, label = "Jurors") %>%
  tab_spanner(columns = 15:20, label = "Alternates") %>%
  tab_style(
    locations = cells_column_spanners(),
    style = cell_text(align = "left", size = px(16), color = "#7E7E7E")
  ) %>%
  tab_header(title = "Where the jurors in the Trump hush-money trial say they get their news") %>%
  tab_source_note(md("Data and original table by New York Times<br>Recreation in R by @andreweatherman")) %>%
  tab_options(
    data_row.padding = 1,
    source_notes.border.bottom.style = "solid",
    source_notes.border.bottom.color = "white",
    source_notes.font.size = 12
  ) %>%
  opt_css(
    css = "
      #table .gt_col_headings {
        visibility: hidden;
        position: relative;
      }
      #table .gt_column_spanner {
        position: absolute;
        visibility: visible;
        padding-left: 3px;
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

path <- here("visualizations", "tables", "nyt_survey", "table.png")
gtsave_extra(table, path, zoom = 12)
crop_gt(path, 150)
