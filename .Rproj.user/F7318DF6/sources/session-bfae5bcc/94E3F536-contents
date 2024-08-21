# Prepare Session ---------------------------------------------------------

library(tidyverse)
library(cbbdata)
library(cbbplotR) # update pkg if given a `select` error
library(vipor)
library(here)

# Data --------------------------------------------------------------------

calculate_quasirandom_jitter <- function(y, x, width = 0.2) {
  jittered_offset <- offsetSingleGroup(y, method = "quasirandom")
  jittered_offset <- jittered_offset * width
  x + jittered_offset
}

data <- cbd_torvik_ncaa_results(2000, 2024) %>%
  filter(r64 >= 5) %>%
  select(team, pase) %>%
  mutate(pase_rk = dense_rank(-pase)) %>%
  mutate(x = calculate_quasirandom_jitter(pase, 1))


# Plot --------------------------------------------------------------------

plot <- data %>%
  ggplot(aes(x, pase)) +
  geom_mean_lines(aes(y0 = pase), color = "grey70") +
  geom_cbb_teams(aes(
    team = team,
    width = ifelse(pase_rk <= 20, 0.07, 0.055),
    alpha = ifelse(pase_rk <= 20, 1, 0.15)
  )) +
  scale_alpha_identity() +
  scale_y_continuous(
    breaks = seq(-10, 20, 5), labels = c("- 10", as.character(seq(-5, 15, 5)), "+ 20"),
    limits = c(-10, 20)
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "RadioCanadaBig-Bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(
      family = "RadioCanadaBig-Regular", hjust = 0.5,
      vjust = 2.7, size = 10
    ),
    plot.caption.position = "plot",
    plot.caption = ggtext::element_markdown(
      family = "RadioCanadaBig-Regular",
      lineheight = 1.2, size = 8
    ),
    axis.text = element_text(family = "RadioCanadaBig-Regular"),
    axis.title = element_text(family = "RadioCanadaBig-SemiBold"),
    axis.title.y = element_text(vjust = 2),
    axis.text.x = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#F6F7F2")
  ) +
  labs(
    title = "The programs who routinely outperform March expectations",
    subtitle = "Sorted by PASE (performance against seed expectation) from 2000-2024.\nMin. five tournament appearances.",
    caption = "Data by cbbdata<br>Viz by @andreweatherman + cbbplotR",
    y = "Aggregate wins +/- seed expectation",
    x = NULL
  )


# Saving ------------------------------------------------------------------

path <- here("visualizations", "graphs", "jittered_logos", "plot.png")
ggsave(plot = plot, path, h = 6.5, w = 6, dpi = 600, device = grDevices::png)
