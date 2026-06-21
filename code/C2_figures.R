# =============================================================================
# C2_figures.R
# Project: Delimitation 2026 -- the double penalty
# Author:  Piyush Zaware
# Last updated: 2026-06-20
#
# Figures:
#   fig_double_penalty.png  -- THE centerpiece: fiscal share change vs seat
#                              change; the South sits in the "loses both" quadrant
#   fig_fiscal_slope.png    -- 14th -> 15th FC devolution share, South vs gainers
#
# IN:  tmp/fiscal.rds
# OUT: output/figures/*.png
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)
})

dbl <- readRDS(file.path(TMPDIR, "fiscal.rds"))

south <- c("Tamil Nadu","Kerala","Karnataka","Andhra Pradesh","Telangana")
hindi <- c("Uttar Pradesh","Bihar","Madhya Pradesh","Rajasthan","Jharkhand",
           "Chhattisgarh","Haryana","Delhi","Uttarakhand","Himachal Pradesh")
classify <- function(s) ifelse(s %in% south, "South",
                        ifelse(s %in% hindi, "Hindi-belt", "Rest of India"))
reg_cols <- c("South" = "#C0392B", "Hindi-belt" = "#2C6FBB", "Rest of India" = "#9AA0A6")

theme_del <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, colour = "#1a1a1a"),
    plot.subtitle = element_text(size = 10.5, colour = "#555555", margin = margin(b = 12)),
    plot.caption  = element_text(size = 8, colour = "#999999", hjust = 0),
    panel.grid.minor = element_blank(),
    axis.title    = element_text(size = 10.5, colour = "#333333"),
    legend.position = "top",
    legend.title  = element_blank()
  )

# ── FIGURE 1: the double penalty scatter ──────────────────────────────────────
#{
d <- dbl %>% filter(seats_now >= 2) %>% mutate(region = classify(state))

p1 <- ggplot(d, aes(x = fiscal_chg_pts, y = chg_webster)) +
  # quadrant shading: bottom-left = loses both
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#C0392B", alpha = 0.05) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#2C6FBB", alpha = 0.05) +
  geom_hline(yintercept = 0, colour = "#BBBBBB", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "#BBBBBB", linewidth = 0.4) +
  geom_point(aes(colour = region, size = pop_2026), alpha = 0.9) +
  geom_text_repel(aes(label = state, colour = region), size = 3,
                  fontface = "bold", max.overlaps = 18, show.legend = FALSE, seed = 3) +
  annotate("text", x = -0.95, y = -9.3, hjust = 0, vjust = 0,
           label = "LOSES BOTH\nvoice and money",
           size = 3.2, colour = "#C0392B", fontface = "bold", lineheight = 0.9) +
  annotate("text", x = 0.78, y = 11, hjust = 1, vjust = 1,
           label = "GAINS BOTH",
           size = 3.2, colour = "#2C6FBB", fontface = "bold") +
  scale_colour_manual(values = reg_cols) +
  scale_size_continuous(range = c(2, 11), guide = "none") +
  scale_x_continuous(breaks = seq(-1, 0.8, 0.25)) +
  scale_y_continuous(breaks = seq(-10, 12, 2)) +
  labs(
    title    = "The Double Penalty: the Same States Lose Both Seats and Money",
    subtitle = "Projected Lok Sabha seat change (delimitation) against change in tax devolution share (14th to 15th Finance Commission).\nBoth penalties fall on the South, and both flow from the same switch: 1971 population replaced by 2011 population.",
    x = "Change in tax-devolution share, 14th to 15th Finance Commission (percentage points)",
    y = "Projected change in Lok Sabha seats",
    caption = "Sources: seat change computed from NCP 2020 projections (Webster); devolution shares from PRS / 15th Finance Commission report, Table 2."
  ) +
  theme_del

ggsave(file.path(FIGDIR, "fig_double_penalty.png"), p1,
       width = 10.5, height = 7.2, dpi = 300, bg = "white")
message("Saved: fig_double_penalty.png")
#}

# ── FIGURE 2: fiscal slopegraph (share 14th -> 15th FC) ───────────────────────
#{
sl <- dbl %>%
  filter(state %in% c(south, "Uttar Pradesh","Bihar","Maharashtra","Rajasthan",
                       "Gujarat","Madhya Pradesh")) %>%
  mutate(region = classify(state)) %>%
  select(state, region, fc14, fc15) %>%
  pivot_longer(c(fc14, fc15), names_to = "fc", values_to = "share") %>%
  mutate(fc = factor(fc, levels = c("fc14", "fc15"),
                     labels = c("14th FC\n(2015-20)", "15th FC\n(2021-26)")))

p2 <- ggplot(sl, aes(x = fc, y = share, group = state, colour = region)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(size = 2.6) +
  geom_text_repel(
    data = sl %>% filter(fc == "15th FC\n(2021-26)"),
    aes(label = paste0(state, " ", sprintf("%.1f", share), "%")),
    hjust = 0, nudge_x = 0.08, size = 3, direction = "y",
    segment.size = 0.2, show.legend = FALSE, seed = 1
  ) +
  scale_colour_manual(values = reg_cols) +
  scale_x_discrete(expand = expansion(mult = c(0.18, 0.55))) +
  labs(
    title    = "Who Lost Fiscal Ground When 1971 Population Was Dropped",
    subtitle = "State share of the tax-devolution pool. The southern states (red) fall; the large northern states (blue) rise.",
    x = NULL,
    y = "Share of states' divisible pool (%)",
    caption = "Source: PRS / 15th Finance Commission report, Table 2. The 15th FC replaced 1971 population with 2011 population."
  ) +
  theme_del +
  theme(legend.position = "top")

ggsave(file.path(FIGDIR, "fig_fiscal_slope.png"), p2,
       width = 9.5, height = 7, dpi = 300, bg = "white")
message("Saved: fig_fiscal_slope.png")
#}

message("C2 complete.")
