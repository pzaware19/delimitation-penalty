# =============================================================================
# C1_figures.R
# Project: Delimitation 2026
# Author:  Piyush Zaware
# Last updated: 2026-06-20
#
# Figures:
#   fig_penalty_scatter.png    -- THE centerpiece: TFR vs projected seat change
#   fig_penalty_literacy.png   -- female literacy vs seat change (companion)
#   fig_seat_change.png        -- diverging bar: seats gained/lost by state
#   fig_malapportionment.png   -- population per seat now (the existing skew)
#   fig_formula_robustness.png -- South loses under all four methods
#
# IN:  tmp/apportion.rds
# OUT: output/figures/*.png
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)
  library(scales)
})

obj     <- readRDS(file.path(TMPDIR, "apportion.rds"))
change  <- obj$change
penalty <- obj$penalty
malapp  <- obj$malapp

# Region classification (for colour)
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

# ── FIGURE 1: The performance penalty (TFR vs seat change) ─────────────────────
#{
pen <- penalty %>% mutate(region = classify(state))
r_tfr <- cor(pen$tfr, pen$chg_webster)
fit   <- lm(chg_webster ~ tfr, data = pen)

p1 <- ggplot(pen, aes(x = tfr, y = chg_webster)) +
  geom_hline(yintercept = 0, colour = "#BBBBBB", linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, colour = "#333333",
              fill = "#E8E8E8", linewidth = 0.7) +
  geom_point(aes(colour = region, size = pop_2026), alpha = 0.9) +
  geom_text_repel(aes(label = state, colour = region),
                  size = 3, fontface = "bold", max.overlaps = 20,
                  show.legend = FALSE, seed = 1) +
  annotate("text", x = 1.45, y = -9,
           label = paste0("r = ", sprintf('%.2f', r_tfr),
                          "\nlower fertility,\nfewer seats"),
           hjust = 0, size = 3.2, colour = "#C0392B", fontface = "italic") +
  scale_colour_manual(values = reg_cols) +
  scale_size_continuous(range = c(2, 11), guide = "none") +
  scale_x_continuous(breaks = seq(1.4, 3.0, 0.2)) +
  scale_y_continuous(breaks = seq(-10, 12, 2)) +
  labs(
    title    = "The States That Cut Their Birth Rates Are the Ones Losing Seats",
    subtitle = "Each state's projected Lok Sabha seat change (543 seats reapportioned by 2026 population)\nplotted against its fertility rate. Point size = population. The pattern is the punishment.",
    x = "Total Fertility Rate (NFHS-5, 2019-21)",
    y = "Projected change in Lok Sabha seats",
    caption = "Sources: Seat allocation computed (Webster method) from NCP 2020 population projections; TFR from NFHS-5.\nSample: 21 major states (current seats >= 4). Slope significant at p < 0.001, R-squared = 0.48."
  ) +
  theme_del

ggsave(file.path(FIGDIR, "fig_penalty_scatter.png"), p1,
       width = 10.5, height = 7, dpi = 300, bg = "white")
message("Saved: fig_penalty_scatter.png")
#}

# ── FIGURE 2: companion, female literacy ──────────────────────────────────────
#{
r_lit <- cor(pen$female_lit, pen$chg_webster)
p2 <- ggplot(pen, aes(x = female_lit, y = chg_webster)) +
  geom_hline(yintercept = 0, colour = "#BBBBBB", linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, colour = "#333333",
              fill = "#E8E8E8", linewidth = 0.7) +
  geom_point(aes(colour = region, size = pop_2026), alpha = 0.9) +
  geom_text_repel(aes(label = state, colour = region), size = 3,
                  fontface = "bold", max.overlaps = 20, show.legend = FALSE, seed = 1) +
  annotate("text", x = 88, y = 8,
           label = paste0("r = ", sprintf('%.2f', r_lit)),
           hjust = 0, size = 3.4, colour = "#C0392B", fontface = "italic") +
  scale_colour_manual(values = reg_cols) +
  scale_size_continuous(range = c(2, 11), guide = "none") +
  labs(
    title    = "The More a State's Girls Can Read, the More Seats It Loses",
    subtitle = "Projected seat change vs female literacy. The relationship runs the wrong way for fairness.",
    x = "Female literacy rate, % (Census 2011)",
    y = "Projected change in Lok Sabha seats",
    caption = "Sources: Seat allocation (Webster) from NCP 2020 projections; female literacy from Census 2011. 21 major states."
  ) +
  theme_del

ggsave(file.path(FIGDIR, "fig_penalty_literacy.png"), p2,
       width = 10.5, height = 7, dpi = 300, bg = "white")
message("Saved: fig_penalty_literacy.png")
#}

# ── FIGURE 3: diverging bar of seat change ────────────────────────────────────
#{
bar <- change %>%
  filter(seats_now >= 2) %>%
  mutate(region = classify(state),
         state = fct_reorder(state, chg_webster))

p3 <- ggplot(bar, aes(x = state, y = chg_webster, fill = region)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = ifelse(chg_webster > 0, paste0("+", chg_webster), chg_webster),
                hjust = ifelse(chg_webster > 0, -0.3, 1.3)),
            size = 2.8, colour = "#333333") +
  coord_flip() +
  scale_fill_manual(values = reg_cols) +
  scale_y_continuous(breaks = seq(-10, 12, 2), expand = expansion(mult = 0.08)) +
  labs(
    title    = "Who Gains and Who Loses if 543 Seats Are Reapportioned by Population",
    subtitle = "Projected Lok Sabha seat change, 2026 population, Webster method. The South and East lose; the Hindi-belt gains.",
    x = NULL,
    y = "Change in Lok Sabha seats",
    caption = "Source: computed from NCP 2020 population projections. The five southern states lose 26 seats combined, the same under all four apportionment methods."
  ) +
  theme_del +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(FIGDIR, "fig_seat_change.png"), p3,
       width = 9.5, height = 8, dpi = 300, bg = "white")
message("Saved: fig_seat_change.png")
#}

# ── FIGURE 4: malapportionment now (pop per seat) ─────────────────────────────
#{
mal <- malapp %>%
  filter(seats_now >= 2) %>%
  mutate(region = classify(state),
         state = fct_reorder(state, pop_per_seat_2026),
         m_people = pop_per_seat_2026 / 1000)   # millions per seat
avg_line <- sum(malapp$pop_2026) / sum(malapp$seats_now) / 1000

p4 <- ggplot(mal, aes(x = state, y = m_people, fill = region)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = avg_line, linetype = "dashed", colour = "#444444") +
  annotate("text", x = 3, y = avg_line + 0.05,
           label = sprintf("national average\n%.1f million / seat", avg_line),
           hjust = 0, size = 2.8, colour = "#444444") +
  coord_flip() +
  scale_fill_manual(values = reg_cols) +
  labs(
    title    = "One Seat, How Many People? The Distortion Frozen Since 1971",
    subtitle = "Population (2026) per current Lok Sabha seat. Northern voters are already under-represented; the South is over-represented.",
    x = NULL,
    y = "Million people per Lok Sabha seat",
    caption = "Source: NCP 2020 projections; current seat allocation. Seats have been frozen on the 1971 census for over five decades."
  ) +
  theme_del +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(FIGDIR, "fig_malapportionment.png"), p4,
       width = 9.5, height = 8, dpi = 300, bg = "white")
message("Saved: fig_malapportionment.png")
#}

# ── FIGURE 5: formula robustness (South under all 4 methods) ──────────────────
#{
rob <- change %>%
  filter(state %in% south) %>%
  select(state, Webster = chg_webster, Jefferson = chg_jefferson,
         `Huntington-Hill` = chg_hh, Hamilton = chg_hamilton) %>%
  pivot_longer(-state, names_to = "method", values_to = "chg") %>%
  mutate(state = fct_reorder(state, chg))

p5 <- ggplot(rob, aes(x = state, y = chg, fill = method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, colour = "#888888") +
  scale_fill_manual(values = c("Webster" = "#C0392B", "Jefferson" = "#E67E22",
                               "Huntington-Hill" = "#8E44AD", "Hamilton" = "#16A085")) +
  labs(
    title    = "It Does Not Matter Which Formula You Use: the South Still Loses",
    subtitle = "Projected seat change for the five southern states under four standard apportionment methods.\nThe choice of mathematics changes almost nothing.",
    x = NULL,
    y = "Projected change in seats (543, 2026 population)",
    caption = "Methods: Webster/Sainte-Lague, Jefferson/D'Hondt, Huntington-Hill (US House), Hamilton/largest remainder."
  ) +
  theme_del

ggsave(file.path(FIGDIR, "fig_formula_robustness.png"), p5,
       width = 10, height = 6, dpi = 300, bg = "white")
message("Saved: fig_formula_robustness.png")
#}

message("C1 complete.")
