# =============================================================================
# C4_population_penalty.R
# Project: Delimitation 2026 -- isolating the pure population penalty
# Author:  Piyush Zaware
# Last updated: 2026-06-22
#
# Figure:
#   fig_pop_decomposition.png -- for each southern state, the net FC14->FC15
#     change in tax-devolution money split into (a) the pure 1971->2011 census
#     switch and (b) everything else in the FC15 redesign. Shows the population
#     switch is only PART of the southern fiscal gap.
#
# IN:  tmp/pop_penalty.rds
# OUT: output/figures/fig_pop_decomposition.png
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

dat <- readRDS(file.path(TMPDIR, "pop_penalty.rds"))

south <- c("Tamil Nadu","Kerala","Karnataka","Andhra Pradesh","Telangana")

theme_del <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, colour = "#1a1a1a"),
    plot.subtitle = element_text(size = 10.5, colour = "#555555", margin = margin(b = 12)),
    plot.caption  = element_text(size = 8, colour = "#999999", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title    = element_text(size = 10.5, colour = "#333333"),
    legend.position = "top",
    legend.title  = element_blank()
  )

# ── Long form: two components per state, in Rs '000 crore ─────────────────────
#{
comp <- dat %>%
  filter(state %in% south) %>%
  transmute(state,
            `1971 to 2011 census switch (population criterion)` = pop_pen_cr / 1000,
            `Rest of the FC15 redesign (equity, performance, tax effort, area, forest)` = other_cr / 1000) %>%
  pivot_longer(-state, names_to = "component", values_to = "rs_000cr") %>%
  mutate(component = factor(component, levels = c(
    "1971 to 2011 census switch (population criterion)",
    "Rest of the FC15 redesign (equity, performance, tax effort, area, forest)")))

ord <- dat %>% filter(state %in% south) %>% arrange(fiscal_chg_pts) %>% pull(state)
comp$state <- factor(comp$state, levels = ord)

cols <- c(
  "1971 to 2011 census switch (population criterion)" = "#C0392B",
  "Rest of the FC15 redesign (equity, performance, tax effort, area, forest)" = "#E59866")

net_lab <- dat %>% filter(state %in% south) %>%
  mutate(state = factor(state, levels = ord),
         net = fiscal_chg_pts / 100 * POOL_CR / 1000)
#}

p <- ggplot(comp, aes(x = rs_000cr, y = state, fill = component)) +
  geom_col(width = 0.62) +
  geom_vline(xintercept = 0, colour = "#888888", linewidth = 0.4) +
  geom_text(data = net_lab,
            aes(x = net, y = state,
                label = sprintf("net %+.0f", net * 1000), fill = NULL),
            hjust = ifelse(net_lab$net < 0, 1.1, -0.1),
            size = 3, fontface = "bold", colour = "#333333") +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = seq(-60, 20, 20),
                     labels = function(x) paste0(x, "k")) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    title    = "How Much of the Southern Fiscal Loss Is Really the Census Switch?",
    subtitle = "Net change in 15th vs 14th Finance Commission tax-devolution money, split into the pure 1971-to-2011\npopulation switch and the rest of the redesign. Negative = money lost. The census switch is only part of the gap.",
    x = "Change in tax-devolution money, 14th to 15th FC (Rs crore, '000s over 2021-26)",
    y = NULL,
    caption = paste0(
      "Population effect = FC15 population criterion (15% weight) reallocated from 2011 to 1971 census, all other criteria held fixed.\n",
      "1971 population: census totals; reorganized states apportioned by 2011 proportion. Devolution shares: PRS / 15th FC report, Table 2.")
  ) +
  theme_del

ggsave(file.path(FIGDIR, "fig_pop_decomposition.png"), p,
       width = 10.5, height = 6.2, dpi = 300, bg = "white")
message("Saved: fig_pop_decomposition.png")
message("C4 complete.")
