# =============================================================================
# A3_fiscal.R
# Project: Delimitation 2026 -- the DOUBLE PENALTY extension
# Author:  Piyush Zaware
# Last updated: 2026-06-20
#
# Goal: Show that the states losing Lok Sabha seats are the same states that
#       lost fiscal share in the 15th Finance Commission, and that both
#       penalties flow from the same mechanism: replacing 1971 population with
#       2011 population, which rewards high-population (high-growth) states.
#
# Tax devolution inter-se shares (% of states' divisible pool):
#   14th FC (2015-20) vs 15th FC (2021-26). Source: PRS Legislative Research,
#   from the official 15th Finance Commission report, Table 2. OFFICIAL (Tier A).
#
# 15th FC states' pool over 2021-26 ~ Rs 42.2 lakh crore, so 1 percentage point
#   of inter-se share ~ Rs 42,200 crore over the award period.
#
# IN:  tmp/apportion.rds   (seat change + development index from A2)
# OUT: output/tables/double_penalty.csv
#      tmp/fiscal.rds
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

POOL_CR <- 4220000   # Rs crore to states over 2021-26 (41% of divisible pool)

# ── Finance Commission inter-se devolution shares ─────────────────────────────
#{
fc <- tribble(
  ~state,              ~fc14,    ~fc15,
  "Andhra Pradesh",     4.305,    4.047,
  "Arunachal Pradesh",  1.370,    1.757,
  "Assam",              3.311,    3.128,
  "Bihar",              9.665,   10.058,
  "Chhattisgarh",       3.080,    3.407,
  "Goa",                0.378,    0.386,
  "Gujarat",            3.084,    3.478,
  "Haryana",            1.084,    1.093,
  "Himachal Pradesh",   0.713,    0.830,
  "Jharkhand",          3.139,    3.307,
  "Karnataka",          4.713,    3.647,
  "Kerala",             2.500,    1.925,
  "Madhya Pradesh",     7.548,    7.850,
  "Maharashtra",        5.521,    6.317,
  "Manipur",            0.617,    0.716,
  "Meghalaya",          0.642,    0.767,
  "Mizoram",            0.460,    0.500,
  "Nagaland",           0.498,    0.569,
  "Odisha",             4.642,    4.528,
  "Punjab",             1.577,    1.807,
  "Rajasthan",          5.495,    6.026,
  "Sikkim",             0.367,    0.388,
  "Tamil Nadu",         4.023,    4.079,
  "Telangana",          2.437,    2.102,
  "Tripura",            0.642,    0.708,
  "Uttar Pradesh",     17.959,   17.939,
  "Uttarakhand",        1.052,    1.118,
  "West Bengal",        7.324,    7.523,
) %>%
  mutate(
    fiscal_chg_pts = fc15 - fc14,
    fiscal_chg_cr  = fiscal_chg_pts / 100 * POOL_CR   # Rs crore over 2021-26
  )
#}

# ── Merge with the political penalty (seat change + dev index from A2) ─────────
#{
ap <- readRDS(file.path(TMPDIR, "apportion.rds"))
seat <- ap$change %>% select(state, seats_now, chg_webster, pop_2026,
                             tfr, female_lit, nsdp_pc)

# development index (same construction as A2, computed over states present here)
dbl <- fc %>%
  inner_join(seat, by = "state") %>%
  mutate(
    z_tfr  = -scale(tfr)[, 1],
    z_lit  =  scale(female_lit)[, 1],
    z_nsdp =  scale(log(nsdp_pc))[, 1],
    dev_index = (z_tfr + z_lit + z_nsdp) / 3,
    loses_both = chg_webster < 0 & fiscal_chg_pts < 0
  )
#}

# ── Headline numbers ──────────────────────────────────────────────────────────
#{
south <- c("Tamil Nadu","Kerala","Karnataka","Andhra Pradesh","Telangana")

cat("\n=== The five southern states: BOTH penalties ===\n")
dbl %>%
  filter(state %in% south) %>%
  mutate(rs_cr = round(fiscal_chg_cr)) %>%
  select(state, seat_change = chg_webster,
         fiscal_pts = fiscal_chg_pts, fiscal_rs_crore = rs_cr) %>%
  arrange(fiscal_pts) %>%
  print()

cat(sprintf("\nSouth bloc: %d Lok Sabha seats lost, %.2f percentage points of the\n",
            -sum(dbl$chg_webster[dbl$state %in% south]),
            -sum(dbl$fiscal_chg_pts[dbl$state %in% south])))
cat(sprintf("divisible pool lost = roughly Rs %s crore over 2021-26.\n",
            format(round(-sum(dbl$fiscal_chg_cr[dbl$state %in% south]), -2),
                   big.mark = ",")))

cat("\n=== States that lose BOTH seats and fiscal share ===\n")
dbl %>% filter(loses_both) %>% arrange(dev_index) %>%
  select(state, chg_webster, fiscal_chg_pts, dev_index) %>% print()

# Correlation: does development predict the FISCAL penalty too?
r_fisc <- cor(dbl$dev_index, dbl$fiscal_chg_pts)
m_fisc <- lm(fiscal_chg_pts ~ dev_index, data = dbl)
cat(sprintf("\nCorr(development index, fiscal share change) = %+.2f (p = %.4f)\n",
            r_fisc, summary(m_fisc)$coefficients["dev_index", 4]))
cat(sprintf("Corr(seat change, fiscal share change)       = %+.2f\n",
            cor(dbl$chg_webster, dbl$fiscal_chg_pts)))
#}

# ── Save ──────────────────────────────────────────────────────────────────────
dir.create(file.path(OUTDIR, "tables"), showWarnings = FALSE, recursive = TRUE)
write_csv(dbl, file.path(OUTDIR, "tables", "double_penalty.csv"))
saveRDS(dbl, file.path(TMPDIR, "fiscal.rds"))
message("A3 complete.")
