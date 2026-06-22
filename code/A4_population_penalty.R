# =============================================================================
# A4_population_penalty.R
# Project: Delimitation 2026 -- isolating the PURE POPULATION penalty
# Author:  Piyush Zaware
# Last updated: 2026-06-22
#
# Goal: The headline "South loses ~Rs 92,000 cr" is the NET change in tax-
#   devolution share between the 14th and 15th Finance Commissions. That net
#   bundles the 1971->2011 census switch together with the rest of the FC15
#   redesign (income-distance/equity weight, the NEW demographic-performance
#   and tax-effort criteria that REWARD the South, area, forest). A reviewer
#   correctly noted we cannot call the full 92k a "population penalty".
#
#   This code isolates the part attributable to the census-year switch ALONE.
#   Method: within the FC15 formula, the population criterion carries 15% weight
#   and is allocated by each state's share of population. We recompute that one
#   criterion using 1971 instead of 2011 population, holding EVERY other
#   criterion fixed at its FC15 value. The resulting change in each state's
#   share is the clean, ceteris-paribus population-year effect:
#
#       pop_penalty_i (points) = 0.15 * ( share2011_i - share1971_i ) * 100
#
#   Shares are computed over the 28 states in the FC15 inter-se distribution
#   (J&K excluded -- it became a UT in 2019). Because the population criterion's
#   per-state contributions sum to 15% under either census, the counterfactual
#   shares still sum to 100%.
#
# 1971 POPULATION (boundary-consistent): 1971 census totals from the official
#   census (via Wikipedia "List of states in India by past population"). The
#   four reorganized states are apportioned from their 1971 parent total by
#   their 2011 population proportion (a transparent, reproducible rule):
#     Andhra Pradesh / Telangana   <- combined 43,503k
#     Bihar / Jharkhand            <- combined 42,127k
#     Madhya Pradesh / Chhattisgarh<- combined 30,017k
#     Uttar Pradesh / Uttarakhand  <- combined 83,850k
#   For the South only Andhra/Telangana are split, so we also report the
#   Andhra+Telangana bloc combined, making the southern number boundary-proof.
#
# IN:  input/state_data.csv          (pop_2011 by current state, from A1)
# OUT: output/tables/population_penalty.csv
#      tmp/pop_penalty.rds
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

POOL_CR  <- 4220000   # Rs crore to states over 2021-26 (41% of divisible pool)
POP_WT   <- 0.15      # FC15 weight on the population criterion (2011 census)

sd <- read_csv(file.path(INPDIR, "state_data.csv"), show_col_types = FALSE)

# == FC15 inter-se devolution shares (28 states), with FC14 for the net change ==
# Source: PRS Legislative Research / 15th Finance Commission report, Table 2.
# (Duplicated from A3_fiscal.R so this script is self-contained and the
#  population-share denominator covers exactly the 28 FC15 states.)
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
  mutate(fiscal_chg_pts = fc15 - fc14)
#}

# == 1971 population ('000), boundary-consistent ===============================
#{
# Stable-boundary states: 1971 census figures used directly.
pop71_stable <- tribble(
  ~state,              ~pop_1971,
  "Maharashtra",        50412,
  "West Bengal",        44312,
  "Tamil Nadu",         41199,
  "Karnataka",          29299,
  "Gujarat",            26697,
  "Rajasthan",          25766,
  "Odisha",             21945,
  "Kerala",             21347,
  "Assam",              14625,
  "Punjab",             13551,
  "Haryana",            10036,
  "Himachal Pradesh",    3460,
  "Tripura",             1556,
  "Manipur",             1074,
  "Meghalaya",           1012,
  "Goa",                  796,
  "Nagaland",             516,
  "Arunachal Pradesh",    468,
  "Mizoram",              332,
  "Sikkim",               210,
)

# Reorganized states: apportion the 1971 parent total by the child's 2011 share.
split_def <- tribble(
  ~state,            ~parent_total,
  "Andhra Pradesh",  43503,
  "Telangana",       43503,
  "Bihar",           42127,
  "Jharkhand",       42127,
  "Madhya Pradesh",  30017,
  "Chhattisgarh",    30017,
  "Uttar Pradesh",   83850,
  "Uttarakhand",     83850,
)
pop71_split <- split_def %>%
  left_join(select(sd, state, pop_2011), by = "state") %>%
  group_by(parent_total) %>%
  mutate(pop_1971 = parent_total * pop_2011 / sum(pop_2011)) %>%
  ungroup() %>%
  select(state, pop_1971)

pop71 <- bind_rows(pop71_stable, pop71_split)
#}

# == Counterfactual: swap 2011 -> 1971 in the 15% population criterion =========
#{
dat <- fc %>%
  left_join(select(sd, state, pop_2011), by = "state") %>%
  left_join(pop71, by = "state") %>%
  mutate(
    share_2011  = pop_2011 / sum(pop_2011),
    share_1971  = pop_1971 / sum(pop_1971),
    # ceteris-paribus effect of the census-year switch on the FC15 share:
    pop_pen_pts = POP_WT * (share_2011 - share_1971) * 100,   # percentage points
    pop_pen_cr  = POP_WT * (share_2011 - share_1971) * POOL_CR,
    # FC15 share the state WOULD have had if population stayed on 1971 census:
    fc15_cf     = fc15 - pop_pen_pts,
    # everything in the net FC14->FC15 change that is NOT the census-year switch:
    other_pts   = fiscal_chg_pts - pop_pen_pts,
    other_cr    = (fiscal_chg_pts / 100 * POOL_CR) - pop_pen_cr
  )

stopifnot(nrow(dat) == 28, !any(is.na(dat$pop_1971)),
          abs(sum(dat$share_2011) - 1) < 1e-9,
          abs(sum(dat$share_1971) - 1) < 1e-9)
#}

# == Headline numbers =========================================================
#{
south <- c("Tamil Nadu","Kerala","Karnataka","Andhra Pradesh","Telangana")

bloc <- function(d) tibble(
  net_pts      = sum(d$fiscal_chg_pts),
  net_cr       = sum(d$fiscal_chg_pts) / 100 * POOL_CR,
  pop_pen_pts  = sum(d$pop_pen_pts),
  pop_pen_cr   = sum(d$pop_pen_cr),
  other_cr     = sum(d$other_cr),
  pop_share_of_net = sum(d$pop_pen_cr) / (sum(d$fiscal_chg_pts) / 100 * POOL_CR)
)

cat("\n=== Per-state decomposition of the FC14->FC15 change (South) ===\n")
dat %>% filter(state %in% south) %>%
  transmute(state,
            net_cr      = round(fiscal_chg_pts / 100 * POOL_CR),
            pop_year_cr = round(pop_pen_cr),
            other_cr    = round(other_cr),
            share_71    = round(share_1971 * 100, 2),
            share_11    = round(share_2011 * 100, 2)) %>%
  arrange(net_cr) %>% print(n = 5)

cat("\n=== South bloc (5 states) ===\n")
print(bloc(filter(dat, state %in% south)))

cat("\n--- Andhra+Telangana COMBINED (boundary-proof) ---\n")
at <- dat %>% filter(state %in% c("Andhra Pradesh","Telangana")) %>%
  summarise(pop_pen_cr = sum(pop_pen_cr), net_cr = sum(fiscal_chg_pts)/100*POOL_CR)
print(at)

south_net <- sum(dat$fiscal_chg_pts[dat$state %in% south]) / 100 * POOL_CR
south_pop <- sum(dat$pop_pen_cr[dat$state %in% south])
cat(sprintf(
"\nSUMMARY: Of the South's net loss of Rs %s cr, the pure 1971->2011 population
switch accounts for Rs %s cr (%.0f%%). The remaining Rs %s cr reflects the rest
of the FC15 redesign (equity/income-distance, the new demographic-performance
and tax-effort credits that REWARD the South, area, forest, and the lower
population weight).\n",
  format(round(-south_net, -2), big.mark = ","),
  format(round(-south_pop, -2), big.mark = ","),
  100 * south_pop / south_net,
  format(round(-(south_net - south_pop), -2), big.mark = ",")))
#}

# == Save =====================================================================
dir.create(file.path(OUTDIR, "tables"), showWarnings = FALSE, recursive = TRUE)
write_csv(dat, file.path(OUTDIR, "tables", "population_penalty.csv"))
saveRDS(dat, file.path(TMPDIR, "pop_penalty.rds"))
message("A4 complete.")
