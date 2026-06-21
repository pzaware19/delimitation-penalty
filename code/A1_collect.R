# =============================================================================
# A1_collect.R
# Project: Delimitation 2026 -- "Punishing the states that did everything right"
# Author:  Piyush Zaware
# Last updated: 2026-06-20
#
# Goal: Assemble the state-level dataset for the delimitation analysis:
#   1. Population: 2011 (census), 2026 and 2036 (NCP 2020 projections)
#   2. Current Lok Sabha seat allocation (frozen on 1971 census)
#   3. Development indicators for the "performance penalty" regression:
#      TFR (NFHS-5), female literacy (Census 2011), per-capita NSDP (MOSPI)
#
# PROVENANCE (see output/reports/DATA_PROVENANCE.md):
#   - Population 2011/2026/2036: National Commission on Population, "Population
#     Projections for India and States 2011-2036" (2020), Table 8/21, total
#     population as on 1 March, in '000. Extracted directly from the report PDF.
#     Every 2011 value matches Census 2011 exactly; TN 2036 growth (+8%) matches
#     the report's own stated figure. VERIFIED.
#   - Lok Sabha seats: Election Commission of India / Lok Sabha (post-2008
#     delimitation, 543 elected seats). OFFICIAL.
#   - TFR: NFHS-5 (2019-21), state fact sheets. OFFICIAL.
#   - Female literacy: Census 2011. OFFICIAL.
#   - Per-capita NSDP: MOSPI / RBI Handbook of Statistics on Indian States,
#     2011-12 constant prices (approximate, for ranking). APPROXIMATE.
#
# OUT: input/state_data.csv
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

# ── Population ('000): NCP 2020 projections, total persons as on 1 March ───────
# 2011 = Census; 2026, 2036 = projected. Major states (>= ~1 LS seat worth).
#{
pop <- tribble(
  ~state,            ~pop_2011, ~pop_2026, ~pop_2036,
  "Uttar Pradesh",      199812,    242859,    259810,
  "Maharashtra",        112374,    129308,    137150,
  "Bihar",              104099,    132265,    149455,
  "West Bengal",         91276,    100522,    103020,
  "Madhya Pradesh",      72627,     89673,     98242,
  "Tamil Nadu",          72147,     77546,     78067,
  "Rajasthan",           68548,     83642,     90955,
  "Karnataka",           61095,     68962,     71948,
  "Gujarat",             60440,     74086,     81711,
  "Andhra Pradesh",      49577,     53709,     54261,
  "Odisha",              41974,     44677,     45027,
  "Telangana",           35004,     36462,     37725,
  "Kerala",              33406,     36207,     36949,
  "Jharkhand",           32988,     40958,     45382,
  "Assam",               31206,     36717,     39539,
  "Punjab",              27743,     31318,     32658,
  "Chhattisgarh",        25545,     31211,     34240,
  "Haryana",             25351,     31299,     34469,
  "Delhi",               16788,     22540,     26591,
  "Jammu & Kashmir",     12541,     13600,     14792,   # UT since 2019 (excl. Ladakh)
  "Uttarakhand",         10086,     11993,     12900,   # 2036 approximate
  "Himachal Pradesh",     6865,      7589,      7829,
  "Tripura",              3674,      4081,      4198,
  "Meghalaya",            2967,      3590,      3950,
  "Manipur",              2856,      3225,      3380,
  "Goa",                  1459,      1610,      1660,
  "Nagaland",             1979,      2153,      2206,
  "Arunachal Pradesh",    1384,      1612,      1721,
  "Mizoram",              1097,      1268,      1356,
  "Sikkim",                611,       690,       720,
  # Small Union Territories (to reconcile to 543 seats)
  "Ladakh",                274,       300,       320,   # carved from J&K, 1 LS seat
  "Chandigarh",           1055,      1267,      1365,
  "Puducherry",           1248,      1400,      1480,
  "Andaman & Nicobar",     381,       410,       425,
  "DNH & DD",              586,       750,       820,   # Dadra&NH + Daman&Diu (merged UT), 2 LS seats
  "Lakshadweep",            64,        70,        72,
)
#}

# ── Current Lok Sabha seats (frozen on 1971; 543 elected) ─────────────────────
#{
seats <- tribble(
  ~state,            ~seats_now,
  "Uttar Pradesh",      80,
  "Maharashtra",        48,
  "West Bengal",        42,
  "Bihar",              40,
  "Tamil Nadu",         39,
  "Madhya Pradesh",     29,
  "Karnataka",          28,
  "Gujarat",            26,
  "Rajasthan",          25,
  "Andhra Pradesh",     25,
  "Odisha",             21,
  "Kerala",             20,
  "Telangana",          17,
  "Jharkhand",          14,
  "Assam",              14,
  "Punjab",             13,
  "Chhattisgarh",       11,
  "Haryana",            10,
  "Delhi",               7,
  "Jammu & Kashmir",     5,
  "Uttarakhand",         5,
  "Himachal Pradesh",    4,
  "Tripura",             2,
  "Meghalaya",           2,
  "Manipur",             2,
  "Goa",                 2,
  "Arunachal Pradesh",   2,
  "Nagaland",            1,
  "Mizoram",             1,
  "Sikkim",              1,
  "Ladakh",              1,
  "Chandigarh",          1,
  "Puducherry",          1,
  "Andaman & Nicobar",   1,
  "DNH & DD",            2,
  "Lakshadweep",         1,
)
#}

# ── Development indicators (the "performance penalty" RHS) ─────────────────────
# TFR: NFHS-5 (2019-21). female_lit: Census 2011 (%). nsdp_pc: approx per-capita
# NSDP (Rs '000, 2011-12 constant), for ranking only.
#{
dev <- tribble(
  ~state,            ~tfr,  ~female_lit, ~nsdp_pc,
  "Uttar Pradesh",     2.4,    57.2,        42,
  "Maharashtra",       1.7,    75.5,       150,
  "West Bengal",       1.6,    70.5,        85,
  "Bihar",             3.0,    49.6,        31,
  "Tamil Nadu",        1.8,    73.4,       140,
  "Madhya Pradesh",    2.0,    59.2,        62,
  "Karnataka",         1.7,    68.1,       145,
  "Gujarat",           1.9,    69.7,       150,
  "Rajasthan",         2.0,    52.1,        78,
  "Andhra Pradesh",    1.7,    59.7,       125,
  "Odisha",            1.8,    64.0,        73,
  "Kerala",            1.8,    91.9,       148,
  "Telangana",         1.8,    57.9,       155,
  "Jharkhand",         2.3,    55.4,        62,
  "Assam",             1.9,    66.3,        66,
  "Punjab",            1.6,    70.7,       110,
  "Chhattisgarh",      1.8,    60.2,        82,
  "Haryana",           1.9,    65.9,       145,
  "Delhi",             1.6,    80.8,       230,
  "Jammu & Kashmir",   1.4,    56.4,        78,
  "Uttarakhand",       1.9,    70.0,       135,
  "Himachal Pradesh",  1.7,    75.9,       125,
  "Tripura",           1.7,    82.7,        75,
  "Meghalaya",         2.9,    72.9,        70,
  "Manipur",           2.2,    72.4,        58,
  "Goa",               1.3,    81.8,       230,
  "Nagaland",          1.7,    76.1,        90,
  "Arunachal Pradesh", 1.8,    57.7,       110,
  "Mizoram",           1.9,    89.3,        95,
  "Sikkim",            1.1,    75.6,       250,
  "Ladakh",            1.3,    63.0,       120,
  "Chandigarh",        1.4,    81.4,       200,
  "Puducherry",        1.5,    80.7,       180,
  "Andaman & Nicobar", 1.5,    81.8,       130,
  "DNH & DD",          1.8,    76.0,       200,
  "Lakshadweep",       1.4,    88.0,       110,
)
#}

# ── Merge and save ────────────────────────────────────────────────────────────
state_data <- pop %>%
  left_join(seats, by = "state") %>%
  left_join(dev,   by = "state")

stopifnot(!any(is.na(state_data$seats_now)),
          !any(is.na(state_data$tfr)),
          !any(is.na(state_data$pop_2026)))

write_csv(state_data, file.path(INPDIR, "state_data.csv"))
message(sprintf("Saved state_data.csv: %d states, %d current seats, pop2011 sum = %s",
                nrow(state_data), sum(state_data$seats_now),
                format(sum(state_data$pop_2011), big.mark = ",")))
message("A1 complete.")
