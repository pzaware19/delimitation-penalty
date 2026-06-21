# =============================================================================
# A2_apportion.R
# Project: Delimitation 2026
# Author:  Piyush Zaware
# Last updated: 2026-06-20
#
# Goal: Allocate Lok Sabha seats to states by population under several
#       apportionment methods and scenarios, and quantify the "performance
#       penalty": the correlation between a state's seat change and its
#       development record.
#
# Methods (all "highest averages", every state guaranteed >= 1 seat):
#   - Webster / Sainte-Lague   (divisor 2s+1) -- the standard, used by Carnegie
#   - Jefferson / D'Hondt      (divisor s+1)  -- favours large states
#   - Huntington-Hill          (divisor sqrt(s(s+1))) -- the US House method
#   - Hamilton / largest remainder (Hare quota) -- intuitive benchmark
#
# Scenarios:
#   - Reallocate 543 seats by 2026 population (the live "fair share" scenario)
#   - Reallocate 543 seats by 2036 population (the trajectory)
#   - Expand to 848 seats by 2026 population (the government's escape hatch)
#
# IN:  input/state_data.csv
# OUT: output/tables/apportionment_results.csv
#      tmp/apportion.rds   (for figures)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

dat <- read_csv(file.path(INPDIR, "state_data.csv"), show_col_types = FALSE)

# ── Highest-averages allocator (generic divisor) ──────────────────────────────
# divisor_fn(s) returns the divisor applied to a state currently holding s seats
# when competing for its next seat. Every state starts with min_seats.
#{
alloc_highest_avg <- function(pop, total, divisor_fn, min_seats = 1) {
  n <- length(pop)
  seats <- rep(min_seats, n)
  remaining <- total - sum(seats)
  if (remaining < 0) stop("total seats < number of states * min_seats")
  for (i in seq_len(remaining)) {
    priority <- pop / divisor_fn(seats)
    j <- which.max(priority)
    seats[j] <- seats[j] + 1
  }
  seats
}

div_webster   <- function(s) 2 * s + 1
div_jefferson <- function(s) s + 1
div_hh        <- function(s) ifelse(s == 0, 1e-9, sqrt(s * (s + 1)))  # min_seats>=1 so s>=1

# Hamilton / largest remainder
alloc_hamilton <- function(pop, total, min_seats = 1) {
  n <- length(pop)
  base <- rep(min_seats, n)
  rem_total <- total - sum(base)
  quota <- rem_total * pop / sum(pop)
  add <- floor(quota)
  left <- rem_total - sum(add)
  frac <- quota - add
  if (left > 0) {
    ord <- order(frac, decreasing = TRUE)[seq_len(left)]
    add[ord] <- add[ord] + 1
  }
  base + add
}
#}

# ── Run all methods x scenarios ───────────────────────────────────────────────
#{
allocate_all <- function(pop, total, label) {
  tibble(
    state     = dat$state,
    scenario  = label,
    webster   = alloc_highest_avg(pop, total, div_webster),
    jefferson = alloc_highest_avg(pop, total, div_jefferson),
    hh        = alloc_highest_avg(pop, total, div_hh),
    hamilton  = alloc_hamilton(pop, total)
  )
}

N_NOW <- sum(dat$seats_now)   # 543

res <- bind_rows(
  allocate_all(dat$pop_2026, N_NOW, "543 by 2026 pop"),
  allocate_all(dat$pop_2036, N_NOW, "543 by 2036 pop"),
  allocate_all(dat$pop_2026, 848,   "848 by 2026 pop")
) %>%
  left_join(dat %>% select(state, seats_now, pop_2011, pop_2026, pop_2036,
                            tfr, female_lit, nsdp_pc), by = "state")
#}

# ── Seat change vs current (Webster headline) ─────────────────────────────────
#{
change_543_2026 <- res %>%
  filter(scenario == "543 by 2026 pop") %>%
  mutate(
    chg_webster   = webster   - seats_now,
    chg_jefferson = jefferson - seats_now,
    chg_hh        = hh        - seats_now,
    chg_hamilton  = hamilton  - seats_now
  )

cat("\n=== 543 seats reallocated by 2026 population (Webster) ===\n")
change_543_2026 %>%
  arrange(chg_webster) %>%
  select(state, seats_now, webster, chg_webster) %>%
  print(n = 30)

cat("\nSeats reallocated (sum of positive changes):",
    sum(change_543_2026$chg_webster[change_543_2026$chg_webster > 0]), "\n")

# Robustness: does the South lose under ALL four methods?
south <- c("Tamil Nadu","Kerala","Karnataka","Andhra Pradesh","Telangana")
cat("\n=== Southern states: seat change under each method (543 by 2026) ===\n")
change_543_2026 %>%
  filter(state %in% south) %>%
  select(state, chg_webster, chg_jefferson, chg_hh, chg_hamilton) %>%
  print()
#}

# ── The performance penalty: development index vs seat change ──────────────────
# Restrict to the 21 major states (current seats >= 4). The tiny min-1-seat UTs
# cannot mechanically change and would add uninformative zeros to the regression.
#{
penalty <- change_543_2026 %>%
  filter(seats_now >= 4) %>%
  mutate(
    z_tfr    = -scale(tfr)[, 1],          # lower TFR = better, so negate
    z_lit    =  scale(female_lit)[, 1],
    z_nsdp   =  scale(log(nsdp_pc))[, 1],
    dev_index = (z_tfr + z_lit + z_nsdp) / 3,
    # seat change per crore of population, to compare like with like
    chg_per_crore = chg_webster / (pop_2026 / 10000)
  )

# Regressions
m_tfr   <- lm(chg_webster ~ tfr, data = penalty)
m_index <- lm(chg_webster ~ dev_index, data = penalty)
m_index_w <- lm(chg_webster ~ dev_index, data = penalty,
                weights = pop_2026)

cat("\n=== Performance penalty regressions ===\n")
cat(sprintf("Seat change ~ TFR:        slope = %+.2f seats per unit TFR (p = %.4f), R2 = %.2f\n",
            coef(m_tfr)["tfr"], summary(m_tfr)$coefficients["tfr", 4],
            summary(m_tfr)$r.squared))
cat(sprintf("Seat change ~ dev index:  slope = %+.2f seats per SD (p = %.4f), R2 = %.2f\n",
            coef(m_index)["dev_index"], summary(m_index)$coefficients["dev_index", 4],
            summary(m_index)$r.squared))
cat(sprintf("Corr(TFR, seat change)      = %+.2f\n", cor(penalty$tfr, penalty$chg_webster)))
cat(sprintf("Corr(female lit, seat chg)  = %+.2f\n", cor(penalty$female_lit, penalty$chg_webster)))
cat(sprintf("Corr(dev index, seat chg)   = %+.2f\n", cor(penalty$dev_index, penalty$chg_webster)))
#}

# ── Malapportionment now: population per seat ─────────────────────────────────
#{
malapp <- dat %>%
  mutate(pop_per_seat_2026 = pop_2026 / seats_now) %>%
  arrange(desc(pop_per_seat_2026))
cat("\n=== Most under-represented (highest people per seat, 2026 pop) ===\n")
malapp %>% select(state, seats_now, pop_per_seat_2026) %>% head(6) %>% print()
cat("\n=== Most over-represented (fewest people per seat) ===\n")
malapp %>% select(state, seats_now, pop_per_seat_2026) %>% tail(6) %>% print()
#}

# ── Save ──────────────────────────────────────────────────────────────────────
dir.create(file.path(OUTDIR, "tables"), showWarnings = FALSE, recursive = TRUE)
write_csv(res, file.path(OUTDIR, "tables", "apportionment_results.csv"))
saveRDS(list(res = res, change = change_543_2026, penalty = penalty, malapp = malapp),
        file.path(TMPDIR, "apportion.rds"))
message("A2 complete. Results saved.")
