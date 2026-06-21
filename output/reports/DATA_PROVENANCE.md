# Data Provenance: Delimitation 2026 Analysis

**Author:** Piyush Zaware
**Last updated:** 2026-06-20

Every series, its source, and its reliability. Same three-tier scheme as the farmer-suicides project.

| Series | Tier | Source | Verification |
|--------|:----:|--------|--------------|
| State population 2011 | **A** | Census of India 2011 | Sum across 36 states/UTs = 1,211,127 thousand, matches India 2011 census (1.21 billion) exactly |
| State population 2026, 2036 | **A** | National Commission on Population, *Population Projections for India and States 2011-2036* (2020), Table 8/21, total persons, '000 | Extracted directly from the report PDF. Tamil Nadu 2011-36 growth (+8%) matches the report's own stated figure; every 2011 value matches census |
| Current Lok Sabha seats | **A** | Election Commission of India / Lok Sabha | Sum = 543 elected seats; verified by hand |
| Total Fertility Rate | **A** | NFHS-5 (2019-21) state fact sheets | Standard published values |
| Female literacy | **A** | Census 2011 | Standard published values |
| Per-capita NSDP | **C** | MOSPI / RBI Handbook of Statistics on Indian States (approx, 2011-12 constant) | Approximate, used only for relative ranking in the composite development index. Not a headline number |

## Method provenance

- **Apportionment:** four standard methods implemented from first principles in `code/A2_apportion.R`: Webster/Sainte-Lague (the standard, also used by Carnegie 2026), Jefferson/D'Hondt, Huntington-Hill (US House), Hamilton/largest remainder. Every state guaranteed at least one seat. Verified: the five southern states lose 26 seats combined under all four methods, matching independent estimates (e.g., P. Chidambaram's "South loses 26").
- **Performance-penalty regression:** OLS of projected seat change on TFR and on a composite development index (z-scores of negative TFR, female literacy, log per-capita NSDP), restricted to 21 major states (current seats >= 4) so that min-1-seat UTs do not add mechanical zeros. Result: corr(development index, seat change) = -0.70, p = 0.0003, R-squared = 0.48.

## Key caveats to state in the piece

1. **Projections, not certainties.** 2026 and 2036 figures are NCP projections from 2020, made before the 2021 census (which has not been conducted). They are the best official numbers available but are projections.
2. **The 2021 census has not happened.** Any actual delimitation would use fresh census data that does not yet exist. This analysis uses NCP projections as the best proxy.
3. **Seat counts assume each state keeps >= 1 seat** and that delimitation reallocates by population alone, the principle in the (defeated) Constitution 131st Amendment Bill, 2026.
4. **Per-capita NSDP is approximate** and enters only the composite index, not any headline number. The TFR and literacy results (Tier A) carry the argument.

## What would strengthen this for a fuller paper

- Replace NSDP approximations with exact MOSPI state domestic product series.
- Add the 848-seat expansion scenario's effect on vote *share* (not just absolute seats) to show expansion does not protect the South's relative voice.
- Project the Samuels-Snyder malapportionment index forward to 2041.
