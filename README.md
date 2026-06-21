# The Delimitation Penalty

A data-driven analysis of India's 2026 delimitation debate: the states set to lose Lok Sabha seats under population-based reapportionment are, almost exactly, the states that did best on the development goals national policy has pursued for fifty years.

**Live site:** https://pzaware19.github.io/delimitation-penalty/

## The finding

Reapportioning the 543 Lok Sabha seats by 2026 population shifts 41 seats. Uttar Pradesh gains 11 and Bihar 10; Tamil Nadu loses 10 and Kerala 6. The five southern states lose 26 seats combined, the same under all four standard apportionment methods. Across the 21 major states, the correlation between a state's development record (fertility, female literacy, income) and its projected seat change is **−0.70** (p = 0.0003, R-squared = 0.48).

## Structure

```
code/    A1_collect.R     assemble population, seats, development indicators
         A2_apportion.R   four apportionment methods + performance-penalty regression
         C1_figures.R     five figures
         E1_hero_image.py hero data-art
         _master.R        pipeline
input/   state_data.csv, NCP projection report (source PDF)
output/  figures/, tables/, reports/ (incl. DATA_PROVENANCE.md)
docs/    rendered Quarto website (GitHub Pages source)
```

## Reproduce

```r
source("code/_master.R")   # runs A1 -> A2 -> C1
```
```bash
python3 code/E1_hero_image.py   # hero image
quarto render                   # build docs/
```

## Data

Census of India 2011; National Commission on Population (2020) projections; Election Commission of India; NFHS-5; MOSPI. Every series and its reliability is documented in `output/reports/DATA_PROVENANCE.md`. The 2026/2036 figures are official projections, not certainties, since the delayed census has not been conducted.

## Author

Piyush Zaware, Global Poverty Research Laboratory, Northwestern Kellogg / University of Chicago.
