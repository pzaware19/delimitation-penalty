# =============================================================================
# C3_figures_marathi.R
# Project: Delimitation 2026 -- Marathi figures
# Author:  Piyush Zaware
# Last updated: 2026-06-20
#
# Regenerates all figures with Marathi (Devanagari) labels for the Marathi site.
# Uses the ragg device with the system "Kohinoor Devanagari" font.
#
# IN:  tmp/apportion.rds, tmp/fiscal.rds
# OUT: output/figures_mr/*.png
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)
  library(ragg)
})

MRDIR <- file.path(OUTDIR, "figures_mr")
dir.create(MRDIR, showWarnings = FALSE, recursive = TRUE)
FONT <- "Kohinoor Devanagari"

obj  <- readRDS(file.path(TMPDIR, "apportion.rds"))
fis  <- readRDS(file.path(TMPDIR, "fiscal.rds"))
change <- obj$change; penalty <- obj$penalty; malapp <- obj$malapp

# Marathi state names
mr <- c(
  "Uttar Pradesh"="उत्तर प्रदेश","Maharashtra"="महाराष्ट्र","Bihar"="बिहार",
  "West Bengal"="पश्चिम बंगाल","Madhya Pradesh"="मध्य प्रदेश","Tamil Nadu"="तामिळनाडू",
  "Rajasthan"="राजस्थान","Karnataka"="कर्नाटक","Gujarat"="गुजरात",
  "Andhra Pradesh"="आंध्र प्रदेश","Odisha"="ओडिशा","Telangana"="तेलंगणा",
  "Kerala"="केरळ","Jharkhand"="झारखंड","Assam"="आसाम","Punjab"="पंजाब",
  "Chhattisgarh"="छत्तीसगड","Haryana"="हरियाणा","Delhi"="दिल्ली",
  "Jammu & Kashmir"="जम्मू-काश्मीर","Uttarakhand"="उत्तराखंड","Himachal Pradesh"="हिमाचल प्रदेश",
  "Tripura"="त्रिपुरा","Meghalaya"="मेघालय","Manipur"="मणिपूर","Goa"="गोवा",
  "Nagaland"="नागालँड","Arunachal Pradesh"="अरुणाचल प्रदेश","Mizoram"="मिझोराम","Sikkim"="सिक्कीम")
mrn <- function(s) ifelse(s %in% names(mr), mr[s], s)

south <- c("Tamil Nadu","Kerala","Karnataka","Andhra Pradesh","Telangana")
hindi <- c("Uttar Pradesh","Bihar","Madhya Pradesh","Rajasthan","Jharkhand",
           "Chhattisgarh","Haryana","Delhi","Uttarakhand","Himachal Pradesh")
classify <- function(s) ifelse(s %in% south, "दक्षिण",
                        ifelse(s %in% hindi, "हिंदी पट्टा", "उर्वरित भारत"))
reg_cols <- c("दक्षिण"="#C0392B","हिंदी पट्टा"="#2C6FBB","उर्वरित भारत"="#9AA0A6")

theme_mr <- theme_minimal(base_size = 12, base_family = FONT) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, colour = "#1a1a1a", family = FONT),
    plot.subtitle = element_text(size = 10.5, colour = "#555555", family = FONT, margin = margin(b = 12)),
    plot.caption  = element_text(size = 8, colour = "#999999", hjust = 0, family = FONT),
    panel.grid.minor = element_blank(),
    axis.title    = element_text(size = 10.5, colour = "#333333", family = FONT),
    axis.text     = element_text(family = FONT),
    legend.position = "top", legend.title = element_blank(),
    legend.text   = element_text(family = FONT)
  )
save_mr <- function(p, name, w, h) {
  ggsave(file.path(MRDIR, name), p, width = w, height = h, dpi = 300,
         bg = "white", device = ragg::agg_png)
  message("Saved: ", name)
}

# ── 1. Performance penalty scatter (TFR) ──────────────────────────────────────
pen <- penalty %>% mutate(region = classify(state), state_mr = mrn(state))
r_tfr <- cor(pen$tfr, pen$chg_webster)
p1 <- ggplot(pen, aes(tfr, chg_webster)) +
  geom_hline(yintercept = 0, colour = "#BBBBBB", linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, colour = "#333333", fill = "#E8E8E8", linewidth = 0.7) +
  geom_point(aes(colour = region, size = pop_2026), alpha = 0.9) +
  geom_text_repel(aes(label = state_mr, colour = region), size = 3, family = FONT,
                  fontface = "bold", max.overlaps = 20, show.legend = FALSE, seed = 1) +
  annotate("text", x = 1.45, y = -9, label = paste0("r = ", sprintf('%.2f', r_tfr)),
           hjust = 0, size = 3.4, colour = "#C0392B", family = FONT) +
  scale_colour_manual(values = reg_cols) +
  scale_size_continuous(range = c(2, 11), guide = "none") +
  scale_x_continuous(breaks = seq(1.4, 3.0, 0.2)) + scale_y_continuous(breaks = seq(-10, 12, 2)) +
  labs(title = "ज्या राज्यांनी जन्मदर कमी केला, तीच जागा गमावत आहेत",
       subtitle = "प्रत्येक राज्याचा अपेक्षित लोकसभा जागाबदल (२०२६ लोकसंख्येनुसार ५४३ जागांचे पुनर्वाटप) विरुद्ध त्याचा प्रजनन दर.\nबिंदूचा आकार = लोकसंख्या.",
       x = "एकूण प्रजनन दर (NFHS-5, 2019-21)", y = "लोकसभा जागांमधील अपेक्षित बदल",
       caption = "स्रोत: जागावाटप (Webster पद्धत) NCP 2020 प्रक्षेपणांवरून; प्रजनन दर NFHS-5. नमुना: 21 प्रमुख राज्ये. p < 0.001, R-squared = 0.48.") +
  theme_mr
save_mr(p1, "fig_penalty_scatter.png", 10.5, 7)

# ── 2. Female literacy companion ──────────────────────────────────────────────
r_lit <- cor(pen$female_lit, pen$chg_webster)
p2 <- ggplot(pen, aes(female_lit, chg_webster)) +
  geom_hline(yintercept = 0, colour = "#BBBBBB", linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, colour = "#333333", fill = "#E8E8E8", linewidth = 0.7) +
  geom_point(aes(colour = region, size = pop_2026), alpha = 0.9) +
  geom_text_repel(aes(label = state_mr, colour = region), size = 3, family = FONT,
                  fontface = "bold", max.overlaps = 20, show.legend = FALSE, seed = 1) +
  annotate("text", x = 88, y = 8, label = paste0("r = ", sprintf('%.2f', r_lit)),
           hjust = 0, size = 3.4, colour = "#C0392B", family = FONT) +
  scale_colour_manual(values = reg_cols) + scale_size_continuous(range = c(2, 11), guide = "none") +
  labs(title = "राज्यातील स्त्रिया जितक्या अधिक साक्षर, तितक्या अधिक जागा गमावल्या",
       subtitle = "अपेक्षित जागाबदल विरुद्ध स्त्री साक्षरता. हे नाते न्याय्यतेच्या विरुद्ध दिशेने जाते.",
       x = "स्त्री साक्षरता दर, % (जनगणना 2011)", y = "लोकसभा जागांमधील अपेक्षित बदल",
       caption = "स्रोत: जागावाटप (Webster) NCP 2020 वरून; स्त्री साक्षरता जनगणना 2011. 21 प्रमुख राज्ये.") +
  theme_mr
save_mr(p2, "fig_penalty_literacy.png", 10.5, 7)

# ── 3. Seat change diverging bar ──────────────────────────────────────────────
bar <- change %>% filter(seats_now >= 2) %>%
  mutate(region = classify(state), state_mr = fct_reorder(mrn(state), chg_webster))
p3 <- ggplot(bar, aes(state_mr, chg_webster, fill = region)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = ifelse(chg_webster > 0, paste0("+", chg_webster), chg_webster),
                hjust = ifelse(chg_webster > 0, -0.3, 1.3)), size = 2.8, colour = "#333333", family = FONT) +
  coord_flip() + scale_fill_manual(values = reg_cols) +
  scale_y_continuous(breaks = seq(-10, 12, 2), expand = expansion(mult = 0.08)) +
  labs(title = "लोकसंख्येनुसार 543 जागांचे पुनर्वाटप झाल्यास कोणाला फायदा, कोणाला तोटा",
       subtitle = "अपेक्षित लोकसभा जागाबदल, 2026 लोकसंख्या, Webster पद्धत. दक्षिण व पूर्व गमावतात; हिंदी पट्टा मिळवतो.",
       x = NULL, y = "लोकसभा जागांमधील बदल",
       caption = "स्रोत: NCP 2020 प्रक्षेपणांवरून गणित. पाच दक्षिणी राज्ये मिळून 26 जागा गमावतात, चारही पद्धतींत समान.") +
  theme_mr + theme(panel.grid.major.y = element_blank())
save_mr(p3, "fig_seat_change.png", 9.5, 8)

# ── 4. Malapportionment ───────────────────────────────────────────────────────
mal <- malapp %>% filter(seats_now >= 2) %>%
  mutate(region = classify(state), state_mr = fct_reorder(mrn(state), pop_per_seat_2026),
         m_people = pop_per_seat_2026 / 1000)
avg_line <- sum(malapp$pop_2026) / sum(malapp$seats_now) / 1000
p4 <- ggplot(mal, aes(state_mr, m_people, fill = region)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = avg_line, linetype = "dashed", colour = "#444444") +
  annotate("text", x = 3, y = avg_line + 0.05, label = sprintf("राष्ट्रीय सरासरी\n%.1f दशलक्ष / जागा", avg_line),
           hjust = 0, size = 2.8, colour = "#444444", family = FONT) +
  coord_flip() + scale_fill_manual(values = reg_cols) +
  labs(title = "एका जागेमागे किती लोक? 1971 पासून गोठलेली विषमता",
       subtitle = "सध्याच्या प्रत्येक लोकसभा जागेमागे लोकसंख्या (2026). उत्तरेकडील मतदार आधीच कमी प्रतिनिधित्व; दक्षिण जास्त प्रतिनिधित्व.",
       x = NULL, y = "प्रति लोकसभा जागा दशलक्ष लोक",
       caption = "स्रोत: NCP 2020 प्रक्षेपण; सध्याचे जागावाटप. जागा पाच दशकांहून अधिक काळ 1971 च्या जनगणनेवर गोठलेल्या आहेत.") +
  theme_mr + theme(panel.grid.major.y = element_blank())
save_mr(p4, "fig_malapportionment.png", 9.5, 8)

# ── 5. Formula robustness ─────────────────────────────────────────────────────
rob <- change %>% filter(state %in% south) %>%
  transmute(state_mr = mrn(state), Webster = chg_webster, Jefferson = chg_jefferson,
            HH = chg_hh, Hamilton = chg_hamilton) %>%
  pivot_longer(-state_mr, names_to = "method", values_to = "chg") %>%
  mutate(state_mr = fct_reorder(state_mr, chg),
         method = recode(method, HH = "Huntington-Hill"))
p5 <- ggplot(rob, aes(state_mr, chg, fill = method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, colour = "#888888") +
  scale_fill_manual(values = c("Webster"="#C0392B","Jefferson"="#E67E22",
                               "Huntington-Hill"="#8E44AD","Hamilton"="#16A085")) +
  labs(title = "कोणतीही पद्धत वापरा: दक्षिण तरीही गमावतेच",
       subtitle = "चार प्रमाणित जागावाटप पद्धतींनुसार पाच दक्षिणी राज्यांचा अपेक्षित जागाबदल.\nगणिताची निवड जवळपास काहीच बदलत नाही.",
       x = NULL, y = "अपेक्षित जागाबदल (543, 2026 लोकसंख्या)",
       caption = "पद्धती: Webster/Sainte-Lague, Jefferson/D'Hondt, Huntington-Hill (अमेरिकी सभागृह), Hamilton.") +
  theme_mr
save_mr(p5, "fig_formula_robustness.png", 10, 6)

# ── 6. Double penalty scatter ─────────────────────────────────────────────────
d <- fis %>% filter(seats_now >= 2) %>% mutate(region = classify(state), state_mr = mrn(state))
p6 <- ggplot(d, aes(fiscal_chg_pts, chg_webster)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "#C0392B", alpha = 0.05) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "#2C6FBB", alpha = 0.05) +
  geom_hline(yintercept = 0, colour = "#BBBBBB", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "#BBBBBB", linewidth = 0.4) +
  geom_point(aes(colour = region, size = pop_2026), alpha = 0.9) +
  geom_text_repel(aes(label = state_mr, colour = region), size = 3, family = FONT,
                  fontface = "bold", max.overlaps = 18, show.legend = FALSE, seed = 3) +
  annotate("text", x = -0.95, y = -9.3, hjust = 0, vjust = 0,
           label = "दोन्ही गमावतात\nआवाज आणि पैसा", size = 3.2, colour = "#C0392B", family = FONT, lineheight = 0.95) +
  annotate("text", x = 0.78, y = 11, hjust = 1, vjust = 1,
           label = "दोन्ही मिळवतात", size = 3.2, colour = "#2C6FBB", family = FONT) +
  scale_colour_manual(values = reg_cols) + scale_size_continuous(range = c(2, 11), guide = "none") +
  scale_x_continuous(breaks = seq(-1, 0.8, 0.25)) + scale_y_continuous(breaks = seq(-10, 12, 2)) +
  labs(title = "दुहेरी दंड: तीच राज्ये जागा आणि पैसा दोन्ही गमावतात",
       subtitle = "अपेक्षित लोकसभा जागाबदल विरुद्ध कर-वाटप वाट्यातील बदल (14व्या ते 15व्या वित्त आयोगादरम्यान).\nदोन्ही दंड दक्षिणेवर पडतात, आणि दोन्ही एकाच बदलातून येतात: 1971 ऐवजी 2011 लोकसंख्या.",
       x = "कर-वाटप वाट्यातील बदल, 14वा ते 15वा वित्त आयोग (टक्के बिंदू)",
       y = "लोकसभा जागांमधील अपेक्षित बदल",
       caption = "स्रोत: जागाबदल NCP 2020 वरून (Webster); वाटप वाटे PRS / 15व्या वित्त आयोग अहवाल, तक्ता 2.") +
  theme_mr
save_mr(p6, "fig_double_penalty.png", 10.5, 7.2)

# ── 7. Fiscal slopegraph ──────────────────────────────────────────────────────
sl <- fis %>% filter(state %in% c(south,"Uttar Pradesh","Bihar","Maharashtra","Rajasthan","Gujarat","Madhya Pradesh")) %>%
  mutate(region = classify(state), state_mr = mrn(state)) %>%
  select(state_mr, region, fc14, fc15) %>%
  pivot_longer(c(fc14, fc15), names_to = "fc", values_to = "share") %>%
  mutate(fc = factor(fc, levels = c("fc14","fc15"), labels = c("14वा आयोग\n(2015-20)","15वा आयोग\n(2021-26)")))
p7 <- ggplot(sl, aes(fc, share, group = state_mr, colour = region)) +
  geom_line(linewidth = 1.1, alpha = 0.85) + geom_point(size = 2.6) +
  geom_text_repel(data = sl %>% filter(grepl("15", fc)),
                  aes(label = paste0(state_mr, " ", sprintf("%.1f", share), "%")),
                  hjust = 0, nudge_x = 0.08, size = 3, family = FONT, direction = "y",
                  segment.size = 0.2, show.legend = FALSE, seed = 1) +
  scale_colour_manual(values = reg_cols) +
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.6))) +
  labs(title = "1971 लोकसंख्या वगळल्यावर कोणी आर्थिक भूमी गमावली",
       subtitle = "कर-वाटप निधीतील राज्याचा वाटा. दक्षिणी राज्ये (लाल) घसरतात; मोठी उत्तरी राज्ये (निळी) वाढतात.",
       x = NULL, y = "राज्यांच्या विभाज्य निधीतील वाटा (%)",
       caption = "स्रोत: PRS / 15व्या वित्त आयोग अहवाल, तक्ता 2. 15व्या आयोगाने 1971 ऐवजी 2011 लोकसंख्या वापरली.") +
  theme_mr + theme(legend.position = "top")
save_mr(p7, "fig_fiscal_slope.png", 9.5, 7)

message("C3 (Marathi figures) complete.")
