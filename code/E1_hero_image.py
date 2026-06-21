"""
E1_hero_image.py
Author: Piyush Zaware
Last updated: 2026-06-20

Hero image for the Delimitation 2026 website. Diverging dot chart: each dot is
one projected Lok Sabha seat change. Gains rise upward in blue (the Hindi-belt),
losses fall downward in red (the South and East). No text baked in; the CSS
overlay handles all text.

OUT: output/figures/fig_hero.png
"""

import os
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

ROOT   = "/Users/piyushzaware/Documents/Unsupervised ML/Delimitation_2026"
FIGDIR = os.path.join(ROOT, "output", "figures")

# Projected seat change, 543 reapportioned by 2026 population (Webster).
# Ordered losers -> gainers so the wave runs red (left) to blue (right).
changes = [
    ("Tamil Nadu", -10), ("Kerala", -6), ("Andhra Pradesh", -5),
    ("West Bengal", -4), ("Odisha", -4), ("Telangana", -3),
    ("Karnataka", -2), ("Punjab", -1), ("Himachal Pradesh", -1),
    ("Assam", 0), ("Uttarakhand", 0),
    ("Maharashtra", 1), ("Jharkhand", 1), ("Chhattisgarh", 1), ("Delhi", 1),
    ("Gujarat", 2), ("Haryana", 2), ("Madhya Pradesh", 5),
    ("Rajasthan", 7), ("Bihar", 10), ("Uttar Pradesh", 11),
]

BG       = "#0A0E1A"   # deep political navy
BLUE     = "#3B82C4"   # gain
BLUE_HI  = "#5FA0DE"
RED      = "#C0392B"   # loss
RED_HI   = "#E0594B"
ZERO     = "#3A4256"

fig, ax = plt.subplots(figsize=(22, 5))
fig.patch.set_facecolor(BG)
ax.set_facecolor(BG)

dot_r   = 0.34
x_gap   = 1.0
y_unit  = 0.78

for i, (state, chg) in enumerate(changes):
    x = i * x_gap
    if chg == 0:
        c = plt.Circle((x, 0), dot_r * 0.7, color=ZERO, alpha=0.7, linewidth=0)
        ax.add_patch(c)
        continue
    n = abs(chg)
    up = chg > 0
    base_col = BLUE if up else RED
    hi_col   = BLUE_HI if up else RED_HI
    for k in range(n):
        y = (k + 1) * y_unit * (1 if up else -1)
        col = hi_col if k == n - 1 else base_col
        alpha = 0.92 if k == n - 1 else 0.80
        c = plt.Circle((x, y), dot_r, color=col, alpha=alpha, linewidth=0)
        ax.add_patch(c)

# Faint zero baseline
ax.plot([-1, len(changes)], [0, 0], color="#2A3142", linewidth=1.0, zorder=0)

ax.set_xlim(-1.2, len(changes) * x_gap + 0.4)
ax.set_ylim(-9.5, 12.5)
ax.axis("off")
plt.tight_layout(pad=0)

out = os.path.join(FIGDIR, "fig_hero.png")
fig.savefig(out, dpi=180, bbox_inches="tight", facecolor=BG)
plt.close()
print(f"Saved: {out}  ({os.path.getsize(out) // 1024}K)")
