# =============================================================================
# _master.R -- Delimitation 2026 project pipeline
# Author: Piyush Zaware
# Last updated: 2026-06-20
# =============================================================================

if (Sys.info()["user"] == "piyushzaware") {
  root <- "/Users/piyushzaware/Documents/Unsupervised ML/Delimitation_2026"
}

INPDIR <- file.path(root, "input")
CODDIR <- file.path(root, "code")
OUTDIR <- file.path(root, "output")
FIGDIR <- file.path(root, "output", "figures")
TMPDIR <- file.path(root, "tmp")

source(file.path(CODDIR, "A1_collect.R"))
source(file.path(CODDIR, "A2_apportion.R"))
source(file.path(CODDIR, "C1_figures.R"))
