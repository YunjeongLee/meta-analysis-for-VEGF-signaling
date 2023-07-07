# Clear all ---------------------------------------------------------------
# Clear plots
if (!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clear workspace
rm(list = ls())

# Change working directory ------------------------------------------------
setwd("C:/Users/lione/Desktop/GitHub/meta-analysis-for-VEGF-signaling/code/R")

# Add path ----------------------------------------------------------------
subfolders = c("etc", "visualize")
for (i in 1:length(subfolders)) {
  a = list.files(path = subfolders[i], pattern = "[.]R$", full.names = TRUE)
  for (j in 1:length(a)) {
    source(a[j])
  }
}

# Check and generate a result folder --------------------------------------
results_path = "../../results/figures/R"

# Generate new results folder
dir.create(results_path, recursive = TRUE)

# Load libraries ----------------------------------------------------------
pkg_list = c("ggplot2", "metafor", "readxl", "weights", "latex2exp", "ggpubr", 
             "shades", "ggnewscale", "scales", "ggsignif", "colormap")
instant_pkgs(pkg_list)

# Load data ---------------------------------------------------------------
filename = '../../data/kon_koffs.xlsx'
vegfkonkoff <- as.data.frame(read_excel(filename))

# Split Data by parameter -------------------------------------------------

VEGF165VEGR1 <- vegfkonkoff[vegfkonkoff["Parameter"] == "VEGF165:VEGR1" ]

VEGF165VEGFR2 <- vegfkonkoff[vegfkonkoff["Parameter"] == "VEGF165:VEGFR2" ]

VEGF165NRP1 <- vegfkonkoff[vegfkonkoff["Parameter"] == "VEGF165:NRP1" ]

VEGFR2NRP1 <- vegfkonkoff[vegfkonkoff["Parameter"] == "VEGFR2:NRP1" ]

# Give assumed SE----------------------------------------------------------
#VEGF165:VEGR1
VEGF165VEGR1 <- cbind(VEGF165VEGR1, kon_SE = VEGF165VEGR1[["Kon SD"]] / sqrt(1), koff_SE = VEGF165VEGR1[["Koff SD"]] / sqrt(1))

#VEGF165:VEGFR2
VEGF165VEGFR2[is.na(vegfkonkoff["SE"]), "SE"] <- vegfkonkoff[is.na(vegfkonkoff["SE"]), "Average"] * 0.1

#VEGF165:NRP1 
VEGF165NRP1[is.na(vegfkonkoff["SE"]), "SE"] <- vegfkonkoff[is.na(vegfkonkoff["SE"]), "Average"] * 0.1

#VEGFR2:NRP1 
VEGFR2NRP1[is.na(vegfkonkoff["SE"]), "SE"] <- vegfkonkoff[is.na(vegfkonkoff["SE"]), "Average"] * 0.1

# Meta-analysis -----------------------------------------------------------
# Compute weighted average and SD -----------------------------------------

