# Clear all ---------------------------------------------------------------
# Clear plots
if (!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clear workspace
rm(list = ls())

# Change working directory ------------------------------------------------
setwd("/Users/yunjeonglee/Documents/repos/meta-analysis-for-VEGF-signaling/code/R")

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
             "shades", "ggnewscale", "scales", "ggsignif", "colormap", "stringr", 
             "dplyr", "pals")
instant_pkgs(pkg_list)

# Load data ---------------------------------------------------------------
filename = '../../data/adipose_tissue_parameters.xlsx'
cbm <- as.data.frame(read_excel(filename, sheet = "CBM thickness"))

cbm_lean = cbm[!is.na(cbm["Lean SE"]), c("Reference", "Lean average", "Lean SE")]
cbm_obese = cbm[!is.na(cbm["Obese SE"]), c("Reference", "Obese average", "Obese SE")]

# Change column names -----------------------------------------------------
colnames(cbm_lean) <- c("Reference", "Average", "SE")
colnames(cbm_obese) <- c("Reference", "Average", "SE")

# Meta-analysis -----------------------------------------------------------
# Capillary BM thickness of lean mice
rm_cbm_lean <- rma(yi = Average, sei = SE, data=cbm_lean)
summary(rm_cbm_lean)

# Capillary BM thickness of obese mice
rm_cbm_obese <- rma(yi = Average, sei = SE, data=cbm_obese)
summary(rm_cbm_obese)


# CBM thickness of lean mice
png(file=sprintf("%s/forest_cbm_lean.png", results_path), width=1300, height=700)
forest_ylee(data=cbm_lean, rm=rm_cbm_lean, slab=cbm_lean$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-50, 200), alim = c(20, 120), cex=2)
dev.off()

# CBM thickness of obese mice
png(file=sprintf("%s/forest_cbm_obese.png", results_path), width=1300, height=500)
forest_ylee(data=cbm_obese, rm=rm_cbm_obese, slab=cbm_obese$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-50, 200), alim = c(20, 120), cex=2)
dev.off()

# Student's t-test --------------------------------------------------------
cbm_lean_vs_obese = wtd.t.test(x=cbm_lean$Average, y=cbm_obese$Average,
                               weight=1/(cbm_lean$SE^2+rm_cbm_lean$tau2), 
                               weighty=1/(cbm_obese$SE^2+rm_cbm_obese$tau2),
                               alternative="two.tailed", samedata=FALSE)

# Merge dataframes for plotting -------------------------------------------
cbm_lean$Source <- "Lean murines"
cbm_obese$Source <- "Obese murines"

cbm = rbind(cbm_lean[c("Source", "Average")],
            cbm_obese[c("Source", "Average")])

# Scatter plot ------------------------------------------------------------
p = ggplot() +
  geom_point(data = cbm_lean, aes(x = "Lean murines", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_lean, aes(x = "Lean murines", y = rm_cbm_lean$b), shape = 95, size = 20, colour = "darkblue") +
  labs(color="Lean murines") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = cbm_obese, aes(x = "Obese murines", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_obese, aes(x = "Obese murines", y = rm_cbm_obese$b), shape = 95, size = 20, colour = "darkred") +
  labs(color="Obese murines") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) +
  xlab("") + ylab(TeX("Capillary basement membrane thickness (nm)")) +
  theme(text = element_text(size = 20)) + ylim(c(0, 150))

show(p)
ggsave(sprintf("%s/cbm_lean_vs_obese.png", results_path), width=3500, height=2500, units="px")
dev.off()
