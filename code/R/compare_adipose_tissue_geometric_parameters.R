# Clear all ---------------------------------------------------------------
# Clear plots
if (!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clear workspace
rm(list = ls())

# Change working directory ------------------------------------------------
setwd("/Users/yunjeong/Documents/repos/meta-analysis-for-VEGF-signaling/code/R")

# Add path ----------------------------------------------------------------
subfolders = c("etc")
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
filename = '../../data/adipose_tissue_parameters.xlsx'
# Adipocyte diameter
adipocyte <- as.data.frame(read_excel(filename, sheet = "Adipocyte diameter"))
# Capillary BM thickness
cbm <- as.data.frame(read_excel(filename, sheet = "CBM thickness"))

# Split adipocyte size and CBM thickness into two dataframes --------------
# Split adipocyte size dataframe into lean and obese adipose tissue
adipocyte_lean = adipocyte[!is.na(adipocyte["Lean SE...3"]), c("Reference", "Lean average...2", "Lean SE...3")]
adipocyte_obese = adipocyte[!is.na(adipocyte["Obese SE...5"]), c("Reference", "Obese average...4", "Obese SE...5")]

# Split CBM thickness dataframe into lean and obese adipose tissue
cbm_lean = cbm[!is.na(cbm["Lean SE"]), c("Reference", "Lean average", "Lean SE")]
cbm_obese = cbm[!is.na(cbm["Obese SE"]), c("Reference", "Obese average", "Obese SE")]

# Change column names -----------------------------------------------------
colnames(adipocyte_lean) <- c("Reference", "Average", "SE")
colnames(adipocyte_obese) <- c("Reference", "Average", "SE")
colnames(cbm_lean) <- c("Reference", "Average", "SE")
colnames(cbm_obese) <- c("Reference", "Average", "SE")

# Meta-analysis -----------------------------------------------------------
# Compute weighted average and SD -----------------------------------------
# Adipocyte diameter of lean mice
rm_adipocyte_lean <- rma(yi = Average, sei = SE, data=adipocyte_lean)
summary(rm_adipocyte_lean)

# Adipocyte diameter of obese mice
rm_adipocyte_obese <- rma(yi = Average, sei = SE, data=adipocyte_obese)
summary(rm_adipocyte_obese)

# Capillary BM thickness of lean mice
rm_cbm_lean <- rma(yi = Average, sei = SE, data=cbm_lean)
summary(rm_cbm_lean)

# Capillary BM thickness of obese mice
rm_cbm_obese <- rma(yi = Average, sei = SE, data=cbm_obese)
summary(rm_cbm_obese)

# Forest plot -------------------------------------------------------------
# Adipocyte diameter of lean mice
png(file=sprintf("%s/forest_adipocyte_diameter_lean.png", results_path), width=1300, height=500)
forest(rm_adipocyte_lean, slab=adipocyte_lean$Reference, header=TRUE,
       xlab="", xlim = c(-40, 120), alim = c(20, 80), cex=2)
mtext(side=1, TeX("Adipocyte diameter (\\mu{m})"), padj=2, cex = 2, line=1)
dev.off()

# Adipocyte diameter of obese mice
png(file=sprintf("%s/forest_adipocyte_diameter_obese.png", results_path), width=1300, height=700)
forest(rm_adipocyte_obese, slab=adipocyte_obese$Reference, header=TRUE,
       xlab="", xlim = c(-60, 180), alim = c(20, 120), cex=2)
mtext(side=1, TeX("Adipocyte diameter (\\mu{m})"), padj=2, cex = 2, line=1)
dev.off()

# CBM thickness of lean mice
png(file=sprintf("%s/forest_cbm_lean.png", results_path), width=1300, height=700)
forest(rm_cbm_lean, slab=cbm_lean$Reference, header=TRUE, 
       xlab="", xlim = c(-60, 180), alim = c(20, 120), cex=2)
mtext(side=1, "Capillary basement membrane thickness (nm)", padj=2, cex = 2, line=1)
dev.off()

# CBM thickness of obese mice
png(file=sprintf("%s/forest_cbm_obese.png", results_path), width=1300, height=500)
forest(rm_cbm_obese, slab=cbm_obese$Reference, header=TRUE, 
       xlab="", xlim = c(-60, 180), alim = c(20, 120), cex=2)
mtext(side=1, "Capillary basement membrane thickness (nm)", padj=2, cex = 2, line=1)
dev.off()

# Student's t-test --------------------------------------------------------
adipocyte_lean_vs_obese = wtd.t.test(x=adipocyte_lean$Average, y=adipocyte_obese$Average,
                                     weight=1/(adipocyte_lean$SE^2+rm_adipocyte_lean$tau2), 
                                     weighty=1/(adipocyte_obese$SE^2+rm_adipocyte_obese$tau2),
                                     alternative="less", samedata=FALSE)

cbm_lean_vs_obese = wtd.t.test(x=cbm_lean$Average, y=cbm_obese$Average,
                               weight=1/(cbm_lean$SE^2+rm_cbm_lean$tau2), 
                               weighty=1/(cbm_obese$SE^2+rm_cbm_obese$tau2),
                               alternative="two.tailed", samedata=FALSE)

# Merge dataframes for plotting -------------------------------------------
# Adipocyte
adipocyte_lean$Source <- "Lean mice"
adipocyte_obese$Source <- "Obese mice"

adipocyte = rbind(adipocyte_lean[c("Source", "Average")],
                  adipocyte_obese[c("Source", "Average")])

# CBM thickness
cbm_lean$Source <- "Lean mice"
cbm_obese$Source <- "Obese mice"

cbm = rbind(cbm_lean[c("Source", "Average")],
            cbm_obese[c("Source", "Average")])

# Scatter plot ------------------------------------------------------------
p = ggplot() +
  geom_point(data = adipocyte_lean, aes(x = "Lean mice", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "Lean mice", y=rm_adipocyte_lean$b, label="-", size=30) +
  labs(color="Lean mice") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = adipocyte_obese, aes(x = "Obese mice", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "Obese mice", y=rm_adipocyte_obese$b, label="-", size=30) +
  lightness(scale_color_colormap('Obese mice', discrete = T,colormap = "autumn", reverse = T), scalefac(0.8)) + 
  xlab("") + ylab(TeX("Adipocyte diameter (\\mu{m})")) +
  geom_bracket(data = adipocyte, aes(x = Source, y = Average), xmin = "Lean mice", xmax = "Obese mice",
               y.position = 110, tip.length = c(0.5, 0.1), 
               label = generate_plabel(adipocyte_lean_vs_obese$coefficients["p.value"])) +
  theme(text = element_text(size = 20)) + ylim(c(0, 150))

show(p)
ggsave(sprintf("%s/adipocyte_diameter_lean_vs_obese.png", results_path), width=3500, height=2500, units="px")
dev.off()

p = ggplot() +
  geom_point(data = cbm_lean, aes(x = "Lean mice", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "Lean mice", y=rm_cbm_lean$b, label="-", size=30) +
  labs(color="Lean mice") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = cbm_obese, aes(x = "Obese mice", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "Obese mice", y=rm_cbm_obese$b, label="-", size=30) +
  labs(color="Obese mice") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) +
  xlab("") + ylab(TeX("Capillary basement membrane thickness (nm)")) +
  theme(text = element_text(size = 20)) + ylim(c(0, 150))

show(p)
ggsave(sprintf("%s/cbm_lean_vs_obese.png", results_path), width=3500, height=2500, units="px")
dev.off()
