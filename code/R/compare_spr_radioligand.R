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
filename = '../../data/binding_affinity.xlsx'
# VEGF:VEGFR1
vegfr1 <- as.data.frame(read_excel(filename, sheet = "VEGFA165_VEGFR1"))
# VEGF:VEGFR2
vegfr2 <- as.data.frame(read_excel(filename, sheet = "VEGFA165_VEGFR2"))
# VEGF:NRP1
nrp1 <- as.data.frame(read_excel(filename, sheet = "VEGFA165_NRP1"))

# Split dataframes into SPR and radioligand dataframes --------------------
# VEGF:VEGFR1
vegfr1_spr <- vegfr1[vegfr1["Method"] == "SPR", c("Reference", "Kd average", "Kd SE")]
vegfr1_radio <- vegfr1[vegfr1["Method"] == "Radioligand", c("Reference", "Kd average", "Kd SE")]

# VEGF:VEGFR2
vegfr2_spr <- vegfr2[vegfr2["Method...2"] == "SPR", c("Reference...1", "Kd average...3", "Kd SE...4")]
vegfr2_radio <- vegfr2[vegfr2["Method...2"] == "Radioligand", c("Reference...1", "Kd average...3", "Kd SE...4")]

# VEGF:NRP1
nrp1_spr <- nrp1[nrp1["Method"] == "SPR", c("Reference", "Kd average", "Kd SE")]
nrp1_radio <- nrp1[nrp1["Method"] == "Radioligand", c("Reference", "Kd average", "Kd SE")]

# Change column names -----------------------------------------------------
colnames(vegfr1_spr) <- c("Reference", "Average", "SE")
colnames(vegfr1_radio) <- c("Reference", "Average", "SE")
colnames(vegfr2_spr) <- c("Reference", "Average", "SE")
colnames(vegfr2_radio) <- c("Reference", "Average", "SE")
colnames(nrp1_spr) <- c("Reference", "Average", "SE")
colnames(nrp1_radio) <- c("Reference", "Average", "SE")

# Drop empty rows ---------------------------------------------------------
vegfr1_spr <- vegfr1_spr[!is.na(vegfr1_spr["Reference"]), ]
vegfr1_radio <- vegfr1_radio[!is.na(vegfr1_radio["Reference"]), ]
vegfr2_spr <- vegfr2_spr[!is.na(vegfr2_spr["Reference"]), ]
vegfr2_radio <- vegfr2_radio[!is.na(vegfr2_radio["Reference"]), ]
nrp1_spr <- nrp1_spr[!is.na(nrp1_spr["Reference"]), ]
nrp1_radio <- nrp1_radio[!is.na(nrp1_radio["Reference"]), ]

# Give assumed SE for radioligand assays ----------------------------------
vegfr1_radio["SE"] <- vegfr1_radio["Average"] * 0.1
vegfr2_radio["SE"] <- vegfr2_radio["Average"] * 0.1
nrp1_radio["SE"] <- nrp1_radio["Average"] * 0.1

# Meta-analysis -----------------------------------------------------------
# Compute weighted average and SD -----------------------------------------
# VEGF:VEGFR1 (SPR)
rm_vegfr1_spr <- rma(yi = Average, sei = SE, data=vegfr1_spr)
summary(rm_vegfr1_spr)

# VEGF:VEGFR1 (Radioligand)
rm_vegfr1_radio <- rma(yi = Average, sei = SE, data=vegfr1_radio)
summary(rm_vegfr1_radio)

# VEGF:VEGFR2 (SPR)
rm_vegfr2_spr <- rma(yi = Average, sei = SE, data=vegfr2_spr)
summary(rm_vegfr2_spr)

# VEGF:VEGFR2 (Radioligand)
rm_vegfr2_radio <- rma(yi = Average, sei = SE, data=vegfr2_radio)
summary(rm_vegfr2_radio)

# VEGF:NRP1 (SPR)
rm_nrp1_spr <- rma(yi = Average, sei = SE, data=nrp1_spr)
summary(rm_nrp1_spr)

# VEGF:NRP1 (Radioligand)
rm_nrp1_radio <- rma(yi = Average, sei = SE, data=nrp1_radio)
summary(rm_nrp1_radio)

# Forest plot -------------------------------------------------------------
# VEGF:VEGFR1 (SPR)
png(file=sprintf("%s/forest_vegfr1_spr.png", results_path), width=1100, height=500)
forest(rm_vegfr1_spr, slab=vegfr1_spr$Reference, header=TRUE,
       xlab="", xlim = c(-15, 30), alim = c(0, 20), cex=2)
mtext(side=1, TeX("Binding affinity, Kd (pM)"), padj=2, cex = 2, line=1)
dev.off()

# VEGF:VEGFR1 (Radioligand)
png(file=sprintf("%s/forest_vegfr1_radio.png", results_path), width=1300, height=700)
forest(rm_vegfr1_radio, slab=vegfr1_radio$Reference, header=TRUE,
       xlab="", xlim = c(-15, 30), alim = c(0, 20), cex=2)
mtext(side=1, TeX("Binding affinity, Kd (pM)"), padj=2, cex = 2, line=1)
dev.off()

# VEGF:VEGFR2 (SPR)
png(file=sprintf("%s/forest_vegfr2_spr.png", results_path), width=1300, height=500)
forest(rm_vegfr2_spr, slab=vegfr2_spr$Reference, header=TRUE,
       xlab="", xlim = c(-900, 1500), alim = c(0, 900), cex=2)
mtext(side=1, TeX("Binding affinity, Kd (pM)"), padj=2, cex = 2, line=1)
dev.off()

# VEGF:VEGFR2 (Radioligand)
png(file=sprintf("%s/forest_vegfr2_radio.png", results_path), width=1300, height=500)
forest(rm_vegfr2_radio, slab=vegfr2_radio$Reference, header=TRUE,
       xlab="", xlim = c(-800, 1600), alim = c(200, 1000), cex=2)
mtext(side=1, TeX("Binding affinity, Kd (pM)"), padj=2, cex = 2, line=1)
dev.off()

# VEGF:NRP1 (SPR)
png(file=sprintf("%s/forest_nrp1_spr.png", results_path), width=1300, height=500)
forest(rm_nrp1_spr, slab=nrp1_spr$Reference, header=TRUE,
       xlab="", xlim = c(-120, 300), alim = c(0, 200), cex=2)
mtext(side=1, TeX("Binding affinity, Kd (nM)"), padj=2, cex = 2, line=1)
dev.off()

# VEGF:NRP1 (Radioligand)
png(file=sprintf("%s/forest_nrp1_radio.png", results_path), width=1300, height=500)
forest(rm_nrp1_radio, slab=nrp1_radio$Reference, header=TRUE,
       xlab="", xlim = c(-4, 7), alim = c(0, 5), cex=2)
mtext(side=1, TeX("Binding affinity, Kd (nM)"), padj=2, cex = 2, line=1)
dev.off()

# Student's t-test --------------------------------------------------------
# VEGF:VEGFR1 (SPR vs radioligand)
vegfr1_ttest = wtd.t.test(x=vegfr1_spr$Average, y=vegfr1_radio$Average,
                          weight=1/(vegfr1_spr$SE^2+rm_vegfr1_spr$tau2), 
                          weighty=1/(vegfr1_radio$SE^2+rm_vegfr1_radio$tau2),
                          alternative="two.tailed", samedata=FALSE)
# VEGFR:VEGFR2 (SPR vs radioligand)
vegfr2_ttest = wtd.t.test(x=vegfr2_spr$Average, y=vegfr2_radio$Average,
                          weight=1/(vegfr2_spr$SE^2+rm_vegfr2_spr$tau2), 
                          weighty=1/(vegfr2_radio$SE^2+rm_vegfr2_radio$tau2),
                          alternative="two.tailed", samedata=FALSE)

# VEGFR:VEGFR2 (SPR vs radioligand)
nrp1_ttest = wtd.t.test(x=nrp1_spr$Average, y=nrp1_radio$Average,
                        weight=1/(nrp1_spr$SE^2+rm_nrp1_spr$tau2), 
                        weighty=1/(nrp1_radio$SE^2+rm_nrp1_radio$tau2),
                        alternative="two.tailed", samedata=FALSE)

# Scatter plot ------------------------------------------------------------
p = ggplot() +
  geom_point(data = vegfr1_radio, aes(x = "VEGFR1", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "VEGFR1", y=rm_vegfr1_radio$b, label="-", size=30, colour = "darkblue") +
  lightness(scale_color_colormap('Cell-based (Radioligand)', discrete = T,colormap = "velocity-blue", reverse = T), scalefac(0.8)) +
  geom_point(data = vegfr2_radio, aes(x = "VEGFR2", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "VEGFR2", y=rm_vegfr2_radio$b, label="-", size=30, colour = "darkblue") +
  geom_point(data = nrp1_radio, aes(x = "NRP1", y = Average*1e3, colour = Reference), size = 7) +
  annotate("text", x = "NRP1", y=rm_nrp1_radio$b*1e3, label="-", size=30, colour = "darkblue") +
  new_scale_color() + 
  geom_point(data = vegfr1_spr, aes(x = "VEGFR1", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "VEGFR1", y=rm_vegfr1_spr$b, label="-", size=30, colour = "darkred") +
  lightness(scale_color_colormap('Chip-based  (SPR)', discrete = T,colormap = "autumn", reverse = T), scalefac(0.8)) +
  geom_point(data = vegfr2_spr, aes(x = "VEGFR2", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "VEGFR2", y=rm_vegfr2_spr$b, label="-", size=30, colour = "darkred") +
  geom_point(data = nrp1_spr, aes(x = "NRP1", y = Average*1e3, colour = Reference), size = 7) +
  annotate("text", x = "NRP1", y=rm_nrp1_spr$b*1e3, label="-", size=30, colour = "darkred") +
  annotate("text", x = "NRP1", y=rm_nrp1_spr$b*1e3, 
           label=generate_plabel(nrp1_ttest$coefficients["p.value"]), hjust=-0.35,
           size=6, colour = "darkred") +
  scale_y_continuous(trans= 'log10', breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)), limits = c(1e-1, 1e7),
                     sec.axis = sec_axis(trans=~./1e3, name="Binding affinity, Kd (nM)",
                                         breaks=trans_breaks('log10', function(x) 10^x),
                                         labels=trans_format('log10', math_format(10^.x)))) +
  scale_x_discrete(limits=c("VEGFR1", "VEGFR2", "NRP1")) +
  xlab("") + ylab("Binding affinity, Kd (pM)") +
  theme(text = element_text(size = 20))

show(p)
ggsave(sprintf("%s/spr_vs_radioligand.png", results_path), width=4000, height=2500, units="px")
dev.off()
