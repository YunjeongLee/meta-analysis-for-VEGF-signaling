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
pkg_list = c("ggplot2", "metafor", "readxl", "weights", "latex2exp", "ggpubr", "shades", "scales", "ggsignif")
instant_pkgs(pkg_list)

# Load data ---------------------------------------------------------------
filename = '../../data/parameters.xlsx'
# VEGF:VEGFR1
vegfr1 <- as.data.frame(read_excel(filename, sheet = "VEGFA165_VEGFR1"))
# VEGF:VEGFR2
vegfr2 <- as.data.frame(read_excel(filename, sheet = "VEGFA165_VEGFR2"))
# VEGF:NRP1
nrp1 <- as.data.frame(read_excel(filename, sheet = "VEGFA165_NRP1"))

# Change column names -----------------------------------------------------
colnames(vegfr1) <- c("Reference", "Method", "Average", "SE")
colnames(vegfr2) <- c("Reference", "Method", "Average", "SE")
colnames(nrp1) <- c("Reference", "Method", "Average", "SE")

# Drop Method column ------------------------------------------------------
vegfr1 <- vegfr1[, c("Reference", "Average", "SE")]
vegfr2 <- vegfr2[1:8, c("Reference", "Average", "SE")]
nrp1 <- nrp1[, c("Reference", "Average", "SE")]

# Deal with missing standard error ----------------------------------------
vegfr1[is.na(vegfr1$SE), "SE"] <- 0.1*(vegfr1[is.na(vegfr1$SE), "Average"])
vegfr2[is.na(vegfr2$SE), "SE"] <- 0.1*(vegfr2[is.na(vegfr2$SE), "Average"])
nrp1[is.na(nrp1$SE), "SE"] <- 0.1*(nrp1[is.na(nrp1$SE), "Average"])

# Change units of Kd for VEGFR1 and VEGFR2 --------------------------------
vegfr1[c("Average", "SE")] = vegfr1[c("Average", "SE")]/1e3
vegfr2[c("Average", "SE")] = vegfr2[c("Average", "SE")]/1e3

# Meta-analysis -----------------------------------------------------------
# Compute weighted average and SD -----------------------------------------
# VEGF-A165:VEGFR1
rm_vegfr1 <- rma(yi = Average, sei = SE, data=vegfr1)
summary(rm_vegfr1)

# VEGF-A165:VEGFR2
rm_vegfr2 <- rma(yi = Average, sei = SE, data=vegfr2)
summary(rm_vegfr2)

# VEGF-A165:NRP1
rm_nrp1 <- rma(yi = Average, sei = SE, data=nrp1)
summary(rm_nrp1)

# Forest plot -------------------------------------------------------------
forest(rm_vegfr1, slab=vegfr1$Reference, header=TRUE,
       xlab="", xlim = c(-0.02, 0.07), alim = c(0, 0.05), cex=2)
mtext(side=1, "Binding affinity, Kd (nM)", padj=2, cex = 2, line=1)
forest(rm_vegfr2, slab=vegfr2$Reference, header=TRUE, 
       xlab="", xlim = c(-0.5, 1.5), alim = c(0, 1), cex=2)
mtext(side=1, "Binding affinity, Kd (nM)", padj=2, cex = 2, line=1)
forest(rm_nrp1, slab=nrp1$Reference, header=TRUE, 
       xlab="", xlim = c(-70, 300), alim = c(0, 200), cex=2)
mtext(side=1, "Binding affinity, Kd (nM)", padj=2, cex = 2, line=1)

# Student's t-test --------------------------------------------------------
wtd.t.test(x=vegfr1$Average, y=vegfr2$Average,
           weight=1/(vegfr1$SE^2+rm_vegfr1$tau2), 
           weighty=1/(vegfr2$SE^2+rm_vegfr2$tau2),
           alternative="less", samedata=FALSE)

wtd.t.test(x=vegfr1$Average, y=nrp1$Average,
           weight=1/(vegfr1$SE^2+rm_vegfr1$tau2), 
           weighty=1/(nrp1$SE^2+rm_nrp1$tau2),
           alternative="less", samedata=FALSE)

wtd.t.test(x=vegfr2$Average, y=nrp1$Average,
           weight=1/(vegfr2$SE^2+rm_vegfr2$tau2), 
           weighty=1/(nrp1$SE^2+rm_nrp1$tau2),
           alternative="less", samedata=FALSE)

# Scatter plot ------------------------------------------------------------
p = ggplot() +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y = Average/1e3, colour = Reference), size = 3) +
  geom_errorbar(data = vegfr1, aes(x = "VEGFR1", ymin = (rm_vegfr1$b - rm_vegfr1$se*1.645)/1e3, 
                                   ymax = (rm_vegfr1$b + rm_vegfr1$se*1.645)/1e3), width=0.2) +
  annotate("text", x = "VEGFR1", y=rm_vegfr1$b/1e3, label="-", size=12) +
  labs(color="VEGFR1") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  new_scale_color() + 
  geom_point(data = vegfr2, aes(x = "VEGFR2", y = Average/1e3, colour = Reference), size = 3) +
  geom_errorbar(data = vegfr2, aes(x = "VEGFR2", ymin = (rm_vegfr2$b - rm_vegfr2$se*1.645)/1e3, 
                                   ymax = (rm_vegfr2$b + rm_vegfr2$se*1.645)/1e3), width=0.2) +
  annotate("text", x = "VEGFR2", y=rm_vegfr2$b/1e3, label="-", size=12) +
  labs(color="VEGFR2") +
  lightness(scale_color_brewer(palette="Greens"),scalefac(0.8)) +
  new_scale_color() +
  geom_point(data = nrp1, aes(x = "NRP1", y = Average, colour = Reference), size = 3) +
  geom_errorbar(data = nrp1, aes(x = "NRP1", ymin = (rm_nrp1$b - rm_nrp1$se*1.645), 
                                 ymax = (rm_nrp1$b + rm_nrp1$se*1.645)), width=0.2) +
  annotate("text", x = "NRP1", y=rm_nrp1$b, label="-", size=12) +
  labs(color="NRP1") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) +
  xlab("") + ylab(TeX("Binding affinity, Kd (nM)")) +
  scale_y_continuous(trans='log10', breaks=trans_breaks('log10', function(x) 10^x),
              labels=trans_format('log10', math_format(10^.x)))

show(p)
