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
             "shades", "ggnewscale", "scales", "ggsignif")
instant_pkgs(pkg_list)

# Load data ---------------------------------------------------------------
filename = '../../data/binding_affinity.xlsx'
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
vegfr2 <- vegfr2[, c("Reference", "Average", "SE")]
nrp1 <- nrp1[, c("Reference", "Average", "SE")]

# Drop rows with no references from vegfr2
vegfr2 <- vegfr2[!is.na(vegfr2$Reference), ]

# Deal with missing standard error ----------------------------------------
vegfr1[is.na(vegfr1["SE"]), "SE"] <- vegfr1[is.na(vegfr1["SE"]), "Average"] * 0.1
vegfr2[is.na(vegfr2["SE"]), "SE"] <- vegfr2[is.na(vegfr2["SE"]), "Average"] * 0.1
nrp1[is.na(nrp1["SE"]), "SE"] <- nrp1[is.na(nrp1["SE"]), "Average"] * 0.1

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
png(file=sprintf("%s/forest_vegfr1.png", results_path), width=1300, height=500)
forest(rm_vegfr1, slab=vegfr1$Reference, header=TRUE,
       xlab="", xlim = c(-0.04, 0.07), alim = c(0, 0.05), cex=2)
mtext(side=1, "Binding affinity, Kd (nM)", padj=2, cex = 2, line=1)
dev.off()
png(file=sprintf("%s/forest_vegfr2.png", results_path), width=1300, height=700)
forest(rm_vegfr2, slab=vegfr2$Reference, header=TRUE, 
       xlab="", xlim = c(-1, 1.5), alim = c(0, 1), cex=2)
mtext(side=1, "Binding affinity, Kd (nM)", padj=2, cex = 2, line=1)
dev.off()
png(file=sprintf("%s/forest_nrp1.png", results_path), width=1300, height=700)
forest(rm_nrp1, slab=nrp1$Reference, header=TRUE, 
       xlab="", xlim = c(-170, 300), alim = c(0, 200), cex=2)
mtext(side=1, "Binding affinity, Kd (nM)", padj=2, cex = 2, line=1)
dev.off()

# Student's t-test --------------------------------------------------------
vegfr1_vs_vegfr2 = wtd.t.test(x=vegfr1$Average, y=vegfr2$Average,
                              weight=1/(vegfr1$SE^2+rm_vegfr1$tau2), 
                              weighty=1/(vegfr2$SE^2+rm_vegfr2$tau2),
                              alternative="less", samedata=FALSE)

vegfr1_vs_nrp1 = wtd.t.test(x=vegfr1$Average, y=nrp1$Average,
                            weight=1/(vegfr1$SE^2+rm_vegfr1$tau2), 
                            weighty=1/(nrp1$SE^2+rm_nrp1$tau2),
                            alternative="less", samedata=FALSE)

vegfr2_vs_nrp1 = wtd.t.test(x=vegfr2$Average, y=nrp1$Average,
                            weight=1/(vegfr2$SE^2+rm_vegfr2$tau2), 
                            weighty=1/(nrp1$SE^2+rm_nrp1$tau2),
                            alternative="less", samedata=FALSE)

# Merge dataframes for plotting -------------------------------------------
vegfr1$Source <- "VEGFR1"
vegfr2$Source <- "VEGFR2"
nrp1$Source <- "NRP1"

df = rbind(vegfr1[c("Source", "Average")],
           vegfr2[c("Source", "Average")],
           nrp1[c("Source", "Average")])

# Scatter plot ------------------------------------------------------------
p = ggplot() +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "VEGFR1", y=rm_vegfr1$b, label="-", size=30) +
  labs(color="VEGFR1") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = vegfr2, aes(x = "VEGFR2", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "VEGFR2", y=rm_vegfr2$b, label="-", size=30) +
  labs(color="VEGFR2") +
  lightness(scale_color_brewer(palette="Greens"),scalefac(0.8)) +
  new_scale_color() +
  geom_point(data = nrp1, aes(x = "NRP1", y = Average, colour = Reference), size = 7) +
  annotate("text", x = "NRP1", y=rm_nrp1$b, label="-", size=30) +
  labs(color="NRP1") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) +
  xlab("") + ylab(TeX("Binding affinity, Kd (nM)")) +
  scale_y_continuous(trans='log10', breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +
  geom_bracket(data = df, aes(x = Source, y = Average), xmin = "VEGFR1", xmax = "VEGFR2",
               y.position = 1, tip.length = c(0.4, 0.05), 
               label = generate_plabel(vegfr1_vs_vegfr2$coefficients["p.value"])) +
  scale_x_discrete(limits=c("VEGFR1", "VEGFR2", "NRP1")) +
  theme(text = element_text(size = 20))

show(p)
ggsave(sprintf("%s/binding_affinity.png", results_path), width=3500, height=2700, units="px")
dev.off()