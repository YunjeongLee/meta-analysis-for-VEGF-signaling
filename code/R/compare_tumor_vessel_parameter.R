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
subfolders = c("etc", "visualize")
for (i in 1:length(subfolders)) {
  a = list.files(path = subfolders[i], pattern = "[.]R$", full.names = TRUE)
  for (j in 1:length(a)) {
    source(a[j])
  }
}

# Load libraries ----------------------------------------------------------
pkg_list = c("ggplot2", "metafor", "readxl", "weights", "latex2exp", "ggpubr", 
             "shades", "ggnewscale", "ggsignif", "stringi")
instant_pkgs(pkg_list)

# Check and generate a result folder --------------------------------------
results_path = "../../results/figures/R"

# Generate new results folder
dir.create(results_path, recursive = TRUE)

# Load data ---------------------------------------------------------------
filename = '../../data/vessel_parameters.xlsx'
# Vessel size
vessel_size_obesity <- as.data.frame(read_excel(filename, sheet = "Vessel size (adipose)"))
vessel_size_tumor <- as.data.frame(read_excel(filename, sheet = "Vessel size (tumor)"))
# Vessel density
vessel_density_obesity <- as.data.frame(read_excel(filename, sheet = "Vessel density (adipose)"))
vessel_density_tumor <- as.data.frame(read_excel(filename, sheet = "Vessel density (tumor)"))
# CBM thickness
cbm_retina <- as.data.frame(read_excel(filename, sheet = "CBM (retina)"))
cbm_muscle <- as.data.frame(read_excel(filename, sheet = "CBM (muscle)"))

# Divide obesity vessel data into separate dataframes ---------------------
# Vessel size
vessel_size_lean <- vessel_size_obesity[!is.na(vessel_size_obesity$Lean_Average), 
                                        c('Reference', 'Lean_Average', 'Lean_SE')]
vessel_size_obese <- vessel_size_obesity[c('Reference', 'Obese_Average', 'Obese_SE')]
# Vessel density
vessel_density_lean <- vessel_density_obesity[!is.na(vessel_size_obesity$Lean_Average), 
                                              c('Reference', 'Lean_Average', 'Lean_SE')]
vessel_density_obese <- vessel_density_obesity[c('Reference', 'Obese_Average', 'Obese_SE')]

# Change column names -----------------------------------------------------
colnames(vessel_size_lean) <- c("Reference", "Average", "SE")
colnames(vessel_size_obese) <- c("Reference", "Average", "SE")
colnames(vessel_density_lean) <- c("Reference", "Average", "SE")
colnames(vessel_density_obese) <- c("Reference", "Average", "SE")

# Meta-analysis using random effects model --------------------------------
# Compute weighted average and SD -----------------------------------------
# Vessel size in adipose tissue of lean mice
rm_vessel_size_lean <- rma(yi = Average, sei = SE, data=vessel_size_lean)
summary(rm_vessel_size_lean)

# Vessel size in adipose tissue of obese mice
rm_vessel_size_obese <- rma(yi = Average, sei = SE, data=vessel_size_obese)
summary(rm_vessel_size_obese)

# Vessel size in tumor
rm_vessel_size_tumor <- rma(yi = Average, sei = SE, data=vessel_size_tumor)
summary(rm_vessel_size_tumor)

# Vessel density in adipose tissue of lean mice
rm_vessel_density_lean <- rma(yi = Average, sei = SE, data=vessel_density_lean)
summary(rm_vessel_density_lean)

# Vessel density in adipose tissue of obese mice
rm_vessel_density_obese <- rma(yi = Average, sei = SE, data=vessel_density_obese)
summary(rm_vessel_density_obese)

# Vessel density in mice tumor
rm_vessel_density_tumor <- rma(yi = Average, sei = SE, data=vessel_density_tumor)
summary(rm_vessel_density_tumor)

# CBM thickness in retina
rm_cbm_retina <- rma(yi = Average, sei = SE, data = cbm_retina)
summary(rm_cbm_retina)

# CBM thickness in muscle
rm_cbm_muscle <- rma(yi = Average, sei = SE, data = cbm_muscle)
summary(rm_cbm_muscle)

# Forest plot -------------------------------------------------------------
# Vessel size
png(file=sprintf("%s/forest_vessel_size_lean.png", results_path), width=1300, height=500)
forest_ylee(data=vessel_size_lean, rm=rm_vessel_size_lean, slab=vessel_size_lean$Reference,
            unit = paste0("µm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            title="Vessel size in adipose tissue of lean mice",
            xlab=TeX("Vessel size $(µm^2)$"), xlim = c(-150, 350), alim = c(0, 200), cex=2)
dev.off()
png(file=sprintf("%s/forest_vessel_size_obese.png", results_path), width=1300, height=700)
forest_ylee(data=vessel_size_obese, rm=rm_vessel_size_obese, slab=vessel_size_obese$Reference, 
            unit = paste0("µm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            title="Vessel size in adipose tissue of obese mice",
            xlab=TeX("Vessel size $(µm^2)$"), xlim = c(-150, 350), alim = c(0, 200), cex=2)
dev.off()
png(file=sprintf("%s/forest_vessel_size_tumor.png", results_path), width=1300, height=700)
forest_ylee(data=vessel_size_tumor, rm=rm_vessel_size_tumor, slab=vessel_size_tumor$Reference, 
            unit = paste0("µm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            title="Vessel size in mice tumor",
            xlab=TeX("Vessel size $(µm^2)$"), xlim = c(-150, 350), alim = c(0, 200), cex=2)
dev.off()

# Vessel density
png(file=sprintf("%s/forest_vessel_density_lean.png", results_path), width=1300, height=500)
forest_ylee(data=vessel_density_lean, rm=rm_vessel_density_lean, slab=vessel_density_lean$Reference, 
            unit = paste0("no./mm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            title="Vessel density in adipose tissue of lean mice",
            xlab=TeX("Vessel density $(no./mm^2)$"), xlim = c(-600, 1700), alim = c(0, 1000), cex = 2)
dev.off()
png(file=sprintf("%s/forest_vessel_density_obese.png", results_path), width=1300, height=700)
forest_ylee(data=vessel_density_obese, rm=rm_vessel_density_obese, slab=vessel_density_obese$Reference, 
            unit = paste0("no./mm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            title="Vessel density in adipose tissue of obese mice",
            xlab=TeX("Vessel density $(no./mm^2)$"), xlim = c(-600, 1700), alim = c(0, 1000), cex=2)
dev.off()
png(file=sprintf("%s/forest_vessel_density_tumor.png", results_path), width=1300, height=700)
forest_ylee(data= vessel_density_tumor, rm=rm_vessel_density_tumor, slab=vessel_density_tumor$Reference, 
            unit = paste0("no./mm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            title="Vessel density in mice tumor",
            xlab=TeX("Vessel density $(no./mm^2)$"), xlim = c(-300, 600), alim = c(0, 350), cex=2)
dev.off()

# CBM thickness
png(file=sprintf("%s/forest_cbm_retina.png", results_path), width=1300, height=500)
forest_ylee(data=cbm_retina, rm=rm_cbm_retina, slab=cbm_retina$Reference,
            unit = "nm", title="Capillary basement membrane thickness in murine retina",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-70, 200), alim=c(0, 120), cex=2)
dev.off()
png(file=sprintf("%s/forest_cbm_muscle.png", results_path), width=1300, height=500)
forest_ylee(data=cbm_muscle, rm=rm_cbm_muscle, slab=cbm_muscle$Reference,
            unit = "nm", title="Capillary basement membrane thickness in murine muscle",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-70, 200), alim=c(0, 120), cex=2)
dev.off()

# Student's t-test --------------------------------------------------------
# Vessel size
vessel_size_lean_vs_obese = wtd.t.test(x=vessel_size_lean$Average, y=vessel_size_obese$Average,
                                       weight=1/(vessel_size_lean$SE^2+rm_vessel_size_lean$tau2), 
                                       weighty=1/(vessel_size_obese$SE^2+rm_vessel_size_obese$tau2),
                                       alternative="less", samedata=FALSE)

vessel_size_lean_vs_tumor = wtd.t.test(x=vessel_size_lean$Average, y=vessel_size_tumor$Average,
                                       weight=1/(vessel_size_lean$SE^2+rm_vessel_size_lean$tau2), 
                                       weighty=1/(vessel_size_tumor$SE^2+rm_vessel_size_tumor$tau2),
                                       alternative="less", samedata=FALSE)

vessel_size_obese_vs_tumor = wtd.t.test(x=vessel_size_obese$Average, y=vessel_size_tumor$Average,
                                        weight=1/(vessel_size_obese$SE^2+rm_vessel_size_obese$tau2), 
                                        weighty=1/(vessel_size_tumor$SE^2+rm_vessel_size_tumor$tau2),
                                        alternative="less", samedata=FALSE)

# Vessel density
vessel_density_lean_vs_obese = wtd.t.test(x=vessel_density_lean$Average, y=vessel_density_obese$Average,
                                          weight=1/(vessel_density_lean$SE^2+rm_vessel_density_lean$tau2), 
                                          weighty=1/(vessel_density_obese$SE^2+rm_vessel_density_obese$tau2),
                                          alternative="greater", samedata=FALSE)

vessel_density_lean_vs_tumor = wtd.t.test(x=vessel_density_lean$Average, y=vessel_density_tumor$Average,
                                          weight=1/(vessel_density_lean$SE^2+rm_vessel_density_lean$tau2), 
                                          weighty=1/(vessel_density_tumor$SE^2+rm_vessel_density_tumor$tau2),
                                          alternative="greater", samedata=FALSE)

vessel_density_obese_vs_tumor = wtd.t.test(x=vessel_density_obese$Average, y=vessel_density_tumor$Average,
                                           weight=1/(vessel_density_obese$SE^2+rm_vessel_density_obese$tau2), 
                                           weighty=1/(vessel_density_tumor$SE^2+rm_vessel_density_tumor$tau2),
                                           alternative="greater", samedata=FALSE)

# CBM thickness
cbm_retina_vs_muscle = wtd.t.test(x=cbm_retina$Average, y=cbm_muscle$Average,
                                  weight=1/(cbm_retina$SE^2+rm_cbm_retina$tau2), 
                                  weighty=1/(cbm_muscle$SE^2+rm_cbm_muscle$tau2),
                                  alternative="two.tailed", samedata=FALSE)

# Merge dataframes for plotting -------------------------------------------
# Vessel size
vessel_size_lean$Source <- "Lean adipose"
vessel_size_obese$Source <- "Obese adipose"
vessel_size_tumor$Source <- "Tumor"

df_size = rbind(vessel_size_lean[c("Source", "Average")],
                vessel_size_obese[c("Source", "Average")],
                vessel_size_tumor[c("Source", "Average")])

# Vessel density
vessel_density_lean$Source <- "Lean adipose"
vessel_density_obese$Source <- "Obese adipose"
vessel_density_tumor$Source <- "Tumor"

df_density = rbind(vessel_density_lean[c("Source", "Average")],
                   vessel_density_obese[c("Source", "Average")],
                   vessel_density_tumor[c("Source", "Average")])

# Scatter plot ------------------------------------------------------------
# Vessel size
p1 = ggplot() +
  geom_point(data = vessel_size_lean, aes(x = "Lean adipose", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_size_lean, aes(x = "Lean adipose", y = rm_vessel_size_lean$b), shape = 95, size = 20, colour = "darkblue") +
  ylim(0, 200) + labs(color="Lean adipose")  +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = vessel_size_obese, aes(x = "Obese adipose", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_size_obese, aes(x = "Obese adipose", y = rm_vessel_size_obese$b), shape = 95, size = 20, colour = "darkgreen") +
  ylim(0, 200) + labs(color="Obese adipose") +
  lightness(scale_color_brewer(palette="Greens"),scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() +
  geom_point(data = vessel_size_tumor, aes(x = "Tumor", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_size_tumor, aes(x = "Tumor", y = rm_vessel_size_tumor$b), shape = 95, size = 20, colour = "darkred") +
  ylim(0, 200) + labs(color="Tumor") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) +
  xlab("") + ylab(TeX("Vessel size $(\\mu m^2)$")) +
  geom_bracket(data = df_size, aes(x = Source, y = Average), xmin = "Lean adipose", xmax = "Tumor",
               y.position = 180, tip.length = c(0.5, 0.1), 
               label = generate_plabel(vessel_size_lean_vs_tumor$coefficients["p.value"])) +
  ggtitle("Comparison of vessel size\n in mice adipose tissue and tumor") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p1)
ggsave(sprintf("%s/vessel_size.png", results_path), width=3500, height=2500, units="px")
dev.off()

# Vessel density
p2 = ggplot() +
  geom_point(data = vessel_density_lean, aes(x = "Lean adipose", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_density_lean, aes(x = "Lean adipose", y = rm_vessel_density_lean$b), shape = 95, size = 20, colour = "darkblue") +
  ylim(0, 1000) + labs(color="Lean adipose")  +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = vessel_density_obese, aes(x = "Obese adipose", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_density_obese, aes(x = "Obese adipose", y = rm_vessel_density_obese$b), shape = 95, size = 20, colour = "darkgreen") +
  ylim(0, 1000) + labs(color="Obese adipose") +
  lightness(scale_color_brewer(palette="Greens"),scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() +
  geom_point(data = vessel_density_tumor, aes(x = "Tumor", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_density_tumor, aes(x = "Tumor", y = rm_vessel_density_tumor$b), shape = 95, size = 20, colour = "darkred") +
  ylim(0, 1000) + labs(color="Tumor") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) +
  xlab("") + ylab(TeX("Vessel density $(no./mm^2)$")) +
  geom_bracket(data = df_size, aes(x = Source, y = Average), xmin = "Lean adipose", xmax = "Tumor",
               y.position = 900, tip.length = c(0.1, 0.05), 
               label = generate_plabel(vessel_density_lean_vs_tumor$coefficients["p.value"])) +
  geom_bracket(data = df_size, aes(x = Source, y = Average), xmin = "Obese adipose", xmax = "Tumor",
               y.position = 800, tip.length = c(0.1, 0.5), 
               label = generate_plabel(vessel_density_obese_vs_tumor$coefficients["p.value"])) +
  ggtitle("Comparison of vessel density\n in mice adipose tissue and tumor") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p2)
ggsave(sprintf("%s/vessel_density.png", results_path), width=3500, height=2500, units="px")
dev.off()

# CBM thickness
p3 = ggplot() +
  geom_point(data = cbm_retina, aes(x = "Retina", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_retina, aes(x = "Retina", y = rm_cbm_retina$b), shape = 95, size = 20, colour = "darkblue") +
  ylim(0, 150) + labs(color="Retina")  +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = cbm_muscle, aes(x = "Muscle", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_muscle, aes(x = "Muscle", y = rm_cbm_muscle$b), shape = 95, size = 20, colour = "darkred") +
  ylim(0, 150) + labs(color="Muscle") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) +
  xlab("") + ylab(TeX("Capillary basement membrane thickness (nm)")) +
  scale_x_discrete(limits=c("Retina", "Muscle")) +
  ggtitle("Comparison of capillary basement membrane thickness\n in murine retina and muscle") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p3)
ggsave(sprintf("%s/cbm.png", results_path), width=3500, height=2500, units="px")
dev.off()
