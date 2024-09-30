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

# Load libraries ----------------------------------------------------------
pkg_list = c("ggplot2", "metafor", "readxl", "weights", "latex2exp", "ggpubr", 
             "shades", "ggnewscale", "ggsignif", "stringi", "pals", "colormap")
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

# Divide obesity vessel data into separate dataframes ---------------------
# Vessel size
vessel_size_lean <- vessel_size_obesity[!is.na(vessel_size_obesity$Lean_Average), 
                                        c('Reference', 'Lean_Average', 'Lean_SE')]
vessel_size_obese <- vessel_size_obesity[c('Reference', 'Obese_Average', 'Obese_SE')]
# Vessel density
vessel_density_lean <- vessel_density_obesity[!is.na(vessel_density_obesity$Lean_Average), 
                                              c('Reference', 'Lean_Average', 'Lean_SE')]
vessel_density_obese <- vessel_density_obesity[!is.na(vessel_density_obesity$Obese_Average), 
                                               c('Reference', 'Obese_Average', 'Obese_SE')]

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

# Forest plot -------------------------------------------------------------
# Vessel size
png(file=sprintf("%s/forest_vessel_size_lean.png", results_path), width=1300, height=600)
forest_ylee(data=vessel_size_lean, rm=rm_vessel_size_lean, slab=vessel_size_lean$Reference,
            unit = paste0("µm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            xlab=TeX("Vessel size $(µm^2)$"), xlim = c(-150, 350), alim = c(0, 200), cex=2, numDigits=0L)
dev.off()
png(file=sprintf("%s/forest_vessel_size_obese.png", results_path), width=1300, height=700)
forest_ylee(data=vessel_size_obese, rm=rm_vessel_size_obese, slab=vessel_size_obese$Reference, 
            unit = paste0("µm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            xlab=TeX("Vessel size $(µm^2)$"), xlim = c(-150, 350), alim = c(0, 200), cex=2, numDigits=0L)
dev.off()
png(file=sprintf("%s/forest_vessel_size_tumor.png", results_path), width=1300, height=700)
forest_ylee(data=vessel_size_tumor, rm=rm_vessel_size_tumor, slab=vessel_size_tumor$Reference, 
            unit = paste0("µm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            xlab=TeX("Vessel size $(µm^2)$"), xlim = c(-500, 600), alim = c(0, 300), cex=2, numDigits=0L)
dev.off()

# Vessel density
png(file=sprintf("%s/forest_vessel_density_lean.png", results_path), width=1500, height=700)
forest_ylee(data=vessel_density_lean, rm=rm_vessel_density_lean, slab=vessel_density_lean$Reference, 
            unit = paste0("no./mm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            xlab=TeX("Vessel density $(no./mm^2)$"), xlim = c(-1000, 2500), alim = c(0, 1500), cex = 2, numDigits=0L)
dev.off()
png(file=sprintf("%s/forest_vessel_density_obese.png", results_path), width=1300, height=700)
forest_ylee(data=vessel_density_obese, rm=rm_vessel_density_obese, slab=vessel_density_obese$Reference, 
            unit = paste0("no./mm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            xlab=TeX("Vessel density $(no./mm^2)$"), xlim = c(-600, 1700), alim = c(0, 1000), cex=2, numDigits=0L)
dev.off()
png(file=sprintf("%s/forest_vessel_density_tumor.png", results_path), width=1500, height=900)
forest_ylee(data= vessel_density_tumor, rm=rm_vessel_density_tumor, slab=vessel_density_tumor$Reference, 
            unit = paste0("no./mm", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B2>"))),
            xlab=TeX("Vessel density $(no./mm^2)$"), xlim = c(-600, 700), alim = c(0, 350), cex=2, numDigits=0L)
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
  ylim(0, 300) + labs(color="Lean adipose")  +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = vessel_size_obese, aes(x = "Obese adipose", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_size_obese, aes(x = "Obese adipose", y = rm_vessel_size_obese$b), shape = 95, size = 20, colour = "darkgreen") +
  ylim(0, 300) + labs(color="Obese adipose") +
  lightness(scale_color_brewer(palette="Greens"),scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() +
  geom_point(data = vessel_size_tumor, aes(x = "Tumor", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_size_tumor, aes(x = "Tumor", y = rm_vessel_size_tumor$b), shape = 95, size = 20, colour = "darkred") +
  ylim(0, 300) + labs(color="Tumor") +
  lightness(scale_color_colormap('Tumor', discrete = T, colormap = brewer.oranges(rm_vessel_size_tumor$k), reverse = T), scalefac(0.8)) +
  xlab("") + ylab(TeX("Vessel size $(\\mu m^2)$")) +
  geom_bracket(data = df_size, aes(x = Source, y = Average), xmin = "Lean adipose", xmax = "Tumor",
               y.position = 290, tip.length = c(0.7, 0.1), label.size = 7, 
               label = generate_plabel(vessel_size_lean_vs_tumor$coefficients["p.value"])) +
  geom_bracket(data = df_size, aes(x = Source, y = Average), xmin = "Obese adipose", xmax = "Tumor",
               y.position = 260, tip.length = c(0.3, 0.1), label.size = 7, 
               label = generate_plabel(vessel_size_obese_vs_tumor$coefficients["p.value"])) +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p1)
ggsave(sprintf("%s/vessel_size.png", results_path), width=4500, height=3000, units="px")
dev.off()

# Vessel density
p2 = ggplot() +
  geom_point(data = vessel_density_lean, aes(x = "Lean adipose", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_density_lean, aes(x = "Lean adipose", y = rm_vessel_density_lean$b), shape = 95, size = 20, colour = "darkblue") +
  ylim(0, 1700) + labs(color="Lean adipose")  +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = vessel_density_obese, aes(x = "Obese adipose", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_density_obese, aes(x = "Obese adipose", y = rm_vessel_density_obese$b), shape = 95, size = 20, colour = "darkgreen") +
  ylim(0, 1700) + labs(color="Obese adipose") +
  lightness(scale_color_brewer(palette="Greens"),scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() +
  geom_point(data = vessel_density_tumor, aes(x = "Tumor", y = Average, colour = Reference), size = 7) +
  geom_point(data = vessel_density_tumor, aes(x = "Tumor", y = rm_vessel_density_tumor$b), shape = 95, size = 20, colour = "darkred") +
  ylim(0, 1700) + labs(color="Tumor") +
  lightness(scale_color_colormap('Tumor', discrete = T, colormap = brewer.oranges(rm_vessel_density_tumor$k), reverse = T), scalefac(0.8)) +
  xlab("") + ylab(TeX("Vessel density $(no./mm^2)$")) +
  geom_bracket(data = df_size, aes(x = Source, y = Average), xmin = "Lean adipose", xmax = "Tumor",
               y.position = 1600, tip.length = c(0.1, 0.3), label.size = 7, 
               label = generate_plabel(vessel_density_lean_vs_tumor$coefficients["p.value"])) +
  geom_bracket(data = df_size, aes(x = Source, y = Average), xmin = "Obese adipose", xmax = "Tumor",
               y.position = 1100, tip.length = c(0.1, 0.5), label.size = 7, 
               label = generate_plabel(vessel_density_obese_vs_tumor$coefficients["p.value"])) +
  geom_bracket(data = df_size, aes(x = Source, y = Average), xmin = "Lean adipose", xmax = "Obese adipose",
               y.position = 1400, tip.length = c(0.1, 0.2), label.size = 7, 
               label = generate_plabel(vessel_density_lean_vs_obese$coefficients["p.value"])) +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p2)
ggsave(sprintf("%s/vessel_density.png", results_path), width=4500, height=3000, units="px")
dev.off()
