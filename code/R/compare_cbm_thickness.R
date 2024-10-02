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

# Split dataframe into small dataframes -----------------------------------
cbm_lean = cbm[!is.na(cbm["Lean SE"]), c("Reference", "Lean average", "Lean SE")]
cbm_obese = cbm[!is.na(cbm["Obese SE"]), c("Reference", "Obese average", "Obese SE")]
cbm_obese$Reference <- str_replace(cbm_obese$Reference, "LETO", "OLETF")

# Change column names -----------------------------------------------------
colnames(cbm_lean) <- c("Reference", "Average", "SE")
colnames(cbm_obese) <- c("Reference", "Average", "SE")

# Split CBM thickness data of lean murines into tissue-dependent --------
cbm_retina = cbm_lean %>% filter(str_detect(Reference, "Retina"))
cbm_muscle = cbm_lean %>% filter(str_detect(Reference, "Muscle"))
cbm_heart = cbm_lean %>% filter(str_detect(Reference, "Heart"))
cbm_kidney = cbm_lean %>% filter(str_detect(Reference, "Kidney"))

# Merge retina, muscle, and heart CBM thickness to generate lean mice data
cbm_lean_tis = cbm_lean %>% filter(str_detect(Reference, "Retina|Muscle|Heart"))

# Remove substring from tissue dataframes
cbm_retina$Reference <- str_remove(cbm_retina$Reference, " & Retina")
cbm_muscle$Reference <- str_remove(cbm_muscle$Reference, " & Muscle")
cbm_heart$Reference <- str_remove(cbm_heart$Reference, " & Heart")
cbm_kidney$Reference <- str_remove(cbm_kidney$Reference, " & Kidney")

# Meta-analysis -----------------------------------------------------------
# Capillary BM thickness of lean mice including only retina, muscle, and heart data
rm_cbm_lean_tis <- rma(yi = Average, sei = SE, data=cbm_lean_tis)
summary(rm_cbm_lean_tis)

# Capillary BM thickness of obese mice
rm_cbm_obese <- rma(yi = Average, sei = SE, data=cbm_obese)
summary(rm_cbm_obese)

# 3. Tissue variability
# Retina (without obese data)
rm_cbm_retina <- rma(yi = Average, sei = SE, data=cbm_retina)
summary(rm_cbm_retina)

# Muscle (without obese data)
rm_cbm_muscle <- rma(yi = Average, sei = SE, data=cbm_muscle)
summary(rm_cbm_muscle)

# Heart (without obese data)
rm_cbm_heart <- rma(yi = Average, sei = SE, data=cbm_heart)
summary(rm_cbm_heart)

# Kidney (without obese data)
rm_cbm_kidney <- rma(yi = Average, sei = SE, data=cbm_kidney)
summary(rm_cbm_kidney)

# Forest plot -------------------------------------------------------------
# CBM thickness of lean mice
png(file=sprintf("%s/forest_cbm_lean.png", results_path), width=2000, height=3000)
forest_ylee(data=cbm_lean_tis, rm=rm_cbm_lean_tis, slab=cbm_lean_tis$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-350, 450), alim = c(0, 250), cex=2, numDigits=0L)
dev.off()

# CBM thickness of obese mice
png(file=sprintf("%s/forest_cbm_obese.png", results_path), width=1300, height=700)
forest_ylee(data=cbm_obese, rm=rm_cbm_obese, slab=cbm_obese$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-300, 400), alim = c(0, 200), cex=2, numDigits=0L)
dev.off()

# 3. Tissue variability
# Retina
png(file=sprintf("%s/forest_cbm_retina.png", results_path), width=1300, height=1700)
forest_ylee(data=cbm_retina, rm=rm_cbm_retina, slab=cbm_retina$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-750, 650), alim = c(0, 300), cex=2, numDigits=0L)
dev.off()

# Muscle
png(file=sprintf("%s/forest_cbm_muscle.png", results_path), width=1300, height=600)
forest_ylee(data=cbm_muscle, rm=rm_cbm_muscle, slab=cbm_muscle$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-300, 300), alim = c(0, 150), cex=2, numDigits=0L)
dev.off()

# Heart
png(file=sprintf("%s/forest_cbm_heart.png", results_path), width=1300, height=700)
forest_ylee(data=cbm_heart, rm=rm_cbm_heart, slab=cbm_heart$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-150, 220), alim = c(0, 120), cex=2, numDigits=0L)
dev.off()

# Kidney
png(file=sprintf("%s/forest_cbm_kidney.png", results_path), width=1300, height=1000)
forest_ylee(data=cbm_kidney, rm=rm_cbm_kidney, slab=cbm_kidney$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-500, 700), alim = c(0, 400), cex=2, numDigits=0L)
dev.off()

# Student's t-test --------------------------------------------------------
# Lean vs. Obese (without kidney data)
cbm_lean_vs_obese_wo_kidney = wtd.t.test(x=cbm_lean_wo_kid$Average, y=cbm_obese$Average,
                                         weight=1/(cbm_lean_wo_kid$SE^2+rm_cbm_lean_wo_kid$tau2), 
                                         weighty=1/(cbm_obese$SE^2+rm_cbm_obese$tau2),
                                         alternative="two.tailed", samedata=FALSE)

# Merge dataframes for plotting -------------------------------------------
cbm_retina$Source <- "Retina"
cbm_muscle$Source <- "Muscle"
cbm_heart$Source <- "Heart"
cbm_kidney$Source <- "Kidney"

cbm_tissue <- rbind(cbm_retina[c("Source", "Average")],
                    cbm_muscle[c("Source", "Average")],
                    cbm_heart[c("Source", "Average")],
                    cbm_kidney[c("Source", "Average")])

# Scatter plot ------------------------------------------------------------
# Compare lean vs. obese (without kidney)
p = ggplot() +
  geom_point(data = cbm_lean_wo_kid, aes(x = "Lean murines", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_lean_wo_kid, aes(x = "Lean murines", y = rm_cbm_lean_wo_kid$b), shape = 95, size = 20, colour = "darkblue") +
  labs(color="Lean murines") +
  lightness(scale_color_colormap('Lean murines', discrete = T, colormap = brewer.blues(rm_cbm_lean_wo_kid$k), reverse = T), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = cbm_obese, aes(x = "Obese murines", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_obese, aes(x = "Obese murines", y = rm_cbm_obese$b), shape = 95, size = 20, colour = "darkred") +
  labs(color="Obese murines") +
  lightness(scale_color_colormap('Obese murines', discrete = T,colormap = brewer.oranges(rm_cbm_obese$k), reverse = T), scalefac(0.8)) +
  xlab("") + ylab(TeX("Capillary basement membrane thickness (nm)")) +
  theme(text = element_text(size = 20), legend.position='none') + ylim(c(0, 350))

show(p)
ggsave(sprintf("%s/cbm_lean_vs_obese_wo_kidney.png", results_path), width=2000, height=2500, units="px")
dev.off()

# Compare by tissue (without obese group)
p = ggplot() +
  geom_point(data = cbm_retina, aes(x = "Retina", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_retina, aes(x = "Retina", y = rm_cbm_retina$b), shape = 95, size = 20, colour = "darkblue") +
  labs(color="Retina") +
  lightness(scale_color_colormap('Retina', discrete = T, colormap = brewer.blues(rm_cbm_retina$k), reverse = T), scalefac(0.8)) +
  new_scale_color() + 
  geom_point(data = cbm_muscle, aes(x = "Muscle", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_muscle, aes(x = "Muscle", y = rm_cbm_muscle$b), shape = 95, size = 20, colour = "darkgreen") +
  labs(color="Muscle") +
  lightness(scale_color_brewer(palette="Greens"), scalefac(0.8)) +
  new_scale_color() + 
  geom_point(data = cbm_heart, aes(x = "Heart", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_heart, aes(x = "Heart", y = rm_cbm_heart$b), shape = 95, size = 20, colour = "darkred") +
  labs(color="Heart") +
  lightness(scale_color_brewer(palette="Oranges"), scalefac(0.8)) +
  new_scale_color() + 
  geom_point(data = cbm_kidney, aes(x = "Kidney", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_kidney, aes(x = "Kidney", y = rm_cbm_kidney$b), shape = 95, size = 20, colour = "black") +
  labs(color="Kidney") +
  lightness(scale_color_colormap('Kidney', discrete = T,colormap = brewer.purples(rm_cbm_kidney$k), reverse = T), scalefac(0.8)) +
  scale_x_discrete(limits = c("Retina", "Muscle", "Heart", "Kidney")) +
  xlab("") + ylab(TeX("Capillary basement membrane thickness (nm)")) +
  geom_bracket(data = cbm_tissue, aes(x = Source, y = Average), xmin = "Retina", xmax = "Kidney",
               y.position = 480, tip.length = c(0.8, 0.1), label.size = 7, 
               label = generate_plabel(cbm_retina_vs_kidney$coefficients["p.value"])) +
  geom_bracket(data = cbm_tissue, aes(x = Source, y = Average), xmin = "Muscle", xmax = "Kidney",
               y.position = 420, tip.length = c(0.6, 0.1), label.size = 7, 
               label = generate_plabel(cbm_muscle_vs_kidney$coefficients["p.value"])) +
  geom_bracket(data = cbm_tissue, aes(x = Source, y = Average), xmin = "Heart", xmax = "Kidney",
               y.position = 360, tip.length = c(0.4, 0.1), label.size = 7, 
               label = generate_plabel(cbm_heart_vs_kidney$coefficients["p.value"])) +
  theme(text = element_text(size = 20), legend.position='none') + ylim(c(0, 500))
  
  show(p)
ggsave(sprintf("%s/cbm.png", results_path), width=3500, height=2500, units="px")
dev.off()
