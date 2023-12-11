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

# Change column names -----------------------------------------------------
colnames(cbm_lean) <- c("Reference", "Average", "SE")
colnames(cbm_obese) <- c("Reference", "Average", "SE")

# Split CBM thickness data of lean murines into tissue-dependent --------
cbm_retina = cbm_lean %>% filter(str_detect(Reference, "Retina"))
cbm_muscle = cbm_lean %>% filter(str_detect(Reference, "Muscle"))
cbm_heart = cbm_lean %>% filter(str_detect(Reference, "Heart"))
cbm_kidney = cbm_lean %>% filter(str_detect(Reference, "Kidney"))

# Remove substring from tissue dataframes
cbm_retina$Reference <- str_remove(cbm_retina$Reference, " & Retina")
cbm_muscle$Reference <- str_remove(cbm_muscle$Reference, " & Muscle")
cbm_heart$Reference <- str_remove(cbm_heart$Reference, " & Heart")
cbm_kidney$Reference <- str_remove(cbm_kidney$Reference, " & Kidney")

# Meta-analysis -----------------------------------------------------------
# 1. Include all kidney data
# Capillary BM thickness of lean mice
rm_cbm_lean <- rma(yi = Average, sei = SE, data=cbm_lean)
summary(rm_cbm_lean)

# Capillary BM thickness of obese mice
rm_cbm_obese <- rma(yi = Average, sei = SE, data=cbm_obese)
summary(rm_cbm_obese)

# 2. Exclude kidney data
cbm_lean_wo_kid <- cbm_lean %>% 
  filter(!str_detect(Reference, "Kidney"))

# Capillary BM thickness of lean mice without kidney data
rm_cbm_lean_wo_kid <- rma(yi = Average, sei = SE, data=cbm_lean_wo_kid)
summary(rm_cbm_lean_wo_kid)

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
# 1. Include all kidney data
# CBM thickness of lean mice
png(file=sprintf("%s/forest_cbm_lean.png", results_path), width=2000, height=2500)
forest_ylee(data=cbm_lean, rm=rm_cbm_lean, slab=cbm_lean$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-350, 550), alim = c(0, 350), cex=2)
dev.off()

# CBM thickness of obese mice
png(file=sprintf("%s/forest_cbm_obese.png", results_path), width=1300, height=700)
forest_ylee(data=cbm_obese, rm=rm_cbm_obese, slab=cbm_obese$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-300, 400), alim = c(0, 200), cex=2)
dev.off()

# 2. Exclude kidney data
# CBM thickness of lean mice
png(file=sprintf("%s/forest_cbm_lean_wo_kidney.png", results_path), width=2000, height=2000)
forest_ylee(data=cbm_lean_wo_kid, rm=rm_cbm_lean_wo_kid, slab=cbm_lean_wo_kid$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-350, 450), alim = c(0, 250), cex=2)
dev.off()

# 3. Tissue variability
# Retina
png(file=sprintf("%s/forest_cbm_retina.png", results_path), width=1300, height=1000)
forest_ylee(data=cbm_retina, rm=rm_cbm_retina, slab=cbm_retina$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-750, 650), alim = c(0, 300), cex=2)
dev.off()

# Muscle
png(file=sprintf("%s/forest_cbm_muscle.png", results_path), width=1300, height=500)
forest_ylee(data=cbm_muscle, rm=rm_cbm_muscle, slab=cbm_muscle$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-200, 300), alim = c(0, 150), cex=2)
dev.off()

# Heart
png(file=sprintf("%s/forest_cbm_heart.png", results_path), width=1300, height=700)
forest_ylee(data=cbm_heart, rm=rm_cbm_heart, slab=cbm_heart$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-150, 220), alim = c(0, 120), cex=2)
dev.off()

# Kidney
png(file=sprintf("%s/forest_cbm_kidney.png", results_path), width=1300, height=1000)
forest_ylee(data=cbm_kidney, rm=rm_cbm_kidney, slab=cbm_kidney$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-450, 650), alim = c(0, 350), cex=2)
dev.off()

# Student's t-test --------------------------------------------------------
# Lean vs. Obese (with kidney data)
cbm_lean_vs_obese = wtd.t.test(x=cbm_lean$Average, y=cbm_obese$Average,
                               weight=1/(cbm_lean$SE^2+rm_cbm_lean$tau2), 
                               weighty=1/(cbm_obese$SE^2+rm_cbm_obese$tau2),
                               alternative="two.tailed", samedata=FALSE)

# Lean vs. Obese (without kidney data)
cbm_lean_vs_obese_wo_kidney = wtd.t.test(x=cbm_lean_wo_kid$Average, y=cbm_obese$Average,
                                         weight=1/(cbm_lean_wo_kid$SE^2+rm_cbm_lean_wo_kid$tau2), 
                                         weighty=1/(cbm_obese$SE^2+rm_cbm_obese$tau2),
                                         alternative="two.tailed", samedata=FALSE)

# Retina vs. Muscle
cbm_retina_vs_muscle = wtd.t.test(x=cbm_retina$Average, y=cbm_muscle$Average,
                                  weight=1/(cbm_retina$SE^2+rm_cbm_retina$tau2), 
                                  weighty=1/(cbm_muscle$SE^2+rm_cbm_muscle$tau2),
                                  alternative="two.tailed", samedata=FALSE)

# Retina vs. Heart
cbm_retina_vs_heart = wtd.t.test(x=cbm_retina$Average, y=cbm_heart$Average,
                                 weight=1/(cbm_retina$SE^2+rm_cbm_retina$tau2), 
                                 weighty=1/(cbm_heart$SE^2+rm_cbm_heart$tau2),
                                 alternative="two.tailed", samedata=FALSE)

# Retina vs. Kidney
cbm_retina_vs_kidney = wtd.t.test(x=cbm_retina$Average, y=cbm_kidney$Average,
                                  weight=1/(cbm_retina$SE^2+rm_cbm_retina$tau2), 
                                  weighty=1/(cbm_kidney$SE^2+rm_cbm_kidney$tau2),
                                  alternative="two.tailed", samedata=FALSE)

# Muscle vs. Heart
cbm_muscle_vs_heart = wtd.t.test(x=cbm_muscle$Average, y=cbm_heart$Average,
                                 weight=1/(cbm_muscle$SE^2+rm_cbm_muscle$tau2), 
                                 weighty=1/(cbm_heart$SE^2+rm_cbm_heart$tau2),
                                 alternative="two.tailed", samedata=FALSE)

# Muscle vs. Kidney
cbm_muscle_vs_kidney = wtd.t.test(x=cbm_muscle$Average, y=cbm_kidney$Average,
                                  weight=1/(cbm_muscle$SE^2+rm_cbm_muscle$tau2), 
                                  weighty=1/(cbm_kidney$SE^2+rm_cbm_kidney$tau2),
                                  alternative="two.tailed", samedata=FALSE)

# Heart vs. Kidney
cbm_heart_vs_kidney = wtd.t.test(x=cbm_heart$Average, y=cbm_kidney$Average,
                                 weight=1/(cbm_heart$SE^2+rm_cbm_heart$tau2), 
                                 weighty=1/(cbm_kidney$SE^2+rm_cbm_kidney$tau2),
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
# Compare lean vs. obese (with kidney)
p = ggplot() +
  geom_point(data = cbm_lean, aes(x = "Lean murines", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_lean, aes(x = "Lean murines", y = rm_cbm_lean$b), shape = 95, size = 20, colour = "darkblue") +
  labs(color="Lean murines") +
  lightness(scale_color_colormap('Lean murines', discrete = T, colormap = brewer.blues(rm_cbm_lean$k), reverse = T), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = cbm_obese, aes(x = "Obese murines", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_obese, aes(x = "Obese murines", y = rm_cbm_obese$b), shape = 95, size = 20, colour = "darkred") +
  labs(color="Obese murines") +
  lightness(scale_color_colormap('Obese murines', discrete = T,colormap = brewer.oranges(rm_cbm_obese$k), reverse = T), scalefac(0.8)) +
  xlab("") + ylab(TeX("Capillary basement membrane thickness (nm)")) +
  theme(text = element_text(size = 20), legend.position='none') + ylim(c(0, 350))

show(p)
ggsave(sprintf("%s/cbm_lean_vs_obese.png", results_path), width=2000, height=2500, units="px")
dev.off()

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

# Split CBM thickness data of obese murines into tissue-dependent -------
# Add string to obese data to show that it is from obese dataset
first <- word(cbm_obese$Reference, 1, sep="\\(")
inside <- str_extract(cbm_obese$Reference, pattern="(?<=\\().*(?=\\))")
cbm_obese$Reference = paste0(first, "(Obese ", inside, ")")
cbm_w_ob = rbind(cbm_lean, cbm_obese)

# Split the data by tissue
cbm_retina_w_ob = cbm_w_ob %>% filter(str_detect(Reference, "Retina"))
cbm_muscle_w_ob = cbm_w_ob %>% filter(str_detect(Reference, "Muscle"))
cbm_heart_w_ob = cbm_w_ob %>% filter(str_detect(Reference, "Heart"))
cbm_kidney_w_ob = cbm_w_ob %>% filter(str_detect(Reference, "Kidney"))

# Remove substring from tissue dataframes
cbm_retina$Reference <- str_remove(cbm_retina_w_ob$Reference, " & Retina")
cbm_muscle$Reference <- str_remove(cbm_muscle_w_ob$Reference, " & Muscle")
cbm_heart$Reference <- str_remove(cbm_heart_w_ob$Reference, " & Heart")
cbm_kidney$Reference <- str_remove(cbm_kidney_w_ob$Reference, " & Kidney")

# Meta-analysis -----------------------------------------------------------
# Tissue variability -- including obese data
# Retina (with obese data)
rm_cbm_retina_w_ob <- rma(yi = Average, sei = SE, data=cbm_retina_w_ob)
summary(rm_cbm_retina_w_ob)

# Muscle (with obese data)
rm_cbm_muscle_w_ob <- rma(yi = Average, sei = SE, data=cbm_muscle_w_ob)
summary(rm_cbm_muscle_w_ob)

# Heart (with obese data)
rm_cbm_heart_w_ob <- rma(yi = Average, sei = SE, data=cbm_heart_w_ob)
summary(rm_cbm_heart_w_ob)

# Kidney (with obese data)
rm_cbm_kidney_w_ob <- rma(yi = Average, sei = SE, data=cbm_kidney_w_ob)
summary(rm_cbm_kidney_w_ob)

# Forest plot -------------------------------------------------------------
# Retina
png(file=sprintf("%s/forest_cbm_retina_w_ob.png", results_path), width=1300, height=1000)
forest_ylee(data=cbm_retina_w_ob, rm=rm_cbm_retina_w_ob, slab=cbm_retina_w_ob$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-750, 650), alim = c(0, 300), cex=2)
dev.off()

# Muscle
png(file=sprintf("%s/forest_cbm_muscle_w_ob.png", results_path), width=1300, height=500)
forest_ylee(data=cbm_muscle_w_ob, rm=rm_cbm_muscle_w_ob, slab=cbm_muscle_w_ob$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-200, 300), alim = c(0, 150), cex=2)
dev.off()

# Heart
png(file=sprintf("%s/forest_cbm_heart_w_ob.png", results_path), width=1300, height=700)
forest_ylee(data=cbm_heart_w_ob, rm=rm_cbm_heart_w_ob, slab=cbm_heart_w_ob$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-150, 220), alim = c(0, 120), cex=2)
dev.off()

# Kidney
png(file=sprintf("%s/forest_cbm_kidney_w_ob.png", results_path), width=1300, height=1000)
forest_ylee(data=cbm_kidney_w_ob, rm=rm_cbm_kidney_w_ob, slab=cbm_kidney_w_ob$Reference, 
            unit="nm",
            xlab="Capillary basement membrane thickness (nm)", xlim = c(-450, 650), alim = c(0, 350), cex=2)
dev.off()

# Student's t-test --------------------------------------------------------
# Retina vs. Muscle
cbm_retina_vs_muscle_w_ob = wtd.t.test(x=cbm_retina_w_ob$Average, y=cbm_muscle_w_ob$Average,
                                       weight=1/(cbm_retina_w_ob$SE^2+rm_cbm_retina_w_ob$tau2), 
                                       weighty=1/(cbm_muscle_w_ob$SE^2+rm_cbm_muscle_w_ob$tau2),
                                       alternative="two.tailed", samedata=FALSE)

# Retina vs. Heart
cbm_retina_vs_heart_w_ob = wtd.t.test(x=cbm_retina_w_ob$Average, y=cbm_heart_w_ob$Average,
                                      weight=1/(cbm_retina_w_ob$SE^2+rm_cbm_retina_w_ob$tau2), 
                                      weighty=1/(cbm_heart_w_ob$SE^2+rm_cbm_heart_w_ob$tau2),
                                      alternative="two.tailed", samedata=FALSE)

# Retina vs. Kidney
cbm_retina_vs_kidney_w_ob = wtd.t.test(x=cbm_retina_w_ob$Average, y=cbm_kidney_w_ob$Average,
                                       weight=1/(cbm_retina_w_ob$SE^2+rm_cbm_retina_w_ob$tau2), 
                                       weighty=1/(cbm_kidney_w_ob$SE^2+rm_cbm_kidney_w_ob$tau2),
                                       alternative="two.tailed", samedata=FALSE)

# Muscle vs. Heart
cbm_muscle_vs_heart_w_ob = wtd.t.test(x=cbm_muscle_w_ob$Average, y=cbm_heart_w_ob$Average,
                                      weight=1/(cbm_muscle_w_ob$SE^2+rm_cbm_muscle_w_ob$tau2), 
                                      weighty=1/(cbm_heart_w_ob$SE^2+rm_cbm_heart_w_ob$tau2),
                                      alternative="two.tailed", samedata=FALSE)

# Muscle vs. Kidney
cbm_muscle_vs_kidney_w_ob = wtd.t.test(x=cbm_muscle_w_ob$Average, y=cbm_kidney_w_ob$Average,
                                       weight=1/(cbm_muscle_w_ob$SE^2+rm_cbm_muscle_w_ob$tau2), 
                                       weighty=1/(cbm_kidney_w_ob$SE^2+rm_cbm_kidney_w_ob$tau2),
                                       alternative="two.tailed", samedata=FALSE)

# Heart vs. Kidney
cbm_heart_vs_kidney_w_ob = wtd.t.test(x=cbm_heart_w_ob$Average, y=cbm_kidney_w_ob$Average,
                                      weight=1/(cbm_heart_w_ob$SE^2+rm_cbm_heart_w_ob$tau2), 
                                      weighty=1/(cbm_kidney_w_ob$SE^2+rm_cbm_kidney_w_ob$tau2),
                                      alternative="two.tailed", samedata=FALSE)

# Merge dataframes for plotting -------------------------------------------
cbm_retina_w_ob$Source <- "Retina"
cbm_muscle_w_ob$Source <- "Muscle"
cbm_heart_w_ob$Source <- "Heart"
cbm_kidney_w_ob$Source <- "Kidney"

cbm_tissue_w_ob <- rbind(cbm_retina_w_ob[c("Source", "Average")],
                         cbm_muscle_w_ob[c("Source", "Average")],
                         cbm_heart_w_ob[c("Source", "Average")],
                         cbm_kidney_w_ob[c("Source", "Average")])


# Scatter plot ------------------------------------------------------------
# Compare by tissue (with obese group)
p = ggplot() +
  geom_point(data = cbm_retina_w_ob, aes(x = "Retina", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_retina_w_ob, aes(x = "Retina", y = rm_cbm_retina_w_ob$b), shape = 95, size = 20, colour = "darkblue") +
  labs(color="Retina") +
  lightness(scale_color_colormap('Retina', discrete = T, colormap = brewer.blues(rm_cbm_retina_w_ob$k), reverse = T), scalefac(0.8)) +
  new_scale_color() + 
  geom_point(data = cbm_muscle_w_ob, aes(x = "Muscle", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_muscle_w_ob, aes(x = "Muscle", y = rm_cbm_muscle_w_ob$b), shape = 95, size = 20, colour = "darkgreen") +
  labs(color="Muscle") +
  lightness(scale_color_brewer(palette="Greens"), scalefac(0.8)) +
  new_scale_color() + 
  geom_point(data = cbm_heart_w_ob, aes(x = "Heart", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_heart_w_ob, aes(x = "Heart", y = rm_cbm_heart_w_ob$b), shape = 95, size = 20, colour = "darkred") +
  labs(color="Heart") +
  lightness(scale_color_brewer(palette="Oranges"), scalefac(0.8)) +
  new_scale_color() + 
  geom_point(data = cbm_kidney_w_ob, aes(x = "Kidney", y = Average, colour = Reference), size = 7) +
  geom_point(data = cbm_kidney_w_ob, aes(x = "Kidney", y = rm_cbm_kidney_w_ob$b), shape = 95, size = 20, colour = "black") +
  labs(color="Kidney") +
  lightness(scale_color_colormap('Kidney', discrete = T,colormap = brewer.purples(rm_cbm_kidney_w_ob$k), reverse = T), scalefac(0.8)) +
  scale_x_discrete(limits = c("Retina", "Muscle", "Heart", "Kidney")) +
  xlab("") + ylab(TeX("Capillary basement membrane thickness (nm)")) +
  geom_bracket(data = cbm_tissue_w_ob, aes(x = Source, y = Average), xmin = "Retina", xmax = "Kidney",
               y.position = 480, tip.length = c(0.8, 0.1), label.size = 7, 
               label = generate_plabel(cbm_retina_vs_kidney_w_ob$coefficients["p.value"])) +
  geom_bracket(data = cbm_tissue_w_ob, aes(x = Source, y = Average), xmin = "Muscle", xmax = "Kidney",
               y.position = 420, tip.length = c(0.6, 0.1), label.size = 7, 
               label = generate_plabel(cbm_muscle_vs_kidney_w_ob$coefficients["p.value"])) +
  geom_bracket(data = cbm_tissue_w_ob, aes(x = Source, y = Average), xmin = "Heart", xmax = "Kidney",
               y.position = 360, tip.length = c(0.4, 0.1), label.size = 7, 
               label = generate_plabel(cbm_heart_vs_kidney_w_ob$coefficients["p.value"])) +
  theme(text = element_text(size = 20), legend.position='none') + ylim(c(0, 500))

show(p)
ggsave(sprintf("%s/cbm_w_ob.png", results_path), width=3500, height=2500, units="px")
dev.off()
