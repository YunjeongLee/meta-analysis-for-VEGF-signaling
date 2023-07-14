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

VEGF165VEGR1 <- vegfkonkoff[vegfkonkoff$Parameter == "VEGF165:VEGR1", ]

VEGF165VEGFR2 <- vegfkonkoff[vegfkonkoff$Parameter == "VEGF165:VEGFR2", ]

VEGF165NRP1 <- vegfkonkoff[vegfkonkoff$Parameter == "VEGF165:NRP1",]


# Give assumed SE----------------------------------------------------------

#VEGF165:VEGFR2 SE creation for 165 and 164 'kon', 'koff', and 'Kd'

# Define a function to calculate and update standard errors
update_se <- function(df, value_col, se_col, ligand) {
  # Create a logical vector indicating which rows to update
  rows_to_update <- is.na(df[[se_col]]) & df$Ligand == ligand
  
  # Extract the values from the specified column for the rows to update
  values <- df[rows_to_update, value_col]
  
  # Calculate the standard error of the values and put the values into the dataframe
  df[rows_to_update, se_col] <- sd(values) / sqrt(length(values))
  
  return(df)
}

# Update standard errors for 'kon', 'koff', and 'Kd' columns
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "kon", "kon_SE", "VEGF165")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "kon", "kon_SE", "VEGF164")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "koff", "koff_SE", "VEGF165")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "koff", "koff_SE", "VEGF164")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "Kd", "Kd_SE", "VEGF165")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "Kd", "Kd_SE", "VEGF164")


#VEGF165:NRP1 
update_se1 <- function(df, value_col, se_col) {
  # Create a logical vector indicating which rows to update
  rows_to_update <- is.na(df[[se_col]])
  
  # Extract the values from the specified column for the rows to update
  values <- df[rows_to_update, value_col]
  
  # Calculate the standard error of the values and put the values into the dataframe
  df[rows_to_update, se_col] <- sd(values) / sqrt(length(values))
  
  return(df)
}
# Update standard errors for 'kon', 'koff', and 'Kd' columns
VEGF165NRP1 <- update_se1(VEGF165NRP1, "kon", "kon_SE")
VEGF165NRP1 <- update_se1(VEGF165NRP1, "koff", "koff_SE")
VEGF165NRP1 <- update_se1(VEGF165NRP1, "Kd", "Kd_SE")

# Rename variables

vegfr1 <- VEGF165VEGR1
vegfr2 <- VEGF165VEGFR2
nrp1 <- VEGF165NRP1

# Meta-analysis -----------------------------------------------------------
# Compute weighted average and SD -----------------------------------------
# VEGF-A165:VEGFR1 kon
rm_vegfr1 <- rma(yi = kon , sei = kon_SE, data=vegfr1)
summary(rm_vegfr1)

# VEGF-A165:VEGFR2 kon
rm_vegfr2 <- rma(yi = kon, sei =  kon_SE, data=vegfr2,measure = "RR", method = "FE")
summary(rm_vegfr2)

# VEGF-A165:NRP1 kon
rm_nrp1 <- rma(yi = kon, sei = kon_SE, data=nrp1)
summary(rm_nrp1)

# VEGF-A165:VEGFR1 koff
rm_vegfr1 <- rma(yi = koff, sei = koff_SE, data=vegfr1)
summary(rm_vegfr1)

# VEGF-A165:VEGFR2 koff
rm_vegfr2 <- rma(yi = koff, sei = koff_SE, data=vegfr2)
summary(rm_vegfr2)

# VEGF-A165:NRP1 koff
rm_nrp1 <- rma(yi = koff, sei = koff_SE, data=nrp1)
summary(rm_nrp1)

# Forest plot -------------------------------------------------------------
png(file=sprintf("%s/forest_vegfr1.png", results_path), width=1300, height=700)
forest_ylee(data=vegfr1, rm=rm_vegfr1, slab=vegfr1$Reference,
            unit="pM", atransf=function(x)1e3*x, title="kon koff of VEGF-A165:VEGFR1",
            xlab="kon koff, Kd (pM)", xlim = c(-0.15, 0.27), alim = c(0, 0.15), cex=2)
dev.off()
png(file=sprintf("%s/forest_vegfr2.png", results_path), width=1300, height=700)
forest_ylee(data=vegfr2, rm=rm_vegfr2, slab=vegfr2$Reference, 
            unit="nM", title="kon koff of VEGF-A165:VEGFR2",
            xlab="kon koff, Kd (nM)", xlim = c(-1.2, 1.8), alim = c(0, 1), cex=2)
dev.off()
png(file=sprintf("%s/forest_nrp1.png", results_path), width=1300, height=700)
forest_ylee(data=nrp1, rm=rm_nrp1, slab=nrp1$Reference, 
            unit="nM", title="kon koff of VEGF-A165:NRP1",
            xlab="kon koff, Kd (nM)", xlim = c(-25, 50), alim = c(0, 30), cex=2)
dev.off()

# Student's t-test --------------------------------------------------------
# kon Comparison
vegfr1_vs_vegfr2 = wtd.t.test(x=vegfr1$kon, y=vegfr2$kon,
                              weight=1/(vegfr1$SE^2+rm_vegfr1$tau2), 
                              weighty=1/(vegfr2$SE^2+rm_vegfr2$tau2),
                              alternative="less", samedata=FALSE)

vegfr1_vs_nrp1 = wtd.t.test(x=vegfr1$kon, y=nrp1$kon,
                            weight=1/(vegfr1$SE^2+rm_vegfr1$tau2), 
                            weighty=1/(nrp1$SE^2+rm_nrp1$tau2),
                            alternative="less", samedata=FALSE)

vegfr2_vs_nrp1 = wtd.t.test(x=vegfr2$kon, y=nrp1$kon,
                            weight=1/(vegfr2$SE^2+rm_vegfr2$tau2), 
                            weighty=1/(nrp1$SE^2+rm_nrp1$tau2),
                            alternative="less", samedata=FALSE)
# koff Comparison
vegfr1_vs_vegfr2 = wtd.t.test(x=vegfr1$koff, y=vegfr2$koff,
                              weight=1/(vegfr1$SE^2+rm_vegfr1$tau2), 
                              weighty=1/(vegfr2$SE^2+rm_vegfr2$tau2),
                              alternative="less", samedata=FALSE)

vegfr1_vs_nrp1 = wtd.t.test(x=vegfr1$koff, y=nrp1$koff,
                            weight=1/(vegfr1$SE^2+rm_vegfr1$tau2), 
                            weighty=1/(nrp1$SE^2+rm_nrp1$tau2),
                            alternative="less", samedata=FALSE)

vegfr2_vs_nrp1 = wtd.t.test(x=vegfr2$koff, y=nrp1$koff,
                            weight=1/(vegfr2$SE^2+rm_vegfr2$tau2), 
                            weighty=1/(nrp1$SE^2+rm_nrp1$tau2),
                            alternative="less", samedata=FALSE)

# Merge dataframes for plotting -------------------------------------------
vegfr1$Reference <- "VEGFR1"
vegfr2$Reference <- "VEGFR2"
nrp1$Reference <- "NRP1"

df = rbind(vegfr1[c("Reference", "kon")],
           vegfr2[c("Reference", "kon")],
           nrp1[c("Reference", "kon")])

# Scatter plot ------------------------------------------------------------
p = ggplot() +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y = kon*1e3, colour = Reference), size = 7) +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y=rm_vegfr1$b*1e3), shape = 95, size=20, colour = "darkblue") +
  labs(color="VEGFR1") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = vegfr2, aes(x = "VEGFR2", y = kon*1e3, colour = Reference), size = 7) +
  geom_point(data = vegfr2, aes(x = "VEGFR2", y=rm_vegfr2$b*1e3), shape = 95, size=20, colour = "darkgreen") +
  labs(color="VEGFR2") +
  lightness(scale_color_colormap('VEGFR2', discrete = T,colormap = "greens", reverse = T), scalefac(0.8)) + 
  guides(color = guide_legend(order=2)) +
  new_scale_color() +
  geom_point(data = nrp1, aes(x = "NRP1", y = kon*1e3, colour = Reference), size = 7) +
  geom_point(data = nrp1, aes(x = "NRP1", y=rm_nrp1$b*1e3), shape = 95, size=20, colour = "darkred") +
  labs(color="NRP1") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) +
  guides(color = guide_legend(order=3)) +
  xlab("") + ylab(TeX("kon koff, Kd (pM)")) +
  scale_y_continuous(trans= 'log10', breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)), limits = c(1e-1, 1e7),
                     sec.axis = sec_axis(trans=~./1e3, name="kon koff, Kd (nM)",
                                         breaks=trans_breaks('log10', function(x) 10^x),
                                         labels=trans_format('log10', math_format(10^.x)))) +
  geom_bracket(data = df, aes(x = Source, y = kon), xmin = "VEGFR1", xmax = "NRP1",
               y.position = 6, tip.length = c(0.2, 0.1), 
               label = generate_plabel(vegfr1_vs_nrp1$coefficients["p.value"])) +
  geom_bracket(data = df, aes(x = Source, y = kon), xmin = "VEGFR1", xmax = "VEGFR2",
               y.position = 4, tip.length = c(0.2, 0.1), 
               label = generate_plabel(vegfr1_vs_vegfr2$coefficients["p.value"])) +
  scale_x_discrete(limits=c("VEGFR1", "VEGFR2", "NRP1")) +
  ggtitle("Comparison of kons and koffs of VEGF-A to its receptors") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p)
ggsave(sprintf("%s/kon_koff.png", results_path), width=4500, height=3000, units="px")
dev.off()



