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
             "shades", "ggnewscale", "scales", "ggsignif", "colormap","stringi")
instant_pkgs(pkg_list)

# Load data ---------------------------------------------------------------
filename = '../../data/kon_koffs.xlsx'
vegfkonkoff <- as.data.frame(read_excel(filename))

# Split Data by parameter -------------------------------------------------

VEGF165VEGR1 <- vegfkonkoff[vegfkonkoff$Parameter == "VEGF165:VEGR1", ]

VEGF165VEGFR2 <- vegfkonkoff[vegfkonkoff$Parameter == "VEGF165:VEGFR2", ]


# Give assumed SE----------------------------------------------------------

#VEGF165:VEGFR2 SE creation for 165 and 164 'kon', 'koff', and 'Kd'

# Define a function to calculate and update standard errors
update_se <- function(df, value_col, se_col, ligand, ligand1) {
  # Create a logical vector indicating which rows to update
  rows_to_update <- is.na(df[[se_col]]) & (df$Ligand == ligand | df$Ligand == ligand1) 
  
  # Extract the values from the specified column for the rows to update
  values <- df[rows_to_update, value_col]
  
  # Calculate the standard error of the values and put the values into the dataframe
  df[rows_to_update, se_col] <- sd(values) / sqrt(length(values))
  
  return(df)
}

# Update standard errors for 'kon', 'koff', and 'Kd' columns
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "kon", "kon_SE", "VEGF165", "VEGF164")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "koff", "koff_SE", "VEGF165", "VEGF164")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "Kd", "Kd_SE", "VEGF165", "VEGF164")

# Rename variables

vegfr1 <- VEGF165VEGR1
vegfr2 <- VEGF165VEGFR2

# Meta-analysis -----------------------------------------------------------
# Compute weighted average and SD -----------------------------------------
# VEGF-A165:VEGFR1 kon
rm_vegfr1_kon <- rma(yi = kon , sei = kon_SE, data=vegfr1)
summary(rm_vegfr1_kon)

# VEGF-A165:VEGFR2 kon
rm_vegfr2_kon <- rma(yi = kon, sei = kon_SE, data = vegfr2, method = "ML")
summary(rm_vegfr2_kon)

# VEGF-A165:VEGFR1 koff
rm_vegfr1_koff <- rma(yi = koff, sei = koff_SE, data=vegfr1)
summary(rm_vegfr1_koff)

# VEGF-A165:VEGFR2 koff
rm_vegfr2_koff <- rma(yi = koff, sei = koff_SE, data=vegfr2)
summary(rm_vegfr2_koff)

# Forest plot -------------------------------------------------------------
# kon
png(file=sprintf("%s/forest_vegfr1vegf165_kon.png", results_path), width=1500, height=700)
forest_ylee(data=vegfr1, rm=rm_vegfr1_kon, slab=vegfr1$Reference,
            unit = paste0("M", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+207B><U+00B9> s<U+207B><U+00B9>"))),
            title="kon of VEGF-A165:VEGFR1",
            xlab=TeX("kon $M^{-1}s^{-1}$"), xlim = c(-3e+7, 0.75e+8), alim = c(0, 3e+7), cex=2, atransf=100)
dev.off()
png(file=sprintf("%s/forest_vegfr2vegf165_kon.png", results_path), width=1300, height=700)
forest_ylee(data=vegfr2, rm=rm_vegfr2_kon, slab=vegfr2$Reference, 
            unit = paste0("M", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+207B><U+00B9> s<U+207B><U+00B9>"))),
            title="kon of VEGF-A165:VEGFR2",
            xlab=TeX("kon $M^{-1}s^{-1}$"), xlim = c(-0.5e+7, 1.4e+7), alim = c(0, 0.5e+7), cex=2)
dev.off()

# koff
png(file=sprintf("%s/forest_vegfr1vegf165_koff.png", results_path), width=1300, height=700)
forest_ylee(data=vegfr1, rm=rm_vegfr1_koff, slab=vegfr1$Reference,
            unit = paste0("s", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+207B><U+00B9>"))),
            title="koff of VEGF-A165:VEGFR1",
            xlab=TeX("koff $s^{-1}$"), xlim = c(-1e-3, 1.9e-3), alim = c(0, 1e-3), cex=2)
dev.off()
png(file=sprintf("%s/forest_vegfr2vegf165_koff.png", results_path), width=1300, height=700)
forest_ylee(data=vegfr2, rm=rm_vegfr2_koff, slab=vegfr2$Reference, 
            unit = paste0("s", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+207B><U+00B9>"))),
            title="koff of VEGF-A165:VEGFR2",
            xlab=TeX("koff $s^{-1}$"), xlim = c(-1e-3, 1.9e-3), alim = c(0, 1e-3), cex=2)
dev.off()

# Student's t-test --------------------------------------------------------
# kon Comparison
vegfr1_vs_vegfr2kon = wtd.t.test(x=vegfr1$kon, y=vegfr2$kon,
                              weight=1/(vegfr1$kon_SE^2+rm_vegfr1_kon$tau2), 
                              weighty=1/(vegfr2$kon_SE^2+rm_vegfr2_kon$tau2),
                              alternative="less", samedata=FALSE)


# koff Comparison
vegfr1_vs_vegfr2koff = wtd.t.test(x=vegfr1$koff, y=vegfr2$koff,
                              weight=1/(vegfr1$koff_SE^2+rm_vegfr1_koff$tau2), 
                              weighty=1/(vegfr2$koff_SE^2+rm_vegfr2_koff$tau2),
                              alternative="less", samedata=FALSE)


# Merge dataframes for plotting -------------------------------------------
vegfr1$Ligand <- "VEGFR1"
vegfr2$Ligand <- "VEGFR2"

df = rbind(vegfr1[c("Ligand", "kon")],
           vegfr2[c("Ligand", "kon")])

# Scatter plot ------------------------------------------------------------
# kon
p = ggplot() +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y = kon, colour = Reference), size = 7) +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y=rm_vegfr1_kon$b), shape = 95, size=20, colour = "darkblue") +
  labs(color="VEGFR1") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = vegfr2, aes(x = "VEGFR2", y = kon, colour = Reference), size = 7) +
  geom_point(data = vegfr2, aes(x = "VEGFR2", y=rm_vegfr2_kon$b), shape = 95, size=20, colour = "darkgreen") +
  labs(color="VEGFR2") +
  lightness(scale_color_colormap('VEGFR2', discrete = T,colormap = "greens", reverse = T), scalefac(0.8)) + 
  guides(color = guide_legend(order=2)) +
  new_scale_color() +
  xlab("") + ylab(expression(paste("kon   (", M^-1, s^-1, ")"))) +
  scale_y_continuous(trans= 'log10', breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)), limits = c(1e5, 1e10),
                     sec.axis = sec_axis(trans=~./1, name=expression(paste("kon   (", M^-1, s^-1, ")")),
                                         breaks=trans_breaks('log10', function(x) 10^x),
                                         labels=trans_format('log10', math_format(10^.x)))) +
  scale_x_discrete(limits=c("VEGFR1", "VEGFR2")) +
  ggtitle("Comparison of VEGF-A kons to its receptors") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p)
ggsave(sprintf("%s/kon.png", results_path), width=4500, height=3000, units="px")
dev.off()


# Merge dataframes for plotting -------------------------------------------
vegfr1$Ligand <- "VEGFR1"
vegfr2$Ligand <- "VEGFR2"

df1 = rbind(vegfr1[c("Ligand", "koff")],
           vegfr2[c("Ligand", "koff")])

#koff
p = ggplot() +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y = koff, colour = Reference), size = 7) +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y=rm_vegfr1_koff$b), shape = 95, size=20, colour = "darkblue") +
  labs(color="VEGFR1") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = vegfr2, aes(x = "VEGFR2", y = koff*1e3, colour = Reference), size = 7) +
  geom_point(data = vegfr2, aes(x = "VEGFR2", y=rm_vegfr2_koff$b), shape = 95, size=20, colour = "darkgreen") +
  labs(color="VEGFR2") +
  lightness(scale_color_colormap('VEGFR2', discrete = T,colormap = "greens", reverse = T), scalefac(0.8)) + 
  guides(color = guide_legend(order=2)) +
  xlab("") + ylab(expression(paste("koff  (", s^-1, ")"))) +
  scale_y_continuous(trans= 'log10', breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)), limits = c(1e-7, 1e3),
                     sec.axis = sec_axis(trans=~./1, name=expression(paste("koff  (", s^-1, ")")),
                                         breaks=trans_breaks('log10', function(x) 10^x),
                                         labels=trans_format('log10', math_format(10^.x)))) +
  scale_x_discrete(limits=c("VEGFR1", "VEGFR2")) +
  ggtitle("Comparison of VEGF-A koffs to its receptors") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p)
ggsave(sprintf("%s/koff.png", results_path), width=4500, height=3000, units="px")
dev.off()



