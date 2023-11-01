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
filename_fitted = '../../data/VEGFA165_VEGFR2_NRP1_fitted_binding_kinetics.xlsx'

vegfkonkoff <- as.data.frame(read_excel(filename))

vegfkonkoff_fitted <- as.data.frame(read_excel(filename_fitted))

# Split Data by parameter -------------------------------------------------

VEGF165VEGFR1 <- vegfkonkoff[vegfkonkoff$Parameter == "VEGF165:VEGR1", ]

VEGF165VEGFR2 <- vegfkonkoff[vegfkonkoff$Parameter == "VEGF165:VEGFR2", ]

VEGF165NRP1 <- vegfkonkoff[vegfkonkoff$Parameter == "VEGF165:NRP1", ]

VEGF165VEGFR2_fitted <- vegfkonkoff_fitted[vegfkonkoff_fitted$Parameter == "VEGF165:VEGFR2", ]

VEGF165NRP1_fitted <- vegfkonkoff_fitted[vegfkonkoff_fitted$Parameter == "VEGF165:NRP1", ]

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
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "koff", "koff_SE", "VEGF165")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "Kd", "Kd_SE", "VEGF165")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "kon", "kon_SE", "VEGF164")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "koff", "koff_SE", "VEGF164")
VEGF165VEGFR2 <- update_se(VEGF165VEGFR2, "Kd", "Kd_SE", "VEGF164")
VEGF165NRP1 <- update_se(VEGF165NRP1,  "kon", "kon_SE", "VEGF165")
VEGF165NRP1 <- update_se(VEGF165NRP1,  "koff", "koff_SE", "VEGF165")
VEGF165NRP1 <- update_se(VEGF165NRP1,  "Kd", "Kd_SE", "VEGF165")

# Change reference names to make them distinct

VEGF165VEGFR2[VEGF165VEGFR2$Ligand == "VEGF165" & VEGF165VEGFR2$Reference == "Huang et al., 1998", "Reference"] <- "Huang et al., 1998 (VEGF-A165)"
VEGF165VEGFR2[VEGF165VEGFR2$Ligand == "VEGF164" & VEGF165VEGFR2$Reference == "Huang et al., 1998", "Reference"] <- "Huang et al., 1998 (VEGF-A164)"
VEGF165VEGFR2[VEGF165VEGFR2$kon == 3600000 & VEGF165VEGFR2$Reference == "Cunningham et al., 1999", "Reference"] <- "Cunningham et al., 1999 (predimer)"
VEGF165VEGFR2[VEGF165VEGFR2$kon == 5230000 & VEGF165VEGFR2$Reference == "Cunningham et al., 1999", "Reference"] <- "Cunningham et al., 1999 (monomer)"

# combined the rows and display the means 

# Huang et al. data
VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A165)", "kon"] <- mean(VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A165)", "kon"])
VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A164)", "kon"] <- mean(VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A164)", "kon"])
VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A165)", "koff"] <- mean(VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A165)", "koff"])
VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A164)", "koff"] <- mean(VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A164)", "koff"])
VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A165)", "Kd"] <- mean(VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A165)", "Kd"])
VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A164)", "Kd"] <- mean(VEGF165VEGFR2[VEGF165VEGFR2$Reference == "Huang et al., 1998 (VEGF-A164)", "Kd"])

# In-house data, 2023
VEGF165VEGFR2[VEGF165VEGFR2$Reference == "In-house data, 2023", "kon"] <- mean(VEGF165VEGFR2[VEGF165VEGFR2$Reference == "In-house data, 2023", "kon"])
VEGF165VEGFR2[VEGF165VEGFR2$Reference == "In-house data, 2023", "koff"] <- mean(VEGF165VEGFR2[VEGF165VEGFR2$Reference == "In-house data, 2023", "koff"])
VEGF165VEGFR2[VEGF165VEGFR2$Reference == "In-house data, 2023", "Kd"] <- mean(VEGF165VEGFR2[VEGF165VEGFR2$Reference == "In-house data, 2023", "Kd"])

VEGF165VEGFR2 <- unique(VEGF165VEGFR2)

VEGF165NRP1[VEGF165NRP1$Reference == "In-house data, 2023", "kon"] <- mean(VEGF165NRP1[VEGF165NRP1$Reference == "In-house data, 2023", "kon"])
VEGF165NRP1[VEGF165NRP1$Reference == "In-house data, 2023", "koff"] <- mean(VEGF165NRP1[VEGF165NRP1$Reference == "In-house data, 2023", "koff"])
VEGF165NRP1[VEGF165NRP1$Reference == "In-house data, 2023", "Kd"] <- mean(VEGF165NRP1[VEGF165NRP1$Reference == "In-house data, 2023", "Kd"])
VEGF165NRP1 <- unique(VEGF165NRP1)

# Add the new data 

VEGF165NRP1 <- rbind(VEGF165NRP1, VEGF165NRP1_fitted)
VEGF165VEGFR2 <- rbind(VEGF165VEGFR2, VEGF165VEGFR2_fitted)

# Rename variables

vegfr1 <- VEGF165VEGFR1
vegfr2 <- VEGF165VEGFR2
nrp1 <- VEGF165NRP1

# Vegfr1 kon mean and kon se divide all values by 1e6

vegfr1[c("kon", "kon_SE")] <- vegfr1[c("kon", "kon_SE")]/1e6
vegfr2[c("kon", "kon_SE")] <- vegfr2[c("kon", "kon_SE")]/1e6
nrp1[c("kon", "kon_SE")] <- nrp1[c("kon", "kon_SE")]/1e6

# Meta-analysis -----------------------------------------------------------
# Compute weighted average and SD -----------------------------------------
# VEGF-A165:VEGFR1 kon
rm_vegfr1_kon <- rma(yi = kon , sei = kon_SE, data=vegfr1)
summary(rm_vegfr1_kon)

# VEGF-A165:VEGFR2 kon
rm_vegfr2_kon <- rma(yi = kon, sei = kon_SE, data = vegfr2)
summary(rm_vegfr2_kon)

# VEGF-A165:NRP1 kon
rm_nrp1_kon <- rma(yi = kon, sei = kon_SE, data=nrp1)
summary(rm_nrp1_kon)

# VEGF-A165:VEGFR1 koff
rm_vegfr1_koff <- rma(yi = koff, sei = koff_SE, data=vegfr1)
summary(rm_vegfr1_koff)

# VEGF-A165:VEGFR2 koff
rm_vegfr2_koff <- rma(yi = koff, sei = koff_SE, data=vegfr2)
summary(rm_vegfr2_koff)

# VEGF-A165:NRP1 koff
rm_nrp1_koff <- rma(yi = koff, sei = koff_SE, data=nrp1)
summary(rm_nrp1_koff)

# Forest plot -------------------------------------------------------------
# kon
png(file=sprintf("%s/forest_vegfr1vegf165_kon.png", results_path), width=1300, height=500)
forest_ylee(data=vegfr1, rm=rm_vegfr1_kon, slab=vegfr1$Reference,
            unit = stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B5>M<U+207B><U+00B9> s<U+207B><U+00B9>")),
            xlab=TeX("Association rate, $k_{on}$ ($\\mu M^{-1} s^{-1}$)"), xlim = c(-40, 75), alim = c(0, 45), cex=2)
dev.off()

png(file=sprintf("%s/forest_vegfr2vegf165_kon.png", results_path), width=1300, height=700)
forest_ylee(data=vegfr2, rm=rm_vegfr2_kon, slab=vegfr2$Reference, 
            unit = stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B5>M<U+207B><U+00B9> s<U+207B><U+00B9>")),
            xlab=TeX("Association rate, $k_{on}$ ($\\mu M^{-1} s^{-1}$)"), xlim = c(-0.09e+2, 1.6e+1), alim = c(0, .95e+1), cex=2)
dev.off()

png(file=sprintf("%s/forest_nrp1vegf165_kon.png", results_path), width=1300, height=500)
forest_ylee(data=nrp1, rm=rm_nrp1_kon, slab=nrp1$Reference, 
            unit = stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+00B5>M<U+207B><U+00B9> s<U+207B><U+00B9>")),
            xlab=TeX("Association rate, $k_{on}$ ($\\mu M^{-1} s^{-1}$)"), xlim = c(-7, 17), alim = c(0, 10), cex=2)
dev.off()

# koff
png(file=sprintf("%s/forest_vegfr1vegf165_koff.png", results_path), width=1300, height=500)
forest_ylee(data=vegfr1, rm=rm_vegfr1_koff, slab=vegfr1$Reference,
            unit = paste0("s", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+207B><U+00B9>"))),
            title="Dissociation rates of VEGF-A165:VEGFR1",
            xlab=TeX("Dissociation rate, $k_{off}$ ($\\times 10^{-3} s^{-1}$)"), xlim = c(-0.5e-3, 0.9e-3), alim = c(0, 0.5e-3), cex=2, atransf=function(x) x*1e3)
dev.off()
png(file=sprintf("%s/forest_vegfr2vegf165_koff.png", results_path), width=1300, height=700)
forest_ylee(data=vegfr2, rm=rm_vegfr2_koff, slab=vegfr2$Reference, 
            unit = paste0("s", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+207B><U+00B9>"))),
            title="Dissociation rates of VEGF-A165:VEGFR2",
            xlab=TeX("Dissociation rate, $k_{off}$ ($\\times 10^{-3} s^{-1}$)"), xlim = c(-4.2e-3, 8e-3), alim = c(0, 5e-3), cex=2, atransf=function(x) x*1e3)
dev.off()
png(file=sprintf("%s/forest_nrp1vegf165_koff.png", results_path), width=1300, height=500)
forest_ylee(data=nrp1, rm=rm_nrp1_koff, slab=nrp1$Reference, 
            unit = paste0("s", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", "<U+207B><U+00B9>"))),
            title="Dissociation rates of VEGF-A165:NRP1",
            xlab=TeX("Dissociation rate, $k_{off}$ ($\\times 10^{-3} s^{-1}$)"), xlim = c(-7e-3, 23e-3), alim = c(0, 15e-3), cex=2, atransf=function(x) x*1e3)
dev.off()

# Student's t-test --------------------------------------------------------
# kon Comparison
vegfr1_vs_vegfr2kon = wtd.t.test(x=vegfr1$kon, y=vegfr2$kon,
                                 weight=1/(vegfr1$kon_SE^2+rm_vegfr1_kon$tau2), 
                                 weighty=1/(vegfr2$kon_SE^2+rm_vegfr2_kon$tau2),
                                 alternative="greater", samedata=FALSE)


# koff Comparison
vegfr1_vs_vegfr2koff = wtd.t.test(x=vegfr1$koff, y=vegfr2$koff,
                                  weight=1/(vegfr1$koff_SE^2+rm_vegfr1_koff$tau2), 
                                  weighty=1/(vegfr2$koff_SE^2+rm_vegfr2_koff$tau2),
                                  alternative="less", samedata=FALSE)

# kon and koff Comparison nrp1 & vegfr2
nrp1_vs_vegfr2kon = wtd.t.test(x=nrp1$kon, y=vegfr2$kon,
                                 weight=1/(nrp1$kon_SE^2+rm_nrp1_kon$tau2), 
                                 weighty=1/(vegfr2$kon_SE^2+rm_vegfr2_kon$tau2),
                                 alternative="less", samedata=FALSE)

nrp1_vs_vegfr2koff = wtd.t.test(x=nrp1$koff, y=vegfr2$koff,
                               weight=1/(nrp1$koff_SE^2+rm_nrp1_koff$tau2), 
                               weighty=1/(vegfr2$koff_SE^2+rm_vegfr2_koff$tau2),
                               alternative="greater", samedata=FALSE)


# kon and koff Comparison nrp1 & vegfr1
nrp1_vs_vegfr1koff = wtd.t.test(x=vegfr1$koff, y=nrp1$koff,
                                  weight=1/(vegfr1$koff_SE^2+rm_vegfr1_koff$tau2), 
                                  weighty=1/(nrp1$koff_SE^2+rm_nrp1_koff$tau2),
                                  alternative="less", samedata=FALSE)

nrp1_vs_vegfr1kon = wtd.t.test(x=vegfr1$kon, y=nrp1$kon,
                                weight=1/(vegfr1$kon_SE^2+rm_vegfr1_kon$tau2), 
                                weighty=1/(nrp1$kon_SE^2+rm_nrp1_kon$tau2),
                                alternative="greater", samedata=FALSE)





# Merge dataframes for plotting -------------------------------------------
vegfr1$Ligand <- "VEGFR1"
vegfr2$Ligand <- "VEGFR2"
nrp1$Ligand <- "NRP1"

df = rbind(vegfr1[c("Ligand", "kon")],
           vegfr2[c("Ligand", "kon")],
           nrp1[c("Ligand", "kon")])  # Add NRP1 kon data

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
  lightness(scale_color_brewer(palette="Greens"),scalefac(0.8)) + 
  guides(color = guide_legend(order=2)) +
  new_scale_color() +
  geom_point(data = nrp1, aes(x = "NRP1", y = kon, colour = Reference), size = 7) +   # Add NRP1 kon plot
  geom_point(data = nrp1, aes(x = "NRP1", y=rm_nrp1_kon$b), shape = 95, size=20, colour = "darkred") +   # Add NRP1 kon plot
  labs(color="NRP1") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) + 
  guides(color = guide_legend(order=3)) +
  xlab("") + ylab(TeX("Association rate, $k_{on} \\, (\\mu M^{-1} s^{-1})$")) +
  scale_y_continuous(trans= 'log10', breaks=trans_breaks('log10', function(x) 10^x,n = 4),
                     labels=trans_format('log10', math_format(10^.x)), limits = c(1e-1, 1e2),
                     sec.axis = sec_axis(trans=~.*1e6, name=TeX("$k_{on} \\, (M^{-1} s^{-1})$"),
                                         breaks=trans_breaks('log10', function(x) 10^x, n= 4),
                                         labels=trans_format('log10', math_format(10^.x)))) +
  scale_x_discrete(limits=c("VEGFR1", "VEGFR2", "NRP1")) +   # Add NRP1 to x-axis
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p)
ggsave(sprintf("%s/kon.png", results_path), width=4500, height=3000, units="px")
dev.off()


# Merge dataframes for plotting -------------------------------------------
vegfr1$Ligand <- "VEGFR1"
vegfr2$Ligand <- "VEGFR2"
nrp1$Ligand <- "NRP1"

df_koff = rbind(vegfr1[c("Ligand", "koff")],
                vegfr2[c("Ligand", "koff")],
                nrp1[c("Ligand", "koff")])   # Add NRP1 koff data

#koff
p_koff = ggplot() +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y = koff, colour = Reference), size = 7) +
  geom_point(data = vegfr1, aes(x = "VEGFR1", y=rm_vegfr1_koff$b), shape = 95, size=20, colour = "darkblue") +
  labs(color="VEGFR1") +
  lightness(scale_color_brewer(palette="Blues"), scalefac(0.8)) +
  guides(color = guide_legend(order=1)) +
  new_scale_color() + 
  geom_point(data = vegfr2, aes(x = "VEGFR2", y = koff, colour = Reference), size = 7) +
  geom_point(data = vegfr2, aes(x = "VEGFR2", y=rm_vegfr2_koff$b), shape = 95, size=20, colour = "darkgreen") +
  labs(color="VEGFR2") +
  lightness(scale_color_brewer(palette="Greens"),scalefac(0.8)) + 
  guides(color = guide_legend(order=2)) +
  new_scale_color() +
  geom_point(data = nrp1, aes(x = "NRP1", y = koff, colour = Reference), size = 7) +   # Add NRP1 koff plot
  geom_point(data = nrp1, aes(x = "NRP1", y=rm_nrp1_koff$b), shape = 95, size=20, colour = "darkred") +   # Add NRP1 koff plot
  labs(color="NRP1") +
  lightness(scale_color_brewer(palette="Oranges"),scalefac(0.8)) + 
  guides(color = guide_legend(order=3)) +
  xlab("") + ylab(TeX("Dissociation rate, $k_{off} \\, (s^{-1})$")) +
  scale_y_continuous(trans= 'log10', breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)), limits = c(1e-7, 1e-0)) +
  scale_x_discrete(limits=c("VEGFR1", "VEGFR2", "NRP1")) +   # Add NRP1 to x-axis
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face="bold"))

show(p_koff)
ggsave(sprintf("%s/koff.png", results_path), width=4500, height=3000, units="px")
dev.off()
