forest_ylee <- function (data=data, rm = rm, slab=slab, xlim=xlim, alim=alim, 
                         xlab=xlab, cex=2) {
  ### Specify colors
  linecolor = "#1e81b0"
  
  ### total number of studies
  k <- nrow(data)
  
  ### generate point sizes
  psize <- weights(rm)
  psize <- 1.2 + (psize - min(psize)) / (max(psize) - min(psize))
  
  ### adjust the margins
  par(mar=c(10,2,1,2))
  
  ### forest plot with extra annotations
  sav <- forest(rm, slab=slab, 
                header=c("Author(s) and Year","Weight (%) and Measurements \n[95% CI]"), 
                xlim=xlim, alim=alim, cex=cex, showweights = TRUE,
                xlab=xlab, refline=NA, pch=18, psize=psize,
                colout=linecolor, col=linecolor, border=linecolor, lwd=4)
  
  ### add vertical reference line at the pooled estimate
  segments(coef(rm), -1, coef(rm), k, col='black', lty="dashed", lwd=2)
  
  ### now we add a bunch of text; since some of the text falls outside of the
  ### plot region, we set xpd=NA so nothing gets clipped
  par(xpd=NA)
  
  ### adjust cex as used in the forest plot and use a bold font
  par(cex=sav$cex, font=1)

  ### add text with heterogeneity statistics
  mtext(side=1, TeX(paste("Test for heterogeneity: ", "$\\tau^2$", "=",
                      formatC(rm$tau2, digits=2, format="f"), "; ", "$\\chi^2$", "=",
                      formatC(rm$QE, digits=2, format="f"), ", df=", rm$k - rm$p,
                      ", ", generate_plabel(rm$QEp), "; ", "$I^2$", "=",
                      formatC(rm$I2, digits=0, format="f"), "%")),
        padj=1, adj=0, cex=cex*0.9, line=5)
}