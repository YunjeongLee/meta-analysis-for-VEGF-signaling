forest_ylee <- function (data, rm, title=NULL, slab, xlim, alim, unit, xlab, cex=2, atransf=NULL, numDigits) {
  ### Specify colors
  linecolor = "#1e81b0"
  
  ### total number of studies
  k <- nrow(data)
  
  ### generate point sizes
  psize <- weights(rm)
  psize <- 0.8 + (psize - min(psize)) / (max(psize) - min(psize))
  
  ### get the weights and format them as will be used in the forest plot
  weights <- formatC(weights(rm), format="f", digits=1)
  
  ### adjust the margins
  par(mar=c(10,2,3,2), lty=0, mgp=c(4,1,0))
  
  ### set ilab position
  ilab_pos = 1.2*alim[2]
  
  ### Get prediction interval
  pred <- predict(rm)
  
  ### forest plot with extra annotations
  sav <- forest(rm, slab=slab, digits=numDigits,
                header=c("Author(s) and Year", ""), 
                xlim=xlim, alim=alim, cex=cex, atransf=atransf,
                ylim=c(-2, length(rm$yi) + 3),
                ilab=weights, ilab.xpos=ilab_pos, ilab.pos=2,
                xlab=xlab, mlab="Random-effects model", refline=NA, pch=18, psize=psize,
                colout=linecolor, col=linecolor, border=linecolor, lwd=4)
  
  ### add vertical reference line at the pooled estimate
  segments(coef(rm), -1, coef(rm), k, col='black', lty="dashed", lwd=2)
  
  ### now we add a bunch of text; since some of the text falls outside of the
  ### plot region, we set xpd=NA so nothing gets clipped
  par(xpd=NA)
  
  ### adjust cex as used in the forest plot and use a bold font
  par(cex=sav$cex, font=2)
  
  ### add headers
  text(ilab_pos, k+2, pos=2, "Weight \n(%)")
  text(xlim[2], k+2, pos=2, paste0("Measurements \n(", unit, ") [95% CI]"))
  
  ### add title
  text(mean(xlim), k+3, pos=3, title)
  
  ### adjust cex as used in the forest plot and use a bold font
  par(cex=sav$cex, font=1)
  
  ### add text with heterogeneity statistics
  if (is.null(atransf))
    mtext(side=1, TeX(paste("Test for heterogeneity: ", "$\\tau^2$", "=",
                            formatC(rm$tau2, digits=2, format="f"), "; ", "$\\chi^2$", "=",
                            formatC(rm$QE, digits=2, format="f"), ", df=", rm$k - rm$p,
                            ", ", generate_plabel(rm$QEp), "; ", "$I^2$", "=",
                            formatC(rm$I2, digits=0, format="f"), "%")),
          padj=1, adj=0, cex=cex*0.9, line=5)
  else
    mtext(side=1, TeX(paste("Test for heterogeneity: ", "$\\tau^2$", "=",
                            formatC(atransf(sqrt(rm$tau2))^2, digits=2, format="f"), "; ", "$\\chi^2$", "=",
                            formatC(rm$QE, digits=2, format="f"), ", df=", rm$k - rm$p,
                            ", ", generate_plabel(rm$QEp), "; ", "$I^2$", "=",
                            formatC(rm$I2, digits=0, format="f"), "%")),
          padj=1, adj=0, cex=cex*0.9, line=5)
}