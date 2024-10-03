anova_posthoc <- function (rma_list, tissue_list, filename) {
  # Generate a fake dataset -------------------------------------------------
  data <- list()
  for (i in 1:length(rma_list)) {
    rma = rma_list[[i]]
    # Generate a fake data using mean, standard error, and the number of data
    data_tissue = rnorm_fixed(mean=rma$beta[[1]], se=rma$se, n=rma$k)
    # Check which tissue is examined
    data[[i]] = cbind(i, data_tissue)
  }
  df = do.call(rbind.data.frame, data)
  colnames(df) <- c("group", "value")
  
  # Perform ANOVA -----------------------------------------------------------
  anova_result = oneway.test(value ~ group, data = df)
  
  
  # Perform Dunnett's T3 test -----------------------------------------------
  dt3_result = dunnettT3Test(value ~ group, data = df)
  
  # Plot Dunnett's T3 test --------------------------------------------------
  # Generate a matrix with rownames and columnnames
  df = dt3_result$p.value
  row.names(df) <- tissue_list[-1]
  colnames(df) <- head(tissue_list, -1)
  # Reverse the order of row
  df = df[nrow(df):1, ]
  gt <- pheatmap(df, breaks=c(0, 0.001, 0.01, 0.05, 1), col=rev(brewer.blues(4)), 
                 angle_col=0, fontsize=25, cluster_rows=FALSE,
                 treeheight_row=FALSE, treeheight_col=FALSE,
                 na_col = "lightgray", border_color = "black",
                 display_numbers=TRUE, number_format='%.3f', number_color='black',
                 legend=FALSE)
  ggsave(filename, plot=gt, width=2000, height=1500, units="px")
  
  return(list(anova_result, dt3_result)) 
}

rnorm_fixed <- function(mean, se, n) {
  sd = se * sqrt(n)
  as.vector(mean + sd * scale(rnorm(n)))
}