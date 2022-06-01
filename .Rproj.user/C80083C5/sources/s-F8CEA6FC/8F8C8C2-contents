# Miss_Left : the case when the parameter is smaller than the left endpoint of CI.
# Miss_Right : the case when the parameter is larger than the right endpoint of CI.
Param_in_CI <- function(parameter, 
                        CI_List, 
                        ratio = T) {
  Cases <- rowSums(sapply(1:nrow(CI_List), function(x) c(CI_List[x, 1] <= parameter & CI_List[x, 2] >= parameter, 
                                                         CI_List[x, 1] > parameter, CI_List[x, 2] < parameter))) /
           if(ratio==T) nrow(CI_List) else 1
  return(data.frame(Exact = Cases[1], Miss_Left = Cases[2], Miss_Right = Cases[3]))
}

Hist_With_CI <- function(sample, 
                         CI_min = c(), 
                         CI_max = c(), 
                         CI_Names = c(), 
                         sample_name = "sample", 
                         n = 0, 
                         print_n_and_B = T, 
                         legend_x = "topright") {
  if(length(CI_min) != length(CI_max) | 
     length(CI_max) != length(CI_Names) | 
     length(CI_min) != length(CI_Names)) {
    warning("Length of the confidence interval vector is not same")
  }
  
  if(print_n_and_B == T) {
    hist(sample, breaks = min(30, round(length(sample) / 5)), border = F, col="skyblue", 
         main = paste("Histogram of ", sample_name, "\n", "n = ", n, " B = ", length(sample), sep=""))
  } else {
    hist(sample, breaks = min(30, round(length(sample) / 5)), border = F, col="skyblue", 
         main = paste("Histogram of ", sample_name, sep=""))
  }
  
  if(length(CI_min) > 0) {
    for(i in 1:length(CI_min)) {
      abline(v = CI_min[i], lty=i)
      abline(v = CI_max[i], lty=i)
    }
    legend(x = legend_x, legend = CI_Names, lty = 1:length(CI_min), cex=0.8)
  }
}