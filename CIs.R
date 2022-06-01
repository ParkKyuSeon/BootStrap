# Normal Exact Confidence Interval with t-dist
Normal_Exact_CI <- function(sample, 
                            print_CI = F, 
                            alpha = 0.05) {
  exact_CI_min <- mean(sample) - qt(alpha / 2, length(sample) - 1, lower.tail=F) * sd(sample) / sqrt(length(sample))
  exact_CI_max <- mean(sample) + qt(alpha / 2, length(sample) - 1, lower.tail=F) * sd(sample) / sqrt(length(sample))
  
  if(print_CI == T) {
    print(paste("Exact ", (1 - alpha) * 100, "% CI of statistics is [", 
                round(exact_CI_min, 4), ", ", round(exact_CI_max, 4), "]", sep=""))
  }
  
  return(data.frame(CI_min = exact_CI_min, CI_max = exact_CI_max))
}


# Exponential Exact Confidence Interval 
Exp_Exact_CI <- function(sample, 
                         print_CI = F, 
                         alpha = 0.05) {
  exact_CI_min <- (2 * sum(sample)) / qchisq(1 - alpha / 2, 2 * length(sample))
  exact_CI_max <- (2 * sum(sample)) / qchisq(alpha / 2, 2 * length(sample))
  if(print_CI == T) {
    print(paste("Exact ", (1 - alpha) * 100, "% CI of statistics is [", 
                round(exact_CI_min, 4), ", ", round(exact_CI_max, 4), "]", sep=""))
  }
  
  return(data.frame(CI_min = exact_CI_min, CI_max = exact_CI_max))
}


# Approximate Confidence Interval with CLT
Approx_CI_CLT <- function(sample, 
                          print_CI = F, 
                          alpha = 0.05) {
  Approx_CI_min <- mean(sample) - qnorm(alpha / 2, lower.tail=F) * sd(sample) / sqrt(length(sample))
  Approx_CI_max <- mean(sample) + qnorm(alpha / 2, lower.tail=F) * sd(sample) / sqrt(length(sample))
  
  if(print_CI == T) {
    print(paste("Approximate ", (1 - alpha) * 100, "% CI of statistics(By CLT) is [", 
                round(Approx_CI_min, 4), ", ", round(Approx_CI_max, 4), "]", sep=""))
  }
  
  return(data.frame(CI_min = Approx_CI_min, CI_max = Approx_CI_max))
}


# Approximate Confidence Interval of median form Normal sample
Normal_Median_CI <- function(sample, 
                             print_CI = F, 
                             alpha = 0.05) {
  if(length(sample) %% 2 != 1) {
    warning("Sample size need to be odd number")
  }
  
  Approx_CI_min <- median(sample) - qnorm(alpha / 2, lower.tail=F) * sqrt(pi) * sd(sample) / sqrt(2 * length(sample))
  Approx_CI_max <- median(sample) + qnorm(alpha / 2, lower.tail=F) * sqrt(pi) * sd(sample) / sqrt(2 * length(sample))
  
  if(print_CI == T) {
    print(paste("Approximate ", (1 - alpha) * 100, "% CI of statistics is [", 
                round(Approx_CI_min, 4), ", ", round(Approx_CI_max, 4), "]", sep=""))
  }
  
  return(data.frame(CI_min = Approx_CI_min, CI_max = Approx_CI_max))
}