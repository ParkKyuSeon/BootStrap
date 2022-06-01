# Get bootstrap sample statistics and CIs
Bootstrap_Implement <- function(sample, 
                                iterations, 
                                statistics = "mean", 
                                print_CI = F, 
                                return_CI = F, 
                                method_CI = "percentile", 
                                seed_number_basenum = 0,
                                seed_number_multiplier = 1, 
                                alpha = 0.05) {
  B_statistic <- c()
  B_statistic_se <- c()
  for(i in 1:iterations) {
    set.seed(seed_number_basenum + i * seed_number_multiplier)
    B_sample <- sample(sample, length(sample), replace = T)
    B_statistic[i] <- get(statistics)(B_sample)
    if(sum(method_CI == "t") >= 1) {
      B_statistic_se[i] <- sd(B_sample) / sqrt(length(sample))
    }
  }
  
  if(print_CI == T | return_CI == T) {
    if(sum(sapply(1:length(method_CI), function(x) sum(method_CI[x] == c("percentile", "t", "BCa")))) <
       length(method_CI)) {
      warning("method_CI argument should be the vector only consists 'percentile', 't', or 'BCa'.")
    }
    
    if(sum(method_CI == "t") >= 1 & statistics != "mean") {
      warning("studentized-t method in this function can be only used when statistics is mean.")
    }
    
    CIs <- data.frame(CI_min = 0, CI_max = 0)[-1, ]
    
    for(i in 1:length(method_CI)) {
      if(method_CI[i] == "percentile") {
        CI_min <- B_statistic[order(B_statistic)][max(1, floor(alpha / 2 * iterations))]
        CI_max <- B_statistic[order(B_statistic)][min(iterations, iterations + 1 - floor(alpha / 2 * iterations))]
      } 
      else if(method_CI[i] == "t") {
        boot_pivot <- (B_statistic - get(statistics)(sample)) / B_statistic_se
        CI_min <- get(statistics)(sample) - 
          boot_pivot[order(boot_pivot)][iterations + 1 - floor((iterations + 1) * (alpha / 2))] * 
          (sd(sample) / sqrt(length(sample)))
        CI_max <- get(statistics)(sample) - 
          boot_pivot[order(boot_pivot)][floor((iterations + 1) * (alpha / 2))] *
          (sd(sample) / sqrt(length(sample)))
      }
      else if(method_CI[i] == "BCa") {
        z_0 <- qnorm(sum(B_statistic < get(statistics)(sample)) / iterations)
        a <- sum((mean(B_statistic) - B_statistic)^3) / 
          (6 * ((sum((mean(B_statistic) - B_statistic)^2)) ^ (3/2)))
        alpha_1 <- pnorm(z_0 + (z_0 + qnorm(alpha / 2)) / (1 - a * (z_0 + qnorm(alpha / 2))))
        alpha_2 <- 1 - pnorm(z_0 + (z_0 + qnorm(1 - alpha / 2)) / (1 - a * (z_0 + qnorm(1 - alpha / 2))))
        CI_min <- B_statistic[order(B_statistic)][max(1, floor(alpha_1 * iterations))]
        CI_max <- B_statistic[order(B_statistic)][min(iterations, iterations + 1 - floor(alpha_2 * iterations))]
      }
      CIs <- rbind(CIs, data.frame(CI_min = CI_min, CI_max = CI_max))
      if(print_CI == T) {
        print(paste("Approximate ", (1 - alpha) * 100, "% CI of statistics(By Bootstrap and method ", 
        method_CI[i], ") is [", round(CI_min, 4), ", ", round(CI_max, 4), "]", sep=""))
      }
    }
    rownames(CIs) <- method_CI
  }
  
  if(return_CI == T) {
    return(CIs)
  } else {
    return(B_statistic)
  }
}