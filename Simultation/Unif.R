source("Bootstrap.R")
source("CIs.R")
source("Utils.R")

# Normal Sample with unknown variance
# Set variance from unif(0.5, 1.5)
set.seed(2016)
Unif_max <- runif(1, 0.5, 1.5)
Unif_max

# Histogram of bootstrap sample mean of 'first sample'
set.seed(1)
sample <- runif(50, 0, Unif_max)
bootstrap_sample <- Bootstrap_Implement(sample, 2000, statistics="max")

CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T, statistics="max")

print("-Approximate CI from Bootstrap-")
print(CI_Boot)

dev.off()
Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Boot[1, 1]), 
             CI_max = c(CI_Boot[1, 2]), 
             CI_Names = c("Approx_Bootstrap"), 
             sample_name = "Bootstrap sample median from Unif(0, theta)", 
             n = 50, 
             legend_x = "topleft")

print(CI_Boot * (51 / 50))

# Repeat for 1000 times
CI_Boot_50 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_200 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]

for(j in 1:1000) {
  set.seed(2 * j - 1)
  sample <- runif(50, 0, Unif_max)
  CI_Boot_50 <- rbind(CI_Boot_50, Bootstrap_Implement(sample, 2000, return_CI=T, statistics="max"))
  
  set.seed(2 * j)
  sample <- runif(200, 0, Unif_max)
  CI_Boot_200 <- rbind(CI_Boot_200, Bootstrap_Implement(sample, 2000, return_CI=T, statistics="max"))
  if(j %% 100 == 0) {
    print(paste(j, "th iteration complete", sep=""))
  }
}

print(Param_in_CI(Unif_max, CI_Boot_50 * (51 / 50)))
print(Param_in_CI(Unif_max, CI_Boot_200 * (201 / 200)))