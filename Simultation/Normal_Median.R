source("Bootstrap.R")
source("CIs.R")
source("Utils.R")

# Normal Sample with unknown variance
# Set variance from unif(0.5, 1.5)
set.seed(2016)
pop_var <- runif(1, 0.5, 1.5)
pop_var

# Histogram of bootstrap sample mean of 'first sample'
set.seed(1)
sample <- rnorm(51, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 2000, statistics="median")

CI_LD <- Normal_Median_CI(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T, statistics="median")

print("----Approximate CI from LD-----")
print(CI_LD)
print("-Approximate CI from Bootstrap-")
print(CI_Boot)

dev.off()
Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_LD[1, 1], CI_Boot[1, 1]), 
             CI_max = c(CI_LD[1, 2], CI_Boot[1, 2]), 
             CI_Names = c("Approx_LimitDist", "Approx_Bootstrap"), 
             sample_name = "Bootstrap sample median from N(0, simga^2)", 
             n = 51)


# Repeat for 1000 times
CI_LD <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot <- data.frame(min_CI = NA, max_CI = NA) [-1, ]

for(j in 1:1000) {
  set.seed(j)
  sample <- rnorm(51, 0, sd = sqrt(pop_var))
  CI_LD <- rbind(CI_LD, Normal_Median_CI(sample))
  CI_Boot <- rbind(CI_Boot, Bootstrap_Implement(sample, 2000, return_CI=T, statistics="median"))
  if(j %% 100 == 0) {
    print(paste(j, "th iteration complete", sep=""))
  }
}

print(Param_in_CI(0, CI_LD))
print(Param_in_CI(0, CI_Boot))