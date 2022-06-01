source("Bootstrap.R")
source("CIs.R")
source("Utils.R")

# Normal Sample with unknown variance
# Set variance from unif(0.5, 1.5)
set.seed(2016)
pop_var <- runif(1, 0.5, 1.5)
pop_var

dev.off()
par(mfrow=c(1, 3))


# Histogram of bootstrap sample median of 'first sample'
# B=2000 with n=11
set.seed(1)
sample <- rnorm(11, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 2000, statistics="median")

CI_LD <- Normal_Median_CI(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T, statistics="median")
CIs <- rbind(CI_LD, CI_Boot)
rownames(CIs) <- c("Approx_LimitDist", "Approx_Bootstrap")
print(CIs)

Hist_With_CI(bootstrap_sample, 
             CI_min = CIs[, 1], 
             CI_max = CIs[, 2], 
             CI_Names = c("Approx_LimitDist", "Approx_Bootstrap"), 
             sample_name = "Bootstrap sample median from N(0, simga^2)", 
             n = 11)


# B=2000 with n=51
set.seed(2)
sample <- rnorm(51, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 2000, statistics="median")

CI_LD <- Normal_Median_CI(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T, statistics="median")
CIs <- rbind(CI_LD, CI_Boot)
rownames(CIs) <- c("Approx_LimitDist", "Approx_Bootstrap")
print(CIs)

Hist_With_CI(bootstrap_sample, 
             CI_min = CIs[, 1], 
             CI_max = CIs[, 2], 
             CI_Names = c("Approx_LimitDist", "Approx_Bootstrap"), 
             sample_name = "Bootstrap sample median from N(0, simga^2)", 
             n = 51)


# B=2000 with n=501
set.seed(3)
sample <- rnorm(501, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 2000, statistics="median")

CI_LD <- Normal_Median_CI(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T, statistics="median")
CIs <- rbind(CI_LD, CI_Boot)
rownames(CIs) <- c("Approx_LimitDist", "Approx_Bootstrap")
print(CIs)

Hist_With_CI(bootstrap_sample, 
             CI_min = CIs[, 1], 
             CI_max = CIs[, 2], 
             CI_Names = c("Approx_LimitDist", "Approx_Bootstrap"), 
             sample_name = "Bootstrap sample median from N(0, simga^2)", 
             n = 501)
