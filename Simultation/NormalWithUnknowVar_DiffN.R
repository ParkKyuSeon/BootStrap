source("Bootstrap.R")
source("CIs.R")
source("Utils.R")

# Normal Sample with unknown variance
# Set variance from unif(0.5, 1.5)
set.seed(2016)
pop_var <- runif(1, 0.5, 1.5)
pop_var

# Reset graph parameter
dev.off()
par(mfrow=c(2, 3))

# Histogram of bootstrap sample mean of 'first sample' with n=10
set.seed(1)
sample <- rnorm(10, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 2000)

CI_Exact <- Normal_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T)

Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Exact[1, 1], CI_CLT[1, 1], CI_Boot[1, 1]), 
             CI_max = c(CI_Exact[1, 2], CI_CLT[1, 2], CI_Boot[1, 2]), 
             CI_Names = c("Exact", "Approx_CLT", "Approx_Bootstrap"), 
             sample_name = "Bootstrap sample mean", 
             n = 10)


# Histogram of bootstrap sample mean of 'first sample' with n=20
set.seed(2)
sample <- rnorm(20, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 2000)

CI_Exact <- Normal_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T)

Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Exact[1, 1], CI_CLT[1, 1], CI_Boot[1, 1]), 
             CI_max = c(CI_Exact[1, 2], CI_CLT[1, 2], CI_Boot[1, 2]), 
             CI_Names = c("Exact", "Approx_CLT", "Approx_Bootstrap"), 
             sample_name = "Bootstrap sample mean", 
             n = 20)


# Histogram of bootstrap sample mean of 'first sample' with n=50
set.seed(3)
sample <- rnorm(50, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 2000)

CI_Exact <- Normal_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T)

Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Exact[1, 1], CI_CLT[1, 1], CI_Boot[1, 1]), 
             CI_max = c(CI_Exact[1, 2], CI_CLT[1, 2], CI_Boot[1, 2]), 
             CI_Names = c("Exact", "Approx_CLT", "Approx_Bootstrap"), 
             sample_name = "Bootstrap sample mean", 
             n = 50)


# Histogram of bootstrap sample mean of 'first sample' with n=100
set.seed(4)
sample <- rnorm(100, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 2000)

CI_Exact <- Normal_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T)

Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Exact[1, 1], CI_CLT[1, 1], CI_Boot[1, 1]), 
             CI_max = c(CI_Exact[1, 2], CI_CLT[1, 2], CI_Boot[1, 2]), 
             CI_Names = c("Exact", "Approx_CLT", "Approx_Bootstrap"), 
             sample_name = "Bootstrap sample mean", 
             n = 100)


# Histogram of bootstrap sample mean of 'first sample' with n=200
set.seed(5)
sample <- rnorm(200, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 2000)

CI_Exact <- Normal_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T)

Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Exact[1, 1], CI_CLT[1, 1], CI_Boot[1, 1]), 
             CI_max = c(CI_Exact[1, 2], CI_CLT[1, 2], CI_Boot[1, 2]), 
             CI_Names = c("Exact", "Approx_CLT", "Approx_Bootstrap"), 
             sample_name = "Bootstrap sample mean", 
             n = 200)


# Repeat for 1000 times
for(j in c(10, 20, 50, 100, 200)) {
  CI <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
  
  assign(paste("CI_Exact_", j, sep=""), CI)
  assign(paste("CI_CLT_", j, sep=""), CI)
  assign(paste("CI_Boot_", j, sep=""), CI)
}


for(j in 1:1000) {
  # n = 10
  set.seed(5 * j - 4)
  sample <- rnorm(10, 0, sd = sqrt(pop_var))
  CI_Exact_10 <- rbind(CI_Exact_10, Normal_Exact_CI(sample))
  CI_CLT_10 <- rbind(CI_CLT_10, Approx_CI_CLT(sample))
  CI_Boot_10 <- rbind(CI_Boot_10, Bootstrap_Implement(sample, 2000, return_CI=T))
  
  # n = 20
  set.seed(5 * j - 3)
  sample <- rnorm(20, 0, sd = sqrt(pop_var))
  CI_Exact_20 <- rbind(CI_Exact_20, Normal_Exact_CI(sample))
  CI_CLT_20 <- rbind(CI_CLT_20, Approx_CI_CLT(sample))
  CI_Boot_20 <- rbind(CI_Boot_20, Bootstrap_Implement(sample, 2000, return_CI=T))
  
  # n = 50
  set.seed(5 * j - 2)
  sample <- rnorm(50, 0, sd = sqrt(pop_var))
  CI_Exact_50 <- rbind(CI_Exact_50, Normal_Exact_CI(sample))
  CI_CLT_50 <- rbind(CI_CLT_50, Approx_CI_CLT(sample))
  CI_Boot_50 <- rbind(CI_Boot_50, Bootstrap_Implement(sample, 2000, return_CI=T))
  
  # n = 100
  set.seed(5 * j - 1)
  sample <- rnorm(100, 0, sd = sqrt(pop_var))
  CI_Exact_100 <- rbind(CI_Exact_100, Normal_Exact_CI(sample))
  CI_CLT_100 <- rbind(CI_CLT_100, Approx_CI_CLT(sample))
  CI_Boot_100 <- rbind(CI_Boot_100, Bootstrap_Implement(sample, 2000, return_CI=T))
  
  # n = 200
  set.seed(5 * j)
  sample <- rnorm(200, 0, sd = sqrt(pop_var))
  CI_Exact_200 <- rbind(CI_Exact_200, Normal_Exact_CI(sample))
  CI_CLT_200 <- rbind(CI_CLT_200, Approx_CI_CLT(sample))
  CI_Boot_200 <- rbind(CI_Boot_200, Bootstrap_Implement(sample, 2000, return_CI=T))
  if(j %% 100 == 0) {
    print(paste(j, "th iteration complete", sep=""))
  }
}

print("--------n = 10--------")
print(Param_in_CI(0, CI_Exact_10))
print(Param_in_CI(0, CI_CLT_10))
print(Param_in_CI(0, CI_Boot_10))

print("--------n = 20--------")
print(Param_in_CI(0, CI_Exact_20))
print(Param_in_CI(0, CI_CLT_20))
print(Param_in_CI(0, CI_Boot_20))

print("--------n = 50--------")
print(Param_in_CI(0, CI_Exact_50))
print(Param_in_CI(0, CI_CLT_50))
print(Param_in_CI(0, CI_Boot_50))

print("--------n = 100--------")
print(Param_in_CI(0, CI_Exact_100))
print(Param_in_CI(0, CI_CLT_100))
print(Param_in_CI(0, CI_Boot_100))

print("--------n = 200--------")
print(Param_in_CI(0, CI_Exact_200))
print(Param_in_CI(0, CI_CLT_200))
print(Param_in_CI(0, CI_Boot_200))
