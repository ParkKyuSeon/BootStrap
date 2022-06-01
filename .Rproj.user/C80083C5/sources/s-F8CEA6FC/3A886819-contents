source("Bootstrap.R")
source("CIs.R")
source("Utils.R")

# Exponential Sample with unknown mean
set.seed(2016)
lambda <- runif(1, 0.5, 1)
lambda

# Reset graph parameter
dev.off()
par(mfrow=c(2, 2))

# Histogram of bootstrap sample mean of 'first sample' with n=10
set.seed(1)
sample <- rexp(10, lambda)
bootstrap_sample <- Bootstrap_Implement(sample, 2000)

CI_Exact <- Exp_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T, method_CI = c("percentile", "t", "BCa"))

print("-------Exact CI with n=10-------")
print(CI_Exact)
print("---Approx CI by CLT with n=10---")
print(CI_CLT)
print("-----Bootstrap CI with n=10-----")
print(CI_Boot)

Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Exact[1, 1], CI_Boot[1, 1], CI_Boot[2, 1], CI_Boot[3, 1]), 
             CI_max = c(CI_Exact[1, 2], CI_Boot[1, 2], CI_Boot[2, 2], CI_Boot[3, 2]), 
             CI_Names = c("Exact", "Approx_Bootstrap_p", "Approx_Bootstrap_t", "Approx_Bootstrap_BCa"), 
             sample_name = "Bootstrap sample mean", 
             n = 10)


# Histogram of bootstrap sample mean of 'first sample' with n=50
set.seed(2)
sample <- rexp(50, lambda)
bootstrap_sample <- Bootstrap_Implement(sample, 2000)

CI_Exact <- Exp_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T, method_CI = c("percentile", "t", "BCa"))

Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Exact[1, 1], CI_Boot[1, 1], CI_Boot[2, 1], CI_Boot[3, 1]), 
             CI_max = c(CI_Exact[1, 2], CI_Boot[1, 2], CI_Boot[2, 2], CI_Boot[3, 2]), 
             CI_Names = c("Exact", "Approx_Bootstrap_p", "Approx_Bootstrap_t", "Approx_Bootstrap_BCa"), 
             sample_name = "Bootstrap sample mean", 
             n = 50)

print("-------Exact CI with n=50-------")
print(CI_Exact)
print("---Approx CI by CLT with n=50---")
print(CI_CLT)
print("-----Bootstrap CI with n=50-----")
print(CI_Boot)


# Histogram of bootstrap sample mean of 'first sample' with n=100
set.seed(3)
sample <- rexp(100, lambda)
bootstrap_sample <- Bootstrap_Implement(sample, 2000)

CI_Exact <- Exp_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T, method_CI = c("percentile", "t", "BCa"))

Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Exact[1, 1], CI_Boot[1, 1], CI_Boot[2, 1], CI_Boot[3, 1]), 
             CI_max = c(CI_Exact[1, 2], CI_Boot[1, 2], CI_Boot[2, 2], CI_Boot[3, 2]), 
             CI_Names = c("Exact", "Approx_Bootstrap_p", "Approx_Bootstrap_t", "Approx_Bootstrap_BCa"), 
             sample_name = "Bootstrap sample mean", 
             n = 100)

print("-------Exact CI with n=100-------")
print(CI_Exact)
print("---Approx CI by CLT with n=100---")
print(CI_CLT)
print("-----Bootstrap CI with n=100-----")
print(CI_Boot)


# Histogram of bootstrap sample mean of 'first sample' with n=500
set.seed(4)
sample <- rexp(500, lambda)
bootstrap_sample <- Bootstrap_Implement(sample, 2000)

CI_Exact <- Exp_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- Bootstrap_Implement(sample, 2000, return_CI=T, method_CI = c("percentile", "t", "BCa"))

Hist_With_CI(bootstrap_sample, 
             CI_min = c(CI_Exact[1, 1], CI_Boot[1, 1], CI_Boot[2, 1], CI_Boot[3, 1]), 
             CI_max = c(CI_Exact[1, 2], CI_Boot[1, 2], CI_Boot[2, 2], CI_Boot[3, 2]), 
             CI_Names = c("Exact", "Approx_Bootstrap_p", "Approx_Bootstrap_t", "Approx_Bootstrap_BCa"), 
             sample_name = "Bootstrap sample mean", 
             n = 500)

print("-------Exact CI with n=500-------")
print(CI_Exact)
print("---Approx CI by CLT with n=500---")
print(CI_CLT)
print("-----Bootstrap CI with n=500-----")
print(CI_Boot)


# Repeat for 500 times
for(j in c(10, 50, 100, 500)) {
  CI <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
  
  assign(paste("CI_Exact_", j, sep=""), CI)
  assign(paste("CI_CLT_", j, sep=""), CI)
  assign(paste("CI_Boot_", j, sep=""), CI)
}


for(j in 1:1000) {
  # n = 10
  set.seed(4 * j - 3)
  sample <- rexp(10, lambda)
  CI_Exact_10 <- rbind(CI_Exact_10, Exp_Exact_CI(sample))
  CI_CLT_10 <- rbind(CI_CLT_10, Approx_CI_CLT(sample))
  CI_Boot_10 <- rbind(CI_Boot_10, 
                      Bootstrap_Implement(sample, 2000, return_CI=T, method_CI = c("percentile", "t", "BCa")))
  
  # n = 50
  set.seed(4 * j - 2)
  sample <- rexp(50, lambda)
  CI_Exact_50 <- rbind(CI_Exact_50, Exp_Exact_CI(sample))
  CI_CLT_50 <- rbind(CI_CLT_50, Approx_CI_CLT(sample))
  CI_Boot_50 <- rbind(CI_Boot_50, 
                      Bootstrap_Implement(sample, 2000, return_CI=T, method_CI = c("percentile", "t", "BCa")))
  
  # n = 100
  set.seed(4 * j - 1)
  sample <- rexp(100, lambda)
  CI_Exact_100 <- rbind(CI_Exact_100, Exp_Exact_CI(sample))
  CI_CLT_100 <- rbind(CI_CLT_100, Approx_CI_CLT(sample))
  CI_Boot_100 <- rbind(CI_Boot_100, 
                      Bootstrap_Implement(sample, 2000, return_CI=T, method_CI = c("percentile", "t", "BCa")))
  
  # n = 500
  set.seed(4 * j)
  sample <- rexp(500, lambda)
  CI_Exact_500 <- rbind(CI_Exact_500, Exp_Exact_CI(sample))
  CI_CLT_500 <- rbind(CI_CLT_500, Approx_CI_CLT(sample))
  CI_Boot_500 <- rbind(CI_Boot_500, 
                       Bootstrap_Implement(sample, 2000, return_CI=T, method_CI = c("percentile", "t", "BCa")))
  if(j %% 100 == 0) {
    print(paste(j, "th iteration complete", sep=""))
  }
}


print("--------n = 10--------")
print(Param_in_CI(1 / lambda, CI_Exact_10))
print(Param_in_CI(1 / lambda, CI_CLT_10))
print(Param_in_CI(1 / lambda, CI_Boot_10[1:nrow(CI_Boot_10) %% 3 == 1, ]))
print(Param_in_CI(1 / lambda, CI_Boot_10[1:nrow(CI_Boot_10) %% 3 == 2, ]))
print(Param_in_CI(1 / lambda, CI_Boot_10[1:nrow(CI_Boot_10) %% 3 == 0, ]))

print("--------n = 50--------")
print(Param_in_CI(1 / lambda, CI_Exact_50))
print(Param_in_CI(1 / lambda, CI_CLT_50))
print(Param_in_CI(1 / lambda, CI_Boot_50[1:nrow(CI_Boot_50) %% 3 == 1, ]))
print(Param_in_CI(1 / lambda, CI_Boot_50[1:nrow(CI_Boot_50) %% 3 == 2, ]))
print(Param_in_CI(1 / lambda, CI_Boot_50[1:nrow(CI_Boot_50) %% 3 == 0, ]))

print("--------n = 100--------")
print(Param_in_CI(1 / lambda, CI_Exact_100))
print(Param_in_CI(1 / lambda, CI_CLT_100))
print(Param_in_CI(1 / lambda, CI_Boot_100[1:nrow(CI_Boot_100) %% 3 == 1, ]))
print(Param_in_CI(1 / lambda, CI_Boot_100[1:nrow(CI_Boot_100) %% 3 == 2, ]))
print(Param_in_CI(1 / lambda, CI_Boot_100[1:nrow(CI_Boot_100) %% 3 == 0, ]))

print("--------n = 500--------")
print(Param_in_CI(1 / lambda, CI_Exact_500))
print(Param_in_CI(1 / lambda, CI_CLT_500))
print(Param_in_CI(1 / lambda, CI_Boot_500[1:nrow(CI_Boot_500) %% 3 == 1, ]))
print(Param_in_CI(1 / lambda, CI_Boot_500[1:nrow(CI_Boot_500) %% 3 == 2, ]))
print(Param_in_CI(1 / lambda, CI_Boot_500[1:nrow(CI_Boot_500) %% 3 == 0, ]))