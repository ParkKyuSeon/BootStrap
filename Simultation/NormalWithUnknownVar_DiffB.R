# Normal Sample with unknown variance
# Set variance from unif(0.5, 1.5)
set.seed(2016)
pop_var <- runif(1, 0.5, 1.5)
pop_var

dev.off()
par(mfrow=c(1, 2))

# Histogram of bootstrap sample mean of 'first sample'
# B=100 with 'enough' n=50
set.seed(5001)
sample <- rnorm(50, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 100)

CI_Exact <- Normal_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
for(i in 1:5) {
  CI_Boot <- rbind(CI_Boot, Bootstrap_Implement(sample, 100, return_CI=T, seed_number_basenum=(i - 1) * 1000))
}
CIs <- rbind(CI_Exact, CI_CLT, CI_Boot)
rownames(CIs) <- c("Exact", "Approx_CLT", 
                   "Approx_Boot_1", "Approx_Boot_2", "Approx_Boot_3", "Approx_Boot_4", "Approx_Boot_5")
print(CIs)

Hist_With_CI(bootstrap_sample, 
             CI_min = CIs[-2, 1], 
             CI_max = CIs[-2, 2], 
             CI_Names = c("Exact", 
                          "Approx_Boot_1", "Approx_Boot_2", "Approx_Boot_3", "Approx_Boot_4", "Approx_Boot_5"), 
             sample_name = "Bootstrap sample mean from N(0, simga^2)", 
             n = 50)


# B=1000 with 'enough' n=50
set.seed(5001)
sample <- rnorm(50, 0, sd = sqrt(pop_var))
bootstrap_sample <- Bootstrap_Implement(sample, 1000)

CI_Exact <- Normal_Exact_CI(sample)
CI_CLT <- Approx_CI_CLT(sample)
CI_Boot <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
for(i in 1:5) {
  CI_Boot <- rbind(CI_Boot, Bootstrap_Implement(sample, 1000, return_CI=T, seed_number_basenum=(i - 1) * 1000))
}
CIs <- rbind(CI_Exact, CI_CLT, CI_Boot)
rownames(CIs) <- c("Exact", "Approx_CLT", 
                   "Approx_Boot_1", "Approx_Boot_2", "Approx_Boot_3", "Approx_Boot_4", "Approx_Boot_5")
print(CIs)

Hist_With_CI(bootstrap_sample, 
             CI_min = CIs[-2, 1], 
             CI_max = CIs[-2, 2], 
             CI_Names = c("Exact", 
                          "Approx_Boot_1", "Approx_Boot_2", "Approx_Boot_3", "Approx_Boot_4", "Approx_Boot_5"), 
             sample_name = "Bootstrap sample mean from N(0, simga^2)", 
             n = 50)


# Repeat for 1000 times
CI_Exact <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_CLT <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_100 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_200 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_500 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_1000 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_2000 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]

for(j in 1:1000) {
  set.seed(5000 + j)
  sample <- rnorm(50, 0, sd = sqrt(pop_var))
  CI_Exact <- rbind(CI_Exact, Normal_Exact_CI(sample))
  CI_CLT <- rbind(CI_CLT, Approx_CI_CLT(sample))
  CI_Boot_100 <- rbind(CI_Boot_100, Bootstrap_Implement(sample, 100, return_CI=T))
  CI_Boot_200 <- rbind(CI_Boot_200, Bootstrap_Implement(sample, 200, return_CI=T, seed_number_basenum = 1000))
  CI_Boot_500 <- rbind(CI_Boot_500, Bootstrap_Implement(sample, 500, return_CI=T, seed_number_basenum = 2000))
  CI_Boot_1000 <- rbind(CI_Boot_1000, Bootstrap_Implement(sample, 1000, return_CI=T, seed_number_basenum = 3000))
  CI_Boot_2000 <- rbind(CI_Boot_2000, Bootstrap_Implement(sample, 2000, return_CI=T, seed_number_basenum = 4000))
  if(j %% 100 == 0) {
    print(paste(j, "th iteration complete", sep=""))
  }
}

print("---result with different samples---")
print(Param_in_CI(0, CI_Exact))
print(Param_in_CI(0, CI_CLT))
print(Param_in_CI(0, CI_Boot_100))
print(Param_in_CI(0, CI_Boot_200))
print(Param_in_CI(0, CI_Boot_500))
print(Param_in_CI(0, CI_Boot_1000))
print(Param_in_CI(0, CI_Boot_2000))


# Repeat getting bootstrap CI by same sample for 1000 times
CI_Exact <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_100 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_200 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_500 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_1000 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]
CI_Boot_2000 <- data.frame(min_CI = NA, max_CI = NA) [-1, ]

set.seed(7)
sample <- rnorm(50, 0, sd = sqrt(pop_var))
dev.off()
hist(sample, breaks = 20, border = F, col="skyblue", 
     main = paste("Histogram of sample with large sampling error(mean = ", round(mean(sample), 3), ")", sep=""))
abline(v = Normal_Exact_CI(sample)$CI_max, lty=2)
abline(v = Normal_Exact_CI(sample)$CI_min, lty=2)
print(Normal_Exact_CI(sample))

for(j in 1:1000) {
  CI_Exact <- rbind(CI_Exact, Normal_Exact_CI(sample))
  CI_Boot_100 <- rbind(CI_Boot_100, Bootstrap_Implement(sample, 100, return_CI=T, seed_number_basenum = (j-1) * 1000))
  CI_Boot_200 <- rbind(CI_Boot_200, Bootstrap_Implement(sample, 200, return_CI=T, seed_number_basenum = 1000000 + (j-1) * 1000))
  CI_Boot_500 <- rbind(CI_Boot_500, Bootstrap_Implement(sample, 500, return_CI=T, seed_number_basenum = 2000000 + (j-1) * 1000))
  CI_Boot_1000 <- rbind(CI_Boot_1000, Bootstrap_Implement(sample, 1000, return_CI=T, seed_number_basenum = 3000000 + (j-1) * 1000))
  CI_Boot_2000 <- rbind(CI_Boot_2000, Bootstrap_Implement(sample, 2000, return_CI=T, seed_number_basenum = 4000000 + (j-1) * 2000))
  if(j %% 100 == 0) {
    print(paste(j, "th iteration complete", sep=""))
  }
}

print("---result with one samples---")
print(Param_in_CI(0, CI_Exact))
print(Param_in_CI(0, CI_Boot_100))
print(Param_in_CI(0, CI_Boot_200))
print(Param_in_CI(0, CI_Boot_500))
print(Param_in_CI(0, CI_Boot_1000))
print(Param_in_CI(0, CI_Boot_2000))

min(CI_Boot_100[, 1]) ; max(CI_Boot_100[, 1])
min(CI_Boot_100[, 2]) ; max(CI_Boot_100[, 2])

min(CI_Boot_1000[, 1]) ; max(CI_Boot_1000[, 1])
min(CI_Boot_1000[, 2]) ; max(CI_Boot_1000[, 2])