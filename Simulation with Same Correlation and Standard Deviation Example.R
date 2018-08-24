install.packages("MASS")
library(MASS)

#Number of samples
n <- 500

#Means
mean_gas <- 10
mean_power <- 12
mean_coal <- 5

#Variances
sd_gas <- 1.3
sd_power <- 1.1
sd_coal <- 1.5

#Covariances
gas_power_rho <- .47
gas_coal_rho <- .18
power_coal_rho <-.53

#Construct a covariance matrix; the diagonal are the variances
#Matrix should be in this order:
#Row 1 = Gas; Row 2 = Power; Row 3 = Coal
#Col 1 = Gas; Col 2 = Power; Col 3 = Coal

covar <- matrix(c(sd_gas, gas_power_rho, gas_coal_rho,
                  gas_power_rho, sd_power, power_coal_rho,
                  gas_coal_rho, power_coal_rho, sd_coal),
                ncol = 3, byrow = TRUE)

set.seed(1)
simulation <- mvrnorm(n, mu = c(mean_gas, mean_power, mean_coal),
                      Sigma = covar, empirical = TRUE)

#Check the covariance matrix - this verifies correlation is the same (note these are actually covariances so we would need to convert to correlation,
#but they will be the same below since SD = 1)
cov(simulation)

#Or check correlation individually
simulation_df <- data.frame(gas = simulation[,1], power = simulation[,2], coal = simulation[,3])
#Gas and Power Correlation:
cov(simulation_df$gas, simulation_df$power)

#Gas and Coal Correlation:
cov(simulation_df$gas, simulation_df$coal)

#Power and Coal Correlation:
cov(simulation_df$power, simulation_df$coal)

hist(simulation[,1], main = "Simulated Gas Prices")
hist(simulation[,2], main = "Simulated Power Prices")
hist(simulation[,3], main = "Simulated Coal Prices")