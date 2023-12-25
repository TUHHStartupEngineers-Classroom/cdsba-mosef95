getwd()
random_vars <- readRDS("content/01_journal/data/random_vars.rds")
#View(random_vars)

x1_mean <- mean(random_vars$age)
x1_var <- var(random_vars$age)
x1_sd <- sd(random_vars$age)

x2_mean <- mean(random_vars$income)
x2_var <- var(random_vars$income)
x2_sd <- sd(random_vars$income)

print(x1_mean)
print(x1_var)
print(x1_sd)

print(x2_mean)
print(x2_var)
print(x2_sd)

covariance <- cov(random_vars$age, random_vars$income)
correlation <- cor(random_vars$age, random_vars$income)

cat("Covariance: ", covariance, "\n")
cat("Correlation: ", correlation, "\n")

subset_minors <- subset(random_vars, age <= 18)
subset_adults <- subset(random_vars, age >= 18 & age <= 65)
subset_seniors <- subset(random_vars, age >= 65)

mean_minors <- mean(subset_minors$income)
mean_adults <- mean(subset_adults$income)
mean_seniors <- mean(subset_seniors$income)

print(mean_minors)
print(mean_adults)
print(mean_seniors)