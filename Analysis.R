rm(list = ls())
setwd("~/Desktop/NYU Academic/Statistics & Econometrics 1/Project/Data")
getwd()
library(tidyverse)
library(dplyr)
library(plm)
library(lmtest)
library(ggplot2)
library(stargazer)

# Import wrangled data file from previous code file, ready for use in model
data <- read_csv("data1.csv")

#Count missing observations, to estimate how unbalanced the panel is.
data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
# There are 29 missing observations across our core data.

# Create new variables for use below. We take logs and quadratic of our input data.
data$exports <- data$fuel + data$ore
data$lnco2 <- log(data$co2)
data$lninc <- log(data$income)
data$lnincsq <- (log(data$income))^2
data$lnexp <- log(data$exports)
data$lnpop <- log(data$popdens)
data$lnland <- log(data$land)

# Convert data frame to panel version for use with plm package
panel <- pdata.frame(data)
# Confirm head of panel data frame has imported as expected
head(panel)
head(attr(panel, "index"))
summary(panel$income)
View(panel)
# We inspect for missing data. We suppose that unbalancedness will not come from attrition bias, since omission seems random.
# Different countries, time periods, and variables are missing.

##### We run test for our basic model (only regressing on income and income^2) here.

# First, run a pooled OLS model, for comparison with fixed effects.
ols <- lm(lnco2~lninc+lnincsq, data = panel)
# Fit an FE basic model
kc.fe1 <- plm(lnco2~lninc+lnincsq, data = panel, model = "within")
summary(kc.fe1)
# Both coefficients are significant, so we proceed to refine the model.

# Check if OLS better than FE
pooltest(lnco2~lninc+lnincsq, data=panel,model = "pooling")
# Since p<0.05, FE is a better choice than Pooled OLS

# Fit a Random Effects model to verify if appropriate.
kc.re1 <- plm(lnco2~lninc+lnincsq, data = panel, model = "random")
# Hausman Test for RE vs FE.
phtest(kc.fe1, kc.re1)
# Shows that FE is the more suitable model, since p < 0.05.

# We then test for time variant effects, to check if a two-way model is appropriate.
plmtest(kc.fe1, c("time"), type=("bp"))
# Since p < 0.05, we should include time-fixed effects. A two-way model is appropriate.
basic <- plm(lnco2~lninc+lnincsq, data = panel, model = "within", effect = "twoways")
summary(basic)
# Again, betas remain significant.
phtest(basic, kc.re1) # We reconfirm that FE still appropriate, which it is.

# Check for heteroskedasticity/cross sectional dependence.
pcdtest(basic, test = c("lm"))
pcdtest(basic)
# P value strongly suggests heteroskedasticity and CSD. So we use the robust SE.
coeftest(basic, vcovHC(basic, method = "white1"))
twoway1 <- coeftest(basic, vcov = vcovSCC(basic, 
                                      type="HC3", 
                                      cluster = "group"))
twoway1
# Standard errors are close for both robust estimators, so we will take the SE estimates from White robust method.
# We save these for use in the results table.
cov1 <- vcovHC(basic, method = "white1")
robust_se1 <- sqrt(diag(cov1))

# Finally, we check the model to confirm that it is not too unbalanced.
punbalancedness(basic)
# Final model is very close to balance, so fixed effects should be appropriate here.


##### We repeat above for a model with log exports control variable added. #####

# Pooled OLS model
ols2 <- lm(lnco2~lninc+lnincsq+lnexp, data = panel)
# Fit FE basic model
kc.fe2 <- plm(lnco2~lninc+lnincsq+lnexp, data = panel, model = "within")
summary(kc.fe2)
# Added variable is significant and <0 for this basic FE specification.
# Check if OLS better than FE:
pooltest(lnco2~lninc+lnincsq+lnexp, data=panel,model = "pooling")
# Since p<0.05, FE is a better choice than Pooled OLS

# Fit a Random Effects model to verify if appropriate.
kc.re2 <- plm(lnco2~lninc+lnincsq+lnexp, data = panel, model = "random")
# Hausman Test for RE vs FE. Shows that FE is the more suitable model, since p<0.05.
phtest(kc.fe2, kc.re2)

# We then test for time variant effects, to check if a two-way model is appropriate.
plmtest(kc.fe2, c("time"), type=("bp"))
# Since p < 0.05, we need to include time-fixed effects. A two-way model is appropriate.
ctrl1 <- plm(lnco2~lninc+lnincsq+lnexp, data = panel, model = "within", effect = "twoways")
summary(ctrl1)
# Now, log export variable is not significant, when we include two-way
phtest(ctrl1, kc.re2) # Reconfirm that FE still appropriate.

# Check for heteroskedasticity/cross sectional dependence.
pcdtest(ctrl1, test = c("lm"))
pcdtest(ctrl1)
# P value strongly suggests heteroskedasticity, since very small. So we use the robust SE.
coeftest(ctrl1, vcovHC(ctrl1, method = "white1"))
twoway2 <- coeftest(ctrl1, vcov = vcovSCC(ctrl1, 
                                          type="HC3", 
                                          cluster = "group"))
twoway2
# Standard errors are close for both robust estimators, so we take the SE estimates from White robust method.
# We save these for use in the results table.
cov2 <- vcovHC(ctrl1, method = "white1")
robust_se2 <- sqrt(diag(cov2))
# Finally, we again check degree of unbalancedness.
punbalancedness(ctrl1)
# Final model is very close to balance, and almost the same as the basic model.


##### Finally, repeat for model with two control variables: log exports and log population density. #####

# Pooled OLS model
ols3 <- lm(lnco2~lninc+lnincsq+lnexp+lnpop, data = panel)
# Fit FE basic model
kc.fe3 <- plm(lnco2~lninc+lnincsq+lnexp+lnpop, data = panel, model = "within")
summary(kc.fe3)
# log export is significant again, but log popdens is only at 10%. This changes when we move to two-way.
# Check if OLS better than FE
pooltest(lnco2~lninc+lnincsq+lnexp+lnpop, data=panel,model = "pooling")
# Since p<0.05, FE is a better choice than Pooled OLS
# Fit a Random Effects model to verify if appropriate.
kc.re3 <- plm(lnco2~lninc+lnincsq+lnexp+lnpop, data = panel, model = "random")
# Hausman Test for RE vs FE. Shows that FE is the more suitable model, since p<0.05.
phtest(kc.fe3, kc.re3)

# We then test for time variant effects, to check if a two-way model is appropriate.
plmtest(kc.fe3, c("time"), type=("bp"))
# Since p < 0.05, we need to include time-fixed effects. A two-way model is appropriate.
ctrl2 <- plm(lnco2~lninc+lnincsq+lnexp+lnpop, data = panel, model = "within", effect = "twoways")
summary(ctrl2)
# Now, log export variable is not significant, again, but log popdens is significant.
phtest(ctrl2, kc.re3) # Reconfirm that FE still appropriate, which it is.

# Check for heteroskedasticity/cross sectional dependence.
pcdtest(ctrl2, test = c("lm"))
pcdtest(ctrl2)
# P value strongly suggests heteroskedasticity, since small. So we use the robust SE.
coeftest(ctrl2, vcovHC(ctrl2, method = "white1"))
twoway3 <- coeftest(ctrl2, vcov = vcovSCC(ctrl2, 
                                          type="HC3", 
                                          cluster = "group"))
twoway3
# Standard errors are close for both robust estimators, so take SE estimates from White robust method.
# Save these for use in the results table.
cov3 <- vcovHC(ctrl2, method = "white1")
robust_se3 <- sqrt(diag(cov3))
# We note that log popdens remains statistically significant under robust errors.

# Again confirm unbalancedness is not too bad.
punbalancedness(ctrl2)
# Final model is very close to balance.

# We export the coefficient table, with robust standard errors, for use in document.
stargazer(basic, ctrl1, ctrl2, type="html", se=list(robust_se1, robust_se2, robust_se3),
          omit.stat = "f",out="models.htm")
