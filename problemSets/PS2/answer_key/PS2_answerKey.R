#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("texreg"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))
# change baseline category to answer Qs
climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, ordered = F), ref="5%")

# run additive model
climate_logit <- glm(choice~countries+sanctions, data=climateSupport, family = binomial(link = "logit"))
summary(climate_logit)
# (1a)
# option #1: use info from summary() output
# set up hypothesis test yourself & reference chi-sq distribution
pchisq(11783-11568, 5, lower.tail = F)

# option #2: manually run null model & anova
null_logit <- glm(choice~1, data=climateSupport, family = binomial(link = "logit"))
anova(null_logit, climate_logit, test="LRT")

# (2a-b)
texreg(list(climate_logit))

# (2c)
# option #1: predict function
round(1/(1+exp(-predict(climate_logit, newdata = data.frame(countries="80 of 192", sanctions="None"), type="response"))), 2)

# option #2: predicted probability "by hand"
exp_coefs <- exp(coef(climate_logit)[1]+(coef(climate_logit)[2]*1)+(coef(climate_logit)[3]*0)+(coef(climate_logit)[4]*1)+(coef(climate_logit)[5]*0)+(coef(climate_logit)[6]*0))
round(exp_coefs/(1+exp_coefs), 2)

# (2d)
# estimate interactive model
climate_logit_interact <- glm(choice~countries*sanctions, data=climateSupport, family = binomial(link = "logit"))
texreg(list(climate_logit, climate_logit_interact))
# compare to additive model using LRT
anova(climate_logit, climate_logit_interact, test="LRT")



