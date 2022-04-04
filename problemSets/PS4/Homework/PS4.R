#Getting started:

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

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)


# And so it begins:

data(infants)

imr <- with(infants, Surv(enter, exit, event))

cox <- coxph(imr ~ sex + age, data = infants)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox, type = "text")


cox_fit <- survfit(cox)
autoplot(cox_fit)

newdat <- with(infants, 
               data.frame(
                 sex = c("male", "female"), age="age"
               )
)

autoplot(newdat)

plot(survfit(cox, newdata = newdat), xscale = 12,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "age",
     ylab = "Survival proportion",
     main = "Survival")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))

# Adding an interaction
cox.int <- coxph(imr ~ sex * age, data = infants)
summary(cox.int)
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")

autoplot(cox.int)

coxfit <- survfit(cox.int)

autoplot(coxfit)


## I am trying to plot this to get a better understanding of the data, but I am having no luck. 
# It seems, however, that the older a person is there would by a higher rate of death. But, again,
# I am having a hard time figuring this out.
