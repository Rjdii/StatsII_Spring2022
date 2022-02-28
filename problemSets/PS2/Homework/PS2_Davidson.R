

## 1

library("fastDummies")
library("tidyverse")

data <- climateSupport

library(dplyr)
data <- data %>%
  mutate(choice = recode(choice, "Supported" = 1, "Not supported" = 0))

logit <- glm(choice ~ .,
             data = data, 
             family = binomial(link = "logit"))

summary(logit)


logit_null <- glm(as.factor(choice) ~ 1, data = data, family = "binomial")
anova(logit_null, logit, test = "Chisq")

exp(confint(logit))

conf_logit <- data.frame(cbind(lower = exp(confint(logit)[,1]), 
                             coefs = exp(coef(logit)), 
                             upper = exp(confint(logit)[,2])))

ggplot(data = conf_logit, mapping = aes(x = row.names(conf_logit), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip()

### I think this should be a different graph. However, it is my understanding that,
# as the p-score is larger than the z-score of the intercept, it is unlikely that
# the number of countries participating and the possibility of sanctions due to 
# non-compliance is not going to be worthwhile predictors of a person's support or not
# for a given policy. 


## 2


data_160_15 <- data %>%
  mutate(sanctions = recode(sanctions, "5" = "15%"))

### I have tried a few things, but I can't seem to change the values in data$Sanctions.
# I thought that I might at least be able to change all the 5's to 15's, and proceed from there,
# but that hasn't worked either. 

# However, my best guess is that this increase would not make much of a difference to the predictability 
# of a person's support or not. 


