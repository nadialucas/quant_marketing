# load the data from 'pbout.txt' in R

library(readr)
library(dplyr)
library(psych)
library(bayesm)
library(mlogit)
library(tidyr)
library(reshape2)
setwd("/Users/nadialucas/Documents/quant_marketing/pset1")
pb_raw <- read_table("pbout.txt")

# Compute what you consider the relevant descriptive statistics by product alternative and report them in a table
### total mkt share (by panelist/trip)
### avg # of brands purchased by each hh
### avg expenditure per hh
### expenditure rank
### fraction of trips that pb is purchased during
### avg price of alternative purchased
### avg price of alternative in general


# first clean the data, so add a 1-9 on each of the panelist/weeks

pb_cleaned <- pb_raw %>% 
  group_by(panelid, date) %>% 
  mutate(choice_alternative = row_number()) %>%
  mutate(avg_price = mean(price)) %>%
  mutate(chosen = ifelse(choice == choice_alternative, 1, 0))



### avg price of alternative in general
# ok and also collapse these down to product level for the future
pb_avg_prices <- pb_cleaned %>% group_by(choice_alternative) %>% mutate(mean_price = mean(price))

pb_avg_prices_condl_purchased <- pb_cleaned %>% filter(choice == choice_alternative) %>% mutate(mean_price = mean(price))
  

# do we want date fixed effects?

### total mkt share (by panelist/trip)


describe(pb_avg_prices_condl_purchased)


describeBy(pb_avg_prices_condl_purchased, group = pb_avg_prices_condl_purchased$choice)


lmout = lm(chosen ~ price, data = pb_cleaned)
summary(lmout)



# multinomial logit

# we want to reshape wide the pb dataset

hi <- spread(pb_cleaned, choice_alternative, price)



data("Heating", package = "mlogit")


H <- dfidx(Heating,choice="depvar",varying=c(3:12))
m <- mlogit(depvar~ic+oc|0,H)


summary(m)



#playing around with bayesm
data(bank)
choiceAtt = bank$choiceAtt
Z = bank$demo
# demean Z, demographic data and remove the identifiers
Z = subset(Z, select = c("age", "income", "gender"))
Z = t(t(Z)-apply(Z,2,mean))
# set number of choice alternative
p = 14
ncoef = 14



hh = levels(factor(choiceAtt$id))
nhh = length(hh)
lgtdata = NULL
for (i in 1:nhh) {
  y = choiceAtt[choiceAtt[,1] == hh[i], 2]
  nobs = length(y)
  X = as.matrix(choiceAtt[choiceAtt[,1]==hh[i], c(3:16)])
  lgtdata[[i]] = list(y=y, X=X)
}

cat("Finished Reading data", fill = TRUE)






