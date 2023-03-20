# load the data from 'pbout.txt' in R
rm(list = ls())

library(readr)
library(dplyr)
library(psych)
library(bayesm)
library(mlogit)
library(tidyr)
library(reshape2)
library(fastDummies)
library(xtable)
library(tibble)
library(sjmisc)

setwd("/Users/nadialucas/Documents/quant_marketing/pset1")
pb_raw <- read_table("pbout.txt")

if (Sys.getenv("LOGNAME")=="nadialucas") {
  outfolder = "/Users/nadialucas/Documents/quant_marketing/pset1/Results/"
  datafolder = "/Users/nadialucas/Documents/quant_marketing/pset1/"
}

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
  mutate(choice_alternative = row_number()) 



### avg price of alternative in general
# ok and also collapse these down to product level for the future
pb_avg_prices <- pb_cleaned %>% group_by(choice_alternative) %>% mutate(mean_price = mean(price))

# doesn't include the no purchase option just for some descriptive statistics
pb_condl_purchased <- pb_cleaned %>% 
  filter(choice == choice_alternative) %>%
  filter(choice != 9)


##################################################################################
#
#                           Descriptive Statistics 
#
##################################################################################


#Compute what you consider the relevant descriptive statistics by product alternative and
#report them in a table.

pb_descriptives <- list(mean_price = mean(pb_cleaned$price),
                         sd_price = sd(pb_cleaned$price),
                         mean_loyalty = mean(pb_cleaned$loyalty),
                         sd_loyalty = sd(pb_cleaned$loyalty),
                         mean_display = mean(pb_cleaned$display),
                         sd_display = sd(pb_cleaned$display),
                         mean_feature = mean(pb_cleaned$feature),
                         sd_feature = sd(pb_cleaned$feature),
                        mean_price_condl = mean(pb_condl_purchased$price),
                        sd_price_condl = sd(pb_condl_purchased$price),
                        mean_loyalty_condl = mean(pb_condl_purchased$loyalty),
                        sd_loyalty_condl = sd(pb_condl_purchased$loyalty),
                        mean_display_condl = mean(pb_condl_purchased$display),
                        sd_display_condl = sd(pb_condl_purchased$display),
                        mean_feature_condl = mean(pb_condl_purchased$feature),
                        sd_feature_condl = sd(pb_condl_purchased$feature))

# Convert the list to a data frame
pb_descriptives_df <- data.frame(pb_descriptives)
# rotate
pb_descriptives_df <- rotate_df(pb_descriptives_df, rn = NULL, cn = FALSE)
# write to a csv
write.csv(pb_descriptives_df, file = paste0(outfolder, "descriptives_table.csv"), row.names = TRUE)

 # multinomial logit

###################################################################################################

# Set up Model
# don't have a dummy for the no-purchase alternative

###################################################################################################



pb_cleaned$choice_alternative <- as.factor(pb_cleaned$choice_alternative)
X_w_dummies <- fastDummies::dummy_cols(pb_cleaned) %>%
  select(-choice_alternative_9) 
# drop the last dummy column though for collinearity purposes

# get y: list of all choices

y <- (X_w_dummies %>% 
        select(choice, panelid, date) %>%
        unique())$choice

# now we can just subset the above to feed into the rmnlIndepMetrop stuff

X_w_dummies_price <- X_w_dummies %>% 
  select(-feature, - display, -loyalty, -panelid, -date, -choice, - choice_alternative) %>%
  relocate(price, .after = last_col())

X_w_dummies_price_names <- names(X_w_dummies_price)


X_model <- X_w_dummies_price

colnames(X_model) <- NULL

# there are 9 total choices so set p
p <- 9

# set the rest of the options 

bayesm_R        <-   10000L #100000L # 20k, 40k variants
bayesm_keep     <-   10L #5L
bayesm_disp     <-   200L  #2000L
bayesm_burnin   <-   100L #1000L / bayesm_keep # Use first 1000 draws as burn in
# ok now let's set up some MCMC models




# helper function to set up a posterior mean and CI

posterior_mean_ci_table <- function(beta_draws, feature_names) {
  n <- ncol(beta_draws)
  mean_beta <- apply(beta_draws, 2, mean)
  ci_lo <- apply(beta_draws, 2, function(x) quantile(x, 0.025))
  ci_hi <- apply(beta_draws, 2, function(x) quantile(x, 0.975))
  out_table <- data.table(do.call(rbind, list(mean_beta, ci_lo, ci_hi)))
  setnames(out_table, names(out_table), feature_names)
  out_table[, statistic := c("Mean", "2.5%", "97.5%")]
  setcolorder(out_table, c("statistic", feature_names))
  return(out_DT)
}


# posterior LL

logMargDenNRTrim <- function(log_likelihood) {
  ci1 <- quantile(log_likelihood, 0.025)
  ci2 <- quantile(log_likelihood, 0.975)
  return(logMargDenNRTrim(log_likelhiood[which(log_likelhiood > ci1 & log_likelihood < ci2)]))
}



# Alternative specific dummy var and price

model_price_only <- rmnlIndepMetrop(Data = list(p = p, 
                                         y = y,
                                         X = data.matrix(X_model)),
                             Mcmc = list(R = bayesm_R,
                                         keep = bayesm_keep,
                                         burnin = bayesm_burnin,
                                         nprint = bayesm_disp))

# report the posterior mean and 95% credibility interval for each
posterior_DT_1_1   <-   posterior_mean_ci_table(model_price_only$betadraw, X_w_dummies_price_names)
print(posterior_DT_1_1)
posterior_DT_like_1_1 <- data.table(log_marginal_likelihood        =   logMargDenNR(model_1_1$loglike[-(1:(bayesm_burnin))]),     # -15649.01
                                    log_marginal_likelihood_trim   =   logMargDenNRTrim(model_1_1$loglike[-(1:(bayesm_burnin))]), # -15644.82
                                    acceptance_rate                =   model_1_1$acceptr)                                            # 0.6058

print(model_price_only$betadraw)


## New Prior

set.seed(base_seed + 1)
model_1_2   <-   rmnlIndepMetrop(Data  = list(p   =   alternative_choices,
                                              y   =   unique(data_DT[, c("choice", "panelid", "date")])$choice,
                                              X   =   as.matrix(model_DT)),
                                 Prior = list(A   =    diag(ncol(model_DT)) * 0.25),
                                 Mcmc  = list(R        =   bayesm_R,
                                              keep     =   bayesm_keep,
                                              nprint   =   bayesm_disp)
)

posterior_DT_1_2   <-   CI_mcmc(model_1_2, names(model_DT), bayesm_burnin)
print(posterior_DT_1_2)
posterior_DT_like_1_2 <- data.table(log_marginal_likelihood        =   logMargDenNR(model_1_2$loglike[-(1:(bayesm_burnin))]),
                                    log_marginal_likelihood_trim   =   logMargDenNRTrim(model_1_2$loglike[-(1:(bayesm_burnin))]),
                                    acceptance_rate                =   model_1_2$acceptr)



###
#Load the data from “pbout.txt” in R
#• Compute what you consider the relevant descriptive statistics by product alternative and
#report them in a table.
#• Estimate the following multinomial logit specifications:
#  1. Price only
#2. Alternative-specificdummyvariables&price
#3. Alternative-specificdummyvariables&price&promotions
#• For each specification, report the posterior mean and 95% credibility interval for each coefficient. How sensitive are your estimates to the prior settings? Across the three specifications, what changes do you notice and what is your intuition for those changes?
#  • Using the function logMargDenNR in Bayesm, compute the posterior log-likelihood for each specification via the Netwon-Raferty approach. Report this in the table along with your coefficient estimates.





