library(dplyr)
library(stanRegression)

#Create a STAN file to do STAN lmer with the interaction level
stan_vote_regression <- function(data_matrix, result_info_matrix, level, filename)
{
  # Get weighted results
  data_matrix <- calc_and_add_weighted_results(data_matrix, result_info_matrix)
  
  # Get formula based on the interaction level
  reg_formula = build_formula(level)
  
  # Fit results 
  stan_fit <- stan_lmer(formula = formula(reg_formula), 
                           family = "binomial", 
                           file_name = filename, data = data_matrix, run_stan = F)
    
  return(stan_fit)
}

#Helper functions
# Calculate the weighted results
calc_and_add_weighted_results <- function(data_matrix,result_info_matrix)
{
  # Calculate the size, the weighted y-bar, and the design effect for each cell
  # and store it in a data_frame
  weighted_results_info <- summarize(group_by(result_info_matrix, grp),
                                     n = sum(ones),
                                     wt_ybar = sum(vote*weight)/sum(weight),
                                     cell_design_eff = 1 + var(weight/mean(weight)))
  
  # Add weighted_results to the data_matrix using the grp column
  data_matrix <- merge(x = data_matrix, y = weighted_results_info, by="grp", all.x=T)  
  data_matrix <- arrange(data_matrix, ix)

  #Calculate the weighted results vector
  data_matrix$n[is.na(data_matrix$n)] <- 0
  design_eff <- weighted_mean(data_matrix$cell_design_eff, data_matrix$n, data_matrix$n > 1)
  data_matrix$n_eff <- data_matrix$n/design_eff
  data_matrix$wt_ybar[data_matrix$n_eff==0] <- 0.5
  data_matrix <- mutate(data_matrix,
                        yes = round(wt_ybar * n_eff),
                        no = round((1 - wt_ybar) * n_eff),
                        n_eff = yes + no
                        )
  return(data_matrix)
}

#Calculate weighted mean for a subset
weighted_mean <- function(data_v, weights=rep(1,length(data_v)), subset=rep(TRUE,length(data_v))) 
{
  keep <- !is.na(data_v) & !is.na(weights) & !is.na(subset) & subset
  return(sum((weights*data_v)[keep])/sum(weights[keep]))
}

#Build the formula based on the interaction level
build_formula <- function(level)
{
  formula <- "cbind(yes, no) ~ z_inc*z_incstt + z_inc*z_trnprv + (1 + z_inc | reg) + (1 + z_inc | stt) + (1 + z_inc | eth) + (1 | inc) + (1 + z_inc | age)"
  if (level >= 2) 
  {
    formula <- paste(formula, "(1 | reg : eth) + (1 | reg : inc) + (1 | reg : age) + (1 | stt : eth) + (1 | stt : inc) + (1 | stt : age) + (1 | eth : inc) + (1 | eth : age) + (1 | inc : age)", sep = " + ")
  }
  if (level == 3)
  {
    formula <- paste(formula, "(1 | stt : eth : inc) + (1 | stt : eth : age) + (1 | stt : inc : age) + (1 | eth : inc : age)", sep = " + ")
  }
  return(formula)
}

#Correct a vector by using the delta that
#minimizes calc_delta_correction's difference
weighted_correction <- function(data_v, weights, x0) 
{
  delta <- optimize(calc_delta_correction, interval=c(-5,5), data_v, weights, x0)$minimum
  corrected <- invlogit(logit(data_v) + delta)
  return(list(delta=delta, corrected=corrected))
}

#Calculate the absolute difference between x0 
#and the sum of inverse logit of logit(data_v) plus delta
calc_delta_correction <- function(delta, data_v, weights, x0) 
{
  abs(x0-sum(invlogit(logit(data_v) + delta)*weights))
}

#Calculate the logit
logit <- function (data_v) log(data_v/(1-data_v))
