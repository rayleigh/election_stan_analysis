#This assumes that you've run naes_data_process_script.R first
remove(list=objects())

processed_file_dir <- "~/Documents/Gelman Research/Replication/Subsequent Research/"
setwd(processed_file_dir)
load("naes_data_processed.Rdata")

library(arm)
library(maps)
library(rstan)
library(rstanarm)
library(dplyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Get state level data
data_file_location <- "~/Documents/Gelman Research/Replication/Original Research/"
setwd(data_file_location)
dat.stt <- read.table("data/state-stats.dat", header=T, sep="\t")
dat.stt$z.inc2000 <- rescale(dat.stt$inc2000)
dat.stt$z.inc2004 <- rescale(dat.stt$inc2004)
dat.stt$z.inc2007 <- rescale(dat.stt$inc2007)
dat.stt$z.rep1996 <- rescale(dat.stt$rep1996)
dat.stt$z.rep2000 <- rescale(dat.stt$rep2000)
dat.stt$z.rep2004 <- rescale(dat.stt$rep2004)
dat.stt$z.rep2008 <- rescale(dat.stt$rep2008)
dat.stt$z.trn1996 <- rescale(dat.stt$vote1996/dat.stt$pop1996)
dat.stt$z.trn2000 <- rescale(dat.stt$vote2000/dat.stt$pop2000)
dat.stt$z.trn2004 <- rescale(dat.stt$vote2004/dat.stt$pop2004)
dat.stt$z.trn2008 <- rescale(dat.stt$vote2008/dat.stt$pop2007)
dat.stt$stt <- 1:nrow(dat.stt)

#Prep for looping
L.z.incstt <- dat.stt$z.inc2007
L.z.repprv <- dat.stt$z.rep2004
L.z.trnprv <- dat.stt$z.trn2004

#Build predictor matrix
code_file_dir <- "~/Documents/Gelman Research/Replication/Subsequent Research/R code/"
setwd(code_file_dir)
source("stan_lmer_voting_analysis.R")

setwd(processed_file_dir)
predictor_matrix <- as.data.frame(expand.grid(1:51, 1:4, 1:5, 1:4))
colnames(predictor_matrix) <- c("stt", "eth", "inc", "age")
predictor_matrix$age <- factor(predictor_matrix$age)
predictor_matrix$eth <- factor(predictor_matrix$eth)
predictor_matrix$inc <- factor(predictor_matrix$inc)
predictor_matrix$stt <- factor(predictor_matrix$stt)

predictor_matrix$grp <- apply(predictor_matrix, 1, paste, collapse="_")
predictor_matrix$ix <- 1:nrow(predictor_matrix)
predictor_matrix <- mutate(predictor_matrix,
                           reg = factor(translate_state_to_reg(stt)),
                           z_inc = rescale(inc),
                           z_incstt = L.z.incstt[stt],
                           z_trnprv = L.z.trnprv[stt],
                           z_repprv = L.z.repprv[stt])

model_name_template <- "Annenberg_analysis"
naes_stan_analyzed_list <- lapply(1:length(naes_data_matrix_list), 
                                  function(i) {cat(i); colMeans(plogis(posterior_linpred(stan_vote_regression(predictor_matrix, naes_data_matrix_list[[i]], 3, paste(model_name_template, i, "sample_output", sep = "_")))))})
save(naes_stan_analyzed_list, file = paste(model_name_template, "prob.Rdata", sep = "_"))

#naes_stan_obj_list <- lapply(naes_data_matrix_list, function(naes_data_matrix) {stan_vote_regression(predictor_matrix, naes_data_matrix, 3, "output.csv")})
#naes_stan_analyzed_list <- lapply(1:length(naes_stan_obj_list), function(i) {stan(file = "../Stan/CPS_Turnout_Model_2004_Level_3.stan", data = naes_stan_obj_list[[i]]$data, model_name = paste(model_name_template, i, sep = "_"), sample_file = paste(model_name_template, i, "sample_output", sep = "_"), iter = 500)})
#save.image(file = "naes_data_analy")
