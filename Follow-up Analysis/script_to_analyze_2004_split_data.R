remove(list=objects())

library(arm)
library(car)
library(maps)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("stan_lmer_voting_analysis.R")
source("generate_stan_fig_2.R")

########################################################################################
### labels

reg.lkup <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)

reg.label <- c("Northeast", "Midwest", "South", "West", "DC")
stt.label <- c(state.abb[1:8], "DC", state.abb[9:50])
eth.label <- c("White", "Black", "Hispanic", "Other")
inc.label <- c("$0-20k", "$20-40k", "$40-75k", "$75-150k", "$150k+")
age.label <- c("18-29", "30-44", "45-64", "65+")
sex.label <- c("Male", "Female")
edu.label <- c("< HS", "HS", "Some Coll", "Coll", "Post-Grad")
mar.label <- c("Married", "Single")
rel.label <- c("Protestant (not born-again)","Born-again Protestant","Catholic","Mormon","Jewish","Other religion","No religion")
chu.label <- c("Nonattenders","Rare attenders","Occasional\nattenders","Frequent attenders","Very frequent\nchurch attenders")
kid.label <- c("Kids", "No Kids")
cit.label <- c("Citizen", "Not Citizen")

n.reg <- length(reg.label)
n.stt <- length(stt.label)
n.eth <- length(eth.label)
n.inc <- length(inc.label)
n.age <- length(age.label)
n.sex <- length(sex.label)
n.edu <- length(edu.label)
n.mar <- length(mar.label)
n.rel <- length(rel.label)
n.chu <- length(chu.label)
n.kid <- length(kid.label)

########################################################################################
### get data

### state-level data
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

### annenberg / pew data for modeling vote choice
dat.vot <- read.table("data/votechoice2000-04-08.dat", header=T, sep="\t")
dat.vot$weight[is.na(dat.vot$weight)] <- 1
ok <- apply(is.na(dat.vot[1:8]), 1, sum)==0 & 
  (dat.vot$cit==1 | is.na(dat.vot$cit)) & 
  (dat.vot$regist=="registered" | is.na(dat.vot$regist))
dat.vot <- dat.vot[ok,]
dat.vot$reg <- reg.lkup[dat.vot$stt]
dat.vot$year <- recode(dat.vot$file, "'2000-ann'=2000; '2004-ann'=2004; '2008-pew'=2008", as.factor.result=F)
dat.vot <- dat.vot[dat.vot$year != 2000,]
dat.vot <- dat.vot[dat.vot$year != 2008,]

remove_list <- generate_unique_index_list(5630, nrow(dat.vot))
dat.vot <- dat.vot[-remove_list,]

split_list <- generate_unique_index_list(19170, nrow(dat.vot))
dat.vot.split_list <- list(dat.vot[split_list,], dat.vot[-split_list,])

### census data from PUMS for population cell sizes
dat.pop <- read.table("data/census-pums-pop-2000-04-08.dat", header=T, sep="\t")

### prepare data for looping through years
years <- c(2004, 2008)
L.z.incstt <- list(dat.stt$z.inc2004, dat.stt$z.inc2007)
L.z.repprv <- list(dat.stt$z.rep2000, dat.stt$z.rep2004)
L.z.trnprv <- list(dat.stt$z.trn2000, dat.stt$z.trn2004)


########################################################################################
### Run MRP for stt, eth, inc, age

data_matrix <- as.data.frame(expand.grid(1:n.stt, 1:n.eth, 1:n.inc, 1:n.age))
colnames(data_matrix) <- c("stt", "eth", "inc", "age")
data_matrix$age <- factor(data_matrix$age)
data_matrix$eth <- factor(data_matrix$eth)
data_matrix$inc <- factor(data_matrix$inc)
data_matrix$stt <- factor(data_matrix$stt)

data_matrix$grp <- apply(data_matrix, 1, paste, collapse="_")
data_matrix$ix <- 1:nrow(data_matrix)

### poststratification setup
interested_rows <- sapply(1:nrow(data_matrix), function(i) paste("p[", i, "]", sep = ""))
interested_cols <- c("mean")

result_matrix <- as.data.frame(expand.grid(1:n.stt, 1:n.eth, 1:n.inc, 1:n.age))
colnames(result_matrix) <- c("stt", "eth", "inc", "age")
result_matrix$grp <- apply(result_matrix, 1, paste, collapse="_")
result_matrix$ix <- 1:nrow(result_matrix)

dat.pop$grp <- apply(dat.pop[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
data_pop_info <- summarize(group_by(dat.pop, grp), pop2004 = sum(wtd2004), pop2008 = sum(wtd2008))
result_matrix <- merge(x=result_matrix, y=data_pop_info, by="grp", all.x=T)
result_matrix <- result_matrix[order(result_matrix$ix),]

### multilevel models
M.cps <- M.vot <- list()
for (i in 1:2) {
  cat(paste("***** Multilevel Models for Split", i, "\n"))
  
  ### covariates
  data_matrix <- mutate(data_matrix,
                        reg = factor(reg.lkup[stt]),
                        z_inc = rescale(inc),
                        z_incstt = L.z.incstt[[1]][stt],
                        z_trnprv = L.z.trnprv[[1]][stt],
                        z_repprv = L.z.repprv[[1]][stt])
  
  ### vote choice model
  cat("*****   Annenberg/Pew Vote Choice Model\n")
  tmp <- dat.vot.split_list[[i]]
  tmp$grp <- apply(tmp[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
  tmp$ones <- 1
  tmp <- rename(tmp,vote = rvote)
  
  stan_lmer_obj <- stan_vote_regression(data_matrix, tmp, 3, paste("Pew_Vote_Model",years[i],sep="_"))
  stanfit_obj <- stan(data = stan_lmer_obj$data, file = "../Stan/CPS_Turnout_Model_2004_Level_3.stan", iter = 500)
  
  ###Poststratification
  #Rebuild design matrix
  result_matrix$vote2004.M <- NA
  result_matrix$vote2004 <- NA
  result_matrix$vote2004.M <- invlogit(summary(stanfit_obj)$summary[interested_rows, interested_cols])
  for (j in 1:n.stt) {
    cat(paste(j, "of", n.stt, "\n"))
    ok <- result_matrix$stt==j
    result_matrix$vote2004[ok] <- weighted_correction(result_matrix$vote2004.M[ok], result_matrix$pop2004[ok]/sum(result_matrix$pop2004[ok]), dat.stt[as.character(j), "rep2004"])$corrected
  }
  M.vot[[i]] <- stanfit_obj
  
  save.image("Split_analysis2.Rdata")
  
  generate_fig_2_for_2004(result_matrix, dat.stt, dat.vot, paste("stan_split", i, "fig_2.png", sep = "_"))  
}