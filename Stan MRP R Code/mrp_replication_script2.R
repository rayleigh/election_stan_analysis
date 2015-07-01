library(arm)
library(car)
library(maps)
library(rstan)

remove(list=objects())
setwd("~/Documents/Gelman Research/Replication/Subsequent Research/")
source("~/Documents/Gelman Research/Replication/Subsequent Research/R code/stan_lmer_voting_analysis.R")

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

### CPS data for modeling turnout
dat.cps <- read.table("data/cps2000-04-08-DKs.dat", header=T, sep="\t")
ok <- apply(is.na(dat.cps[1:8]) , 1, sum)==0 & (dat.cps$cit==1 | is.na(dat.cps$cit))
dat.cps <- dat.cps[ok,]
dat.cps$reg <- reg.lkup[dat.cps$stt]
dat.cps$year <- recode(dat.cps$file, "'2000-cps'=2000; '2004-cps'=2004; '2008-cps'=2008", as.factor.result=F)
dat.cps <- dat.cps[dat.cps$year != 2000,]

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

### multilevel models
M.cps <- M.vot <- list()
for (i.year in 1:2) {
  cat(paste("***** Multilevel Models for", years[i.year], "\n"))
  
  ### covariates
  data_matrix <- mutate(data_matrix,
                        reg = factor(reg.lkup[stt]),
                        z_inc = rescale(inc),
                        z_incstt = L.z.incstt[[i.year]][stt],
                        z_trnprv = L.z.trnprv[[i.year]][stt],
                        z_repprv = L.z.repprv[[i.year]][stt])
  
  ### turnout model
  cat("*****   CPS Turnout Model\n")
  
  tmp <- dat.cps[dat.cps$year==years[i.year],]
  tmp$grp <- apply(tmp[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
  tmp$ones <- 1
  
  M.cps[[i.year]] <- stan_vote_regression(data_matrix, tmp, 3, paste("CPS_Turnout_Model",years[i.year],sep="_"))
  stan_rdump(names(M.cps[[i.year]]$data), file = paste("CPS_Turnout_Model",years[i.year],"data.csv",sep="_"), envir = list2env(M.cps[[i.year]]$data))
  
  ### vote choice model
  cat("*****   Annenberg/Pew Vote Choice Model\n")
  tmp <- dat.vot[dat.vot$year==years[i.year],]
  tmp$grp <- apply(tmp[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
  tmp$ones <- 1
  tmp <- rename(tmp,vote = rvote)
  
  M.vot[[i.year]] <- stan_vote_regression(data_matrix, tmp, 3, paste("Pew_Vote_Model",years[i.year],sep="_"))
  stan_rdump(names(M.vot[[i.year]]$data), file = paste("Pew_Vote_Model",years[i.year],"data.csv",sep="_"), envir = list2env(M.vot[[i.year]]$data))
  
  
}
data_matrix$turn2004.M <- fitted(M.cps[[1]])
data_matrix$turn2008.M <- fitted(M.cps[[2]])
data_matrix$vote2004.M <- fitted(M.vot[[1]])
data_matrix$vote2008.M <- fitted(M.vot[[2]])

### poststratification
dat.pop$grp <- apply(dat.pop[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
data_pop_info <- summarize(data = group_by(dat.pop, grp),
                           pop2004=sum(wtd2004), 
                           pop2008=sum(wtd2008))
data_pop_info$grp <- rownames(data_pop_info)
data_matrix <- merge(x=data_matrix, y=data_pop_info, by="grp", all.x=T)
data_matrix <- data_matrix[order(data_matrix$ix),]

### add up and alter based on actual turnout and vote estimates
data_matrix$vote2008 <- data_matrix$vote2004 <- data_matrix$turn2008 <- data_matrix$turn2004 <- NA
for (i in 1:n.stt) {
  cat(paste(i, "of", n.stt, "\n"))
  ok <- data_matrix$stt==i
  data_matrix$turn2004[ok] <- weighted_correction(a=data_matrix$turn2004.M[ok], w=data_matrix$pop2004[ok], x=dat.stt[as.character(i), "vote2004"])$corrected
  data_matrix$turn2008[ok] <- weighted_correction(a=data_matrix$turn2008.M[ok], w=data_matrix$pop2008[ok], x=dat.stt[as.character(i), "vote2008"])$corrected
  data_matrix$vote2004[ok] <- weighted_correction(a=data_matrix$vote2004.M[ok], w=data_matrix$pop2004[ok]/sum(data_matrix$pop2004[ok]), x=dat.stt[as.character(i), "rep2004"])$corrected
  data_matrix$vote2008[ok] <- weighted_correction(a=data_matrix$vote2008.M[ok], w=data_matrix$pop2008[ok]/sum(data_matrix$pop2008[ok]), x=dat.stt[as.character(i), "rep2008"])$corrected
}

save.image("MRP-20120715.RData")