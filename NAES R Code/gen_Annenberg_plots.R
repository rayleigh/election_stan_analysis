remove(list=objects())

library(arm)
library(maps)
library(rstan)
library(rstanarm)
library(dplyr)

code_file_dir <- "~/Documents/Gelman Research/Replication/Subsequent Research/R code/"
setwd(code_file_dir)
source("stan_lmer_voting_analysis.R")
source("generate_stan_fig_2.R")

#####Build stuff needed
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
data_matrix <- as.data.frame(expand.grid(1:n.stt, 1:n.eth, 1:n.inc, 1:n.age))
colnames(data_matrix) <- c("stt", "eth", "inc", "age")
data_matrix$age <- factor(data_matrix$age)
data_matrix$eth <- factor(data_matrix$eth)
data_matrix$inc <- factor(data_matrix$inc)
data_matrix$stt <- factor(data_matrix$stt)

data_matrix$grp <- apply(data_matrix, 1, paste, collapse="_")
data_matrix$ix <- 1:nrow(data_matrix)

### census data from PUMS for population cell sizes
data_matrix_file_dir <- "~/Documents/Gelman Research/Replication/Original Research/"

setwd(data_matrix_file_dir)
dat.pop <- read.table("data/census-pums-pop-2000-04-08.dat", header=T, sep="\t")

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



dat.pop$grp <- apply(dat.pop[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
data_pop_info <- summarize(group_by(dat.pop, grp),
                           pop2004=sum(wtd2004), 
                           pop2008=sum(wtd2008))
data_matrix <- merge(x=data_matrix, y=data_pop_info, by="grp", all.x=T)
data_matrix <- data_matrix[order(data_matrix$ix),]

#Load up results
processed_file_dir <- "~/Documents/Gelman Research/Replication/Subsequent Research/"
setwd(processed_file_dir)
load("naes_data_processed2.Rdata")
load("Annenberg_analysis_prob.Rdata")

#Generate figure for phone data
phone_prob <- do.call(rbind, naes_stan_analyzed_list[1:10])
phone_prob_avg <- colMeans(phone_prob)
names(phone_prob_avg) <- sapply(names(phone_prob_avg), function(num) {paste("p[", num, "]", sep = "")})
data_matrix$vote2008.M <- phone_prob_avg

### add up and alter based on actual turnout and vote estimates
data_matrix$vote2008 <- NA
for (i in 1:n.stt) {
  cat(paste(i, "of", n.stt, "\n"))
  ok <- data_matrix$stt==i
  data_matrix$vote2008[ok] <- weighted_correction(data_matrix$vote2008.M[ok], data_matrix$pop2008[ok]/sum(data_matrix$pop2008[ok]), dat.stt[as.character(i), "rep2008"])$corrected
}
naes_data_matrix_list[[1]]$year = 2008
naes_data_matrix_list[[1]]$rvote = naes_data_matrix_list[[1]]$vote
generate_fig_2_for_2008(data_matrix, dat.stt, naes_data_matrix_list[[1]], "2008_Annenberg_phone_stan_fig2.png", "National Annenberg Election Survey Telephone data sets")

#Generate figure for online data
data_matrix$vote2008.M <- NA
data_matrix$vote2008.M <- naes_stan_analyzed_list[[11]]
data_matrix$vote2008 <- NA
for (i in 1:n.stt) {
  cat(paste(i, "of", n.stt, "\n"))
  ok <- data_matrix$stt==i
  data_matrix$vote2008[ok] <- weighted_correction(data_matrix$vote2008.M[ok], data_matrix$pop2008[ok]/sum(data_matrix$pop2008[ok]), dat.stt[as.character(i), "rep2008"])$corrected
}
naes_data_matrix_list[[11]]$year = 2008
naes_data_matrix_list[[11]]$rvote = naes_data_matrix_list[[11]]$vote
generate_fig_2_for_2008(data_matrix, dat.stt, naes_data_matrix_list[[11]], "2008_Annenberg_online_stan_fig2.png", "National Annenberg Election Survey Online data sets")

