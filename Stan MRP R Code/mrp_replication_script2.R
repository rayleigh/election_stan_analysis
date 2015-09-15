library(arm)
library(car)
library(maps)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

remove(list=objects())
source("stan_lmer_voting_analysis.R")

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
  
  stan_cps <- stan_vote_regression(data_matrix, tmp, 3, paste("CPS_Turnout_Model",years[i.year],sep="_"))
  M.cps[[i.year]] <- stan(data = stan_cps$data, file = "../Stan/CPS_Turnout_Model_2004_Level_3.stan", iter = 500) 

  ### vote choice model
  cat("*****   Annenberg/Pew Vote Choice Model\n")
  tmp <- dat.vot[dat.vot$year==years[i.year],]
  tmp$grp <- apply(tmp[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
  tmp$ones <- 1
  tmp <- rename(tmp,vote = rvote)
  
  stan_pew <- stan_vote_regression(data_matrix, tmp, 3, paste("Pew_Vote_Model",years[i.year],sep="_"))
  M.vot[[i.year]] <- stan(data = stan_pew$data, file = file = "../Stan/CPS_Turnout_Model_2004_Level_3.stan", iter = 500)

  
}

interested_rows <- sapply(1:nrow(data_matrix), function(i) paste("p[", i, "]", sep = ""))
interested_cols <- c("mean")

data_matrix$turn2004.M <- invlogit(summary(M.cps[[1]])$summary[interested_rows, interested_cols])
data_matrix$turn2008.M <- invlogit(summary(M.cps[[2]])$summary[interested_rows, interested_cols])
data_matrix$vote2004.M <- invlogit(summary(M.vot[[1]])$summary[interested_rows, interested_cols])
data_matrix$vote2008.M <- invlogit(summary(M.vot[[2]])$summary[interested_rows, interested_cols])

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
  data_matrix$turn2004[ok] <- weighted_correction(data_matrix$turn2004.M[ok], data_matrix$pop2004[ok], dat.stt[as.character(i), "vote2004"])$corrected
  data_matrix$turn2008[ok] <- weighted_correction(data_matrix$turn2008.M[ok], data_matrix$pop2008[ok], dat.stt[as.character(i), "vote2008"])$corrected
  data_matrix$vote2004[ok] <- weighted_correction(data_matrix$vote2004.M[ok], data_matrix$pop2004[ok]/sum(data_matrix$pop2004[ok]), dat.stt[as.character(i), "rep2004"])$corrected
  data_matrix$vote2008[ok] <- weighted_correction(data_matrix$vote2008.M[ok], data_matrix$pop2008[ok]/sum(data_matrix$pop2008[ok]), dat.stt[as.character(i), "rep2008"])$corrected
}

save.image("stan-MRP-20120715.RData")

########################################################################################
### figure 2 -- model checking

load.image("stan-MRP-20120715.RData")

data_matrix$plot.grp <- apply(data_matrix[, c("stt", "inc")], 1, paste, collapse="_")
D2 <- summarize(group_by(data_matrix, plot.grp), vote2008 = sum(vote2008 * pop2008)/sum(pop2008), vote2004 = sum(vote2004 * pop2004)/sum(pop2004), stt = unique(stt), inc = unique(inc))
D3 <- summarize(group_by(data_matrix[data_matrix$eth==1,], plot.grp), vote2008 = sum(vote2008 * pop2008)/sum(pop2008), vote2004 = sum(vote2004 * pop2004)/sum(pop2004), stt = unique(stt), inc = unique(inc))

#2008
stt.inc <- as.data.frame(expand.grid(sort(unique(D2$stt)), sort(unique(D2$inc))))
colnames(stt.inc) <- c("stt", "inc")
stt.inc$fit2 <- stt.inc$se2 <- stt.inc$mu2 <- 
  stt.inc$fit1 <- stt.inc$se1 <- stt.inc$mu1 <- NA

for (i in 1:nrow(stt.inc)) {
  ok <- which(dat.vot$stt==stt.inc$stt[i] & dat.vot$inc==stt.inc$inc[i] & dat.vot$year==2008)
  if (length(ok) > 1) {
    stt.inc$fit1[i] <- D2$vote2008[D2$stt==stt.inc$stt[i] & D2$inc==stt.inc$inc[i]]
    stt.inc$mu1[i] <- min(0.98, max(0.02, weighted_mean(dat.vot$rvote[ok], w=dat.vot$weight[ok])))
    stt.inc$se1[i] <- sqrt((stt.inc$fit1[i]*(1-stt.inc$fit1[i])) / length(ok))
  }
  ok <- which(dat.vot$stt==stt.inc$stt[i] & dat.vot$inc==stt.inc$inc[i] & dat.vot$year==2008 & dat.vot$eth==1)
  if (length(ok) > 1) {
    stt.inc$fit2[i] <- D3$vote2008[D3$stt==stt.inc$stt[i] & D3$inc==stt.inc$inc[i]]
    stt.inc$mu2[i] <- min(0.98, max(0.02, weighted_mean(dat.vot$rvote[ok], w=dat.vot$weight[ok])))
    stt.inc$se2[i] <- sqrt((stt.inc$fit2[i]*(1-stt.inc$fit2[i])) / length(ok))
  }
}

png ("2008_stan_fig2.png", height=10, width=10, res=300, units="in")
graph.dims <- c(7,7)
par(mfrow=graph.dims, mar=c(1,0.75,1,0.75), tck=0, mgp=c(1.5,.5,0), 
    oma=c(7,2,4,0), lwd=0.5)
sort.state <- rev(order(dat.stt$rep2008[1:51]))
state.names <- c(state.name[1:8], "Washington DC", state.name[9:50])
colors <- c ("gray40", "darkorange")
pchs <- c (20, 21)
count <- 0
for (i in sort.state){
  if (!(stt.label[i] %in% c("AK","HI","DC"))){
    count <- count + 1
    plot(c(1,5), c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", type="n", yaxs="i")
    if (count%%graph.dims[2]==1)
      axis(2, c(.02,.5,.95), c("0%","50%","100%"), cex.axis=1.1, lwd=0.5)
    if (count > (48-graph.dims[2]))
      axis(1, c(1.2,3,4.8), c("poor","mid","rich"), cex.axis=1.2, lwd=0.5)
    abline (.5, 0, col="gray", lwd=.5)
    
    tmp <- stt.inc[stt.inc$stt==i,]
    points(1:5 - 0.05, tmp$mu1, pch=20, col=colors[1], cex=0.5)
    segments(1:5 - 0.05, tmp$mu1 - tmp$se1, 1:5 - 0.05, tmp$mu1 + tmp$se1, col=colors[1])
    points(1:5, tmp$fit1, type="l", col=colors[1])
    
    points(1:5 + 0.05, tmp$mu2, ylim=c(0,1), pch=20, col=colors[2], cex=0.5)
    segments(1:5 + 0.05, tmp$mu2 - tmp$se2, 1:5 + 0.05, tmp$mu2 + tmp$se2, col=colors[2])
    points(1:5, tmp$fit2, type="l", col=colors[2])
    text (3, .9, state.names[i], cex=1.3)
  }
}
mtext ("2008 election:  McCain share of the two-party vote in each income category\nwithin each state among all voters (gray) and just non-Hispanic whites (orange)", outer=TRUE, cex=1, side=3, line=1)
mtext (" Dots are weighted averages from pooled June-Nov Pew surveys; error bars show +/- 1 s.e. bounds.\n Curves are estimated using multilevel models and have a s.e. of about 3% at each point.\n States are ordered in decreasing order of McCain vote (Alaska, Hawaii, and D.C. excluded).", outer=TRUE, cex=.9, side=1, line=5)
dev.off()

#2004
stt.inc <- as.data.frame(expand.grid(sort(unique(D2$stt)), sort(unique(D2$inc))))
colnames(stt.inc) <- c("stt", "inc")
stt.inc$fit2 <- stt.inc$se2 <- stt.inc$mu2 <- 
  stt.inc$fit1 <- stt.inc$se1 <- stt.inc$mu1 <- NA

for (i in 1:nrow(stt.inc)) {
  ok <- which(dat.vot$stt==stt.inc$stt[i] & dat.vot$inc==stt.inc$inc[i] & dat.vot$year==2004)
  if (length(ok) > 1) {
    stt.inc$fit1[i] <- D2$vote2004[D2$stt==stt.inc$stt[i] & D2$inc==stt.inc$inc[i]]
    stt.inc$mu1[i] <- min(0.98, max(0.02, weighted_mean(dat.vot$rvote[ok], w=dat.vot$weight[ok])))
    stt.inc$se1[i] <- sqrt((stt.inc$fit1[i]*(1-stt.inc$fit1[i])) / length(ok))
  }
  ok <- which(dat.vot$stt==stt.inc$stt[i] & dat.vot$inc==stt.inc$inc[i] & dat.vot$year==2004 & dat.vot$eth==1)
  if (length(ok) > 1) {
    stt.inc$fit2[i] <- D3$vote2004[D3$stt==stt.inc$stt[i] & D3$inc==stt.inc$inc[i]]
    stt.inc$mu2[i] <- min(0.98, max(0.02, weighted_mean(dat.vot$rvote[ok], w=dat.vot$weight[ok])))
    stt.inc$se2[i] <- sqrt((stt.inc$fit2[i]*(1-stt.inc$fit2[i])) / length(ok))
  }
}

png ("stan_fig2.png", height=10, width=10, res=300, units="in")
graph.dims <- c(7,7)
par(mfrow=graph.dims, mar=c(1,0.75,1,0.75), tck=0, mgp=c(1.5,.5,0), 
    oma=c(7,2,4,0), lwd=0.5)
sort.state <- rev(order(dat.stt$rep2008[1:51]))
state.names <- c(state.name[1:8], "Washington DC", state.name[9:50])
colors <- c ("gray40", "darkorange")
pchs <- c (20, 21)
count <- 0
for (i in sort.state){
  if (!(stt.label[i] %in% c("AK","HI","DC"))){
    count <- count + 1
    plot(c(1,5), c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", type="n", yaxs="i")
    if (count%%graph.dims[2]==1)
      axis(2, c(.02,.5,.95), c("0%","50%","100%"), cex.axis=1.1, lwd=0.5)
    if (count > (48-graph.dims[2]))
      axis(1, c(1.2,3,4.8), c("poor","mid","rich"), cex.axis=1.2, lwd=0.5)
    abline (.5, 0, col="gray", lwd=.5)
    
    tmp <- stt.inc[stt.inc$stt==i,]
    points(1:5 - 0.05, tmp$mu1, pch=20, col=colors[1], cex=0.5)
    segments(1:5 - 0.05, tmp$mu1 - tmp$se1, 1:5 - 0.05, tmp$mu1 + tmp$se1, col=colors[1])
    points(1:5, tmp$fit1, type="l", col=colors[1])
    
    points(1:5 + 0.05, tmp$mu2, ylim=c(0,1), pch=20, col=colors[2], cex=0.5)
    segments(1:5 + 0.05, tmp$mu2 - tmp$se2, 1:5 + 0.05, tmp$mu2 + tmp$se2, col=colors[2])
    points(1:5, tmp$fit2, type="l", col=colors[2])
    text (3, .9, state.names[i], cex=1.3)
  }
}
mtext ("2004 election:  Bush share of the two-party vote in each income category\nwithin each state among all voters (gray) and just non-Hispanic whites (orange)", outer=TRUE, cex=1, side=3, line=1)
mtext (" Dots are weighted averages from pooled June-Nov Annenberg surveys; error bars show +/- 1 s.e. bounds.\n Curves are estimated using multilevel models and have a s.e. of about 3% at each point.\n States are ordered in decreasing order of the 2008 Mcain vote (Alaska, Hawaii, and D.C. excluded).", outer=TRUE, cex=.9, side=1, line=5)
dev.off()