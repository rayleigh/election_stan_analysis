library(arm)
library(car)
library(mapReduce)
library(maps)

remove(list=objects())
setwd("~/Documents/Gelman Research/Replication/Original Research/")

########################################################################################
### helper functions

WMean <- function(a, w=rep(1,length(a)), subset=rep(TRUE,length(a))) {
  keep <- !is.na(a) & !is.na(w) & !is.na(subset) & subset
  return(sum((w*a)[keep])/sum(w[keep]))
}

FindDelta <- function(delta, a, w, x0) {
  abs(x0-sum(invlogit(logit(a) + delta)*w))
}

CorrectWeighted <- function(a, w, x0) {
  delta <- optimize(FindDelta, interval=c(-5,5), a, w, x0)$minimum
  corrected <- invlogit(logit(a) + delta)
  return(list(delta=delta, corrected=corrected))
}

recenter_n <- function(y_new, y_bar_wt) {
  
}

logit <- function (a) log(a/(1-a))

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

run.full.models <- T

########################################################################################
### Run MRP for stt, eth, inc, age

D <- as.data.frame(expand.grid(1:n.stt, 1:n.eth, 1:n.inc, 1:n.age))
colnames(D) <- c("stt", "eth", "inc", "age")
D$grp <- apply(D, 1, paste, collapse="_")
D$ix <- 1:nrow(D)

### multilevel models
M.cps <- M.vot <- list()
for (i.year in 1:2) {
  cat(paste("***** Multilevel Models for", years[i.year], "\n"))
  
  ### covariates
  stt <- D$stt
  eth <- D$eth
  inc <- D$inc
  age <- D$age
  reg <- reg.lkup[stt]
  reg.eth <- 10*reg + eth
  reg.inc <- 10*reg + inc
  reg.age <- 10*reg + age
  stt.eth <- 10*stt + eth
  stt.inc <- 10*stt + inc
  stt.age <- 10*stt + age
  eth.inc <- 10*eth + inc
  eth.age <- 10*eth + age
  inc.age <- 10*inc + age
  reg.eth.inc <- 100*reg + 10*eth + inc
  stt.eth.inc <- 100*stt + 10*eth + inc
  reg.eth.age <- 100*reg + 10*eth + age
  reg.inc.age <- 100*reg + 10*inc + age
  stt.eth.age <- 100*stt + 10*eth + age
  stt.inc.age <- 100*stt + 10*inc + age
  eth.inc.age <- 100*eth + 10*inc + age
  reg.eth.inc.age <- 1000*reg + 100*eth + 10*inc + age
  stt.eth.inc.age <- 1000*stt + 100*eth + 10*inc + age
  z.inc <- rescale(inc)
  z.incstt <- L.z.incstt[[i.year]][stt]
  z.trnprv <- L.z.trnprv[[i.year]][stt]
  z.repprv <- L.z.repprv[[i.year]][stt]

  ### turnout model
  cat("*****   CPS Turnout Model\n")

  tmp <- dat.cps[dat.cps$year==years[i.year],]
  tmp$grp <- apply(tmp[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
  tmp$ones <- 1
  mr <- as.data.frame(mapReduce(data=tmp, map=grp,
                                n=sum(ones),
                                ybar.wt=sum(vote*weight)/sum(weight),
                                des.eff.cell=1 + var(weight/mean(weight))))
  mr$grp <- rownames(mr)
  D.tmp <- merge(x=D, y=mr, by="grp", all.x=T)  
  D.tmp <- D.tmp[order(D.tmp$ix),]
  D.tmp$n[is.na(D.tmp$n)] <- 0
  des.eff <- WMean(D.tmp$des.eff.cell, D.tmp$n, D.tmp$n > 1)
  D.tmp$n.eff <- D.tmp$n/des.eff
  D.tmp$ybar.wt[D.tmp$n.eff==0] <- 0.5
  y <- cbind(D.tmp$ybar.wt * D.tmp$n.eff, (1 - D.tmp$ybar.wt) * D.tmp$n.eff)

  if (!run.full.models) {
    M.cps[[i.year]] <- lmer(y ~ 1 + (1 | stt) + (1 | eth) + (1 | inc) + (1 | age), family=binomial, verbose=T)
  } else {
  M.cps[[i.year]] <- lmer(y ~ z.inc*z.incstt + 
                              z.inc*z.trnprv + 
                              (1 + z.inc | reg) + 
                              (1 + z.inc | stt) + 
                              (1 + z.inc | eth) + 
                              (1 | inc) + 
                              (1 + z.inc | age) + 
                              (1 | reg.eth) + 
                              (1 | reg.inc) + 
                              (1 | reg.age) + 
                              (1 | stt.eth) +
                              (1 | stt.inc) +
                              (1 | stt.age) +
                              (1 | eth.inc) +
                              (1 | eth.age) +
                              (1 | inc.age) +
                              (1 | stt.eth.inc) +
                              (1 | stt.eth.age) +
                              (1 | stt.inc.age) +
                              (1 | eth.inc.age),
                              family=binomial(link="logit"), verbose=T)
  }
  
  ### vote choice model
  cat("*****   Annenberg/Pew Vote Choice Model\n")
  tmp <- dat.vot[dat.vot$year==years[i.year],]
  tmp$grp <- apply(tmp[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
  tmp$ones <- 1
  mr <- as.data.frame(mapReduce(data=tmp, map=grp,
                                n=sum(ones),
                                ybar.wt=sum(rvote*weight)/sum(weight),
                                des.eff.cell=1 + var(weight/mean(weight))))
  mr$grp <- rownames(mr)
  D.tmp <- merge(x=D, y=mr, by="grp", all.x=T)  
  D.tmp <- D.tmp[order(D.tmp$ix),]
  D.tmp$n[is.na(D.tmp$n)] <- 0
  des.eff <- WMean(D.tmp$des.eff.cell, D.tmp$n, D.tmp$n > 1)
  D.tmp$n.eff <- D.tmp$n/des.eff
  D.tmp$ybar.wt[D.tmp$n.eff==0] <- 0.5
  y <- cbind(D.tmp$ybar.wt * D.tmp$n.eff, (1 - D.tmp$ybar.wt) * D.tmp$n.eff)
  y <- round(y)
  D.tmp$n.eff <- rowSums(y)
  
  if (!run.full.models) {
    M.vot[[i.year]] <- lmer(y ~ 1 + (1 | stt) + (1 | eth) + (1 | inc) + (1 | age), family=binomial, verbose=T)
  } else {
  M.vot[[i.year]] <- lmer(y ~ z.inc*z.incstt + 
                              z.inc*z.repprv + 
                              (1 + z.inc | reg) + 
                              (1 + z.inc | stt) + 
                              (1 + z.inc | eth) + 
                              (1 | inc) + 
                              (1 + z.inc | age) + 
                              (1 | reg.eth) + 
                              (1 | reg.inc) + 
                              (1 | reg.age) + 
                              (1 | stt.eth) +
                              (1 | stt.inc) +
                              (1 | stt.age) +
                              (1 | eth.inc) +
                              (1 | eth.age) +
                              (1 | inc.age) +
                              (1 | stt.eth.inc) +
                              (1 | stt.eth.age) +
                              (1 | stt.inc.age) +
                              (1 | eth.inc.age),
                              family=binomial(link="logit"), verbose=T)
  }
}
D$turn2004.M <- fitted(M.cps[[1]])
D$turn2008.M <- fitted(M.cps[[2]])
D$vote2004.M <- fitted(M.vot[[1]])
D$vote2008.M <- fitted(M.vot[[2]])

### poststratification
dat.pop$grp <- apply(dat.pop[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
mr <- as.data.frame(mapReduce(data=dat.pop, map=grp,
                    pop2004=sum(wtd2004), pop2008=sum(wtd2008)))
mr$grp <- rownames(mr)
D <- merge(x=D, y=mr, by="grp", all.x=T)
D <- D[order(D$ix),]

### add up and alter based on actual turnout and vote estimates
D$vote2008 <- D$vote2004 <- D$turn2008 <- D$turn2004 <- NA
for (i in 1:n.stt) {
  cat(paste(i, "of", n.stt, "\n"))
  ok <- D$stt==i
  D$turn2004[ok] <- CorrectWeighted(a=D$turn2004.M[ok], w=D$pop2004[ok], x=dat.stt[as.character(i), "vote2004"])$corrected
  D$turn2008[ok] <- CorrectWeighted(a=D$turn2008.M[ok], w=D$pop2008[ok], x=dat.stt[as.character(i), "vote2008"])$corrected
  D$vote2004[ok] <- CorrectWeighted(a=D$vote2004.M[ok], w=D$pop2004[ok]/sum(D$pop2004[ok]), x=dat.stt[as.character(i), "rep2004"])$corrected
  D$vote2008[ok] <- CorrectWeighted(a=D$vote2008.M[ok], w=D$pop2008[ok]/sum(D$pop2008[ok]), x=dat.stt[as.character(i), "rep2008"])$corrected
}

save.image("MRP-20120715.RData")

########################################################################################
########################################################################################
########################################################################################
### Graph Helper Functions

library(arm)
library(car)
library(mapReduce)
library(maps)

remove(list=objects())
setwd("~/Documents/Gelman Research/Replication/Original Research/")
load("MRP-20120715.RData")

ColorAlpha <- function(x, alpha=1)
  sapply(x, function(i) {
    tmp <- col2rgb(i) / 255
    rgb(tmp[1], tmp[2], tmp[3], alpha=alpha)
  })

ColorGroups <- function(x, xSeq, color1, color2, color3, alpha=1) {
  x <- round(x)
  x[x < xSeq[1]] <- xSeq[1]
  x[x > xSeq[2]] <- xSeq[2]
  x <- x - xSeq[1]+1
  xCol <- colorRampPalette(c(color1, color2, color3), space="Lab")(xSeq[2]-xSeq[1]+1)
  xCol <- ColorAlpha(xCol)
  return(xCol[x])
}

PlotBubble <- function(x, y, sz, bg="white", fg="black",
             tit="", xlab="", ylab="", axis.x=F, axis.y=F, xlim=c(0,1), ylim=c(0,1), ...) {
  n <- length(x)
  ord <- order(sz, decreasing=T)
  if (length(bg)==1)
    bg <- rep(bg,n)
  if (length(fg)==1)
    fg <- rep(fg,n)
  
  plot(0,0,type="n",xlim=xlim,ylim=ylim,xaxs="i",yaxs="i",axes=F,xlab=xlab,ylab=ylab,main=tit, cex.main=1.4, ...)
  symbols(x[ord], y[ord], circles=sz[ord], bg=bg[ord], fg=fg[ord], add=T, inches=F, lwd=0.5)
  box(lwd=0.5)
  segments(mean(xlim),ylim[1],mean(xlim),ylim[2],lty=3,col="grey",lwd=0.5)
  segments(xlim[1],mean(ylim),xlim[2],mean(ylim),lty=3,col="grey",lwd=0.5)
  if (axis.x)
    axis(1,at=c(xlim[1],mean(xlim),xlim[2]), cex.axis=1.2, tck=-0.02, lwd=0.5)
  if (axis.y)
    axis(2,at=c(ylim[1],mean(ylim),ylim[2]), cex.axis=1.2, tck=-0.02, lwd=0.5)
}

PlotMap <- function(sttColors, tit="", cex.main=0.9) {
  sttPlot <- as.data.frame(1:51)
  colnames(sttPlot) <- "stt"
  sttPlot$sttColors <- sttColors

  sttLabelLong <- as.data.frame(1:51)
  colnames(sttLabelLong) <- "stt"
  sttLabelLong$long <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")

  mapStt <- as.data.frame(c(1,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,22,22,23,23,24,25,26,27,28,29,30,31,32,33,33,33,33,34,34,34,35,36,37,38,39,40,41,42,43,44,45,46,47,47,47,48,48,48,48,48,49,50,51))
  colnames(mapStt) <- "stt"
  mapStt <- merge(mapStt, sttLabelLong, all.x=T)
  mapStt <- merge(mapStt, sttPlot, all.x=T)
  mapStt$fg <- ifelse(is.na(mapStt$sttColors), "gray94", "gray60")

  map('state', proj='bonne', param=25, lty=0)
  m <- map('state', proj='bonne', param=25, fill=TRUE, plot=FALSE, lty=0, interior=FALSE, boundary=FALSE, add=TRUE)
  polygon(m$x,m$y, col=mapStt$sttColors, lwd=0.1, border=mapStt$fg)
  
  mapStt$fg <- ifelse(is.na(mapStt$sttColors), NA, "gray30")
  polygon(m$x,m$y, col=mapStt$sttColors, lwd=0.5, border=mapStt$fg)

  title(tit, cex.main=cex.main)
}

PlotLegend <- function(cols, lab=NULL, at=NULL, horizontal=T) {
  n <- length(cols)
  if (horizontal==T) {
    plot(0,0,type="n",xlab="",ylab="",axes=F, xlim=c(-0.05,1.05), ylim=c(-0.5,0.5), yaxs="i", xaxs="i")
    for (i in 1:n) {
      x1 <- (i-1)/n
      x2 <- i/n
      rect(x1, 0, x2, 0.5, col=cols[i], border=NA)
    }
    if (!is.null(lab))
      for (i in 1:length(lab))
        text((at[i]-1)/n, -0.25, lab[i], adj=0, cex=1.3)
  }
  if (horizontal==F) {
    plot(0,0,type="n",xlab="",ylab="",axes=F, xlim=c(-0.5,1.5), ylim=c(-0.05, 1.05), yaxs="i", xaxs="i")
    for (i in 1:n) {
      x1 <- (i-1)/n
      x2 <- i/n
      rect(-0.5, x1, 0, x2, col=cols[i], border=NA)
    }
    if (!is.null(lab))
      for (i in 1:length(lab))
        text(0.25, (at[i]-1)/n, lab[i], adj=0, cex=1.3)
  }
}

########################################################################################
### figure 1 -- parallel coordinates plot

M1 <- glmer(rvote ~ inc + (1 | stt), weights=weight, data=dat.vot, 
            subset=dat.vot$file=="2008-pew" & dat.vot$eth==1,
            family=binomial, link="logit", verbose=T)

M2 <- glmer(rvote ~ inc + (1 + inc | stt), weights=weight, data=dat.vot, 
            subset=dat.vot$file=="2008-pew" & dat.vot$eth==1,
             family=binomial, link="logit", verbose=T)

dat.pc <- as.data.frame(expand.grid(1:n.stt, 1:n.inc))
colnames(dat.pc) <- c("stt", "inc")
dat.pc$M3 <- dat.pc$M2 <- dat.pc$M1 <- dat.pc$raw <- NA
for (i in 1:nrow(dat.pc)) {
  cat(paste(i, "of", nrow(dat.pc), "\n"))
  ok <- dat.vot$file=="2008-pew" & dat.vot$eth==1 & dat.vot$stt==dat.pc$stt[i] & dat.vot$inc==dat.pc$inc[i]
  dat.pc$raw[i] <- WMean(dat.vot$rvote[ok], w=dat.vot$weight[ok])
  dat.pc$M1[i] <- invlogit(fixef(M1)["(Intercept)"] + fixef(M1)["inc"]*dat.pc$inc[i] +
            ranef(M1)$stt[as.character(dat.pc$stt[i]),"(Intercept)"])
  dat.pc$M2[i] <- invlogit(fixef(M2)["(Intercept)"] + fixef(M2)["inc"]*dat.pc$inc[i] +
            ranef(M2)$stt[as.character(dat.pc$stt[i]),"(Intercept)"] + 
            ranef(M2)$stt[as.character(dat.pc$stt[i]),"inc"]*dat.pc$inc[i])
}

png("fig1.png", width=6.5, height=3, units="in", res=300)
par(mfrow=c(1,3), pty="s", mar=c(3.5,0,2.3,0), mgp=c(2,0.5,0))

stt.col <- ColorGroups(dat.stt$rep2008*100, xSeq=c(25,75), 
                       color1="dark blue", color2="grey70", color3="dark red")
plot.lab <- c("Raw Values", "Income coefficient\nconsistent across states", "Income coefficient\nvarying by state")
bold <- stt.label %in% c("MS", "OH", "CT")
for (i.plot in 1:3) {
  plot(0,0,type="n", xlim=c(0.5,5.5), ylim=c(0,1), xlab="", ylab="", axes=F, main=plot.lab[i.plot], cex.main=0.9)
  axis(1, at=1:5, lab=inc.label, cex.axis=0.9, tck=-0.01,las=2, lwd=0.5)
  for (i in 1:5)
    segments(i,0,i,1,col="grey", lwd=0.5)
  segments(1, 0.5, 5, 0.5, lty=2, col="grey", lwd=0.5)

  for (i.stt in which(!bold))
    points(1:5, dat.pc[dat.pc$stt==i.stt, i.plot+2], type="l", col=stt.col[i.stt], lwd=0.1)
  text(0.9, dat.pc[dat.pc$inc==1 & !bold, i.plot+2], stt.label[!bold], adj=1, cex=0.5, col="grey")
  text(5.1, dat.pc[dat.pc$inc==5 & !bold, i.plot+2], stt.label[!bold], adj=0, cex=0.5, col="grey")

  for (i.stt in which(bold))
    points(1:5, dat.pc[dat.pc$stt==i.stt, i.plot+2], type="l", col=stt.col[i.stt], lwd=2)
  text(0.9, dat.pc[dat.pc$inc==1 & bold, i.plot+2], stt.label[bold], adj=1, cex=0.7, col="black", font=2)
  text(5.1, dat.pc[dat.pc$inc==5 & bold, i.plot+2], stt.label[bold], adj=0, cex=0.7, col="black", font=2)
}
dev.off()

########################################################################################
### figure 2 -- model checking

D$plot.grp <- apply(D[, c("stt", "inc")], 1, paste, collapse="_")
D2 <- as.data.frame(mapReduce(data=D, map=plot.grp,
                              stt=unique(stt),
                              inc=unique(inc),
                              vote2008=sum(vote2008*pop2008)/sum(pop2008)))
D3 <- as.data.frame(mapReduce(data=D[D$eth==1,], map=plot.grp,
                              stt=unique(stt),
                              inc=unique(inc),
                              vote2008=sum(vote2008*pop2008)/sum(pop2008)))

stt.inc <- as.data.frame(expand.grid(sort(unique(D2$stt)), sort(unique(D2$inc))))
colnames(stt.inc) <- c("stt", "inc")
stt.inc$fit2 <- stt.inc$se2 <- stt.inc$mu2 <- 
stt.inc$fit1 <- stt.inc$se1 <- stt.inc$mu1 <- NA
for (i in 1:nrow(stt.inc)) {
  ok <- which(dat.vot$stt==stt.inc$stt[i] & dat.vot$inc==stt.inc$inc[i] & dat.vot$year==2008)
  if (length(ok) > 1) {
    stt.inc$fit1[i] <- D2$vote2008[D2$stt==stt.inc$stt[i] & D2$inc==stt.inc$inc[i]]
    stt.inc$mu1[i] <- min(0.98, max(0.02, WMean(dat.vot$rvote[ok], w=dat.vot$weight[ok])))
    stt.inc$se1[i] <- sqrt((stt.inc$fit1[i]*(1-stt.inc$fit1[i])) / length(ok))
  }
  ok <- which(dat.vot$stt==stt.inc$stt[i] & dat.vot$inc==stt.inc$inc[i] & dat.vot$year==2008 & dat.vot$eth==1)
  if (length(ok) > 1) {
    stt.inc$fit2[i] <- D3$vote2008[D3$stt==stt.inc$stt[i] & D3$inc==stt.inc$inc[i]]
    stt.inc$mu2[i] <- min(0.98, max(0.02, WMean(dat.vot$rvote[ok], w=dat.vot$weight[ok])))
    stt.inc$se2[i] <- sqrt((stt.inc$fit2[i]*(1-stt.inc$fit2[i])) / length(ok))
  }
}

png ("fig2.png", height=10, width=10, res=300, units="in")
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

########################################################################################
### figure 3 -- demographic expansion

png("fig3.png",width=500*6.5,height=500*2.75,res=300)

nf <- layout(matrix(c(1,1,1,1,
                      2,4,5,6,
                      3,3,3,3),nrow=3,byrow=T),
       widths=c(0.5,8,8,8),
       heights=c(1,8,0.5))
# layout.show(nf)

par(mar=c(0,0,0,0))
par(pty="m")
plot(0,0,type="n",xlab="",ylab="",axes=F)
text(0,0.4,"Turnout and Vote Choice for Population Subgroups, 2008", cex=1.7, font=2)
text(0,-0.5,"Size=Subgroup population size 2007;  Color by Ethnicity: White=White, Black=Black, Red=Hispanic, Green=Other", cex=1.5)

plot(0,0,type="n",xlab="",ylab="",axes=F)
text(0,0,"Turnout 2008",srt=90,font=2, cex=1.4)

plot(0,0,type="n",xlab="",ylab="",axes=F)
text(0,0,"McCain Vote 2008",font=2, cex=1.4)

par(mar=c(2,1.5,2,1))
par(mgp=c(2,0.5,0))
par(pty="s")

D$grp.plot <- apply(D[, c("stt", "eth")], 1, paste, collapse="_")
D.plot <- as.data.frame(mapReduce(data=D, map=grp.plot,
                                  eth=unique(eth),
                                  vote2008=sum(vote2008*turn2008*pop2008) / sum(turn2008*pop2008),
                                  turn2008=sum(turn2008*pop2008) / sum(pop2008),
                                  pop2008=sum(pop2008)))
PlotBubble(x=D.plot$vote2008,
           y=D.plot$turn2008,
           sz=sqrt(D.plot$pop2008)/15000,
           bg=ColorAlpha(recode(D.plot$eth,"1='white';2='black';3='red';4='green'"),alpha=0.75),
           fg=ColorAlpha("grey50",alpha=0.5),
           tit="State x Ethnicity",
           axis.x=T, axis.y=T)

D$grp.plot <- apply(D[, c("stt", "eth", "inc")], 1, paste, collapse="_")
D.plot <- as.data.frame(mapReduce(data=D, map=grp.plot,
                                  eth=unique(eth),
                                  vote2008=sum(vote2008*turn2008*pop2008) / sum(turn2008*pop2008),
                                  turn2008=sum(turn2008*pop2008) / sum(pop2008),
                                  pop2008=sum(pop2008)))
PlotBubble(x=D.plot$vote2008,
           y=D.plot$turn2008,
           sz=sqrt(D.plot$pop2008)/15000,
           bg=ColorAlpha(recode(D.plot$eth,"1='white';2='black';3='red';4='green'"),alpha=0.75),
           fg=ColorAlpha("grey50",alpha=0.5),
           tit="State x Ethnicity x Income",
           axis.x=T)

D$grp.plot <- apply(D[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_")
D.plot <- as.data.frame(mapReduce(data=D, map=grp.plot,
                                  eth=unique(eth),
                                  vote2008=sum(vote2008*turn2008*pop2008) / sum(turn2008*pop2008),
                                  turn2008=sum(turn2008*pop2008) / sum(pop2008),
                                  pop2008=sum(pop2008)))
PlotBubble(x=D.plot$vote2008,
           y=D.plot$turn2008,
           sz=sqrt(D.plot$pop2008)/15000,
           bg=ColorAlpha(recode(D.plot$eth,"1='white';2='black';3='red';4='green'"),alpha=0.75),
           fg=ColorAlpha("grey50",alpha=0.5),
           tit="State x Ethnicity x Income x Age",
           axis.x=T)

dev.off()

########################################################################################
### figure 4 -- vote shift maps

### denote which groups are > 1% of the state
ok.grp <- rep(F, nrow(D))
for (i in 1:n.stt) {
  ok <- D$stt==i
  tmp <- D$pop2008[ok] * D$turn2008[ok]
  tmp <- which(tmp >= sum(tmp)/100)
  ok.grp[ok][tmp] <- T
}

png("fig4.png", width=500*6.5, height=500*5.5, units="px", res=300)

nf <- layout(matrix(c( 0, 1, 1, 1, 1,31,
             0, 2, 3, 4, 5,31,
             6,11,16,21,26,31,
             7,12,17,22,27,31,
             8,13,18,23,28,31,
             9,14,19,24,29,31,
            10,15,20,25,30,31),nrow=7,ncol=6,byrow=T),
       widths=c(1,4,4,4,4,2),
       heights=c(1,1,2.8,2.8,2.8,2.8,2.8),
       respect=T)
# layout.show(nf)
par(mar=c(0,0,0,0))
plot(0,0,type="n",xlab="",ylab="",axes=F)
text(0,0.4,"McCain 2008 minus Bush 2004 among whites", cex=1.7, font=2)
text(0,-0.5,"Red=McCain better than Bush;  Blue=McCain worse than Bush;  Only groups with > 1% of state voters shown", cex=1.5)
for (i in 1:n.age) {
  plot(0,0,type="n",xlab="",ylab="",axes=F)
  text(0,0,age.label[i], font=2, cex=1.4)
}
for (i in 1:n.inc) {
  plot(0,0,type="n",xlab="",ylab="",axes=F)
  text(0,0,inc.label[i], font=2,srt=90, cex=1.4)
}

par(mar=c(0,0,0,0))
for (i.age in 1:n.age)
  for (i.inc in 1:n.inc) {
    ok <- D$age==i.age & D$inc==i.inc & D$eth==1
    x <- ColorGroups(x=(D$vote2008[ok]-D$vote2004[ok])*100, xSeq=c(-25,25), 
                     color1="dark blue", color2="white", color3="dark red", alpha=0.75)
    x[ok.grp[ok]==F] <- NA
    PlotMap(x)
  }

par(mar=c(0,1,0,0))
PlotLegend(cols=ColorAlpha(colorRampPalette(c("dark blue", "white", "dark red"))(51), 0.75),
           lab <- c("-25%","0%","+25%"), at <- c(2,26,51), horizontal=F)

dev.off()

########################################################################################
### figure 5 -- turnout swing

png("fig5.png", width=500*6.5, height=500*6.5*19/18, units="px", res=300)

agg.mean <- sum(D$pop2008*D$turn2008)/sum(D$pop2008) - 
  sum(D$pop2004*D$turn2004)/sum(D$pop2004)

nf <- layout(matrix(c( 0, 1, 1, 1, 1, 0, 0,
             0, 2, 3, 4, 5, 0, 0,
             6,16,20,24,28,12,11,
             7,17,21,25,29,13,11,
             8,18,22,26,30,14,11,
             9,19,23,27,31,15,11,
             0,10,10,10,10, 0, 0),nrow=7,ncol=7,byrow=T),
       widths=c(0.5,4,4,4,4,0.25,1.25),
       heights=c(1,0.5,4,4,4,4,1.5),
       respect=T)
# layout.show(nf)

par(mar=c(0,0,0,0), pty="m")
plot(0,0,type="n",xlab="",ylab="",axes=F)
text(0,0.4,"Turnout Swing Mainly Isolated to African Americans and Young Minorities", cex=1.7, font=2)
text(0,-0.3,"Turnout change as line graph, histogram (from the census) as bars", cex=1.5)
for (i in 1:n.eth) {
  plot(0,0,type="n",xlab="",ylab="",axes=F)
  text(0,0,eth.label[i], font=2, cex=1.4)
}
for (i in 1:n.age) {
  plot(0,0,type="n",xlab="",ylab="",axes=F)
  text(0,0,age.label[i], font=2,srt=90, cex=1.4)
}

plot(0,0,type="n",xlab="",ylab="",axes=F)
text(0,-0.7,"Household Income", font=2, cex=1.4)
plot(0,0,type="n",xlab="",ylab="",axes=F)
text(0,0,"Turnout 2008 - Turnout 2004", font=2, srt=90, cex=1.4)

for (i in 1:n.age) {
  par(mar=c(0.6,0,0.6,0))
  plot(0,0,type="n",xlab="",ylab="",axes=F,xlim=c(0,1),ylim=c(-0.01,0.2),xaxs="i",yaxs="i")
  axis(4, at=c(0,0.1,0.2), cex.axis=1.2, tcl=-0.25, mgp=c(3,0.25,0))
}

par(mar=c(0.6,0.6,0.6,0.6),pty="s")
for (i.eth in 1:n.eth)
  for (i.age in 1:n.age) {
    ok <- D$eth==i.eth & D$age==i.age
    ret <- t(sapply(1:n.inc, function(i.inc) {
      ok2 <- ok & D$inc==i.inc
        c(sum(D$pop2004[ok2]),
        sum(D$pop2004[ok2]*D$turn2004[ok2])/sum(D$pop2004[ok2]),
        sum(D$pop2008[ok2]),
        sum(D$pop2008[ok2]*D$turn2008[ok2])/sum(D$pop2008[ok2]))
    }))
    for (i.col in c(1,3))
      ret[,i.col] <- ret[,i.col] / sum(ret[,i.col]) / 2

    grpMean <- sum(D$pop2008[ok]*D$turn2008[ok])/sum(D$pop2008[ok]) - 
      sum(D$pop2004[ok]*D$turn2004[ok])/sum(D$pop2004[ok])
    col1 <- ifelse(grpMean > 0.05, "light blue", "grey")
    col2 <- ifelse(grpMean > 0.05, "dark blue", "black")
    x <- barplot(ret[,3],border=NA, col=col1,
      xlab="", ylab="", main="",axes=F, ylim=c(-0.01,0.2))
    points(x, ret[,4]-ret[,2], type="o", pch=20, col=col2)
    box(lwd=ifelse(grpMean > 0.05, 2.5, 1), col=ifelse(grpMean > 0.05, "dark blue", "black"))
    abline(h=0,lty=3)
    abline(h=agg.mean,lty=3)
    
    if (i.age == n.age)
      axis(1, at=x, labels=inc.label, las=2, cex.axis=1.2, tcl=-0.1, mgp=c(3,0.25,0))
  }

dev.off()

########################################################################################
### figure 6 -- turnout maps

D$plot.grp <- apply(D[, c("stt", "eth", "age")], 1, paste, collapse="_")
D3 <- as.data.frame(mapReduce(data=D, map=plot.grp,
                    stt=unique(stt), eth=unique(eth), age=unique(age),
                    pop2008=sum(pop2008),
                    turn2008=sum(pop2008*turn2008) / sum(pop2008)))

### denote which groups are > 1% of the state
ok.grp <- rep(F, nrow(D3))
for (i in 1:n.stt) {
  ok <- D3$stt==i
  tmp <- D3$pop2008[ok]
  tmp <- which(tmp >= sum(tmp)/100)
  ok.grp[ok][tmp] <- T
}
D3$ok <- ok.grp
D3 <- D3[order(D3$stt*100 + D3$eth + D3$age),]

png("fig6.png",width=500*6.5,height=500*4.5,res=300)

nf <- layout(matrix(c( 0, 1, 1, 1, 1,26,
             0, 6, 7, 8, 9,26,
             2,10,11,12,13,26,
             3,14,15,16,17,26,
             4,18,19,20,21,26,
             5,22,23,24,25,26),nrow=6,ncol=6,byrow=T),
       widths=c(1,4,4,4,4,2),
       heights=c(1,1,2.8,2.8,2.8,2.8),
       respect=T)
# layout.show(nf)
par(mar=c(0,0,0,0))
plot(0,0,type="n",xlab="",ylab="",axes=F)
text(0,0.4,"Turnout among Young Whites and Minorities Over-Emphasized", cex=1.7, font=2)
text(0,-0.5,"Only groups with > 1% of state population shown", cex=1.5)
for (i in 1:n.age) {
  plot(0,0,type="n",xlab="",ylab="",axes=F)
  text(0,0,age.label[i], font=2,srt=90, cex=1.4)
}
for (i in 1:n.eth) {
  plot(0,0,type="n",xlab="",ylab="",axes=F)
  text(0,0,eth.label[i], font=2, cex=1.4)
}

par(mar=c(0,0,0,0))
for (i.age in 1:n.age)
  for (i.eth in 1:n.eth) {
    ok <- D3$age==i.age & D3$eth==i.eth
    x <- ColorGroups(x=(D3$turn2008[ok])*100, xSeq=c(35,85),
                         color1="dark red", color2="white", color3="dark green", alpha=0.75)
    x[!D3$ok[ok]] <- NA
    PlotMap(x)
  }

par(mar=c(0,1,0,0))
PlotLegend(cols=ColorAlpha(colorRampPalette(c("dark red", "white", "dark green"))(51), 0.75),
           lab=c("35%","60%","85%"), at=c(2,26,51), horizontal=F)

dev.off()
