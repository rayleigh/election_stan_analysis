library(dplyr)
library(arm)
library(car)

generate_fig_2_for_2008 <- function(data_matrix, dat.stt, dat.vot)
{
  data_matrix$plot.grp <- apply(data_matrix[, c("stt", "inc")], 1, paste, collapse="_")
  
  D2 <- summarize(group_by(data_matrix, plot.grp), 
                  vote2008 = sum(vote2008 * pop2008)/sum(pop2008), 
                  stt = unique(stt), 
                  inc = unique(inc))

  D3 <- summarize(group_by(data_matrix[data_matrix$eth==1,], plot.grp), 
                  vote2008 = sum(vote2008 * pop2008)/sum(pop2008), 
                  stt = unique(stt), 
                  inc = unique(inc))
  
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
  
  plot_title_file = "2008_stan_fig2.png"
  mtext_line1 <- "2008 election:  McCain share of the two-party vote in each income category\nwithin each state among all voters (gray) and just non-Hispanic whites (orange)"
  mtext_line2 <- " Dots are weighted averages from pooled June-Nov Pew surveys; error bars show +/- 1 s.e. bounds.\n Curves are estimated using multilevel models and have a s.e. of about 3% at each point.\n States are ordered in decreasing order of McCain vote (Alaska, Hawaii, and D.C. excluded)."   
  generate_fig_2(stt.inc, dat.stt, plot_title_file, mtext_line1, mtext_line2)
}

generate_fig_2_for_2004 <- function(data_matrix, dat.stt, dat.vot, plot_title_file = "2004_stan_fig2.png")
{
  data_matrix$plot.grp <- apply(data_matrix[, c("stt", "inc")], 1, paste, collapse="_")
  
  D2 <- summarize(group_by(data_matrix, plot.grp), 
                  vote2004 = sum(vote2004 * pop2004)/sum(pop2004), 
                  stt = unique(stt), 
                  inc = unique(inc))
  
  D3 <- summarize(group_by(data_matrix[data_matrix$eth==1,], plot.grp), 
                  vote2004 = sum(vote2004 * pop2004)/sum(pop2004), 
                  stt = unique(stt), 
                  inc = unique(inc))
  
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
  
  mtext_line1 <- "2004 election:  Bush share of the two-party vote in each income category\nwithin each state among all voters (gray) and just non-Hispanic whites (orange)"
  mtext_line2 <- " Dots are weighted averages from pooled June-Nov Annenberg surveys; error bars show +/- 1 s.e. bounds.\n Curves are estimated using multilevel models and have a s.e. of about 3% at each point.\n States are ordered in decreasing order of the 2008 Mcain vote (Alaska, Hawaii, and D.C. excluded)."   
  generate_fig_2(stt.inc, dat.stt, plot_title_file, mtext_line1, mtext_line2)
}

generate_fig_2 <- function(stt.inc, dat.stt, fig_file_name, mtext_line1, mtext_line2)
{
  stt.label <- c(state.abb[1:8], "DC", state.abb[9:50])
  
  png (fig_file_name, height=10, width=10, res=300, units="in")
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
  mtext (mtext_line1, outer=TRUE, cex=1, side=3, line=1)
  mtext (mtext_line2, outer=TRUE, cex=.9, side=1, line=5)
  dev.off()
}


