#Get data 1
dir1 = "~/Documents/Gelman Research/Replication/Original Research/"
file1 = "MRP-201207152.RData"
setwd(dir1)
load(file1)
orig_M.cps <- M.cps
orig_M.vot <- M.vot

#Get subsequent data
dir2 = "~/Documents/Gelman Research/Replication/Original Research Round/"
file2 = "MRP-20120715.RData"
setwd(dir2)
load(file2)
new_M.cps <- M.cps
new_M.vot <- M.vot

#Generate plot data
outputDir="~/Documents/Gelman Research/Replication/Subsequent Research/Boxplot Compare/Original v round/Var Plot/"
source("~/Documents/Gelman Research/Replication/Subsequent Research/R code/plot_coef_diff.R")

setwd(outputDir)

num_years <- length(orig_M.cps)
year_list <- c(2004, 2008)
par(new = "F")
#png(filename = "Overall_plot.png")
#plot(-1, 0, type = "p", main = "Overall", xlim=c(-1, 2000), ylim=c(-0.2, 0.2), xlab = "", ylab = "Difference", axes = F)
#axis(2)
#par(new = "T")
sapply(1:num_years, function(i) {gen_box_plot_for_coef_dif(orig_M.cps[[i]], new_M.cps[[i]], paste("CPS", year_list[i], sep="_"))})
sapply(1:num_years, function(i) {gen_box_plot_for_coef_dif(orig_M.vot[[i]], new_M.vot[[i]], paste("Vote", year_list[i], sep="_"))})
#dev.off()