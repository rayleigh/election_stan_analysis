library(rstan)

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

#Set up plot settings
source("~/Documents/Gelman Research/Replication/Subsequent Research/R code/plot_fixed_and_rand_effects.R")
pointstyle_list <- c(1, 2, 3)
colstyle_list <- c("blue", "red", "green")

# #Generate plot for comparison between original 
# output_dir="~/Documents/Gelman Research/Replication/Subsequent Research/Boxplot Compare/Original v round/Fixed and Random Plot/"
# years <- c(2004, 2008)
# models <- c("CPS", "Vot")
# 
# for (i in 1:length(years))
# {
#   for (j in 1:length(models))
#   {
#     year = years[i]
#     model = models[j]
#     folder_name = paste(model, year, sep = " ")
#     output_folder = paste(output_dir, folder_name, sep = "")
#     setwd(output_folder)
#     if (j == 1)
#     {
#       compare_list <- list(orig_M.cps[[i]], new_M.cps[[i]])
#     }
#     else
#     {
#       compare_list <- list(orig_M.vot[[i]], new_M.vot[[i]])
#     }
#     plot_fixed_effects(compare_list, list(), colstyle_list, pointstyle_list)
#     plot_rand_effects(compare_list, list(), colstyle_list, pointstyle_list)
#   }
# }

#Generate plot for comparison between original 
output_dir="~/Documents/Gelman Research/Replication/Subsequent Research/Boxplot Compare/Original v stan/"
stan_dir_template="~/Documents/Gelman Research/Replication/Subsequent Research/Stan/Level_3"
years <- c(2004, 2008)
models <- c("CPS", "Vot")

for (i in 1:length(years))
{
  for (j in 1:length(models))
  {
    year = years[i]
    model = models[j]
    
    stan_data_dir <- paste(stan_dir_template, year, model, sep = "_")
    setwd(stan_data_dir)
    cps_rstan <- read_stan_csv(csvfiles = c('output.csv','output2.csv','output3.csv','output4.csv'))
    stan_list <- list(cps_rstan)
    
    if (j == 1)
    {
      compare_list <- list(orig_M.cps[[i]], new_M.cps[[i]])
    }
    else
    {
      compare_list <- list(orig_M.vot[[i]], new_M.vot[[i]])
    }
    
    #Set output directory
    folder_name = paste(model, year, sep = " ")
    output_folder = paste(output_dir, folder_name, sep = "")
    setwd(output_folder)
    
    plot_fixed_effects(compare_list, stan_list, colstyle_list, pointstyle_list)
    plot_rand_effects(compare_list, stan_list, colstyle_list, pointstyle_list)
  }
}
