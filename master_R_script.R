#Set the working directory to where all the code is
base_dir = "~/Documents/Gelman Research/Replication/election_stan_analysis/" 

#Run the original analysis
setwd(base_dir)
setwd("lmer MRP R Code/Original")
source("original-mrp-replication-4.R")

#Run the rounded analysis
setwd(base_dir)
setwd("lmer MRP R Code/Rounded")
source("round-mrp-replication-4.R")

#Run the Stan analysis
setwd(base_dir)
setwd("Stan MRP R Code/")
source("mrp_replication_script2.R")

#Create the box plots
setwd(base_dir)
setwd("Boxplot Comparison R Code/Term Comparison/")
source("script_to_plot_fixed_and_rand_effects.R")

#Run the NAES data process
setwd(base_dir)
setwd("NAES R Code/")
source("naes_data_process_script.R")

#Run the NAES data analysis
setwd(base_dir)
setwd("NAES R Code/")
source("naes_stan_analysis_script.R")