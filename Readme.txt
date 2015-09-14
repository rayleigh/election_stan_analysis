This holds the R code needed to do a replication of Ghitza and Gelman's 2013 paper using Stan.

The code is divided at two levels. First, the code is split into R Code folders based on the code's purpose. Any other folder are just to hold images, data files, or Stan code files. Then, if there are multiple files within the folder, these are divided into R scripts (the R file(s) with "script" in its name) and R helper function files. The R scripts call functions in the R helper function files. So, the code that should be run within each folder is the R script file.

The script files used and the order they should be called in is all laid out in the master_R_script.R file.

