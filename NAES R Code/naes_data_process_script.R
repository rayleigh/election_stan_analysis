#Got data from http://www.annenbergpublicpolicycenter.org/naes-data-sets/
#Make sure to set the filepaths to where you have the files
remove(list=objects())

library(dplyr)
library(mi)

naes_helper_function_filepath = "naes_helper_functions.R"
source(naes_helper_function_filepath)

##Make sure that these are set to your data files before running the script##
naes_online_data_filepath = "~/Documents/Gelman Research/Replication/Subsequent Research/naes08-online-all-waves-data-compact.txt"
naes_online_weights_filepath = "~/Documents/Gelman Research/Replication/Subsequent Research/naes08-online-weights.txt"
naes_phone_data_filepath = "~/Documents/Gelman Research/Replication/Subsequent Research/naes08-phone-nat-rcs-data-full.txt"

naes_online_survey_data_matrix <- read.table(naes_online_data_filepath, header = T, sep = "\t")
naes_online_weights_data_matrix <- read.table(naes_online_weights_filepath, header = T, sep = "\t")
naes_phone_data_matrix <- read.table(naes_phone_data_filepath, header = T, sep = "\t", fill = T)

key_col_names <- c("age", "inc", "eth", "stt", "vote", "weight")
reg_translate <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)

#Parse online data frame
naes_online_data_matrix <- create_online_data_matrix_from_questions(naes_online_survey_data_matrix, naes_online_weights_data_matrix)

#Parse phone data 
naes_phone_matrix_list <- create_phone_data_from_database(naes_phone_data_matrix)

#lapply(1:10, function(i) summarize_inc_distr(naes_phone_matrix_list[[i]]))

combine_data_sets = F

#Run if you want to combine data sets
if (combine_data_sets)
{
  naes_data_matrix_list <- lapply(naes_phone_matrix_list, function(naes_phone_matrix) {rbind(naes_online_data_matrix[, key_col_names], naes_phone_matrix[, key_col_names])})
} else {
  naes_data_matrix_list <- lapply(naes_phone_matrix_list, function(naes_phone_matrix) {return(naes_phone_matrix[, key_col_names])})
  naes_data_matrix_list[[(length(naes_data_matrix_list) + 1)]] <- naes_online_data_matrix[, key_col_names]
}

naes_data_matrix_list <- lapply(naes_data_matrix_list, function(naes_data_matrix) {transform(naes_data_matrix, age = (as.integer(age)),
                                                                                                               stt = (as.integer(stt)),
                                                                                                               eth = (as.integer(eth)),
                                                                                                               inc = (as.integer(inc)),
                                                                                                               vote = (as.integer(vote)))})
naes_data_matrix_list <- lapply(naes_data_matrix_list, function(naes_data_matrix) {naes_data_matrix$"ones" <- 1; 
                                                                                   naes_data_matrix$"grp" <- apply(naes_data_matrix[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_");
                                                                                   naes_data_matrix$"reg" <- translate_state_to_reg(naes_data_matrix$"stt");
                                                                                   return(naes_data_matrix)})
setwd("~/Documents/Gelman Research/Replication/Subsequent Research/Stan")
save(naes_data_matrix_list, file = "naes_data_matrix_processed.Rdata")

  
