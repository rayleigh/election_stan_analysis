#Got data from http://www.annenbergpublicpolicycenter.org/naes-data-sets/

library(dplyr)
library(mi)

naes_helper_function_filepath = "~/Documents/Gelman Research/Replication/Subsequent Research/naes_helper_functions.R"
source(naes_helper_function_filepath)

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

lapply(1:10, function(i) summarize_inc_distr(naes_phone_matrix_list[[i]]))

save.image(file = "naes_data_processed.R")

naes_phone_missing_matrix[, c("age", "inc", "edu")] <- naes_phone_imputations$inc

naes_data_matrix_list <- lapply(naes_phone_matrix_list, function(naes_phone_matrix) {rbind(naes_online_data_matrix[, key_col_names], naes_phone_matrix[, key_col_names])})
naes_data_matrix_list <- lapply(naes_data_matrix_list, function(naes_data_matrix) {transform(naes_data_matrix, age = (as.integer(age)),
                                                                                                               stt = (as.integer(stt)),
                                                                                                               eth = (as.integer(eth)),
                                                                                                               inc = (as.integer(inc)),
                                                                                                               vote = (as.integer(vote)))})
naes_data_matrix_list <- lapply(naes_data_matrix_list, function(naes_data_matrix) {naes_data_matrix$"ones" <- 1; 
                                                                                   naes_data_matrix$"grp" <- apply(naes_data_matrix[, c("stt", "eth", "inc", "age")], 1, paste, collapse="_");
                                                                                   naes_data_matrix$"reg" <- translate_state_to_reg(naes_data_matrix$"stt");
                                                                                   return(naes_data_matrix)})

predictor_matrix <- as.data.frame(expand.grid(1:n.stt, 1:n.eth, 1:n.inc, 1:n.age))
colnames(predictor_matrix) <- c("stt", "eth", "inc", "age")
predictor_matrix$age <- factor(predictor_matrix$age)
predictor_matrix$eth <- factor(predictor_matrix$eth)
predictor_matrix$inc <- factor(predictor_matrix$inc)
predictor_matrix$stt <- factor(predictor_matrix$stt)

predictor_matrix$grp <- apply(predictor_matrix, 1, paste, collapse="_")
predictor_matrix$ix <- 1:nrow(predictor_matrix)

