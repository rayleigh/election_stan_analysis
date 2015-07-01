library(dplyr)

create_online_data_matrix_from_questions <- function(naes_online_data_matrix, naes_weights_data_matrix)
{
  #Parse online data frame
  
  #Create lists to help parse the data
  #Age, Income, Race/ethnicity, State of residency, Would vote today for Obama/McCain, Would vote for Republican
  online_interested_questions <- c("WA02", "WA05", "WC01", "WFc01", "RCa02", "RCa01", "RKEY")
  naes_online_eth <- 1:5
  online_naes_eth_translate <- c(1, 2, 4, 3, 4)
  naes_online_inc <- 1:19
  online_naes_inc_translate <- c(rep(1, 6), rep(2, 4), rep(3, 3), rep(4, 4), rep(5, 2))  
  online_voting_pref_questions <- c(sapply(4:1, function(i) {paste("RCa02", i, sep="_")}, simplify = T), sapply(2:1, function(i) {paste("RCa01", i, sep = "_")}, simplify = T))
  
  #Create matrix with only columns we're interested in
  online_interested_data_matrix <- get_data_matrix_with_only_interested_cols(online_interested_questions, naes_online_data_matrix)
  
  online_interested_data_matrix$"age" <- sapply(online_interested_data_matrix$WA02_a, translate_age, simplify = T)
  online_interested_data_matrix$"eth" <- plyr::mapvalues(online_interested_data_matrix$WC01_a, naes_online_eth, online_naes_eth_translate)
  online_interested_data_matrix$"inc" <- plyr::mapvalues(online_interested_data_matrix$WA05_a, naes_online_inc, online_naes_inc_translate)
  online_interested_data_matrix$"stt" <- translate_state(online_interested_data_matrix$WFc01_a)

  online_interested_data_matrix <- get_data_matrix_with_only_col_pref_known(online_voting_pref_questions, c(1:2), "vote", online_interested_data_matrix)
  online_interested_data_matrix$"vote"[online_interested_data_matrix$"vote" == 2] = 0

  online_interested_data_matrix <- merge(x = online_interested_data_matrix, y = naes_weights_data_matrix, by = "RKEY", all.x = T, all.y = F)
  online_interested_data_matrix$weight <- sapply(1:dim(online_interested_data_matrix)[1], function(i) {translate_question_to_weight(online_interested_data_matrix$resp_question[i], online_interested_data_matrix[i,])})
  
  online_interested_data_matrix <- online_interested_data_matrix[!is.na(rownames(online_interested_data_matrix)),]
  
  online_interested_data_matrix$weight <- online_interested_data_matrix$weight/(mean(online_interested_data_matrix$weight))
  
  return(online_interested_data_matrix)
}

create_phone_data_from_database <- function(naes_phone_data_matrix)
{
  #Parse phone data 
  phone_voting_pref_questions <- sapply(8:3, function(i) {paste("RCa0", i, sep="")}, simplify = T)
  phone_voting_pref_questions <- sapply(phone_voting_pref_questions, function(question) {paste(question, "c", sep = "_")}, simplify = T, USE.NAMES = F)
  core_identity_questions <- c("WA02", "WA04", "WA05", "WC01", "WC03", "WFc01")
  core_identity_questions <- sapply(core_identity_questions, function(question) {paste(question, "c", sep = "_")}, simplify = T, USE.NAMES = F)
  phone_interested_cols <- c(core_identity_questions, "WA03", "WB01", phone_voting_pref_questions)
  
  naes_phone_interested_matrix <- get_data_matrix_with_only_interested_cols(phone_interested_cols, naes_phone_data_matrix)
  naes_phone_interested_matrix <- get_data_matrix_without_missing_vals_cols(core_identity_questions, naes_phone_interested_matrix)
  naes_phone_interested_matrix <- get_data_matrix_with_only_col_pref_known(phone_voting_pref_questions, c(1:2), "vote", naes_phone_interested_matrix)
  naes_phone_interested_matrix$vote[naes_phone_interested_matrix$vote == 2] = 0
  
  naes_phone_interested_matrix$"age" <- sapply(naes_phone_interested_matrix$WA02_c, translate_age, simplify = T)
  naes_phone_interested_matrix$"stt" <- translate_state(naes_phone_interested_matrix$WFc01_c)
  naes_phone_interested_matrix$"eth" <- sapply(1:length(naes_phone_interested_matrix$WC01_c), function(i) {translate_phone_eth(naes_phone_interested_matrix$WC01_c[i], naes_phone_interested_matrix$WC03_c[i])})
  naes_phone_interested_matrix <- get_data_matrix_with_only_col_pref_known(c("WA04_c", "WA05_c"), 1:10, "inc", naes_phone_interested_matrix, F)
  naes_phone_interested_matrix$inc[naes_phone_interested_matrix$inc == -1] <- NA
  naes_phone_interested_matrix$inc <- sapply(naes_phone_interested_matrix$inc, function(val) {ceiling(val/2)}) 
  naes_phone_interested_matrix$weight <- 1
  naes_phone_interested_matrix <- rename(naes_phone_interested_matrix, 
                                         edu = WA03_c,
                                         emp = WB01_c)
  naes_phone_interested_matrix <- naes_phone_interested_matrix[!is.na(rownames(naes_phone_interested_matrix)),]
  
  tmp_phone_missing_data_matrix <- naes_phone_interested_matrix[, c("age", "inc", "edu", "emp")]
  tmp_phone_missing_data_matrix$edu[tmp_phone_missing_data_matrix$edu > 997] <- NA
  tmp_phone_missing_data_matrix$emp[tmp_phone_missing_data_matrix$emp > 997] <- NA
  
  naes_phone_missing_matrix <- missing_data.frame(tmp_phone_missing_data_matrix)
  naes_phone_missing_matrix <- change(naes_phone_missing_matrix, y = c("edu", "emp"), what = "type", to = c("ordered-categorical", "ordered-categorical"))

  #Run imputation 10 times, grab last chain of 10
  naes_imputed_data <- lapply(1:10, function(i) impute_and_grab_data(naes_phone_missing_matrix))
  naes_phone_interested_matrix_list <- lapply(naes_imputed_data, function(naes_imputed_data_matrix) {naes_phone_interested_matrix$inc <- naes_imputed_data_matrix$inc; return(naes_phone_interested_matrix)})
  
  return(naes_phone_interested_matrix_list)
}

get_data_matrix_with_only_interested_cols <- function(interested_questions, data_matrix)
{
  col_names <- colnames(data_matrix)
  interested_cols_list <- unlist(sapply(interested_questions, function(question) {grep(question, col_names)}, simplify = T))
  return(data_matrix[, interested_cols_list])
}

translate_age <- function(age)
{
  if (age >= 18 && age < 30)
  {
    return(1)
  }
  else if (age >= 30 && age < 45)
  {
    return(2)
  }
  else if (age >= 45 && age < 65)
  {
    return(3)
  }
  else if (age >= 65)
  {
    return(4)
  }
  else
  {
    return(NA)
  }
}

translate_state <- function(survey_state_col)
{
  survey_stt <- c(1, 2, 4:6, 8:13, 15:42, 44:51, 53:56)
  stt_translate <- 1:51
  
  return(plyr::mapvalues(survey_state_col, survey_stt, stt_translate))
}

translate_state_to_reg <- function(survey_state_col)
{
  survey_stt <- 1:51
  reg_translate <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)
  
  return(plyr::mapvalues(survey_state_col, survey_stt, reg_translate))
}

translate_phone_eth <- function(hispanic_origin_answer, race_answer)
{
  #Is Hispanic
  if (hispanic_origin_answer == 1 | (race_answer == 5))
  {
    return(3)
  }
  #Is black or white
  else if (race_answer < 3)
  {
    return(race_answer)
  }
  #Falls under other ace
  else if (race_answer < 998)
  {
    return(4)
  }
  #Don't know
  else
  {
    return(NA)
  }
}

translate_phone_inc <- function(inc_resp)
{
  if (is.na(inc_resp) | inc_resp > 997)
  {
    return(NA)
  }
  else
  {
    return(ceiling(inc_resp/2))
  }
}

translate_question_to_weight <- function(resp_question, data_matrix_row)
{
  weight_col <- paste("WT", strsplit(resp_question, "_")[[1]][2], sep = "_")
  weight <- data_matrix_row[[weight_col]]
  if (is.na(weight))
  {
    return(1)
  }
  else
  {
    return(weight)
  }
}

get_data_matrix_with_only_col_pref_known <- function(questions_list, interested_resps, col_name, data_matrix, remove_pref_unknown = T)
{
  data_matrix[[col_name]] <- -1
  data_matrix$"resp_question" <- "Don't know"
  rows_with_known_resps_to_question <- c()
  for (i in 1:length(questions_list))
  {
    question <- questions_list[i]
    for (j in 1:length(interested_resps))
    {
      response <- interested_resps[j]
      row_has_response <- (data_matrix[[question]] == response)
      pref_resp_known_rows <- which(row_has_response & (data_matrix[[col_name]] == -1))
      data_matrix[[col_name]][pref_resp_known_rows] <- response
      data_matrix$"resp_question"[pref_resp_known_rows] <- question
      rows_with_known_resps_to_question <- c(rows_with_known_resps_to_question, pref_resp_known_rows)
    }
  }
  if (remove_pref_unknown)
  {
    return(data_matrix[sort(unique(rows_with_known_resps_to_question)),])
  }
  else
  {
    return(data_matrix)
  }
}

get_data_matrix_without_missing_vals_cols <- function(key_questions, data_matrix)
{
  row_val_is_na <- lapply(key_questions, function(question) {is.na(data_matrix[[question]])})
  row_val_is_unknown <- lapply(key_questions, function(question) {data_matrix[[question]] > 997})
  row_only_has_na <- Reduce("&", row_val_is_na)
  row_only_has_unknown <- Reduce("&", row_val_is_unknown)
  remove_row <- Reduce("|", list(row_only_has_na, row_only_has_unknown))
  return(data_matrix[!remove_row, ])
}

impute_and_grab_data <- function(missing_matrix)
{
  missing_imputations <- mi(missing_matrix, n.iter = 50, n.chains = 10)
  imputated_matrix <- complete(missing_imputations, m = 10)[[10]]
}

summarize_inc_distr <- function(data_matrix)
{
  data_matrix$"ones" <- 1
  print(summarize(group_by(data_matrix, inc), n = sum(ones)/dim(data_matrix)[1]))
}