#Generate box plot for coefficient differences between two merMod objects
gen_box_plot_for_coef_dif <- function(merMod_obj_1, merMod_obj_2, model_name)
{
  var_list <- names(coef(merMod_obj_1))

  sapply(var_list, function(var) {gen_box_plot_for_var_dif(merMod_obj_1, merMod_obj_2, var, model_name)})
}

#Generate box plot for coefficient differences between two merMod object's columns
gen_box_plot_for_var_dif <- function(merMod_obj_1, merMod_obj_2, var, model_name)
{
  coef_matrix_1 <- coef(merMod_obj_1)[[var]]
  coef_matrix_2 <- coef(merMod_obj_2)[[var]]
  col_names_list <- colnames(coef_matrix_1)
  num_cols <- length(col_names_list)
  max_val <- max(max(coef_matrix_1), max(coef_matrix_2))
  min_val <- min(min(coef_matrix_1), min(coef_matrix_2))
  point_style_1 = 1
  col_1 = "red"
  point_style_2 = 3
  col_2 = "blue"
  
  png(filename = paste(model_name, var, "plot.png", sep ="_"))
  plot(coef_matrix_1[,1], type = "p", main = "Scatterplot of two models", xlab = "", ylab = "Coefficient", axes = F, pch = point_style_1, col = col_1, ylim=c(min_val, max_val))
  axis(2)
  points(coef_matrix_2[,1], pch = point_style_2, col = col_2)
      
  sapply(2:num_cols, function(i) {plot_points_for_cols(i, coef_matrix_1, coef_matrix_2, point_style_1, point_style_2, col_1, col_2)})

  dev.off()
}

#Generate box plot for the difference in a column
plot_points_for_cols <- function(col_num, matrix_1, matrix_2, point_style_1, point_style_2, col_1, col_2)
{
  points(matrix_1[, col_num], pch = point_style_1, col = col_1)
  points(matrix_2[, col_num], pch = point_style_2, col = col_2)
}

#Generate box plot for the difference in a column
gen_box_plot_for_col_dif <- function(diff_matrix, col_num, col_name, var, model_name, overall=F)
{
  if (overall)
  {
    points(diff_matrix[, col_num])
  }
  else
  {
    png(filename = paste(model_name, var, col_name, "plot.png", sep ="_"))
    plot(diff_matrix[,col_num], type = "p", main = paste("Difference between", col_name, sep = " "), xlab = "", ylab = "Difference", axes = F)
    axis(2)
    dev.off()
  }
}



