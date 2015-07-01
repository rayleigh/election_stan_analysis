library(lme4)

#Note: This assumes that there is one merMod object to plot

#Plot the fixed effects
plot_fixed_effects <- function(merMod_list, stanfit_list, color_list, point_list)
{
  #Get the length
  num_merMod <- length(merMod_list)
  num_stanfit <- length(stanfit_list)
  
  #Get the merMod fixed effects, min, and max
  if (num_merMod > 0)
  {  
    merMod_fixed_eff_list <- lapply(merMod_list, fixef)
    merMod_info <- get_eff_info(merMod_fixed_eff_list)
    max_merMod <- merMod_info$max
    min_merMod <- merMod_info$min
  }
  
  #Get the stanfit fixed effects, min, and max
  if (num_stanfit > 0) 
  {  
    stanfit_fixed_eff_list <- lapply(stanfit_list, get_fixed_effects_from_stanfit)
    stanfit_info <- get_eff_info(stanfit_fixed_eff_list)
    max_stanfit <- stanfit_info$max
    min_stanfit <- stanfit_info$min
  }  
  
  #Get the max and min y to know how to set up plot if both models are present
  val_info <- get_min_and_max_y(min_merMod, max_merMod, num_merMod, min_stanfit, max_stanfit, num_stanfit)
  min_val <- val_info$min
  max_val <- val_info$max    

  #Plot the fixed effects points  
  plot_effects(merMod_fixed_eff_list, stanfit_fixed_eff_list, num_merMod, num_stanfit, max_val, min_val, color_list, point_list, "Fixed Effects")
}

#Plot the random effects
plot_rand_effects <- function(merMod_list, stanfit_list, color_list, point_list)
{
  names_list <- names(ranef(merMod_list[[1]]))
  sapply(names_list, function(var_name) {plot_rand_effects_for_var(var_name, merMod_list, stanfit_list, color_list, point_list)})
}

#Plot random effects for var
plot_rand_effects_for_var <- function(var_name, merMod_list, stanfit_list, color_list, point_list)
{
  #Get the length
  num_merMod <- length(merMod_list)
  num_stanfit <- length(stanfit_list)
  
  #Get the merMod random effects and max, min, and length
  if (num_merMod > 0)
  { 
    merMod_rand_eff_list <- lapply(merMod_list, function(merMod_obj) {ranef(merMod_obj)[[var_name]]})
    merMod_info <- get_eff_info(merMod_rand_eff_list)
    max_merMod <- merMod_info$max
    min_merMod <- merMod_info$min
  } 
  
  #Get the stanfit random effects and max, min, and length
  if (num_stanfit > 0)
  {  
    stanfit_rand_eff_list <- lapply(stanfit_list, function(stanfit_obj) {get_rand_effects_from_stanfit(stanfit_obj, var_name)})
    stanfit_info <- get_eff_info(stanfit_rand_eff_list)
    max_stanfit <- stanfit_info$max
    min_stanfit <- stanfit_info$min
  } 
  
  #Get the max and min y to know how to set up plot if both models are present
  val_info <- get_min_and_max_y(min_merMod, max_merMod, num_merMod, min_stanfit, max_stanfit, num_stanfit)
  min_val <- val_info$min
  max_val <- val_info$max    
  
  plot_effects(merMod_rand_eff_list, stanfit_rand_eff_list, num_merMod, num_stanfit, max_val, min_val, color_list, point_list, paste(var_name, "Random Effects", sep = " "))
}

#Get the fixed effects from the stanfit object
get_fixed_effects_from_stanfit <- function(stanfit_obj)
{
  names_list <- rownames(summary(stanfit_obj)$summary)
  fixed_effects_index <- c(which(names_list == 'Intercept'), grep('b_', names_list))
  return(summary(stanfit_obj)$summary[fixed_effects_index])
}

#Get the random effects from the stanfit object
get_rand_effects_from_stanfit <- function(stanfit_obj, var_name)
{
  #Set up
  names_list <- rownames(summary(stanfit_obj)$summary)
  stanfit_name <- translate_var_name(var_name)

  #Get rand_effects_index
  rand_effects_index <- grep(stanfit_name, names_list, perl = T)
  if (length(rand_effects_index) == 0)
  {
    stanfit_name <- paste(var_name, "z", "inc\\[", sep = "_")
    rand_effects_index <- grep(stanfit_name, names_list)
  }
  
  return(summary(stanfit_obj)$summary[rand_effects_index])
}

#Translate var_name from merMod to stanfit
translate_var_name <- function(merMod_var_name)
{
  stanfit_var_name <- gsub("\\.","_X_",c(merMod_var_name))
  return(paste("^.", stanfit_var_name, "Intercept\\[", sep = "_"))
}

#Get min and length
get_eff_info <- function(eff_list)
{
  max_eff <- max(sapply(eff_list, max))
  min_eff <- min(sapply(eff_list, min))
  
  return(list("max" = max_eff, "min" = min_eff))
}

#Get min_y and max_y
get_min_and_max_y <- function(min_merMod, max_merMod, num_merMod, min_stanfit, max_stanfit, num_stanfit)
{
  #Get the max and min y to know how to set up plot if both models are present
  if (num_stanfit > 0 && num_merMod > 0)
  {  
    max_y <- max(max_merMod, max_stanfit)
    min_y <- min(min_merMod, min_stanfit)
  }
  else if (num_stanfit > 0)
  {
    max_y <- max_stanfit
    min_y <- min_stanfit
  }
  else
  {
    max_y <- max_merMod
    min_y <- min_merMod
  }
  return(list("min" = min_y, "max" = max_y))
}

#Sort and plot the effects from the merMods and stanfit objects' effects lists
plot_effects <- function(merMod_eff_list, stanfit_eff_list, num_merMod, num_stanfit, max_val, min_val, color_list, point_list, plot_name)
{
  #Plot the random effects points
  png(paste(plot_name,"png", sep="."))
  plot(sort(unlist(as.vector(merMod_eff_list[[1]]))), type = "p", main = plot_name, xlab = "", ylab = "Random effects", axes = F, pch = point_list[1], col = color_list[1], ylim=c(min_val, max_val))
  axis(2)
  
  if (num_merMod > 1)
  {
    sapply(2:num_merMod, function(i) {points(sort(unlist(as.vector(merMod_eff_list[[i]]))), pch = point_list[i], col = color_list[i])})
  }
  if (num_stanfit > 0)
  {
    sapply(1:num_stanfit, function(i) {points(sort(stanfit_eff_list[[i]]), pch = point_list[i + num_merMod], col = color_list[i + num_merMod])})
  }
  dev.off()
}