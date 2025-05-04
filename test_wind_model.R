# test different wind direction, strength and ash
source("./model/utils.R")
source("./model/wind_model.R")
fire_prob <- 0.01

# get name from named vector by value
get_name <- function(named_vector, value) {
  names(named_vector)[which(named_vector == value)]
}

# paste gif file name
paste_file_name <- function(d, s, ash) {
  if (d == 0 || s == 0) {
    file_name <- "NO_WIND"
  } else {
    direction_str <- get_name(direction, d)
    strength_str <- get_name(strength, s)
    ash_str <- get_name(ash_seq, ash)

    file_name <- paste(
      na.omit(c(direction_str, strength_str, ash_str)),
      collapse = "_"
    )
  }
  return(file_name)
}

# --------for different wind direction--------
# direction[2:9], strength[2:4], ash_seq

param_grid <- expand.grid(
  direction = direction[2:9],
  strength = strength[2:4],
  ash = ash_seq
)
no_wind <- c(
  direction = 0,
  strength = 0,
  ash = 0
)

param_grid <- rbind(param_grid, no_wind)
n <- nrow(param_grid)

# test all parameters
for (i in 1:n) {
  d <- param_grid[i, "direction"]
  s <- param_grid[i, "strength"]
  ash <- param_grid[i, "ash"]

  file_name <- paste_file_name(d, s, ash)

  result <- forest_fire_model(size, alpha, beta, fire_prob, d, s, ash)

  iter <- result$iter
  forest_tracker <- result$forest_tracker

  g <- plot_forest_gif(forest_tracker, iter, file_name)

  # Save each animation as a separate GIF file
  anim_save(paste0("./animation/", file_name, ".gif"), g)
}
