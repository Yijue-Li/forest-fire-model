# test different alpha and beta
#
# The size is represented by the proportion of trees burned
# The speed is represented by the time it takes to burn out
alpha_seq <- seq(0.1, 0.9, by = 0.1)
beta_seq <- seq(0.1, 0.9, by = 0.1)
set.seed(123)

source("./model/utils.R")
source("./model/base_model.R")

# count the spread of forest fire by spread_tracker
count_spread <- function(spread_tracker) {
  # Calculate the difference between the latter and the former,
  # and then take the average
  spread_tracker <- unlist(spread_tracker)
  return(max(diff(spread_tracker)))
}


# use parameter grid to generate all possible combinations
param_grid <- expand.grid(alpha = alpha_seq, beta = beta_seq)
n <- nrow(param_grid)

# store results
size_list <- vector("list", n)
spread_list <- vector("list", n)

# test
for (i in 1:n) {
  alpha <- param_grid[i, "alpha"]
  beta <- param_grid[i, "beta"]
  result <- forest_fire_model(size, alpha, beta, fire_prob)
  spread_tracker <- result$spread_tracker
  forest <- result$forest
  size_list[[i]] <- count_proportion(forest, state["REMOVED"])
  spread_list[[i]] <- count_spread(spread_tracker)
}


# -----------------------------------------------------
# plot result
library(ggplot2)
plot_param <- function(data_list, lab, color, size) {
  results_df <- data.frame(
    alpha = rep(param_grid$alpha, each = 1),
    beta = rep(param_grid$beta, each = 1),
    size = unlist(data_list)
  )

  p <- ggplot(results_df, aes(x = alpha, y = beta, fill = size)) +
    geom_tile() +
    scale_fill_gradient(low = color$low, high = color$high) +
    labs(
      title = cat("Heatmap of ", lab, " vs Alpha and Beta"),
      x = "Alpha",
      y = "Beta",
      fill = lab
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      axis.ticks = element_blank(),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      plot.background = element_rect(fill = "white"),
    )

  return(p)
}

# more red means more trees burned
# more green means less trees burned
size_plot <- plot_param(size_list, "Size",
  color = list(low = "green", high = "red", size)
)
ggsave("./image/size_heatmap_01.png",
  plot = size_plot,
  width = 10, height = 8, dpi = 300
)


spread_plot <- plot_param(spread_list, "Spread",
  color = list(low = "green", high = "red", size)
)
ggsave("./image/spread_heatmap_01.png",
  plot = spread_plot,
  width = 10, height = 8, dpi = 300
)
