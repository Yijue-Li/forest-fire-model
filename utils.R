library(ggplot2)
library(gganimate)
# --------------------default parameters------------------------
size <- 30 # forest size
fire_prob <- 0.03 # probability of fire
state <- c(REMOVED = 0, INFECT = 1, SUSCEPTIBLE = 2)
alpha <- 0.2
beta <- 0.7


# --------------------plot functions------------------------

# plot forest matrix
#
# Arguments:
# ---
# **forest** is a matrix with 3 states: SUSCEPTIBLE, INFECT, REMOVED
#
# return:
# ---
# a ggplot2 object
# save as forest.png
# ggsave("./image/forest.png", plot = p, width = 10, height = 8, dpi = 300)
plot_forest <- function(forest) {
  nrow <- nrow(forest)
  ncol <- ncol(forest)

  # 创建一个数据框来存储每个单元格的状态
  forest_df <- data.frame(
    x = rep(1:ncol, each = nrow),
    y = rep(nrow:1, times = ncol),
    state = as.vector(t(forest))
  )

  # 将状态转换为因子，并设置颜色
  forest_df$state <- factor(forest_df$state,
    levels = c(state["SUSCEPTIBLE"], state["INFECT"], state["REMOVED"]),
    labels = c("SUSCEPTIBLE", "INFECT", "REMOVED")
  )

  # 使用 ggplot2 绘制森林状态图
  p <- ggplot(forest_df, aes(x = x, y = y, fill = state)) +
    geom_tile() +
    scale_fill_manual(values = c(
      "SUSCEPTIBLE" = "green",
      "INFECT" = "red",
      "REMOVED" = "gray"
    )) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = "white")
    ) +
    coord_fixed(ratio = 1) + # set aspect ratio
    labs(title = "Forest State")
  return(p)
}

# plot forest change over time
#
# Arguments:
# ---
# **forest_tracker** is a list of forest matrices
# **iter** is the number of iterations
# **size** is the size of forest
#
# return:
# ---
# a gif file
# save as forest_animation.gif
# anim_save("forest_animation.gif", g)
plot_forest_gif <- function(forest_tracker, iter, title) {
  df <- data.frame(
    time = rep(1:iter, each = size**2),
    forest = unlist(lapply(forest_tracker, function(x) as.vector(x)))
  )

  # 创建行和列的索引
  df$row <- rep(rep(1:size, times = ncol(forest_tracker[[1]])), times = iter)
  df$col <- rep(1:size, each = nrow(forest_tracker[[1]]), times = iter)
  # 将forest值映射到因子状态
  df$state <- factor(df$forest,
    levels = c(state["SUSCEPTIBLE"], state["INFECT"], state["REMOVED"]),
    labels = c("SUSCEPTIBLE", "INFECT", "REMOVED")
  )

  g <- ggplot(df, aes(x = col, y = row, fill = state)) +
    geom_tile() + # draw tiles
    scale_fill_manual(values = c(
      "SUSCEPTIBLE" = "green",
      "INFECT" = "red",
      "REMOVED" = "gray"
    )) + # set color for each state
    theme_minimal() + # set theme
    labs(title = title, subtitle = "Time: {closest_state}") + # title
    coord_fixed(ratio = 1) + # set aspect ratio
    transition_states(time,
      transition_length = 0,
      state_length = 1,
      wrap = FALSE
    ) + # switch between states
    ease_aes("linear") # set animation easing


  return(g)
}
