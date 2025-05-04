# 设置随机种子
set.seed(123)
source("./model/utils.R")
source("./model/wind_model.R")

alpha <- 0.2
beta <- 0.8

result <- forest_fire_model(
  size, alpha, beta, fire_prob,
  direction["NORTH"], strength["LOW"], ash_seq["NO_ASH"]
)
iter <- result$iter
forest_tracker <- result$forest_tracker
g <- plot_forest_gif(forest_tracker, iter, "wind_north_low")
anim_save("./animation/forest_animation_wind_low.gif", g)
