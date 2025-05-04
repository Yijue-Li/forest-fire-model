# 设置随机种子


set.seed(123)
source("./model/utils.R")
source("./model/base_model.R")

alpha <- 0.2
beta <- 0.8

result <- forest_fire_model(size, alpha, beta, fire_prob)

iter <- result$iter
forest <- result$forest
forest_tracker <- result$forest_tracker

# g <- plot_forest(forest)
# ggsave("./image/forest_01.png", g, width = 10, height = 10, dpi = 300)

p <- plot_forest_gif(forest_tracker, iter, "forest_fire")
anim_save("./animation/forest_animation.gif", p)
