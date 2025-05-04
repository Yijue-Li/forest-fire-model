#-------------- STEP1: set up some constants -----------------


# state of forest
state <- c(REMOVED = 0, INFECT = 1, SUSCEPTIBLE = 2)


# -------------- STEP2: helper functions -----------------

# generate a random number between 0 and 1
#
# return:
# ---
# a number between 0 and 1
random <- function() {
  return(runif(1, 0, 1))
}

# get all 8 neighbors of a given point
#
# Arguments:
# ---
# **x** is the x coordinate of the point
# **y** is the y coordinate of the point
#
# return:
# ---
# a matrix with 8 rows and 2 columns, each row represents a neighbor point
get_neighbors <- function(x, y) {
  neighbors <- matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("x", "y")))

  for (i in -1:1) {
    for (j in -1:1) {
      if (i != 0 || j != 0) {
        neighbors <- rbind(neighbors, c(x + i, y + j))
      }
    }
  }

  return(neighbors)
}

# make sure given point is within bounds
ensure_in_bounds <- function(x, y, nrow, ncol) {
  return(x >= 1 & x <= nrow & y >= 1 & y <= ncol)
}

# fire occurs in forest, to init some fire tree
#
# Arguments:
# ---
# **forest** is a matrix with 3 states: SUSCEPTIBLE, INFECT, REMOVED
# **fire_prob** is the probability of a tree being infected
#
# return:
# ---
# the fired forest matrix
fire_occurs <- function(forest, fire_prob) {
  m <- nrow(forest)
  n <- ncol(forest)
  for (i in 1:m) {
    for (j in 1:n) {
      prob <- random()
      # set fire tree based on fire_prob
      if (prob < fire_prob) {
        forest[i, j] <- state["INFECT"]
      }
    }
  }
  return(forest)
}


# fire neighbor trees
#
# Arguments:
# ---
# **old_forest** is the old forest matrix
# **new_forest** is the new forest matrix
# **alpha** is the probability of a susceptible tree being infected
# **i** is the x coordinate of the tree
# **j** is the y coordinate of the tree
# **nrow** is the number of rows in forest
# **ncol** is the number of columns in forest
# return:
# ---
# the new forest matrix
fire_neighbor <- function(old_forest, new_forest, alpha, i, j, nrow, ncol) {
  neighbors <- get_neighbors(i, j)

  r <- nrow(neighbors) # r is always 8
  for (k in 1:r) {
    x <- neighbors[k, 1]
    y <- neighbors[k, 2]
    if (!ensure_in_bounds(x, y, nrow, ncol)) {
      # if neighbor index is out of bounds, skip
      next
    }

    if (old_forest[x, y] == state["SUSCEPTIBLE"]) {
      prob <- random()
      # if prob < alpha, tree will be infected
      if (prob < alpha) {
        new_forest[x, y] <- state["INFECT"]
      }
    }
  }
  return(new_forest)
}

# spread fire in forest
#
# Arguments:
# ---
# **old_forest** is the old forest matrix
# **alpha** is the probability of a susceptible tree being infected
# **beta** is the probability of an infected tree being removed
#
# return:
# ---
# the new forest matrix
spread <- function(old_forest, alpha, beta) {
  # get forest size
  nrow <- nrow(old_forest)
  ncol <- ncol(old_forest)
  # create a new forest matrix, to store the result
  new_forest <- old_forest

  for (i in 1:nrow) {
    for (j in 1:ncol) {
      # if tree is infected, check its 8 neighbors
      if (old_forest[i, j] == state["INFECT"]) {
        new_forest <- fire_neighbor(
          old_forest, new_forest, alpha,
          i, j, nrow, ncol
        )
        # if tree is infected, check if it will be removed
        p <- random()
        if (p < beta) {
          new_forest[i, j] <- state["REMOVED"]
        }
      }
    }
  }
  return(new_forest)
}


# count the number of trees in specified state
count_proportion <- function(forest, flag) {
  nrow <- nrow(forest)
  ncol <- ncol(forest)
  count <- 0
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      if (forest[i, j] == flag) {
        count <- count + 1
      }
    }
  }
  return(count * 100 / (nrow * ncol))
}



# ---------------- STEP3: main function -----------------

# forest fire model
#
# Arguments:
# ---
# **size** is the size of forest
# **alpha** is the probability of a susceptible tree being infected
# **beta** is the probability of an infected tree being removed
# **fire_prob** is the probability of a tree being infected
#
# return:
# ---
# a list with 3 elements: iter, spread_tracker, forest, forest_tracker
# iter: number of iterations
# spread_tracker: a list with number of infected trees in each iteration
# forest: the final forest matrix
# forest_tracker: a list with forest matrix in each iteration
forest_fire_model <- function(size, alpha, beta, fire_prob) {
  forest <- matrix(state["SUSCEPTIBLE"], size, size) # init forest matrix
  forest <- fire_occurs(forest, fire_prob) # fire occurs

  t <- 1
  spread_tracker <- vector("list", 0)
  forest_tracker <- vector("list", 0)
  while (TRUE) {
    spread_tracker[[t]] <- count_proportion(forest, state["INFECT"]) +
      count_proportion(forest, state["REMOVED"])

    forest_tracker[[t]] <- forest

    new_forest <- spread(forest, alpha, beta)
    if (all(new_forest == forest)) {
      return(list(
        iter = t, spread_tracker = spread_tracker,
        forest = forest, forest_tracker = forest_tracker
      ))
    } else {
      forest <- new_forest
    }
    t <- t + 1
  }
}
