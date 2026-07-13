#-------------------------------
# Simple Markov Decision Process
#-------------------------------

size <- 4

start_state <- c(1, 1)
goal_state  <- c(4, 4)

current <- start_state

grid <- matrix("", size, size)
grid[start_state[1], start_state[2]] <- "S"
grid[goal_state[1], goal_state[2]] <- "G"

cat("Grid Layout\n")
print(grid)

count <- 1

cat("\nSimulation Begins\n\n")

while (!all(current == goal_state)) {
  
  r <- current[1]
  c <- current[2]
  
  moves <- list()
  
  if (r > 1) moves[["Up"]] <- c(r - 1, c)
  if (r < size) moves[["Down"]] <- c(r + 1, c)
  if (c > 1) moves[["Left"]] <- c(r, c - 1)
  if (c < size) moves[["Right"]] <- c(r, c + 1)
  
  chosen_move <- ""
  shortest <- Inf
  
  for (direction in names(moves)) {
    
    temp <- moves[[direction]]
    
    dist <- abs(goal_state[1] - temp[1]) +
      abs(goal_state[2] - temp[2])
    
    if (dist < shortest) {
      shortest <- dist
      chosen_move <- direction
      next_pos <- temp
    }
  }
  
  points <- if (all(next_pos == goal_state)) 100 else -1
  
  cat("Move :", count, "\n")
  cat("Current :", current, "\n")
  cat("Selected :", chosen_move, "\n")
  cat("Next :", next_pos, "\n")
  cat("Reward :", points, "\n\n")
  
  current <- next_pos
  count <- count + 1
}