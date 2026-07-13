#--------------------------
# Grid World Path Plot
#--------------------------

n <- 4

row_pos <- c(1, 1, 2, 3, 4, 4, 4)
col_pos <- c(1, 2, 2, 2, 2, 3, 4)

plot(1:n,
     1:n,
     type = "n",
     xlim = c(1, n),
     ylim = c(n, 1),
     xlab = "Grid Column",
     ylab = "Grid Row",
     main = "Path Followed by Agent")

grid(nx = n - 1, ny = n - 1, col = "gray")

text(1, 1, labels = "S", col = "blue", cex = 1.8)
text(4, 4, labels = "G", col = "red", cex = 1.8)

lines(col_pos, row_pos, lwd = 2.5)
points(col_pos, row_pos, pch = 16, cex = 1.4)