#############################################################################
#  MARKOV DECISION PROCESS (MDP)  -  Full R Program
#  States : S1, S2, S3
#  Actions: A1, A2
#
#  A NOTE ON THE SOURCE DATA (read this first):
#  ---------------------------------------------------------------------
#  For a valid MDP, the transition probabilities out of any (state, action)
#  pair must sum to exactly 1. Checking the given data:
#     (S1,A1): 0.6 + 0.2       = 0.8   -> incomplete (0.2 missing)
#     (S1,A2): 0.4             = 0.4   -> incomplete (0.6 missing)
#     (S2,A1): 0.7 + 0.5       = 1.2   -> INVALID (exceeds 1)
#     (S2,A2): 0.3 + 0.5       = 0.8   -> incomplete (0.2 missing)
#     (S3,A1): 0.9 + 0.4       = 1.3   -> INVALID (exceeds 1)
#     (S3,A2): 0.1 + 0.6       = 0.7   -> incomplete (0.3 missing)
#
#  Fix applied (documented, not hidden):
#   - Rows that summed to LESS than 1 (S1,A1 / S1,A2 / S2,A2 / S3,A2):
#     the missing mass is assigned to a SELF-TRANSITION (state -> itself),
#     which is consistent with the reward table already containing
#     R(S1,A2,S1) = -5, i.e. a S1->S1 move under A2 was clearly intended.
#   - Rows that summed to MORE than 1 (S2,A1 / S3,A1):
#     these were re-normalised (each entry divided by the row's total)
#     so they sum to exactly 1, with the self-transition probability set
#     to 0 for those two rows (there is no room left for one).
#   - Any (state, action, next_state) transition that has no reward listed
#     in the original table (this only happens for the added self-loops)
#     is assigned a reward of 0, the standard MDP convention for an
#     unspecified/neutral transition.
#############################################################################


#############################################################################
### MODULE 1: MDP Definition - States, Actions, Transition Probabilities,
###           Rewards, and basic counts
#############################################################################

cat("======================================================================\n")
cat("MODULE 1: MDP SETUP\n")
cat("======================================================================\n\n")

# ---- 1.1 States and Actions ----
states  <- c("S1", "S2", "S3")
actions <- c("A1", "A2")

n_states  <- length(states)
n_actions <- length(actions)

# ---- 1.2 Transition Probability array: P[state, action, next_state] ----
P <- array(0,
           dim = c(n_states, n_actions, n_states),
           dimnames = list(states, actions, states))

P["S1","A1", ] <- c(S1 = 0.2000000, S2 = 0.6, S3 = 0.2000000)
P["S1","A2", ] <- c(S1 = 0.6000000, S2 = 0.4, S3 = 0.0000000)
P["S2","A1", ] <- c(S1 = 0.5833333, S2 = 0.0, S3 = 0.4166667)
P["S2","A2", ] <- c(S1 = 0.3000000, S2 = 0.2, S3 = 0.5000000)
P["S3","A1", ] <- c(S1 = 0.6923077, S2 = 0.3076923, S3 = 0.0000000)
P["S3","A2", ] <- c(S1 = 0.1000000, S2 = 0.6, S3 = 0.3000000)

# ---- 1.3 Reward array: R[state, action, next_state] ----
R <- array(0,
           dim = c(n_states, n_actions, n_states),
           dimnames = list(states, actions, states))

R["S1","A1", ] <- c(S1 = 0,  S2 = 5,  S3 = -1)
R["S1","A2", ] <- c(S1 = -5, S2 = 10, S3 = 0)
R["S2","A1", ] <- c(S1 = 3,  S2 = 0,  S3 = 2)
R["S2","A2", ] <- c(S1 = 7,  S2 = 0,  S3 = 1)
R["S3","A1", ] <- c(S1 = 4,  S2 = 0,  S3 = 0)
R["S3","A2", ] <- c(S1 = 6,  S2 = -2, S3 = 0)

# ---- 1.4 Long-format table of every (state, action, next_state) with
###     nonzero transition probability - this is our master reference table
build_long_table <- function(P, R, states, actions) {
  rows <- list()
  k <- 1
  for (s in states) {
    for (a in actions) {
      for (s2 in states) {
        p <- P[s, a, s2]
        if (p > 0) {
          rows[[k]] <- data.frame(CurrentState = s, Action = a,
                                  NextState = s2, Probability = p,
                                  Reward = R[s, a, s2],
                                  stringsAsFactors = FALSE)
          k <- k + 1
        }
      }
    }
  }
  do.call(rbind, rows)
}

mdp_table <- build_long_table(P, R, states, actions)
n_transitions <- nrow(mdp_table)

# ---- 1.5 Display counts ----
cat("Number of States                     :", n_states, "->", paste(states, collapse=", "), "\n")
cat("Number of Actions                    :", n_actions, "->", paste(actions, collapse=", "), "\n")
cat("Number of Transition Probabilities   :", n_transitions, "(non-zero state-action-state triples)\n\n")

# ---- 1.6 Display transition probabilities and rewards together ----
mdp_table_display <- mdp_table
mdp_table_display$Probability <- round(mdp_table_display$Probability, 4)
cat("Transition Probabilities AND Rewards:\n")
print(mdp_table_display, row.names = FALSE)
cat("\n")


#############################################################################
### MODULE 2: Transition Probability Matrix for each Action
#############################################################################

cat("======================================================================\n")
cat("MODULE 2: TRANSITION PROBABILITY MATRICES (ONE PER ACTION)\n")
cat("======================================================================\n\n")

for (a in actions) {
  cat("Transition Probability Matrix for Action", a, "\n")
  mat <- P[, a, ]              # 3x3 matrix: rows = current state, cols = next state
  mat <- matrix(as.numeric(mat), nrow = n_states, ncol = n_states,
                dimnames = list(states, states))
  print(round(mat, 4))
  cat("Row sums (should be 1):", round(rowSums(mat), 4), "\n\n")
}


#############################################################################
### MODULE 3: Reward Matrix / Table
###   Columns: Current State | Action | Next State | Reward
#############################################################################

cat("======================================================================\n")
cat("MODULE 3: REWARD TABLE\n")
cat("======================================================================\n\n")

reward_table <- mdp_table[, c("CurrentState", "Action", "NextState", "Reward")]
colnames(reward_table) <- c("Current State", "Action", "Next State", "Reward")

print(reward_table, row.names = FALSE)
cat("\n")


#############################################################################
### MODULE 4: Expected Immediate Reward for each (State, Action) pair
###   R_bar(s,a) = sum over s' of  P(s'|s,a) * R(s,a,s')
#############################################################################

cat("======================================================================\n")
cat("MODULE 4: EXPECTED IMMEDIATE REWARD PER (STATE, ACTION)\n")
cat("======================================================================\n\n")

expected_reward <- matrix(0, nrow = n_states, ncol = n_actions,
                          dimnames = list(states, actions))

for (s in states) {
  for (a in actions) {
    terms <- c()
    total <- 0
    for (s2 in states) {
      p <- P[s, a, s2]
      r <- R[s, a, s2]
      if (p > 0) {
        terms <- c(terms, sprintf("(%.4f * %g)", p, r))
        total <- total + p * r
      }
    }
    expected_reward[s, a] <- total
    cat(sprintf("E[R | %s, %s] = %s = %.4f\n",
                s, a, paste(terms, collapse = " + "), total))
  }
  cat("\n")
}

cat("Expected Immediate Reward Matrix (rows = state, cols = action):\n")
print(round(expected_reward, 4))
cat("\n")


#############################################################################
### MODULE 5: Output / Summary Table
###   For each state, also flag the best action under the immediate-reward
###   criterion (E[R|s,a]). NOTE: this is a ONE-STEP / myopic comparison —
###   the problem gives no discount factor (gamma), so full Value Iteration
###   / Bellman optimality (which would need gamma) is out of scope here;
###   we summarise what the data actually supports: immediate reward.
#############################################################################

cat("======================================================================\n")
cat("MODULE 5: SUMMARY TABLE\n")
cat("======================================================================\n\n")

summary_rows <- list()
k <- 1
for (s in states) {
  best_a <- actions[which.max(expected_reward[s, ])]
  for (a in actions) {
    summary_rows[[k]] <- data.frame(
      State           = s,
      Action          = a,
      ExpectedReward  = round(expected_reward[s, a], 4),
      BestActionForState = ifelse(a == best_a, "YES <-- best", ""),
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }
}
summary_table <- do.call(rbind, summary_rows)
colnames(summary_table) <- c("State", "Action", "Expected Immediate Reward", "Greedy Best Action?")

print(summary_table, row.names = FALSE)

cat("\nGreedy (one-step) policy implied by immediate reward alone:\n")
for (s in states) {
  best_a <- actions[which.max(expected_reward[s, ])]
  cat(sprintf("  pi(%s) = %s   (E[R] = %.4f)\n", s, best_a, expected_reward[s, best_a]))
}
cat("\n")


#############################################################################
### MODULE 6: Visualization of States, Actions and Transitions on a Grid
###   - Nodes  = states  (distinct fill colour per state)
###   - Edges  = transitions, coloured by action (A1 vs A2)
###   - Edge labels show the transition probability
###   Uses ONLY base R graphics (no external packages needed).
#############################################################################

cat("======================================================================\n")
cat("MODULE 6: MDP STATE-ACTION-TRANSITION DIAGRAM\n")
cat("======================================================================\n")
cat("Rendering diagram to 'MDP_diagram.png' ...\n\n")

# ---- 6.0 Pre-compute the "verdict" (best action per state + overall winner) ----
# (expected_reward matrix was already calculated in Module 4)
best_action_per_state <- sapply(states, function(s) actions[which.max(expected_reward[s, ])])
best_value_per_state  <- sapply(states, function(s) max(expected_reward[s, ]))
avg_reward_per_action <- colMeans(expected_reward)              # average across states, per action
overall_best_action   <- names(which.max(avg_reward_per_action))
n_states_favoring     <- table(factor(best_action_per_state, levels = actions))

png("MDP_diagram.png", width = 1000, height = 1080, res = 120)

# ---- 6.1 Node layout (triangle) ----
node_pos <- data.frame(
  state = states,
  x     = c(0,   -2.6, 2.6),
  y     = c(2.6, -1.3, -1.3)
)
rownames(node_pos) <- node_pos$state

node_colors   <- c(S1 = "#4C72B0", S2 = "#DD8452", S3 = "#55A868")  # distinct per state
action_colors <- c(A1 = "#1f77b4", A2 = "#d62728")                  # distinct per action
node_radius <- 0.55

par(mar = c(1, 1, 3, 1))
# extra vertical room at the bottom (ylim goes down to -6.4) is reserved for
# the verdict / conclusion panel drawn in section 6.6 below
plot(NA, xlim = c(-4.2, 4.2), ylim = c(-6.4, 3.6),
     xlab = "", ylab = "", axes = FALSE,
     main = "MDP Environment: States (nodes) & Actions (coloured edges)")
grid(col = "grey85", lty = "dotted")   # <- literal grid backdrop, as requested

# helper: point on the boundary of a circle, in the direction of another point
edge_point <- function(x0, y0, x1, y1, r) {
  d <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
  c(x0 + (x1 - x0) / d * r, y0 + (y1 - y0) / d * r)
}

# ---- 6.2 Draw transition arrows (state -> state, excluding self-loops) ----
offset <- 0.12  # small perpendicular offset so A1 & A2 arrows don't overlap
for (s in states) {
  for (a in actions) {
    for (s2 in states) {
      p <- P[s, a, s2]
      if (p > 0 && s != s2) {
        x0 <- node_pos[s, "x"];  y0 <- node_pos[s, "y"]
        x1 <- node_pos[s2, "x"]; y1 <- node_pos[s2, "y"]
        
        # perpendicular unit vector for offsetting A1 vs A2 arrows
        dx <- x1 - x0; dy <- y1 - y0
        len <- sqrt(dx^2 + dy^2)
        perp <- c(-dy, dx) / len
        sign <- ifelse(a == "A1", 1, -1)
        ox <- perp[1] * offset * sign
        oy <- perp[2] * offset * sign
        
        p0 <- edge_point(x0 + ox, y0 + oy, x1 + ox, y1 + oy, node_radius)
        p1 <- edge_point(x1 + ox, y1 + oy, x0 + ox, y0 + oy, node_radius)
        
        arrows(p0[1], p0[2], p1[1], p1[2],
               length = 0.12, angle = 20, lwd = 2,
               col = action_colors[a])
        
        mx <- (p0[1] + p1[1]) / 2
        my <- (p0[2] + p1[2]) / 2
        text(mx, my, labels = sprintf("%.2f", p),
             col = action_colors[a], cex = 0.75, font = 2)
      }
    }
  }
}

# ---- 6.3 Draw self-loops (state -> same state) ----
for (s in states) {
  for (a in actions) {
    p <- P[s, a, s]
    if (p > 0) {
      x0 <- node_pos[s, "x"]; y0 <- node_pos[s, "y"]
      side <- ifelse(a == "A1", 1, -1)
      loop_cx <- x0 + side * 0.05
      loop_cy <- y0 + node_radius + 0.55
      theta <- seq(0.2, 2 * pi - 0.2, length.out = 60)
      lx <- loop_cx + 0.42 * cos(theta)
      ly <- loop_cy + 0.42 * sin(theta) - 0.4
      lines(lx, ly, col = action_colors[a], lwd = 2)
      arrows(lx[length(lx) - 1], ly[length(ly) - 1], lx[length(lx)], ly[length(ly)],
             length = 0.10, angle = 20, col = action_colors[a], lwd = 2)
      text(loop_cx, loop_cy + 0.25, labels = sprintf("%.2f", p),
           col = action_colors[a], cex = 0.7, font = 2)
    }
  }
}

# ---- 6.4 Draw state nodes on top ----
theta <- seq(0, 2 * pi, length.out = 100)
for (s in states) {
  x0 <- node_pos[s, "x"]; y0 <- node_pos[s, "y"]
  polygon(x0 + node_radius * cos(theta), y0 + node_radius * sin(theta),
          col = node_colors[s], border = "black", lwd = 2)
  text(x0, y0, labels = s, col = "white", font = 2, cex = 1.3)
}

# ---- 6.5 Legend ----
legend("bottomright", legend = c("Action A1", "Action A2"),
       col = action_colors, lwd = 3, bty = "n", cex = 0.9)
legend("topleft",
       legend = states, fill = node_colors, border = "black",
       title = "States", bty = "n", cex = 0.85)

# ---- 6.6 VERDICT PANEL: the judgement / conclusion, drawn under the diagram ----
# separator line between the diagram and the verdict panel
segments(-4.2, -3.3, 4.2, -3.3, col = "grey70", lwd = 1)

panel_top <- -3.7
rect(-4.15, -6.35, 4.15, panel_top, col = "grey97", border = "grey60", lwd = 1)

text(0, panel_top - 0.35, labels = "VERDICT \u2014 Best action per state (by expected immediate reward)",
     font = 2, cex = 1.0)

# one line per state: "S1 : A1 wins  (E[R]=2.80 vs 1.00)"  in that state's colour
row_y <- panel_top - 0.85
for (s in states) {
  a_best  <- best_action_per_state[s]
  a_other <- actions[actions != a_best]
  line <- sprintf("%s : Action %s wins   (E[R|%s,%s]=%.2f  vs  E[R|%s,%s]=%.2f)",
                  s, a_best, s, a_best, expected_reward[s, a_best],
                  s, a_other, expected_reward[s, a_other])
  text(-4.0, row_y, labels = line, adj = c(0, 0.5), cex = 0.82,
       family = "mono", col = action_colors[a_best], font = 2)
  row_y <- row_y - 0.42
}

# overall judgement across all 3 states
overall_line <- sprintf(
  "OVERALL: Action %s is the stronger policy \u2014 favoured in %d of %d states (avg E[R]: %s=%.2f vs %s=%.2f)",
  overall_best_action, n_states_favoring[overall_best_action], n_states,
  actions[1], avg_reward_per_action[actions[1]],
  actions[2], avg_reward_per_action[actions[2]]
)
text(0, row_y - 0.25, labels = overall_line, cex = 0.85, font = 2,
     col = action_colors[overall_best_action])

caveat_line <- "(Note: greedy on immediate reward only \u2014 no discount factor was given, so this is not a full Bellman-optimal policy.)"
text(0, row_y - 0.65, labels = caveat_line, cex = 0.68, col = "grey40", font = 3)

dev.off()

cat("Diagram saved as MDP_diagram.png\n")
cat("\n======================================================================\n")
cat("END OF PROGRAM\n")
cat("======================================================================\n")