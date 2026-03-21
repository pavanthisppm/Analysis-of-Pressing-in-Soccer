library(expm)

################################ HARD #####################################
transition_matrix = read.csv("C:/Users/pavan/Desktop/SFU/Research/Datasets/10_14/hard_transition_table_w_2m_wo_events_updated.csv")

transition_matrix

# ca ---> ac
transition_matrix[1,4] = transition_matrix[1,4]+transition_matrix[3,2]
transition_matrix[3,2] = NA

transition_matrix

# cb ---> ab
transition_matrix[1,3] = transition_matrix[1,3]+transition_matrix[3,3]
transition_matrix[3,3] = NA

transition_matrix

# cc ---> aa
transition_matrix[1,2] = transition_matrix[1,2]+transition_matrix[3,4]
transition_matrix[3,4] = NA

transition_matrix

# cd ---> af
transition_matrix[1,7] = transition_matrix[1,7]+transition_matrix[3,5]
transition_matrix[3,5] = NA

transition_matrix

# ce ---> ae
transition_matrix[1,6] = transition_matrix[1,6]+transition_matrix[3,6]
transition_matrix[3,6] = NA

transition_matrix

# cf ---> ad
transition_matrix[1,5] = transition_matrix[1,5]+transition_matrix[3,7]
transition_matrix[3,7] = NA

transition_matrix

# c: BP, OB, TO ----> a: BP, OB, TO

transition_matrix[1,8] = transition_matrix[1,8]+transition_matrix[3,8]
transition_matrix[3,8] = NA

transition_matrix[1,9] = transition_matrix[1,9]+transition_matrix[3,9]
transition_matrix[3,9] = NA

transition_matrix[1,10] = transition_matrix[1,10]+transition_matrix[3,10]
transition_matrix[3,10] = NA

transition_matrix

# fa ---> dc
transition_matrix[4,4] = transition_matrix[4,4]+transition_matrix[6,2]
transition_matrix[6,2] = NA

transition_matrix

# fb ---> db
transition_matrix[4,3] = transition_matrix[4,3]+transition_matrix[6,3]
transition_matrix[6,3] = NA

transition_matrix

# fc ---> da
transition_matrix[4,2] = transition_matrix[4,2]+transition_matrix[6,4]
transition_matrix[6,4] = NA

transition_matrix

# fd ---> df
transition_matrix[4,7] = transition_matrix[4,7]+transition_matrix[6,5]
transition_matrix[6,5] = NA

transition_matrix

# fe ---> de
transition_matrix[4,6] = transition_matrix[4,6]+transition_matrix[6,6]
transition_matrix[6,6] = NA

transition_matrix

# ff ---> dd
transition_matrix[4,5] = transition_matrix[4,5]+transition_matrix[6,7]
transition_matrix[6,7] = NA

transition_matrix

# c: BP, OB, TO ----> a: BP, OB, TO

transition_matrix[4,8] = transition_matrix[4,8]+transition_matrix[6,8]
transition_matrix[6,8] = NA

transition_matrix[4,9] = transition_matrix[4,9]+transition_matrix[6,9]
transition_matrix[6,9] = NA

transition_matrix[4,10] = transition_matrix[4,10]+transition_matrix[6,10]
transition_matrix[6,10] = NA

transition_matrix


# ec ---> ea
transition_matrix[5,2] = transition_matrix[5,2]+transition_matrix[5,4]
transition_matrix[5,4] = NA

transition_matrix


# ef ---> ed
transition_matrix[5,5] = transition_matrix[5,5]+transition_matrix[5,7]
transition_matrix[5,7] = NA

transition_matrix

# bc ---> ba
transition_matrix[2,2] = transition_matrix[2,2]+transition_matrix[2,4]
transition_matrix[2,4] = NA

transition_matrix

# bf ---> bd
transition_matrix[2,5] = transition_matrix[2,5]+transition_matrix[2,7]
transition_matrix[2,7] = NA

transition_matrix

# Ignore the unlikely transitions
# ac(0), af(0), dc(0), df(2)

transition_matrix[1,4] = NA
transition_matrix[1,7] = NA
transition_matrix[4,4] = NA
transition_matrix[4,7] = NA

transition_matrix

# remove rows and columns

transition_matrix = transition_matrix[-c(3,6),]
transition_matrix

transition_matrix = transition_matrix[,-c(4,7)]
transition_matrix

clear_transition_matrix = transition_matrix

# write.csv(clear_transition_matrix, "C:/Users/pavan/Desktop/SFU/Research/Datasets/9_26/hard_clear_transition_matrix.csv")


clear_transition_matrix

# Convert counts to probabilities
transition_probs <- clear_transition_matrix[,-1] / rowSums(clear_transition_matrix[,-1])
round(transition_probs,3)

set.seed(1)   

# 4 in-play regions
description <- c("a", "b", "d", "e")

# Transition (pass) matrix

A <- matrix(c(0.490, 0.067, 0.182, 0.135,
              0.079, 0.512, 0.010, 0.254,
              0.038, 0.003, 0.469, 0.076,
              0.021, 0.045, 0.108, 0.553),
            nrow = 4, byrow = TRUE)

# Terminal outcome probabilities
p_break      <- c(0.044, 0.022,  0.271, 0.197)
p_out    <- c(0.026, 0.008, 0.036, 0)
p_turnover <- c(0.056, 0.116, 0.107, 0.076)


P <- rbind(
  cbind(A, p_out, p_break, p_turnover),
  cbind(matrix(0, 3, 4), diag(3))
)

rownames(P) <- c(description, "Out of Bounds", "Break Press", "Turnover")
colnames(P) <- rownames(P)
P

alpha <- c((617+564)/7596, 825/7596,  (1323+1214)/7596, 3053/7596,0,0,0 )
steps <- c(1, 2, 3, 5, 10, 15, 20, 30, 100)

for (n in steps) {
  Pn <- P %^% n
  
  outcomes_by_region <- round(Pn[1:4, 5:7], 3)
  
  
  overall <- alpha %*% Pn
  overall_outcomes <- round(overall[, 5:7], 3)
  
  cat("\n=============================================\n")
  cat("After", n, "steps:\n")
  cat("=============================================\n")
  cat("Outcome probabilities by starting region:\n")
  print(outcomes_by_region)
  
  cat("\nWeighted overall outcome probabilities (using alpha):\n")
  print(overall_outcomes)
}

############################### SOFT ########################################
transition_matrix = read.csv("C:/Users/pavan/Desktop/SFU/Research/Datasets/10_14/soft_transition_table_w_2m_wo_events_updated.csv")

transition_matrix

# ca ---> ac
transition_matrix[1,4] = transition_matrix[1,4]+transition_matrix[3,2]
transition_matrix[3,2] = NA

transition_matrix

# cb ---> ab
transition_matrix[1,3] = transition_matrix[1,3]+transition_matrix[3,3]
transition_matrix[3,3] = NA

transition_matrix

# cc ---> aa
transition_matrix[1,2] = transition_matrix[1,2]+transition_matrix[3,4]
transition_matrix[3,4] = NA

transition_matrix

# cd ---> af
transition_matrix[1,7] = transition_matrix[1,7]+transition_matrix[3,5]
transition_matrix[3,5] = NA

transition_matrix

# ce ---> ae
transition_matrix[1,6] = transition_matrix[1,6]+transition_matrix[3,6]
transition_matrix[3,6] = NA

transition_matrix

# cf ---> ad
transition_matrix[1,5] = transition_matrix[1,5]+transition_matrix[3,7]
transition_matrix[3,7] = NA

transition_matrix

# c: BP, OB, TO ----> a: BP, OB, TO

transition_matrix[1,8] = transition_matrix[1,8]+transition_matrix[3,8]
transition_matrix[3,8] = NA

transition_matrix[1,9] = transition_matrix[1,9]+transition_matrix[3,9]
transition_matrix[3,9] = NA

transition_matrix[1,10] = transition_matrix[1,10]+transition_matrix[3,10]
transition_matrix[3,10] = NA

transition_matrix

# fa ---> dc
transition_matrix[4,4] = transition_matrix[4,4]+transition_matrix[6,2]
transition_matrix[6,2] = NA

transition_matrix

# fb ---> db
transition_matrix[4,3] = transition_matrix[4,3]+transition_matrix[6,3]
transition_matrix[6,3] = NA

transition_matrix

# fc ---> da
transition_matrix[4,2] = transition_matrix[4,2]+transition_matrix[6,4]
transition_matrix[6,4] = NA

transition_matrix

# fd ---> df
transition_matrix[4,7] = transition_matrix[4,7]+transition_matrix[6,5]
transition_matrix[6,5] = NA

transition_matrix

# fe ---> de
transition_matrix[4,6] = transition_matrix[4,6]+transition_matrix[6,6]
transition_matrix[6,6] = NA

transition_matrix

# ff ---> dd
transition_matrix[4,5] = transition_matrix[4,5]+transition_matrix[6,7]
transition_matrix[6,7] = NA

transition_matrix

# c: BP, OB, TO ----> a: BP, OB, TO

transition_matrix[4,8] = transition_matrix[4,8]+transition_matrix[6,8]
transition_matrix[6,8] = NA

transition_matrix[4,9] = transition_matrix[4,9]+transition_matrix[6,9]
transition_matrix[6,9] = NA

transition_matrix[4,10] = transition_matrix[4,10]+transition_matrix[6,10]
transition_matrix[6,10] = NA

transition_matrix


# ec ---> ea
transition_matrix[5,2] = transition_matrix[5,2]+transition_matrix[5,4]
transition_matrix[5,4] = NA

transition_matrix


# ef ---> ed
transition_matrix[5,5] = transition_matrix[5,5]+transition_matrix[5,7]
transition_matrix[5,7] = NA

transition_matrix

# bc ---> ba
transition_matrix[2,2] = transition_matrix[2,2]+transition_matrix[2,4]
transition_matrix[2,4] = NA

transition_matrix

# bf ---> bd
transition_matrix[2,5] = transition_matrix[2,5]+transition_matrix[2,7]
transition_matrix[2,7] = NA

transition_matrix

# Ignore the unlikely transitions
# ac(0), af(0), dc(0), df(2)

transition_matrix[1,4] = NA
transition_matrix[1,7] = NA
transition_matrix[4,4] = NA
transition_matrix[4,7] = NA

transition_matrix

# remove rows and columns

transition_matrix = transition_matrix[-c(3,6),]
transition_matrix

transition_matrix = transition_matrix[,-c(4,7)]
transition_matrix

clear_transition_matrix = transition_matrix

# write.csv(clear_transition_matrix, "C:/Users/pavan/Desktop/SFU/Research/Datasets/9_26/soft_clear_transition_matrix.csv")
clear_transition_matrix

# Convert counts to probabilities
transition_probs <- clear_transition_matrix[,-1] / rowSums(clear_transition_matrix[,-1])
round(transition_probs,3)

set.seed(1)   

# 4 in-play regions
description <- c("a", "b", "d", "e")

# Transition (pass) matrix

A <- matrix(c(0.463, 0.052, 0.200, 0.170,
              0.084, 0.467, 0.006, 0.360,
              0.031, 0.002, 0.455, 0.079,
              0.020, 0.026, 0.100, 0.550),
            nrow = 4, byrow = TRUE)

# Terminal outcome probabilities
p_break      <- c(0.047, 0.023,  0.306, 0.220)
p_out    <- c(0.024, 0.005, 0.025, 0)
p_turnover <- c(0.045, 0.055, 0.103, 0.084)

P <- rbind(
  cbind(A, p_out, p_break, p_turnover),
  cbind(matrix(0, 3, 4), diag(3))
)

rownames(P) <- c(description, "Out of Bounds", "Break Press", "Turnover")
colnames(P) <- rownames(P)

alpha <- c((617+564)/7596, 825/7596,  (1323+1214)/7596, 3053/7596,0,0,0 )
steps <- c(1, 2, 3, 5, 10, 15, 20, 24, 100)

for (n in steps) {
  Pn <- P %^% n
  
  outcomes_by_region <- round(Pn[1:4, 5:7], 3)
  
  
  overall <- alpha %*% Pn
  overall_outcomes <- round(overall[, 5:7], 3)
  
  cat("\n=============================================\n")
  cat("After", n, "steps:\n")
  cat("=============================================\n")
  cat("Outcome probabilities by starting region:\n")
  print(outcomes_by_region)
  
  cat("\nWeighted overall outcome probabilities (using alpha):\n")
  print(overall_outcomes)
}
