
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
 
Y_A_counts <- c(565, 76, 182, 140, 56, 28, 56)
Y_B_counts <- c(74, 596, 5, 233, 24, 11, 177)
Y_D_counts <- c(87, 2, 949, 156, 509, 75, 233)
Y_E_counts <- c(76, 157, 308, 1675, 510, 19, 227)

stan_data <- list(
  K = 7,
  Y_A = Y_A_counts,
  Y_B = Y_B_counts,
  Y_D = Y_D_counts,
  Y_E = Y_E_counts
)

 
fit <- stan(
  file = "C:/Users/pavan/Desktop/SFU/Research/STAN/stage3.stan",    
  data = stan_data,
  chains = 4,
  iter = 8000,
  warmup = 4000,
  seed = 42,
  control = list(adapt_delta = 0.99)
)

print(fit, pars = c("theta_A","theta_B","theta_D","theta_E"), digits = 3)

 
post <- rstan::extract(fit)

theta_A_draws <- post$theta_A
theta_B_draws <- post$theta_B
theta_D_draws <- post$theta_D
theta_E_draws <- post$theta_E

n_iter <- dim(theta_A_draws)[1]
 
TO_index <- 7                  
absorbing_indices <- c(5,6,7)  

 
simulate_sequence <- function(theta_A, theta_B, theta_D, theta_E,
                              start_state = "A") {
  
  current_state <- start_state
  n_transitions <- 0
  
  repeat {
    probs <- switch(current_state,
                    "A" = theta_A,
                    "B" = theta_B,
                    "D" = theta_D,
                    "E" = theta_E
    )
    
    next_state <- sample(1:7, size = 1, prob = probs)
    n_transitions <- n_transitions + 1
    
    if (next_state %in% absorbing_indices) {
      if (next_state == TO_index) {
        return(n_transitions)    
      } else {
        return(Inf)              
      }
    }
   
    current_state <- c("A","B","D","E")[next_state]
  }
}
 
starting_states <- c("A","B","D","E")
results_by_start <- list()

set.seed(42)
for (start in starting_states) {
  
  outcomes <- numeric(n_iter)
  
  for (s in 1:n_iter) {
    outcomes[s] <- simulate_sequence(
      theta_A_draws[s, ],
      theta_B_draws[s, ],
      theta_D_draws[s, ],
      theta_E_draws[s, ],
      start_state = start
    )
  }
  
  results_by_start[[start]] <- c(
    "P(no ultimate TO)"       = mean(outcomes == Inf),
    "P(TO on 1st transition)" = mean(outcomes == 1),
    "P(TO on 2nd transition)" = mean(outcomes == 2),
    "P(TO on 3rd transition)" = mean(outcomes == 3),
    "P(TO on 4+ transitions)" = mean(outcomes >= 4 & outcomes < Inf)
  )
}
 

for (start in starting_states) {
  cat("\nStarting region:", start, "\n")
  print(round(results_by_start[[start]], 3))
}
print(
  fit,
  pars = c("theta_A","theta_B","theta_D","theta_E"),
  probs = c(0.025, 0.5, 0.975),
  digits = 3
)

