# TMRCA Simulation - Based on Chang's Model
# Simulates the number of generations to reach a Most Recent Common Ancestor (MRCA)

# Function to simulate MRCA for a given population size
simulate_TMRCA <- function(n) {
  population <- 1:n
  generations <- 0
  
  # Repeat until all individuals have the same ancestor
  while (length(unique(population)) > 1) {
    generations <- generations + 1
    population <- sample(population, size = n, replace = TRUE)
  }
  
  return(generations)
}

# Function to run multiple simulations and track how long it takes
run_simulations <- function(n, num_trials = 25) {
  results <- numeric(num_trials)
  start_time <- Sys.time()
  
  for (i in 1:num_trials) {
    results[i] <- simulate_TMRCA(n)
  }
  
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  
  return(list(times = results, duration = time_taken))
}

# Set population sizes and run simulations
set.seed(123)  # For consistent results
pop_sizes <- c(1000, 2000, 4000)
all_results <- list()

for (n in pop_sizes) {
  cat("Running simulations for population size:", n, "\n")
  result <- run_simulations(n, num_trials = 25)
  all_results[[as.character(n)]] <- result
}

# Display summary for each population size
for (n in names(all_results)) {
  cat("\nPopulation:", n, "\n")
  cat("Mean generations to MRCA:", mean(all_results[[n]]$times), "\n")
  cat("Standard deviation:", sd(all_results[[n]]$times), "\n")
  cat("Simulation time:", all_results[[n]]$duration, "\n")
}

# Save data as CSV
tmrca_data <- data.frame()

for (n in names(all_results)) {
  df <- data.frame(
    Population = as.integer(n),
    Trial = 1:25,
    Generations = all_results[[n]]$times
  )
  tmrca_data <- rbind(tmrca_data, df)
}

write.csv(tmrca_data, "tmrca_results.csv", row.names = FALSE)
