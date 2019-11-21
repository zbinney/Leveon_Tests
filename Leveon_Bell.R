pacman::p_load(tidyverse)

#Calculate P(any one player selected >=5x in 10 times)
a <- dbinom(c(0:10), 10, 10/77)
chance_single_5plus <- sum(a[6:11])


#Simulate number of players on a single team with >= 5 tests
  simulate_team <- function(ignore = NA){
    
    t <- c(rep(1,10), rep(0,67))
    output <- matrix(nrow = 77, ncol = 10)
    
    for(wk in 1:10){
      
      output[,wk] <- sample(t)
      
    }
    
    a <- rowSums(output)
    
    return(sum(a >= 5))
    
  }

  nreps <- 1e5
  results <- c(rep(NA, nreps))
  set.seed <- 45
  results <- sapply(results, simulate_team)
  
  pct_1_or_more_players <- sum(results >= 1)/nreps
