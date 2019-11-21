pacman::p_load(tidyverse)

#Calculate P(any one player selected >=5x in 10 times)
a <- dbinom(c(0:10), 10, 10/77)
chance_single_5plus <- sum(a[6:11])


#Simulate number of players on a single team with >= 5 tests
simulate_team <- function(ignore = NA) {
  
  tests_left <- 100
  tests_done <- c(rep(NA, 77))
  
  
  for(i in 1:77){
    
    if(tests_left < 1){
      
      tests_done[i] <- 0
    }
    
    else if(i == 77){
      
      tests_done[i] <- tests_left
    }
    
    else if(tests_left >= 10*(77-i)){
      
      tests_done[i] <- 10
      tests_left <- tests_left - 10
      
    }
    
    else{
      
    tests_done[i] <- min(rbinom(1, 10, 10*(tests_left/100)/(77-i+1)), tests_left)
    tests_left <- tests_left - tests_done[i]
    
    }
  }
  
  return(sum(tests_done >= 5))
  
}

results <- c(rep(NA, 1e5))
set.seed <- 45
results <- sapply(results, simulate_team)

pct_1_or_more_players <- sum(results >= 1)/1e4
