library(tidyverse)

input <- read_csv("Data/Day05_input.txt", col_names = FALSE)

Day5Input <- as.vector(t(input))

#  ProgramOutput <- as.vector(t(input))

Intcode <- function(ProgramOutput){
  Steps <- ProgramOutput
  curidx <- 1
  
  while (curidx < length(ProgramOutput)){
    if (ProgramOutput[curidx]==1){
      # Do summation 
      Ridx <- ProgramOutput[(curidx+1):(curidx+3)] + 1
      ProgramOutput[Ridx[3]] <- sum(ProgramOutput[Ridx[1:2]])
      Steps <- cbind(Steps, ProgramOutput)
      curidx <- curidx + 4
    } else if (ProgramOutput[curidx]==2){
      Ridx <- ProgramOutput[(curidx+1):(curidx+3)] + 1
      ProgramOutput[Ridx[3]] <- prod(ProgramOutput[Ridx[1:2]])
      Steps <- cbind(Steps, ProgramOutput)
      curidx <- curidx + 4
    } else if (ProgramOutput[curidx]==99){
      break
    }
    
  }
  
  Steps_df <- Steps %>% as_tibble(.name_repair = "unique") %>%
    set_names(str_c("X", 1:ncol(Steps)))

  return(list(ProgramOutput=ProgramOutput, Steps=Steps_df))  
}

