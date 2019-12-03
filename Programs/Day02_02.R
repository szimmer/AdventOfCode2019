library(tidyverse)

input <- read_csv("Data/Day02_input.txt", col_names = FALSE)


ProgramInput <- ProgramOutput <- as.vector(t(input))

# To do this, before running the program, 
# replace position 1 with the value 12 and replace position 2 with the value 2.

# ProgramOutput[2] <- 12
# ProgramOutput[3] <- 2

IntFunNounVerb <- function(noun, verb, ProgramOutput){
  ProgramOutput[2] <- noun
  ProgramOutput[3] <- verb
  curidx <- 1
  
  while (curidx < length(ProgramOutput)){
    if (ProgramOutput[curidx]==1){
      # Do summation 
      Ridx <- ProgramOutput[(curidx+1):(curidx+3)] + 1
      ProgramOutput[Ridx[3]] <- sum(ProgramOutput[Ridx[1:2]])
      curidx <- curidx + 4
    } else if (ProgramOutput[curidx]==2){
      Ridx <- ProgramOutput[(curidx+1):(curidx+3)] + 1
      ProgramOutput[Ridx[3]] <- prod(ProgramOutput[Ridx[1:2]])
      curidx <- curidx + 4
    } else if (ProgramOutput[curidx]==99){
      break
    }
    
  }
  
  return(ProgramOutput[1])
}

IntFunNounVerb(12, 2, ProgramInput) # Check from last time

TestVals <- expand.grid(noun=0:99, verb=0:99, KEEP.OUT.ATTRS = FALSE)

EvaluateTestVals <- TestVals %>%
  rowwise() %>%
  mutate(Result=IntFunNounVerb(noun, verb, ProgramInput))

(Solution <- EvaluateTestVals %>%
  filter(Result==19690720) %>%
  mutate(Answer=100 * noun + verb))

