library(tidyverse)

input <- read_csv("Data/Day02_input.txt", col_names = FALSE)


ProgramInput <- ProgramOutput <- as.vector(t(input))

# To do this, before running the program, 
# replace position 1 with the value 12 and replace position 2 with the value 2.

ProgramOutput[2] <- 12
ProgramOutput[3] <- 2

# ProgramOutput <- c(1,1,1,4,99,5,6,0,99)

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
