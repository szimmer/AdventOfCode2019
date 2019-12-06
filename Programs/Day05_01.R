library(tidyverse)

input <- read_csv("Data/Day05_input.txt", col_names = FALSE)

ProgramOutput <- Day5Input <- as.vector(t(input))

input <- read_csv("Data/Day02_input.txt", col_names = FALSE)

ProgramOutput <- Day2Input <- as.vector(t(input))
ProgramOutput[2] <- 12
ProgramOutput[3] <- 2

 
Intcode <- function(ProgramOutput){
  Steps <- ProgramOutput
  curidx <- 1
  Stop <- FALSE
  
  while (curidx < length(ProgramOutput) & !Stop){
    inst <- str_pad(ProgramOutput[curidx], 5, "left", "0")
    opcode <- str_sub(inst, 4, 5) 
    modes <- str_sub(inst, 1, 3) %>% str_split(pattern="", simplify=TRUE) %>% 
      as.vector() %>%
      as.numeric()
    
    if (opcode=="99"){
      Stop <- TRUE
      break
    } else if (opcode=="01"){
      #Do summation
      ImmVals <- (ProgramOutput[(curidx+1):(curidx+3)] ) 
      ImmVals[3] <- ImmVals[3] + 1.0
      PosVals <- ProgramOutput[ImmVals] %>% as.double()
      UseVals <- if_else(modes==0, PosVals, ImmVals)
      print(ImmVals)
      print(PosVals)
      print(UseVals)
      ProgramOutput[UseVals[3]] <- sum(UseVals[1:2])
      curidx <- curidx + 4
    } else if (opcode=="02"){
      #Do multiply
      ImmVals <- (ProgramOutput[(curidx+1):(curidx+3)] ) 
      ImmVals[3] <- ImmVals[3] + 1.0
      PosVals <- ProgramOutput[ImmVals] %>% as.double()
      UseVals <- if_else(modes==0, PosVals, ImmVals)
      print(ImmVals)
      print(PosVals)
      print(UseVals)
      
      ProgramOutput[UseVals[3]] <- prod(UseVals[1:2])
      curidx <- curidx + 4
    } else if (opcode=="03"){
      # Opcode 3 takes a single integer as input and saves it to the position 
      # given by its only parameter. 
      # For example, the instruction 3,50 would take an input value 
      # and store it at address 50.
    } else if (opcode=="04"){
      # Opcode 4 outputs the value of its only parameter. 
      # For example, the instruction 4,50 would output the value at address 50.
      
    }
    
    Steps <- cbind(Steps, ProgramOutput)

    
  }
  
  
  Steps_df <- Steps %>% as_tibble(.name_repair = "unique") %>%
     set_names(str_c("X", 1:ncol(Steps)))

  return(list(ProgramOutput=ProgramOutput, Steps=Steps))
  # return(ProgramOutput)
}

Day2Try <- Intcode(c(1,1,1,4,99,5,6,0,99))
Day2Try[[1]]
