library(tidyverse)

input <- read_csv("Data/Day05_input.txt", col_names = FALSE)

ProgramOutput <- Day5Input <- as.vector(t(input))

# input <- read_csv("Data/Day02_input.txt", col_names = FALSE)
# 
# ProgramOutput <- Day2Input <- as.vector(t(input))
# ProgramOutput[2] <- 12
# ProgramOutput[3] <- 2


Intcode <- function(ProgramOutput, tmpmax=NULL){
  Steps <- ProgramOutput
  curidx <- 1
  Stop <- FALSE
  if (is.null(tmpmax)){
    tmpmax <- length(ProgramOutput)
  }
  Counter <- 0
  while (curidx < tmpmax ){
    Counter <- Counter + 1
    
    # Parse the instruction into opcode and modes
    inst <- str_pad(ProgramOutput[curidx], 5, "left", "0")
    opcode <- str_sub(inst, 4, 5) 
    modes <- str_sub(inst, 1, 3) %>% 
      str_split(pattern="", simplify=TRUE) %>% 
      as.vector() %>%
      as.numeric() %>% 
      rev() #reverse because it is read right to left
     
    if (opcode=="99"){
      break
    } else if (opcode=="01"){
      #Do summation
      Res <- 0
      for (i in 1:2){
        if (modes[i]==0){ # positional mode
          Res <- Res + ProgramOutput[ProgramOutput[curidx+i]+1]
        } else{ # immediate mode
          Res <- Res + ProgramOutput[curidx+i]
        }
      }

      if (modes[3]==0){
        ProgramOutput[ProgramOutput[curidx+3]+1] <- Res
      } else {
        stop("Parameters that an instruction writes to will never be in immediate mode.")
      }
      curidx <- curidx + 4
    } else if (opcode=="02"){
      #Do multiply
      Res <- 1
      for (i in 1:2){
        if (modes[i]==0){ # positional mode
          Res <- Res * ProgramOutput[ProgramOutput[curidx+i]+1]
        } else{ # immediate mode
          Res <- Res * ( ProgramOutput[curidx+i])
        }
      }
      
      if (modes[3]==0){
        ProgramOutput[ProgramOutput[curidx+3]+1] <- Res
      } else {
        stop("Parameters that an instruction writes to will never be in immediate mode.")
      }
      curidx <- curidx + 4
    } else if (opcode=="03"){
      # Opcode 3 takes a single integer as input and saves it to the position 
      # given by its only parameter. 
      # For example, the instruction 3,50 would take an input value 
      # and store it at address 50.
      Par <- ProgramOutput[curidx+1]   %>% as.double()
      # mode should always be 0 - check for this but that is expected
      if (modes[1]==0){
        ProgramOutput[Par+1] <- 1 # Input is 1
      } else{
        stop("Parameters that an instruction writes to will never be in immediate mode.")
      }
      curidx <- curidx + 2
    } else if (opcode=="04"){
      # Opcode 4 outputs the value of its only parameter. 
      # For example, the instruction 4,50 would output the value at address 50.
      # can use mode, would output value of 50 if mode is 1
      ImmVals <- (ProgramOutput[(curidx+1)] )  %>% as.double()
      PosVals <- ProgramOutput[ImmVals+1] %>% as.double()
      # mode should always be 0 - check for this but that is expected
      if (modes[1]==0){
        print(str_c("Step: ", Counter, ", Output: ", PosVals))
      } else if (modes[1]==1){
        print(str_c("Step: ", Counter, ", Output: ", ImmVals))
      }
      curidx <- curidx + 2
    }
    
  Steps <- cbind(Steps, ProgramOutput)
    
  }
  
  Steps <- t(Steps) 
  row.names(Steps) <- NULL
  return(list(ProgramOutput=ProgramOutput, Steps=Steps))
}

# TestCases
Intcode(c(3, 0, 4, 0, 99))
Intcode(c(3, 0, 4, 0, 104, 0 , 99))
Intcode(c(3, 0, 4, 0, 104, 0, 99))
Intcode(c(4, 0, 1101,100,-1,4,0))

j <- Intcode(Day5Input)
