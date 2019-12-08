library(tidyverse)

input <- read_csv("Data/Day05_input.txt", col_names = FALSE)

ProgramOutput <- Day5Input <- as.vector(t(input))

# input <- read_csv("Data/Day02_input.txt", col_names = FALSE)
# 
# ProgramOutput <- Day2Input <- as.vector(t(input))
# ProgramOutput[2] <- 12
# ProgramOutput[3] <- 2


Intcode <- function(ProgramOutput, InputVal=1, tmpmax=NULL){
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
    if (opcode=="99"){
      break
    }
    modes <- str_sub(inst, 1, 3) %>% 
      str_split(pattern="", simplify=TRUE) %>% 
      as.vector() %>%
      as.numeric() %>% 
      rev() #reverse because it is read right to left
     
 
    if (opcode=="01"){
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
        ProgramOutput[Par+1] <- InputVal
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
    } else if (opcode=="05"){
      # Opcode 5 is jump-if-true: 
      # if the first parameter is non-zero, it sets the instruction pointer
      # to the value from the second parameter. Otherwise, it does nothing.
      ImmVals <- ProgramOutput[curidx+1] 
      PosVals <- ProgramOutput[ImmVals+1]
      if (modes[1]==0){ # positional
        UseVal <- PosVals
      } else if (modes[1]==1){
        UseVal <- ImmVals
      }
      if (UseVal!=0){
        curidx <- ProgramOutput[curidx+2]+1
      } else{
        curidx <- curidx+3
      }
    } else if (opcode=="06"){
      # Opcode 6 is jump-if-false: if the first parameter is zero, 
      # it sets the instruction pointer to the value from the second parameter. 
      # Otherwise, it does nothing.
      ImmVals <- ProgramOutput[curidx+1] 
      PosVals <- ProgramOutput[ImmVals+1]
      if (modes[1]==0){ # positional
        UseVal <- PosVals
      } else if (modes[1]==1){
        UseVal <- ImmVals
      }
      if (UseVal==0){
        curidx <- ProgramOutput[curidx+2]+1
      } else{
        curidx <- curidx+3
      }
    } else if (opcode=="07"){
      # Opcode 7 is less than: if the first parameter is less than the second 
      # parameter, it stores 1 in the position given by the third parameter. 
      # Otherwise, it stores 0.
      ValV <- rep(NA, 2)
      for (i in 1:2){
        if (modes[i]==0){ # positional mode
          ValV[i] ProgramOutput[ProgramOutput[curidx+i]+1]
        } else{ # immediate mode
          ValV[i] <- ProgramOutput[curidx+i]
        }
      }
      
      if (ValV[i] < ValV[2]){
        ProgramOutput[ProgramOutput[curidx+3]+1] <- 1
      } else{
        ProgramOutput[ProgramOutput[curidx+3]+1] <- 0
      }

    } else if (opcode=="08"){
      # Opcode 8 is equals: if the first parameter is equal to the second 
      # parameter, it stores 1 in the position given by the third parameter. 
      # Otherwise, it stores 0.
      ValV <- rep(NA, 2)
      for (i in 1:2){
        if (modes[i]==0){ # positional mode
          ValV[i] ProgramOutput[ProgramOutput[curidx+i]+1]
        } else{ # immediate mode
          ValV[i] <- ProgramOutput[curidx+i]
        }
      }
      
      if (ValV[1]==ValV[2]){
        ProgramOutput[ProgramOutput[curidx+3]+1] <- 1
      } else{
        ProgramOutput[ProgramOutput[curidx+3]+1] <- 0
      }
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
