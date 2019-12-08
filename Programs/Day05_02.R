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
  prevcuridx <- 0
  curidx <- 1
  
  if (is.null(tmpmax)){
    tmpmax <- length(ProgramOutput)
  }
  Counter <- 0
  HistIDX <- NULL
  while (curidx < tmpmax ){
    HistIDX <- c(HistIDX, curidx)
    if (curidx != prevcuridx){
      prevcuridx <- curidx
    } else{
      break # this means the index didn't change at all and is stuck
    }
    
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
          ValV[i] <- ProgramOutput[ProgramOutput[curidx+i]+1]
        } else{ # immediate mode
          ValV[i] <- ProgramOutput[curidx+i]
        }
      }
      
      if (ValV[i] < ValV[2]){
        ProgramOutput[ProgramOutput[curidx+3]+1] <- 1
      } else{
        ProgramOutput[ProgramOutput[curidx+3]+1] <- 0
      }
      curidx <- curidx + 4

    } else if (opcode=="08"){
      # Opcode 8 is equals: if the first parameter is equal to the second 
      # parameter, it stores 1 in the position given by the third parameter. 
      # Otherwise, it stores 0.
      ValV <- rep(NA, 2)
      for (i in 1:2){
        if (modes[i]==0){ # positional mode
          ValV[i] <- ProgramOutput[ProgramOutput[curidx+i]+1]
        } else{ # immediate mode
          ValV[i] <- ProgramOutput[curidx+i]
        }
      }
      
      if (ValV[1]==ValV[2]){
        ProgramOutput[ProgramOutput[curidx+3]+1] <- 1
      } else{
        ProgramOutput[ProgramOutput[curidx+3]+1] <- 0
      }
      curidx <- curidx + 4
    } else {
      stop("Opcode isn't valid")
    }
    
    
    Steps <- cbind(Steps, ProgramOutput)
    
  }
  
  Steps <- t(Steps) 
  row.names(Steps) <- NULL
  return(list(ProgramOutput=ProgramOutput, Steps=Steps, HistIDX=HistIDX))
}


# #Here's a larger example:
# # The program will then output 999 if the input value is below 8, output 1000 
# # if the input value is equal to 8, or output 1001 if the input value is greater 
# # than 8.
longp <- c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
           1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
           999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
j7 <- Intcode(longp, InputVal = 1)
j7 <- Intcode(longp, InputVal = 8)
j7 <- Intcode(longp, InputVal = 9)
j7[[3]]
Steps <- j7[[2]]
Steps[1, 1:2] # Save 9 to positon 22
Steps[1:2, 22]
Steps[2, 3:6] #Check if value at 22 == 8
Steps[2, 22] #doesn't equal so save a 0 at 21
Steps[1:3, 21]
Steps[3, 7:10] # ....

# j <- Intcode(Day5Input, InputVal = 5)
# Steps <- j[[2]]
# #Checking steps
# Steps[1, 1:2] # Should take input and save to position 226
# Steps[1:2, 226]
# Steps[2, 3:6] # Should take value at 226 and value at 7 and save to position 7
# Steps[2, 226]+Steps[2,7]
# Steps[1:3, 7]
# Steps[3, 7:9] # since 1!=0, jump to 239 for inxtructions (immediate evaluation)
# Steps[4, 239:241] #since 0==0, go to 242
# Steps[5, 242:244] #since 227!=0, jump to position at 248
# Steps[6, 248:250] # find value at 228 - check if it is 0
# Steps[6,228] # It is 0 so just go to next step
# Steps[7, 251:253] # find value at 1 and check if it is 0
# Steps[7, 1] # It is not 0 so jump to 257
# Steps[8, 257:259] #227!=0 so go to next instruction
# Steps[9, 260:262] # It is 0 so jump, pointer goes to 266
# Steps[10, 266:268] # Check whether value at positon 0[1 in R]==0
# Steps[10, 1] # This is not 0 so do not jump and just go to 269
# Steps[11, 269:271] #Check whether value at position 227[228 in R]==0
# Steps[11, 228] # this is 0 so jump to 275
# Steps[12, 275:277] #1==1 so jump to 281
# Steps[13, 281:284] # Add value at postion 226 to value at 226 and put it in 226
# Steps[13:14, 226]
# Steps[14, 285:288] # Add 294 and 0 and put it in first position
# Steps[1:15, 1] 
# Steps[15, 289:291] # jump if true, 1=1... this jumps back to 1
# Steps[16, 1:4]
# 
# j[[3]]
# #Expect curidx to be c(1,3,7,239,242,248,251,257, 260, 266, 269, 275, 281, 285, 289)
