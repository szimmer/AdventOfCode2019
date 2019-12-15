library(tidyverse)

# datin <- read_lines("Data/Day14_Input.txt")
# datin <- read_lines("Data/Day14_TestInput.txt")
datin <- read_lines("Data/Day14_TestInput2.txt")
# datin <- read_lines("Data/Day14_TestInput3.txt")

chemreaction <- tibble(React=datin) %>%
  separate(React, into=c("Input", "Output"), sep="=>") %>% #10
  mutate(Input=str_squish(Input),
         Output=str_squish(Output)) %>%
  separate(Input, into=str_c("Input", 1:10), sep=",", fill="right") %>%
  mutate(ReactionNumber=row_number()) %>%
  pivot_longer(cols=starts_with("Input"), values_drop_na=TRUE, values_to="Input") %>%
  select(-name) %>%
  mutate(Input=str_squish(Input)) %>%
  separate(Input, into=c("InputQuant", "InputChem"), sep=" ", convert=TRUE) %>%
  pivot_wider(id_cols=c("ReactionNumber", "Output"), names_from=InputChem, values_from=InputQuant, values_fill=list(InputQuant=0)) %>%
  separate(Output, into=c("OutputQuant", "OutputChem"), convert=TRUE) 

OutputChem <- chemreaction$OutputChem
OutputQuant <- chemreaction$OutputQuant
QuantMat <- chemreaction %>% 
  select(-ReactionNumber, -OutputQuant, -OutputChem) %>%
  as.matrix()

needs <- QuantMat[which(OutputChem=="FUEL"),]
newneeds <- totneeds <- rep(0, length(needs))
StopCrit <- 0

onlyInputOre <- c(OutputChem[which(QuantMat[, -1] %>% rowSums()==0)], "ORE")

while (StopCrit==0){
  newneeds <- rep(0, length(needs))
  print(needs)
  for (i in 2:length(needs)){
    if (needs[i]>0){
      needi <- needs[i] 
      nameneedi <- names(needi)
      if (nameneedi %in% onlyInputOre){
        totneeds[i] <- totneeds[i] + needi
      } else{
        newneeds <- newneeds + QuantMat[which(OutputChem==nameneedi), ]*needi
        # print(newneeds)
      }
    }
  }
  
  if (sum(newneeds)==0){
    StopCrit <- 1 
    break
  } else{
    needs <- newneeds
  }
  
}

needs_df <- tibble(OutputChem=names(needs),
                   NumberNeed=totneeds)

chemreaction %>%
  left_join(needs_df, by="OutputChem") %>%
  filter(NumberNeed>0) %>%
  select(OutputQuant, OutputChem, ORE, NumberNeed) %>%
  mutate(RoundUpNeed=ceiling(NumberNeed/OutputQuant)*ORE) %>%
  pull(RoundUpNeed) %>% sum()
