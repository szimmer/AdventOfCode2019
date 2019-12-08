library(tidyverse)

imagedat_in <- scan("Data/Day08_input.txt", what=character())

imagesize <- c(25, 6)
n_layers <- str_length(imagedat_in)/prod(imagesize)
endnum <- 1:n_layers*prod(imagesize)
startnum <- endnum - prod(imagesize)+1
# To make sure the image wasn't corrupted during transmission, 
# the Elves would like you to find the layer that contains the 
# fewest 0 digits. 
# On that layer, what is the number of 1 digits multiplied by the 
# number of 2 digits?


Layers <- str_sub(imagedat_in, start=startnum, end=endnum)
ZeroCount <- Layers %>% str_count("0")
LayerSelect <- Layers[which.min(ZeroCount)]

str_count(LayerSelect, "1")*str_count(LayerSelect, "2")
