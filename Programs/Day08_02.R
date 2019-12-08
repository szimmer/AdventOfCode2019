library(tidyverse)

imagedat_in <- scan("Data/Day08_input.txt", what=character())
imagesize <- c(25, 6)

# imagedat_in <- "0222112222120000"
# imagesize <- c(2,2)
n_layers <- str_length(imagedat_in)/prod(imagesize)
endnum <- 1:n_layers*prod(imagesize)
startnum <- endnum - prod(imagesize)+1
# To make sure the image wasn't corrupted during transmission, 
# the Elves would like you to find the layer that contains the 
# fewest 0 digits. 
# On that layer, what is the number of 1 digits multiplied by the 
# number of 2 digits?


Layers <- str_sub(imagedat_in, start=startnum, end=endnum)

# 0 is black, 1 is white, and 2 is transparent.

FinalPicture <- rep(2, prod(imagesize))

for (pix in 1:prod(imagesize)){
  lyr <- 0
  while(FinalPicture[pix]==2 & lyr< 100 ){
    lyr <- lyr + 1
    FinalPicture[pix] <- str_sub(Layers[lyr], pix, pix) %>% as.numeric()
  }
}

dfp <- tibble(ColourNum=FinalPicture) %>%
  filter(ColourNum !=2) %>%
  mutate(x=rep(1:25, 6),
         y=rep(6:1, each=25),
         Colour=case_when(
           ColourNum==0~"black",
           ColourNum==1~"white")) 

dfp %>% 
  filter(ColourNum==1) %>%
  ggplot(aes(x=x, y=y))+
  geom_point()


# CJZHR
