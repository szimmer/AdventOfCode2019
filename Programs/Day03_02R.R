
library(tidyverse)
wirepath_in <- read_csv("Data/Day03_input.txt", col_names = FALSE)



MakePath <- function(dir){
  lastx <- lasty <- x <- y <- 0
  Nsteps <- length(dir)
  Direction <- str_sub(dir, 1, 1)
  Dist <- str_sub(dir, 2) %>% as.numeric() 
  Turn <- 1
  for (i in 1:Nsteps){
    Diri <- Direction[i]
    Disti <- Dist[i]
    if (Diri=="U"){
      # Go up
      x <- c(x, rep(lastx, Disti)) # x doesn't change and just goes along
      y <- c(y, lasty+1:Disti) #y increases
    } else if (Diri=="D"){
      # Go down
      x <- c(x, rep(lastx, Disti)) # x doesn't change and just goes along
      y <- c(y, lasty-1:Disti) #y decreases
    } else if (Diri=="L"){
      # Go left
      y <- c(y, rep(lasty, Disti)) # y doesn't change and just goes along
      x <- c(x, lastx-1:Disti) #x decreases
    } else if (Diri=="R"){
      # Go right
      y <- c(y, rep(lasty, Disti)) # y doesn't change and just goes along
      x <- c(x, lastx+1:Disti) #x decreases
    }
    Turn <- c(Turn, rep(0, Disti-1), 1)
    lastx <- x[length(x)]
    lasty <- y[length(y)]
  }
  PathList <- tibble(x=x, y=y)
  return(PathList)
}  

dir1 <- "R75,D30,R83,U83,L12,D49,R71,U7,L72" %>% str_split(",", simplify = TRUE) %>% as.vector()
dir2 <- "U62,R66,U55,R34,D71,R55,D58,R83" %>% str_split(",", simplify = TRUE) %>% as.vector()
Path1 <- MakePath(dir1) %>% mutate(Step1=row_number()-1)
Path2 <- MakePath(dir2) %>% mutate(Step2=row_number()-1)

# Expect 610 as best answer
Path1 %>% inner_join(Path2, by=c("x", "y")) %>%
  mutate(Dist=abs(x)+abs(y),
         TotalSteps=(Step1+Step2)) %>%
  arrange(TotalSteps) %>% 
  slice(2)

### Solution for challenge
wiredirections1 <- wirepath_in %>%
  slice(1) %>%
  unlist(., use.names=FALSE)
wiredirections2 <- wirepath_in %>%
  slice(2) %>%
  unlist(., use.names=FALSE)

Path1 <- MakePath(wiredirections1) %>% mutate(Step1=row_number()-1)
Path2 <- MakePath(wiredirections2) %>% mutate(Step2=row_number()-1)

Path1 %>% inner_join(Path2, by=c("x", "y")) %>%
  mutate(Dist=abs(x)+abs(y),
         TotalSteps=(Step1+Step2)) %>%
  arrange(TotalSteps) %>% 
  slice(2)
