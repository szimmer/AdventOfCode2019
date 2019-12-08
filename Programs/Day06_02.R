library(tidyverse)

OrbTest=c(
'COM)B',
'B)C',
'C)D',
'D)E',
'E)F',
'B)G',
'G)H',
'D)I',
'E)J',
'J)K',
'K)L',
'K)YOU',
'I)SAN'
)

CountOrbits <- function(Orb){
  Orb_DF <- tibble(Orb=Orb) %>%
    separate(Orb, into=c("Orb1", "Orb2"), "\\)", remove=FALSE)
  
  Planets <- unique(c(Orb_DF$Orb1, Orb_DF$Orb2))
  PlanetList <- vector("list", length(Planets))
  names(PlanetList) <- Planets  
  
  ListLength <- 0
  
  # List direct orbits
  for (pl in Planets){
    orbitees <- Orb_DF %>% 
      filter(Orb1==pl) %>%
      pull(Orb2)
    PlanetList[[pl]] <- orbitees
    }
  
  
  StillChecking <- TRUE
  while (StillChecking){
    ListLength <- PlanetList %>% map_int(length) %>% sum()  
    for (pl in Planets){
      suborbits <- PlanetList[PlanetList[[pl]]] %>% unlist() %>% unname()
      PlanetList[[pl]] <- unique(c(PlanetList[[pl]], suborbits))
    }
    newListLength <- PlanetList %>% map_int(length) %>% sum()  
    if (newListLength==ListLength){
      StillChecking <- FALSE
    }
  }
  
  return(list(NumberOrbits=ListLength, OrbitList=PlanetList))
}

Test <- CountOrbits(OrbTest)
OrbitList <- Test[[2]]



# Day6Inp <- read_csv("Data/Day06_input.txt", col_names = FALSE)
# Day6Orb <- Day6Inp %>% pull(X1)
# OrbitListFindSanta <- CountOrbits(Day6Orb)
# str(OrbitListSanta)