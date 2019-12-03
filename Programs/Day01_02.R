library(tidyverse)

mass_input <- read_table("Data/Day01_Mass.txt",
                         col_names = FALSE)

#Fuel required to launch a given module is based on its mass. 
#Specifically, to find the fuel required for a module, take its mass, 
#divide by three, round down, and subtract 2.

fuel_function_base <- function(mass){
  floor(mass/3)-2
}

fuel_function <- function(mass){
  totfuel <- 0
  stop <- 0
  while (stop==0){
    mass <- fuel_function_base(mass)
    if (mass > 0){
      totfuel <- totfuel+mass
    } else{
      stop <- 1
    }
  }
  return(totfuel)
}

eachfuel <- mass_input$X1 %>% map_dbl(fuel_function)

sum(eachfuel)
