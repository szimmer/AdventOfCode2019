library(tidyverse)

mass_input <- read_table("Data/Day01_Mass.txt",
                         col_names = FALSE)

#Fuel required to launch a given module is based on its mass. 
#Specifically, to find the fuel required for a module, take its mass, 
#divide by three, round down, and subtract 2.

fuel_function <- function(mass){
  floor(mass/3)-2
}

fuel_data <- mass_input %>%
  rename(mass=X1) %>%
  mutate(fuel=fuel_function(mass))

fuel_data %>% pull(fuel) %>% sum()
