
library(tidyverse)
wirepath_in <- read_csv("Data/Day03_input.txt", col_names = FALSE)

wirepath1 <- wirepath_in %>%
  slice(1) %>%
  pivot_longer(cols=starts_with("X"))
