# This is a very clever solution that
# @aronstrandberg posted on Twitter
# https://github.com/aronolof/advent-of-code-2019

library(tidyverse)

input <- read_delim("Data/Day06_input.txt", delim=")", col_names=paste(1:2))
df <- filter(input, `1`=="COM")
while(!all(is.na(df[[ncol(df)]]))){
  df <- left_join(df, setNames(input, ncol(df) + 0:1), by=paste(ncol(df)))
}

df %>%
  pivot_longer(-1) %>%
  filter(!is.na(value), !duplicated(value)) %>%
  summarise(TotOrbits=sum(as.numeric(name)-1))

# --- Part Two ---
df %>%
  filter_all(any_vars(. == "YOU" | . == "SAN")) %>% 
  {as_tibble(t(.))} %>%
  filter(xor(is.na(V1), is.na(V2)) | V1 != V2) %>%
  summarise(Steps=sum(!is.na(.)) - 2)
