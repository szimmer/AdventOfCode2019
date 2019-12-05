# It is a six-digit number.
# The value is within the range given in your puzzle input.
# Two adjacent digits are the same (like 22 in 122345).
# Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

# Input 168630-718098

# How many different passwords within the range given in your puzzle input meet these criteria?

library(tidyverse)

pwdcheck <- tibble(PWDTry=168630:718098) %>%
  mutate(PWDChar=as.character(PWDTry)) %>%
  separate(PWDChar, into=str_c("Dig", 1:6), sep=1:5, convert=TRUE) %>%
  mutate(Check1=Dig1==Dig2|Dig2==Dig3|Dig3==Dig4|Dig4==Dig5|Dig5==Dig6) %>%
  filter(Check1) %>%
  mutate(Check2=Dig1<=Dig2&Dig2<=Dig3&Dig3<=Dig4&Dig4<=Dig5&Dig5<=Dig6) %>%
  filter(Check2)
  
nrow(pwdcheck)
