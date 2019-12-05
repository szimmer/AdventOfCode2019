# It is a six-digit number.
# The value is within the range given in your puzzle input.
# Two adjacent digits are the same (like 22 in 122345).
# Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

# Input 168630-718098

# How many different passwords within the range given in your puzzle input meet these criteria?

# An Elf just remembered one more important detail: 
# the two adjacent matching digits are not part of 
# a larger group of matching digits.


library(tidyverse)

pwdcheck <- tibble(PWDTry=168630:718098) %>%
  mutate(PWDChar=as.character(PWDTry)) %>%
  separate(PWDChar, into=str_c("Dig", 1:6), sep=1:5, convert=TRUE, remove=FALSE) %>%
  mutate(Check1=Dig1==Dig2|Dig2==Dig3|Dig3==Dig4|Dig4==Dig5|Dig5==Dig6) %>%
  filter(Check1) %>%
  mutate(Check2=Dig1<=Dig2&Dig2<=Dig3&Dig3<=Dig4&Dig4<=Dig5&Dig5<=Dig6) %>%
  filter(Check2)

nrow(pwdcheck)
# 123456
# Need a group of exactly 2, not 3, 4, 5, or 6
pwdcheckmore <- pwdcheck %>%
  select(-Check1, -Check2) %>%
  mutate(Num1s=(Dig1==1)+(Dig2==1)+(Dig3==1)+(Dig4==1)+(Dig5==1)+(Dig6==1),
         Num2s=(Dig1==2)+(Dig2==2)+(Dig3==2)+(Dig4==2)+(Dig5==2)+(Dig6==2),
         Num3s=(Dig1==3)+(Dig2==3)+(Dig3==3)+(Dig4==3)+(Dig5==3)+(Dig6==3),
         Num4s=(Dig1==4)+(Dig2==4)+(Dig3==4)+(Dig4==4)+(Dig5==4)+(Dig6==4),
         Num5s=(Dig1==5)+(Dig2==5)+(Dig3==5)+(Dig4==5)+(Dig5==5)+(Dig6==5),
         Num6s=(Dig1==6)+(Dig2==6)+(Dig3==6)+(Dig4==6)+(Dig5==6)+(Dig6==6),
         Num7s=(Dig1==7)+(Dig2==7)+(Dig3==7)+(Dig4==7)+(Dig5==7)+(Dig6==7),
         Num8s=(Dig1==8)+(Dig2==8)+(Dig3==8)+(Dig4==8)+(Dig5==8)+(Dig6==8),
         Num9s=(Dig1==9)+(Dig2==9)+(Dig3==9)+(Dig4==9)+(Dig5==9)+(Dig6==9),
         Groupof2=Num1s==2|Num2s==2|Num3s==2|Num4s==2|Num5s==2|Num6s==2|Num7s==2|Num8s==2|Num9s==2) %>%
  filter(Groupof2)

nrow(pwdcheckmore)
