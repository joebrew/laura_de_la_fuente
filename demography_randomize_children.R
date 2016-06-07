# This code reads in DSS data
# removes those mothers without a PERMID
# and selects (randomly) one child for each mother
# before writing a csv with the random selection

library(foreign)
library(readstata13)
library(dplyr)

# Read in data
df <- 
  readstata13::read.dta13('/media/joebrew/KINGSTON/DSS_member_METRO_0.dta')

# Keep only children who are born after 2012-05-10
df <- df %>%
  filter(birth_date >= '2012-05-10')

# Exclude those women without permid
df <- df %>% filter(mother_id != '888')

# Select one child for each mother

# First, we are going to enumerate the children RANDOMLY
df <- df[sample(1:nrow(df)),]
df <- 
  df %>% 
  mutate(dummy = 1) %>%
  group_by(mother_id) %>%
  mutate(child_number = cumsum(dummy))

# Only keep those who randomly got the 1. In the case of a 1-child household
# that child automatically gets kept
df <- df %>% filter(child_number == 1)

# Remove uncessary columns
df <- df %>% dplyr::select(-dummy, -child_number)
# Ahora que hemos hecho los cambios necesarios,

# Write a csv too
write.csv(df,
          file = '/media/joebrew/KINGSTON/DSS_member_METRO_0_randomized_children.csv')
