library(tidyverse)
library(janitor)

# Load Baltimore calls and ZCTA demographics
baltimore_911 <- read_csv('data/baltimore_911.csv') %>%
  clean_names() %>%
  mutate(zip_code = as.character(zip_code))

maryland_zcta <- read_csv('data/maryland_zcta.csv') %>%
  mutate(ZCTA5N = as.character(ZCTA5N))

# Join
baltimore_911_with_demographics <- baltimore_911 %>%
  left_join(maryland_zcta, join_by(zip_code == ZCTA5N))

# Inspect the field PLT18SP (percentage under 18?)
cat('PLT18SP summary:\n')
print(summary(baltimore_911_with_demographics$PLT18SP))

# Rows where PLT18SP >= 75
rows75 <- baltimore_911_with_demographics %>%
  filter(!is.na(PLT18SP) & PLT18SP >= 75)

cat('\nDistinct zip_code, POP100, PLT18SP for PLT18SP >= 75:\n')
print(rows75 %>% select(zip_code, POP100, PLT18SP) %>% distinct())

# Calls per zip for PLT18SP >= 75 (zip, population, calls)
cat('\nCalls per zip where PLT18SP >= 75:\n')
calls_by_zip <- rows75 %>%
  group_by(zip_code, POP100, PLT18SP) %>%
  summarise(calls = n(), .groups = 'drop') %>%
  arrange(desc(calls))
print(calls_by_zip)
