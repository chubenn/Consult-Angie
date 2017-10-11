library(mice)
library(Amelia)

#josue's computer
setwd('~/Documents/GitHub/AngieG-Thesis')
angie_clean <- read.csv('~/Documents/GitHub/AngieG-Thesis/angie_clean.csv') 

#Ben's
angie_clean <- read_csv("~/GitHub/AngieG-Thesis/angie_clean.csv")

md.pattern(angie_clean)
missmap(angie_clean)

#impute data
angie_impute <- angie_clean[ ,64:363] %>%
  mice(m=1, maxit = 5, method = 'fastpmm', seed = 69) %>%
  complete() %>%
  tbl_df() %>%
  janitor::clean_names()

angie_impute1 <- angie_impute %>%
  mice(m=1, maxit = 1, method = 'fastpmm', seed = 69) %>%
  complete()

angie_impute2 <- angie_impute1 %>%
  mice(m=1, maxit = 1, method = 'fastpmm', seed = 69) %>%
  complete()


missmap(angie_clean)
missmap(angie_impute)
missmap(angie_impute1)
missmap(angie_impute2)


angie_cat <- angie_clean[,1:63]

angie_complete <- cbind(angie_impute2,angie_cat)

