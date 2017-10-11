library(tidyverse)
library(foreign)
library(haven)

#ben's computer
amg_clean <- read_csv("~/GitHub/AngieG-Thesis/bentest.csv")
amg_clean <- read_sav("~/GitHub/AngieG-Thesis/AMG working data 8-29-17.sav")

#josues computer
setwd('~/Documents/GitHub/AngieG-Thesis/')
amg_clean <- read.spss('~/Documents/GitHub/AngieG-Thesis/AMG working data 8-29-17.sav') 
amg_clean <- read.csv('~/Documents/GitHub/AngieG-Thesis/bentest.csv') 

#clean names and turn into tibble
amg_clean %>%  
  janitor::clean_names() %>% 
  tbl_df()

# set column names. first is 1 to 5 and second is 1 to 7 recodes
col_names <- c("MPCA9","MPCA11","MPCA20","MPCA22","MPCA23",
               'MPCA9.2','MPCA11.2','MPCA20.2','MPCA22.2','MPCA23.2',
               'MPCA9.3','MPCA11.3','MPCA20.3','MPCA22.3',"MPCA23.3",
               "MPCAG3", "MPCAG5", "MPCAG8", "MPCAG12","MPCAG13", 'MPCAG14',
               'MPCAG3.2','MPCAG5.2','MPCAG8.2','MPCAG12.2','MPCAG13.2','MPCAG14.2',
               'MPCAG3.3','MPCAG5.3','MPCAG8.3','MPCAG12.3','MPCAG13.3','MPCAG14.3',
               'PSISF1','PSISF2','PSISF3','PSISF4','PSISF5','PSISF6','PSISF7','PSISF8','PSISF9','PSISF10',
               'PSISF11','PSISF12','PSISF13','PSISF14','PSISF15','PSISF16','PSISF17','PSISF18','PSISF19','PSISF20',
               'PSISF21','PSISF22','PSISF23','PSISF24','PSISF25','PSISF26','PSISF27','PSISF28','PSISF29','PSISF30',
               'PSISF31','PSISF32','PSISF33','PSISF34',
               'PSISF1.2','PSISF2.2','PSISF3.2','PSISF4.2','PSISF5.2','PSISF6.2','PSISF7.2',
               'PSISF8.2','PSISF9.2','PSISF10.2','PSISF11.2','PSISF12.2','PSISF13.2','PSISF14.2',
               'PSISF15.2','PSISF16.2','PSISF17.2','PSISF18.2','PSISF19.2','PSISF20.2','PSISF21.2',
               'PSISF22.2','PSISF23.2','PSISF24.2','PSISF25.2','PSISF26.2','PSISF27.2','PSISF28.2',
               'PSISF29.2','PSISF30.2','PSISF31.2','PSISF32.2','PSISF33.2','PSISF34.2',
               'PSISF1.3','PSISF2.3','PSISF3.3','PSISF4.3','PSISF5.3','PSISF6.3','PSISF7.3',
               'PSISF8.3','PSISF9.3','PSISF10.3','PSISF11.3','PSISF12.3','PSISF13.3','PSISF14.3',
               'PSISF15.3','PSISF16.3','PSISF17.3','PSISF18.3','PSISF19.3','PSISF20.3','PSISF21.3',
               'PSISF22.3','PSISF23.3','PSISF24.3','PSISF25.3','PSISF26.3','PSISF27.3','PSISF28.3',
               'PSISF29.3','PSISF30.3','PSISF31.3','PSISF32.3','PSISF33.3','PSISF34.3')

col_names2 <- c('EIPSES1','EIPSES2','EIPSES3','EIPSES4','EIPSES5','EIPSES6','EIPSES7','EIPSES8',
                'EIPSES9','EIPSES10','EIPSES11','EIPSES12','EIPSES13','EIPSES14','EIPSES15','EIPSES16',
                'EIPSES1.2','EIPSES2.2','EIPSES3.2','EIPSES4.2','EIPSES5.2','EIPSES6.2','EIPSES7.2',
                'EIPSES8.2','EIPSES9.2','EIPSES10.2','EIPSES11.2','EIPSES12.2','EIPSES13.2','EIPSES14.2',
                'EIPSES15.2','EIPSES16.2',
                'EIPSES1.3','EIPSES2.3','EIPSES3.3','EIPSES4.3','EIPSES5.3','EIPSES6.3','EIPSES7.3',
                'EIPSES8.3','EIPSES9.3','EIPSES10.3','EIPSES11.3','EIPSES12.3','EIPSES13.3','EIPSES14.3',
                'EIPSES15.3','EIPSES16.3')


amg_clean[col_names] <- Recode(amg_clean[col_names], '1=5; 2=4; 4=2; 5=1')
amg_clean[col_names2] <- Recode(unlist(amg_clean[col_names2]) ,'1=7; 2=6; 3=5; 5=3; 6=2; 7=1')

write.csv(amg_clean, file = "angie_clean.csv", sep = " ", col.names = TRUE)

