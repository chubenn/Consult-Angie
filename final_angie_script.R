library(tidyverse)
library(mice)
library(Amelia)
library(foreign)
library(car)
library(haven)
library(janitor)
library(DescTools)
library(ez)
library(ggplot2)
options(scipen=9)
options(contrasts=c("contr.helmert",  "contr.poly")) 

amg_clean <- read_sav("~/GitHub/AngieG-Thesis/AMG working data 8-29-17.sav")
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
#puttings things back together
amg_clean[col_names] <- Recode(amg_clean[col_names], '1=5;2=4;4=2;5=1')
amg_clean[col_names2] <- Recode(unlist(amg_clean[col_names2]) ,'1=7;2=6;3=5;5=3;6=2;7=1')

write.csv(amg_clean, file = "angie_clean.csv", sep = " ", col.names = TRUE)
#this is the end of the basic cleaning and recoding
angie_clean <- read_csv("~/GitHub/AngieG-Thesis/angie_clean.csv")

#impute data
angie_impute <- angie_clean[ ,64:363] %>%
                      mice(m=1, maxit = 5, method = 'fastpmm', seed = 69) %>% complete()

angie_impute1 <- angie_impute %>% 
  mice(m=1, maxit = 1, method = 'fastpmm', seed = 69) %>% complete()

angie_impute2 <- angie_impute1 %>% mice(m=1, maxit = 1, method = 'fastpmm', seed = 69) %>%
  complete()

#preparing files to be bound together
angie_cat <- angie_clean[,1:63]
full_imputed_untransformed <- read_csv("GitHub/AngieG-Thesis/full_imputed untransformed.csv")
final_angie <- cbind(angie_cat,full_imputed_untransformed)
write.csv(final_angie_script, file = "final_angie_dat.csv", sep = " ", col.names = TRUE)

#data is ready to roll
angie_complete <- final_angie_dat %>%
  tbl_df %>%
  janitor::clean_names() %>%
  mutate(dv_mpca     =  (mpca1 + mpca2 + mpca3 + mpca4 + mpca5 + mpca6 + mpca7 + mpca8 + 
                         mpca9 + mpca10 + mpca11 + mpca12 + mpca13 + mpca14 + mpca15 + mpca16 + 
                         mpca17 + mpca18 + mpca19 + mpca20 + mpca21 + mpca22 + mpca23)/23,
         dv_mpcag    =  (mpcag1 + mpcag2 + mpcag3 + mpcag4 + mpcag5 + mpcag6 + mpcag7 + 
                         mpcag8 + mpcag9 + mpcag10 + mpcag11 + mpcag12 + mpcag13 + mpcag14 + mpcag15)/15,
         dv_eipses   =  (eipses1 + eipses2 + eipses3 + eipses4 + eipses5 + eipses6 + eipses7 + eipses8 + 
                         eipses9 + eipses10 + eipses11 + eipses12 + eipses13 + eipses14 + eipses15 + eipses16)/16,
         dv_psisf    =  (psisf1 + psisf2 + psisf3 + psisf4 + psisf5 + psisf6 + psisf7 + psisf8 + psisf9 + 
                         psisf10 + psisf11 + psisf12 + psisf13 + psisf14 + psisf15 + psisf16 + psisf17 +psisf18 + 
                         psisf19 + psisf20 + psisf21 + psisf22 + psisf23 + psisf24 + psisf25 + psisf26 + psisf27 + 
                         psisf28 + psisf29 + psisf30 + psisf31 + psisf32 + psisf33 + psisf34 + psisf35 + psisf36)/36,
         dv_airs     =  (airs1 + airs2 + airs3 + airs4 + airs5 +
                         airs6 + airs7 + airs8 +airs9 + airs10)/10,
         dv_mpca_2   =  (mpca1_2 + mpca2_2 + mpca3_2 + mpca4_2 + mpca5_2 + mpca6_2 + mpca7_2 + mpca8_2 + 
                         mpca9_2 + mpca10_2 + mpca11_2 + mpca12_2 + mpca13_2 + mpca14_2 + mpca15_2 + mpca16_2 + 
                         mpca17_2 + mpca18_2 + mpca19_2 + mpca20_2 + mpca21_2 + mpca22_2 + mpca23_2)/23,
         dv_mpcag_2  =  (mpcag1_2 + mpcag2_2 + mpcag3_2 + mpcag4_2 + mpcag5_2 + mpcag6_2 + mpcag7_2 + 
                         mpcag8_2 + mpcag9_2 + mpcag10_2 + mpcag11_2 + mpcag12_2 + mpcag13_2 + mpcag14_2 + mpcag15_2)/15,
         dv_eipses_2 =  (eipses1_2 + eipses2_2 + eipses3_2 + eipses4_2 + eipses5_2 + eipses6_2 + eipses7_2 + eipses8_2 + 
                         eipses9_2 + eipses10_2 + eipses11_2 + eipses12_2 + eipses13_2 + eipses14_2 + eipses15_2 + eipses16_2)/16,
         dv_psisf_2  =  (psisf1_2 + psisf2_2 + psisf3_2 + psisf4_2 + psisf5_2 + psisf6_2 + psisf7_2 + psisf8_2 + psisf9_2 + 
                         psisf10_2 + psisf11_2 + psisf12_2 + psisf13_2 + psisf14_2 + psisf15_2 + psisf16_2 + psisf17_2 + psisf18_2 + 
                         psisf19_2 + psisf20_2 + psisf21_2 + psisf22_2 + psisf23_2 + psisf24_2 + psisf25_2 + psisf26_2 + psisf27_2 + 
                         psisf28_2 + psisf29_2 + psisf30_2 + psisf31_2 + psisf32_2 + psisf33_2 + psisf34_2 + psisf35_2 + psisf36_2)/36,
         dv_airs_2   =  (airs1_2 + airs2_2 + airs3_2 + airs4_2 + airs5_2 +
                         airs6_2 + airs7_2 + airs8_2 + airs9_2 + airs10_2)/10,
         dv_mpca_3   =  (mpca1_3 + mpca2_3 + mpca3_3 + mpca4_3 + mpca5_3 + mpca6_3 + mpca7_3 + mpca8_3 + 
                         mpca9_3 + mpca10_3 + mpca11_3 + mpca12_3 + mpca13_3 + mpca14_3 + mpca15_3 + mpca16_3 + 
                         mpca17_3 + mpca18_3 + mpca19_3 + mpca20_3 + mpca21_3 + mpca22_3 + mpca23_3)/23,
         dv_mpcag_3  =  (mpcag1_3 + mpcag2_3 + mpcag3_3 + mpcag4_3 + mpcag5_3 + mpcag6_3 + mpcag7_3 + 
                         mpcag8_3 + mpcag9_3 + mpcag10_3 + mpcag11_3 + mpcag12_3 + mpcag13_3 + mpcag14_3 + mpcag15_3)/15,
         dv_eipses_3 =  (eipses1_3 + eipses2_3 + eipses3_3 + eipses4_3 + eipses5_3 + eipses6_3 + eipses7_3 + eipses8_3 + 
                         eipses9_3 + eipses10_3 + eipses11_3 + eipses12_3 + eipses13_3 + eipses14_3 + eipses15_3 + eipses16_3)/16,
         dv_psisf_3  =  (psisf1_3 + psisf2_3 + psisf3_3 + psisf4_3 + psisf5_3 + psisf6_3 + psisf7_3 + psisf8_3 + psisf9_3 + 
                         psisf10_3 + psisf11_3 + psisf12_3 + psisf13_3 + psisf14_3 + psisf15_3 + psisf16_3 + psisf17_3 +psisf18_3 + 
                         psisf19_3 + psisf20_3 + psisf21_3 + psisf22_3 + psisf23_3 + psisf24_3 + psisf25_3 + psisf26_3 + psisf27_3 + 
                         psisf28_3 + psisf29_3 + psisf30_3 + psisf31_3 + psisf32_3 + psisf33_3 + psisf34_3 + psisf35_3 + psisf36_3)/36,
         dv_airs_3   =  (airs1_3 + airs2_3 + airs3_3 + airs4_3 + airs5_3 +
                         airs6_3 + airs7_3 + airs8_3 + airs9_3 + airs9)/9,
         iv_time     =   ifelse(time == 1, "pre",
                         ifelse(time == 2, "post","follow up")),
         iv_groups   =   ifelse(group == 1, "treatment",
                         ifelse(group == 2, "completed",
                         ifelse(group == 3, "treatment as usual","graduate"))),
          iv_sbcgroup =  ifelse(group == 1, "sbp",
                         ifelse(group == 2, "sbp",
                         ifelse(group == 3, "no_sbp","post_sbp"))),
         iv_teacher  =  (teacher),
         iv_id          =  (x),
         iv_language =   ifelse(lang == 1, "english",
                         ifelse(lang == 2, "spanish","other")),
         iv_impserv  =   ifelse(impserv == 1, "no_improvement",
                         ifelse(impserv == 2, "mild_improvement", "substancial_improvement")),
         iv_ever_st  =   ifelse(childservst == 1, "yes","no"),
         iv_ever_bt  =   ifelse(childservbt == 1, "yes","no"),
         iv_ever_ot  =   ifelse(childservot == 1, "yes","no"),
         iv_ever_pt  =   ifelse(childservpt == 1, "yes","no"),
         iv_ever_sb  =   ifelse(childservsb == 1, "yes","no"),
         iv_curr_st  =   ifelse(childrecst == 1, "yes","no"),
         iv_curr_bt  =   ifelse(childrecbt == 1, "yes","no"),
         iv_curr_ot  =   ifelse(childrecot == 1, "yes","no"),
         iv_curr_pt  =   ifelse(childrecpt == 1, "yes","no"),
         iv_curr_sb  =   ifelse(childrecsb == 1, "yes","no"),
         iv_lang.2   =   ifelse(lang == 1, "english","no interpret"))

angie_thesis <- angie_complete %>%
  select(dv_mpca, dv_mpcag, dv_eipses, dv_psisf, dv_airs,
         dv_mpca_2, dv_mpcag_2, dv_eipses_2, dv_psisf_2, dv_airs_2,
         dv_mpca_3, dv_mpcag_3, dv_eipses_3, dv_psisf_3, dv_airs_3,
         iv_time, iv_groups, iv_sbcgroup, iv_teacher, iv_language, iv_impserv,
         id, iv_ever_st, iv_ever_bt, iv_ever_ot, iv_ever_pt, iv_ever_sb,
         iv_curr_st, iv_curr_bt, iv_curr_ot, iv_curr_pt, iv_curr_sb, iv_lang.2)

lapply(angie_thesis[1:15],Skew,method = 2, conf.level =.99)   
lapply(angie_thesis[1:15],Kurt, method = 2, conf.level = .99)

transform <- function (x, reflected = FALSE) {
  
  if (reflected == FALSE) {
    print('SQUAREROOT')
    print(sqrt(x + 1)) # squareroot
    
    print('LOG')
    print(log10(x + 1)) # log
    
    print('INVERSE')
    print(1/(x + 1))
  } else {
    print('REFLECTED SQUAREROOT')
    print(sqrt(max(x) - x + 1))
    
    print('REFLECTED LOG')
    print(log10(max(x) - x + 1))
    
    print('REFLECTED INVERSE')
    print(1/(max(x) - x + 1))
  }
}

transform(angie_thesis$dv_mpcag_2, reflected = FALSE)
transform(angie_thesis$dv_eipses_2, reflected = FALSE)
transform(angie_thesis$dv_mpcag_3, reflected = FALSE)

angie_thesis$dv_mpcag_3 <- 1/(angie_thesis$dv_mpcag_3 +1)
angie_thesis$dv_mpcag_2 <- 1/(angie_thesis$dv_mpcag_2 +1)
angie_thesis$dv_eipses_2   <- 1/(angie_thesis$dv_eipses_2 +1)

##original
angie_gathered = angie_thesis %>%
  gather(dv_mpca,dv_mpca_2,dv_mpca_3,key = "dv_mpca_long", value = "mpca_score") %>%
  gather(dv_mpcag,dv_mpcag_2,dv_mpcag_3, key = "dv_mpcag_long", value = "mpcag_score") %>%
  gather(dv_eipses,dv_eipses_2,dv_eipses_3, key = "dv_eipses_long", value = "eipses_score") %>%
  gather(dv_psisf,dv_psisf_2,dv_psisf_3, key = "dv_psisf_long", value = "psisf_score") %>%
  gather(dv_airs,dv_airs_2,dv_airs_3, key = "dv_airs_long", value = "airs_score")

##is this right?
angie_test5 = angie_thesis %>%
  select(dv_mpca,dv_mpca_2,dv_mpca_3) %>%
  gather(dv_mpca,dv_mpca_2,dv_mpca_3,key = "dv_mpca_long", value = "mpca_score")
angie_test1 = angie_thesis %>%
  select(dv_mpcag,dv_mpcag_2,dv_mpcag_3) %>%
  gather(dv_mpcag,dv_mpcag_2,dv_mpcag_3, key = "dv_mpcag_long", value = "mpcag_score")
angie_test2 = angie_thesis %>%
  select(dv_eipses,dv_eipses_2,dv_eipses_3) %>%
  gather(dv_eipses,dv_eipses_2,dv_eipses_3, key = "dv_eipses_long", value = "eipses_score")
angie_test3 = angie_thesis %>%
  select(dv_psisf,dv_psisf_2,dv_psisf_3) %>%
  gather(dv_psisf,dv_psisf_2,dv_psisf_3, key = "dv_psisf_long", value = "psisf_score")
angie_test4 = angie_thesis %>%
  select(dv_airs,dv_airs_2,dv_airs_3) %>%
  gather(dv_airs,dv_airs_2,dv_airs_3, key = "dv_airs_long", value = "airs_score")

angie_othercat = angie_thesis[,16:32]

angie_cbind = cbind(angie_test1,angie_test2,angie_test3,angie_test4,angie_test5,angie_othercat)

angie_gathered = angie_cbind
 
treatment_only = angie_gathered %>% filter(iv_groups == "treatment" | iv_groups == "treatment as usual")

angie_gathered = treatment_only

#MPCA
ezANOVA(data = angie_gathered, dv =. (mpca_score), wid=. (id), within=.(dv_mpca_long), 
                     between =.(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))
lines <- ggplot(angie_gathered, aes(dv_mpca_long, mpca_score, group=iv_groups, color=iv_groups))
lines +stat_summary(fun.y = mean, geom="line") 


#for df
.836 * 2
.836 * 154


#for means
tapply(angie_gathered$mpca_score, list(angie_gathered$dv_mpca_long,angie_gathered$iv_groups),mean)
tapply(angie_gathered$mpca_score, list(angie_gathered$dv_mpca_long,angie_gathered$iv_groups),sd)

#MPCAG
ezANOVA(data = angie_gathered, dv =. (mpcag_score), wid=. (id), within=.(dv_mpcag_long), 
        between =.(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))
lines <- ggplot(angie_gathered, aes(dv_mpcag_long, mpcag_score, group=iv_groups, color=iv_groups))
lines +stat_summary(fun.y = mean, geom="line") 

#EIPSES
ezANOVA(data = angie_gathered, dv =. (eipses_score), wid=. (id), within=.(dv_eipses_long), 
        between =.(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))
lines <- ggplot(angie_gathered, aes(dv_eipses_long, eipses_score, group=iv_groups, color=iv_groups))
lines +stat_summary(fun.y = mean, geom="line") 

tapply(angie_gathered$eipses_score, list(angie_gathered$dv_eipses_long,angie_gathered$iv_groups),mean)
tapply(angie_gathered$eipses_score, list(angie_gathered$dv_eipses_long,angie_gathered$iv_groups),sd)

#PSISF
ezANOVA(data = angie_gathered, dv =. (psisf_score), wid=. (id), within=.(dv_psisf_long), 
        between =.(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))

lines <- ggplot(angie_gathered, aes(dv_psisf_long, psisf_score, group=iv_groups, color=iv_groups))
lines +stat_summary(fun.y = mean, geom="line") 

tapply(angie_gathered$psisf_score, list(angie_gathered$dv_psisf_long,angie_gathered$iv_groups),mean)
tapply(angie_gathered$psisf_score, list(angie_gathered$dv_psisf_long,angie_gathered$iv_groups),sd)

.84 * 2
.84 * 6
.84 * 154

#AIRS
ezANOVA(data = angie_gathered, dv =. (airs_score), wid=. (id), within=.(dv_airs_long), 
        between =.(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))
lines <- ggplot(angie_gathered, aes(dv_airs_long, airs_score, group=iv_groups, color=iv_groups))
lines +stat_summary(fun.y = mean, geom="line") 


#correlation test
cor.test(angie_gathered$airs_score,angie_gathered$psisf_score)
cor.test(angie_gathered$airs_score,angie_gathered$eipses_score)
cor.test(angie_gathered$airs_score,angie_gathered$mpca_score)


### SBC stuff

sbc_group_mpca = aov(mpca_score ~ iv_sbcgroup, data = angie_gathered)
summary(sbc_group_mpca)
EtaSq(sbc_group_mpca)
tapply(angie_gathered$mpca_score, angie_gathered$iv_sbcgroup,mean)
tapply(angie_gathered$mpca_score, angie_gathered$iv_sbcgroup,sd)
table(angie_gathered$iv_sbcgroup)

sbc_group_eipses = aov(eipses_score ~ iv_sbcgroup, data = angie_gathered)
summary(sbc_group_eipses)
EtaSq(sbc_group_eipses)
tapply(angie_gathered$eipses_score, angie_gathered$iv_sbcgroup,mean)
tapply(angie_gathered$eipses_score, angie_gathered$iv_sbcgroup,sd)
table(angie_gathered$iv_sbcgroup)

sbc_group_psisf = aov(psisf_score ~ iv_sbcgroup, data = angie_gathered)
summary(sbc_group_psisf)
EtaSq(sbc_group_psisf)
tapply(angie_gathered$psisf_score, angie_gathered$iv_sbcgroup,mean)
tapply(angie_gathered$psisf_score, angie_gathered$iv_sbcgroup,sd)
table(angie_gathered$iv_sbcgroup)

#t-tests groups
angie_thesis <- angie_thesis %>%
  mutate(
    dv_mpca_all = (dv_mpca + dv_mpca_2 + dv_mpca_3)/3,
    dv_mpcag_all = (dv_mpcag + dv_mpcag_2 + dv_mpcag_3)/3,
    dv_eipses_all = (dv_eipses + dv_eipses_2 + dv_eipses_3)/3,
    dv_psisf_all = (dv_psisf + dv_psisf_2 + dv_psisf_3)/3,
    dv_airs_all = (dv_airs + dv_airs_2 + dv_airs_3)/3
  )
t.treat_treat_as_use <- angie_thesis %>%
  filter(iv_groups == "treatment" | iv_groups == "treatment as usual")
t.treat_completed <- angie_thesis %>%
  filter(iv_groups == "treatment" | iv_groups == "completed")
t.treat_graduated <- angie_thesis %>%
  filter(iv_groups == "treatment" | iv_groups == "graduate")
t.completed_treat_as_use <- angie_thesis %>%
  filter(iv_groups == "completed" | iv_groups == "treatment as usual")
t.completed_graduated <- angie_thesis %>%
  filter(iv_groups == "completed" | iv_groups == "graduate")
t.graduated_treat_as_use <- angie_thesis %>%
  filter(iv_groups == "graduate" | iv_groups == "treatment as usual")

t.test(dv_mpca_all~iv_groups, data = t.treat_treat_as_use)
t.test(dv_mpcag_all~iv_groups, data = t.treat_treat_as_use)
t.test(dv_eipses_all~iv_groups, data = t.treat_treat_as_use)
t.test(dv_psisf_all~iv_groups, data = t.treat_treat_as_use)
t.test(dv_airs_all~iv_groups, data = t.treat_treat_as_use)

t.test(dv_mpca_all~iv_groups, data = t.treat_completed)
t.test(dv_mpcag~iv_groups, data = t.treat_completed)
t.test(dv_eipses_all~iv_groups, data = t.treat_completed)
t.test(dv_psisf_all~iv_groups, data = t.treat_completed)
t.test(dv_airs_all~iv_groups, data = t.treat_completed)

t.test(dv_mpca_all~iv_groups, data = t.treat_graduated)
t.test(dv_mpcag_all~iv_groups, data = t.treat_graduated)
t.test(dv_eipses_all~iv_groups, data = t.treat_graduated)
t.test(dv_psisf_all~iv_groups, data = t.treat_graduated)
t.test(dv_airs_all~iv_groups, data = t.treat_graduated)

t.test(dv_mpca_all~iv_groups, data = t.completed_treat_as_use)
t.test(dv_mpcag_all~iv_groups, data = t.completed_treat_as_use)
t.test(dv_eipses_all~iv_groups, data = t.completed_treat_as_use)
t.test(dv_psisf_all~iv_groups, data = t.completed_treat_as_use)
t.test(dv_airs_all~iv_groups, data = t.completed_treat_as_use)

t.test(dv_mpca_all~iv_groups, data = t.completed_graduated)
t.test(dv_mpcag_all~iv_groups, data = t.completed_graduated)
t.test(dv_eipses_all~iv_groups, data = t.completed_graduated)
t.test(dv_psisf_all~iv_groups, data = t.completed_graduated)
t.test(dv_airs_all~iv_groups, data = t.completed_graduated)

t.test(dv_mpca_all~iv_groups, data = t.graduated_treat_as_use)
t.test(dv_mpcag_all~iv_groups, data = t.graduated_treat_as_use)
t.test(dv_eipses_all~iv_groups, data = t.graduated_treat_as_use)
t.test(dv_psisf_all~iv_groups, data = t.graduated_treat_as_use)
t.test(dv_airs_all~iv_groups, data = t.graduated_treat_as_use)

####updated
##t.tests for esl/none
t.test(dv_mpca_all~iv_lang.2, data = angie_thesis)
cohensD(dv_mpca_all ~ iv_lang.2,dat = angie_thesis)
tapply(angie_thesis$dv_mpca_all,angie_thesis$iv_lang.2, describe)

t.test(dv_mpcag_all~iv_lang.2, data = angie_thesis)
cohensD(dv_mpcag_all ~ iv_lang.2,dat = angie_thesis)
tapply(angie_thesis$dv_mpcag_all,angie_thesis$iv_lang.2, describe)


t.test(dv_eipses_all~iv_lang.2, data = angie_thesis)
cohensD(dv_eipses_all ~ iv_lang.2,dat = angie_thesis)
tapply(angie_thesis$dv_eipses_all,angie_thesis$iv_lang.2, describe)


t.test(dv_psisf_all~iv_lang.2, data = angie_thesis)
cohensD(dv_psisf_all ~ iv_lang.2,dat = angie_thesis)
tapply(angie_thesis$dv_psisf_all,angie_thesis$iv_lang.2, describe)

t.test(dv_airs_all~iv_lang.2, data = angie_thesis)
cohensD(dv_airs_all ~ iv_lang.2,dat = angie_thesis)
tapply(angie_thesis$dv_airs_all,angie_thesis$iv_lang.2, describe)

#### correlation matrix
cor.matrix <- angie_thesis %>%
  select(dv_mpca_all, dv_mpcag_all, dv_eipses_all,dv_psisf_all,dv_airs_all)
Hmisc::rcorr(as.matrix(cor.matrix))


###whose all missing?
mean(is.na(amg_clean$MPCA1))
sum(is.na(amg_clean$MPCA1))
mean(is.na(amg_clean$`MPCA1#2`))
sum(is.na(amg_clean$`MPCA1#2`))
mean(is.na(amg_clean$`MPCA1#3`))
sum(is.na(amg_clean$`MPCA1#3`))

###descriptive statistics?
describe(amg_clean$AnnInc)
describe(amg_clean$PA)
describe(amg_clean$CA)

describe(angie_thesis$dv_mpca_all)
describe(angie_thesis$dv_psisf_all)
describe(angie_thesis$dv_eipses_all)
describe(angie_thesis$dv_airs_all)
