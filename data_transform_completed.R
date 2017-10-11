library(DescTools)
# Create data set with needed DVs and IVs
angie_mutated <- angie_complete %>%
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
         ##FIX IV_SBC        
         iv_sbcgroup =  ifelse(group == 1, "sbp",
                        ifelse(group == 2, "sbp",
                        ifelse(group == 3, "no_sbp","post_sbp"))),
         
         iv_teacher  =  (teacher),
         
         iv_id       =  (id),
         
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
         iv_curr_sb  =   ifelse(childrecsb == 1, "yes","no"))

# select variables of interest
angie_thesis <- angie_mutated %>%
  select(dv_mpca, dv_mpcag, dv_eipses, dv_psisf, dv_airs,
         dv_mpca_2, dv_mpcag_2, dv_eipses_2, dv_psisf_2, dv_airs_2,
         dv_mpca_3, dv_mpcag_3, dv_eipses_3, dv_psisf_3, dv_airs_3,
         iv_time, iv_groups, iv_sbcgroup, iv_teacher, iv_language, iv_impserv,
         id, iv_ever_st, iv_ever_bt, iv_ever_ot, iv_ever_pt, iv_ever_sb,
         iv_curr_st, iv_curr_bt, iv_curr_ot, iv_curr_pt, iv_curr_sb)

# check for skewness and kurtosis issues
lapply(angie_thesis[1:15],Skew,method = 2, conf.level =.99)   
lapply(angie_thesis[1:15],Kurt, method = 2, conf.level = .99)

# Transformations
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

transform(angie_thesis$dv_mpcag_3, reflected = FALSE)

angie_thesis$dv_mpcag_3 <- log10(max(angie_thesis$dv_mpcag_3) - angie_thesis$dv_mpcag_3 +1)
angie_thesis$dv_mpcag_2 <- log10(max(angie_thesis$dv_mpcag_2) - angie_thesis$dv_mpcag_2 +1)
angie_thesis$dv_mpcag <- log10(max(angie_thesis$dv_mpcag) - angie_thesis$dv_mpcag +1)