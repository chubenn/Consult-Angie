library(ggplot2)
library(ez)

angie_gathered = angie_thesis %>%
  gather(dv_mpca,dv_mpca_2,dv_mpca_3,key = "dv_mpca_long", value = "mpca_score") %>%
  gather(dv_mpcag,dv_mpcag_2,dv_mpcag_3, key = "dv_mpcag_long", value = "mpcag_score") %>%
  gather(dv_eipses,dv_eipses_2,dv_eipses_3, key = "dv_eipses_long", value = "eipses_score") %>%
  gather(dv_psisf,dv_psisf_2,dv_psisf_3, key = "dv_psisf_long", value = "psisf_score") %>%
  gather(dv_airs,dv_airs_2,dv_airs_3, key = "dv_airs_long", value = "airs_score")

options(contrasts=c("contr.helmert",  "contr.poly"))  

#MPCA
ezANOVA(data = angie_gathered, dv = .(mpca_score), wid= .(id), within = .(dv_mpca_long), 
        between =.(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))
#MPCAG
ezANOVA(data = angie_gathered, dv = .(mpcag_score), wid= .(id), within = .(dv_mpcag_long), 
        between = .(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))

#EIPSES
ezANOVA(data = angie_gathered, dv = .(eipses_score), wid= .(id), within = .(dv_eipses_long), 
        between = .(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))

eipses_graph <- ggplot(angie_gathered, aes(x = dv_eipses_long, y = eipses_score, group=iv_groups, color=iv_groups))
eipses_graph + stat_summary(fun.y = mean, geom = "line") 



#PSISF
ezANOVA(data = angie_gathered, dv = .(psisf_score), wid= .(id), within = .(dv_psisf_long), 
        between = .(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))
#AIRS
ezANOVA(data = angie_gathered, dv = .(airs_score), wid= .(id), within= .(dv_airs_long), 
        between = .(iv_groups), type = 3, detailed = TRUE, observed = .(iv_groups))

airs_graph <- ggplot(angie_gathered, aes(x = dv_airs_long,y = airs_score, group = iv_groups, color = iv_groups))
airs_graph + stat_summary(fun.y = mean, geom = "line") 
