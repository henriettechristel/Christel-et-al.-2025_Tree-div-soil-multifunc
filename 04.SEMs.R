rm(list=ls())

library("tidyverse")
library("lavaan")
library("piecewiseSEM")
library("performance")
library("readxl")
library("patchwork")
library("multcompView")
library("lmerTest")

MyDiv_data <- read_xlsx("../../02.data/MyDiv_TreeDi_data.xlsx")

for (i in 1: nrow(MyDiv_data)) {
  if (MyDiv_data$N_content[i] == 0) {
    MyDiv_data$N_content[i] <- 0.0000001
  } 
  if (MyDiv_data$C_content[i] == 0) {
    MyDiv_data$C_content[i] <- 0.0000001
  } 
}

MyDiv <- MyDiv_data %>%
  dplyr::select(h2o_perc, pH, mean_basal_area, cmic, basal, bgluc, xyl, nag, 
                phos, N_content, C_content, WSA_perc) %>%
  dplyr::mutate(CN = (.$C_content/.$N_content)) %>% 
  apply(., 2, scale) %>%
  data.frame() %>% 
  bind_cols(MyDiv_data %>% 
              dplyr::select(block, plot, plot_myc, sp_nr, sampling_date, meanFunction, dist, type)) %>% 
  mutate(block = as.factor(block),
         plot = as.factor(plot),
         type = ifelse(dist == 70, 1, 0)
  )

# create three dataframes for AM, Mix and EM
MyDiv_AM <- MyDiv %>% filter(plot_myc == "AM")
MyDiv_EM <- MyDiv %>% filter(plot_myc == "EM")
MyDiv_MIX <- MyDiv %>% filter(plot_myc == "AE")

# MFI ----

MFI_AM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'meanFunction ~ sp_nr * dist + 
                              pH + C_content + h2o_perc + mean_basal_area +
                              (1|block/plot)', data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr + dist + mean_basal_area +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist + 
                              (1|block/plot)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'mean_basal_area ~ sp_nr +
                              (1|block/plot)', 
                   data = MyDiv_AM)
  )
summary(MFI_AM.piecewiseSEM, standardized = T)
mfi.am.sum <- summary(MFI_AM.piecewiseSEM, standardized = T)

MFI_EM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'meanFunction ~ sp_nr * dist + 
                              pH + C_content + h2o_perc + mean_basal_area +
                              (1|block/plot)', data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr + mean_basal_area + 
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'mean_basal_area ~ sp_nr +
                              (1|block/plot)', 
                   data = MyDiv_EM)
  )

summary(MFI_EM.piecewiseSEM, standardized = T)
mfi.em.sum <- summary(MFI_EM.piecewiseSEM, standardized = T)

ggplot() +
  geom_point(aes(x = sp_nr, y = meanFunction, color = type), data = MyDiv_EM)

MFI_MIX.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'meanFunction ~ sp_nr * dist + mean_basal_area + 
                              pH + C_content + 
                              (1|block/plot)', data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr + mean_basal_area + 
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'mean_basal_area ~ sp_nr +
                              (1|block/plot)', 
                   data = MyDiv_MIX)
  )

summary(MFI_MIX.piecewiseSEM, standardized = T)
mfi.mix.sum <- summary(MFI_MIX.piecewiseSEM, standardized = T)

# CMIC ----

CMIC_AM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'cmic ~ sp_nr * dist + mean_basal_area + 
                              pH + C_content + h2o_perc +
                              (1|block/plot)', data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr + mean_basal_area +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_AM)
  )
summary(CMIC_AM.piecewiseSEM, standardized = T)
cmic.am.sum <- summary(CMIC_AM.piecewiseSEM, standardized = T)

CMIC_EM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'cmic ~ sp_nr * dist + mean_basal_area +
                              pH + C_content + h2o_perc +
                              (1|block/plot)', data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr + 
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_EM)
  )
summary(CMIC_EM.piecewiseSEM, standardized = T)
cmic.em.sum <- summary(CMIC_EM.piecewiseSEM, standardized = T)

CMIC_MIX.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'cmic ~ sp_nr + mean_basal_area +
                              pH + C_content +  
                              (1|block/plot)', data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_MIX)
  )
summary(CMIC_MIX.piecewiseSEM, standardized = T)
cmic.mix.sum <- summary(CMIC_MIX.piecewiseSEM, standardized = T)

# BASAL ----

BASAL_AM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'basal ~ sp_nr + 
                              pH + C_content + h2o_perc +
                              (1|block/plot) + (1|sampling_date)', data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_AM)
  )
summary(BASAL_AM.piecewiseSEM, standardized = T)
BASAL.am.sum <- summary(BASAL_AM.piecewiseSEM, standardized = T)

BASAL_EM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'basal ~ sp_nr * dist + 
                              C_content + 
                              (1|block/plot) + (1|sampling_date)', data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_EM)
  )
summary(BASAL_EM.piecewiseSEM, standardized = T)
BASAL.em.sum <- summary(BASAL_EM.piecewiseSEM, standardized = T)

BASAL_MIX.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'basal ~ sp_nr * dist + 
                              C_content + 
                              (1|block/plot) + (1|sampling_date)', data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_MIX)
  )
summary(BASAL_MIX.piecewiseSEM, standardized = T)
BASAL.mix.sum <- summary(BASAL_MIX.piecewiseSEM, standardized = T)

# BGLUC ----

BGLUC_AM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'bgluc ~ sp_nr * dist + 
                              pH + C_content + h2o_perc +
                              (1|block/plot)', data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_AM)
  )
summary(BGLUC_AM.piecewiseSEM, standardized = T)
BGLUC.am.sum <- summary(BGLUC_AM.piecewiseSEM, standardized = T)

BGLUC_EM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'bgluc ~ sp_nr * dist + 
                              pH + C_content + 
                              (1|block/plot)', data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_EM)
  )
summary(BGLUC_EM.piecewiseSEM, standardized = T)
BGLUC.em.sum <- summary(BGLUC_EM.piecewiseSEM, standardized = T)

BGLUC_MIX.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'bgluc ~ sp_nr + dist + 
                              pH + C_content + 
                              (1|block/plot)', data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_MIX)
  )
summary(BGLUC_MIX.piecewiseSEM, standardized = T)
BGLUC.mix.sum <- summary(BGLUC_MIX.piecewiseSEM, standardized = T)

# XYL ----

XYL_AM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'xyl ~ sp_nr * dist + 
                              pH + C_content + h2o_perc +
                              (1|block/plot)', data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_AM)
  )
summary(XYL_AM.piecewiseSEM, standardized = T)
XYL.am.sum <- summary(XYL_AM.piecewiseSEM, standardized = T)

XYL_EM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'xyl ~ sp_nr * dist + 
                              pH + C_content + 
                              (1|block/plot)', data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_EM)
  )
summary(XYL_EM.piecewiseSEM, standardized = T)
XYL.em.sum <- summary(XYL_EM.piecewiseSEM, standardized = T)

XYL_MIX.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'xyl ~ sp_nr * dist + 
                              pH + C_content + 
                              (1|block/plot)', data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_MIX)
  )
summary(XYL_MIX.piecewiseSEM, standardized = T)
XYL.mix.sum <- summary(XYL_MIX.piecewiseSEM, standardized = T)

# NAG ----

NAG_AM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'nag ~ sp_nr + 
                              pH + C_content + h2o_perc +
                              (1|block/plot)', data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_AM)
  )
summary(NAG_AM.piecewiseSEM, standardized = T)
NAG.am.sum <- summary(NAG_AM.piecewiseSEM, standardized = T)

NAG_EM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'nag ~ sp_nr + dist + 
                              C_content + 
                              (1|block/plot)', data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_EM)
  )
summary(NAG_EM.piecewiseSEM, standardized = T)
NAG.em.sum <- summary(NAG_EM.piecewiseSEM, standardized = T)

NAG_MIX.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'nag ~ sp_nr + 
                              pH + h2o_perc +
                              (1|block/plot)', data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_MIX)
  )
summary(NAG_MIX.piecewiseSEM, standardized = T)
NAG.mix.sum <- summary(NAG_MIX.piecewiseSEM, standardized = T)

# PHOS ----

PHOS_AM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'phos ~ sp_nr + 
                              pH + h2o_perc +
                              (1|block/plot)', data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_AM)
  )
summary(PHOS_AM.piecewiseSEM, standardized = T)
PHOS.am.sum <- summary(PHOS_AM.piecewiseSEM, standardized = T)

PHOS_EM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'phos ~ sp_nr * dist + 
                              pH + C_content + 
                              (1|block/plot)', data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_EM)
  )
summary(PHOS_EM.piecewiseSEM, standardized = T)
PHOS.em.sum <- summary(PHOS_EM.piecewiseSEM, standardized = T)

PHOS_MIX.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'phos ~ sp_nr + dist + 
                              pH + C_content +
                              (1|block/plot)', data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_MIX)
  )
summary(PHOS_MIX.piecewiseSEM, standardized = T)
PHOS.mix.sum <- summary(PHOS_MIX.piecewiseSEM, standardized = T)

# WSA ----

WSA_AM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'WSA_perc ~ sp_nr + 
                              pH + h2o_perc +
                              (1|block/plot)', data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_AM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_AM)
  )
summary(WSA_AM.piecewiseSEM, standardized = T)
WSA.am.sum <- summary(WSA_AM.piecewiseSEM, standardized = T)

WSA_EM.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'WSA_perc ~ sp_nr * dist + 
                              pH + 
                              (1|block/plot)', data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_EM),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_EM)
  )
summary(WSA_EM.piecewiseSEM, standardized = T)
WSA.em.sum <- summary(WSA_EM.piecewiseSEM, standardized = T)

WSA_MIX.piecewiseSEM = 
  psem(
    lmerTest::lmer(formula = 'WSA_perc ~ sp_nr + dist + 
                              pH + C_content + h2o_perc +
                              (1|block/plot)', data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'h2o_perc ~ sp_nr +
                              (1|block/plot) + (1|sampling_date)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'pH ~ dist +
                              (1|block/plot)',
                   data = MyDiv_MIX),
    
    lmerTest::lmer(formula = 'C_content ~ sp_nr + dist +
                              (1|block/plot)',
                   data = MyDiv_MIX)
  )
summary(WSA_MIX.piecewiseSEM, standardized = T)
WSA.mix.sum <- summary(WSA_MIX.piecewiseSEM, standardized = T)