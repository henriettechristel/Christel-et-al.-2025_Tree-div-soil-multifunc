rm(list=ls())

library("tidyverse")
library("tidyr")
library("dplyr")
library("readxl")
library("reshape2")
library("lme4")
library("lmerTest")
library("multifunc")
library("emmeans")

#read in MyDiv data
MyDiv <- read_xlsx("../../02.data/MyDiv_TreeDi_data.xlsx")

# calculate multifunctionality ----
# averaging approach Byrnes et al. 2014

#define variables for MFI
allVars<-qw(bgluc, xyl, nag,  phos, basal, cmic)

varIdx<-which(names(MyDiv) %in% allVars)

vars<-whichVars(MyDiv, allVars) 

#bind new functions with averaged multifunctional index
MyDiv_average <-cbind(MyDiv, getStdAndMeanFunctions(MyDiv, vars, standardizeZScore))

#check plot
ggplot(aes(x = sp_nr, y = meanFunction), data = MyDiv_average) + 
  geom_point(size = 3) +
  theme_bw(base_size = 15) +
  stat_smooth(method ="lm") + 
  xlab("\n Species Richness") +
  ylab("Average Value of Standardized Functions\n")

#plot nicely
MFI_div_type <- ggplot(data = MyDiv_average, aes(x = sp_nr, y = meanFunction)) +
  geom_smooth(aes(x = sp_nr, y = meanFunction, col = type, fill = type), method = "lm", alpha = 0.1) +
  theme_bw() +
  theme(plot.background = element_rect(fill = "gray93", colour = "gray93")) +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  scale_x_log10(breaks = c(1,2,4)) +
  labs(x = "Tree species richness", y = "Ecosystem multifunctionality index ", color = "Spatial scale", fill = "Spatial scale") +
  scale_color_manual(labels = c("Interaction zone", "Target tree zone"), values = c("#298c8c", "#800074")) +
  scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), values = c("#298c8c", "#800074"))

#We may want to look at all of the functions on a standardized scale,  ####
#and add in the averaged line for comparison as well. We can do this by reshaping the data, and plotting it.

#reshape for plotting everything with ggplot2
MyDivMeanForPlotting <- melt(MyDiv_average[,c(5,7,28:35)], id.vars= c("sp_nr", "type"))
#nice names for plotting
levels(MyDivMeanForPlotting$variable) <- c('bgluc', 'xyl', 'nag', 'phos', 'basal', 'cmic', 'WSA', 'Mean Multifuncion Index')

#plot it
ggplot(aes(x=sp_nr, y=value),data=MyDivMeanForPlotting)+geom_point(size=3)+
  facet_grid(~variable) +
  theme_bw(base_size=15)+
  stat_smooth(method= "lm") + 
  xlab("\nSpecies Richness") +
  ylab("Standardized Value of Function\n")

#Last, let's test the statistical fit of the effect of species richness on the averaged multifunctionality index.

#statistical fit
aveFit<-lm(meanFunction ~ sp_nr, data=MyDiv_average)
Anova(aveFit)
summary(aveFit)

# models ----

MyDiv_data <- MyDiv_data %>% 
  mutate(scale = case_when(
    type == "Rhizo" ~ "Target tree zone",
    type == "Bulk" ~ "Interaction zone")) %>% 
  mutate(dist = case_when(
    dist == 20 ~ 0, 
    dist == 70 ~ 1)) %>% 
  mutate(plot_myc = case_when(
    plot_myc == "A" ~ "AM", 
    plot_myc == "E" ~ "EM",
    plot_myc == "AE" ~ "AE"
  ))

# CMIC
cmic.mod <- lmer(cmic ~ plot_myc + sp_nr + scale + 
                   plot_myc:sp_nr + 
                   plot_myc:scale + 
                   sp_nr:scale +
                   (1|block/plot) + (1|device), MyDiv_data)
anova(cmic.mod, type = 1)
summary(cmic.mod)
emmeans(cmic.mod, list(pairwise ~ plot_myc), adjust = "tukey")

model_performance(cmic.mod)
check_model(cmic.mod)

# BASAL
basal.mod <- lmer(basal ~ plot_myc + sp_nr + scale + 
                    plot_myc:sp_nr + 
                    plot_myc:scale + 
                    sp_nr:scale +
                    (1|block/plot) + (1|device), MyDiv_data)
anova(basal.mod, type = 1) 

model_performance(basal.mod)
check_model(basal.mod)

# BGLUC
bgluc.mod <- lmer(bgluc ~ plot_myc + sp_nr + scale + 
                    plot_myc:sp_nr + 
                    plot_myc:scale + 
                    sp_nr:scale +
                    (1|block/plot), MyDiv_data)
anova(bgluc.mod, type = 1) #
emmeans(bgluc.mod, list(pairwise ~ plot_myc), adjust = "tukey")

model_performance(bgluc.mod)
check_model(bgluc.mod)

# XYL
xyl.mod <- lmer(xyl ~ plot_myc + sp_nr + scale + 
                  plot_myc:sp_nr + 
                  plot_myc:scale + 
                  sp_nr:scale +
                  (1|block/plot), MyDiv_data)
anova(xyl.mod, type = 1)
emmeans(xyl.mod, list(pairwise ~ plot_myc), adjust = "tukey")

model_performance(xyl.mod)
check_model(xyl.mod)

# NAG
nag.mod <- lmer(nag ~ plot_myc + sp_nr + scale + 
                  plot_myc:sp_nr + 
                  plot_myc:scale + 
                  sp_nr:scale +
                  (1|block/plot), MyDiv_data)
anova(nag.mod, type = 1)
emmeans(nag.mod, list(pairwise ~ plot_myc), adjust = "tukey")

model_performance(nag.mod)
check_model(nag.mod)

# PHOS
phos.mod <- lmer(phos ~ plot_myc + sp_nr + scale + 
                   plot_myc:sp_nr + 
                   plot_myc:scale + 
                   sp_nr:scale +
                   (1|block/plot), MyDiv_data)
anova(phos.mod, type = 1)
summary(phos.mod)#

model_performance(phos.mod)
check_model(phos.mod)

# WSA
wsa.mod <- lmer(WSA_perc ~ plot_myc + sp_nr + scale + 
                  plot_myc:sp_nr + 
                  plot_myc:scale + 
                  sp_nr:scale +
                  (1|block/plot), MyDiv_data)
anova(wsa.mod, type = 1) #
emmeans(wsa.mod, list(pairwise ~ plot_myc), adjust = "tukey")

model_performance(wsa.mod)
check_model(wsa.mod)

# MFI
mfi.mod <- lmer(meanFunction ~ plot_myc + sp_nr + scale + 
                   plot_myc:sp_nr + 
                   plot_myc:scale + 
                   sp_nr:scale +
                   (1|block/plot), MyDiv_data)
anova(mfi.mod, type = 1) #
emmeans(mfi.mod, list(pairwise ~ plot_myc), adjust = "tukey")


model_performance(mfi.mod)
check_model(mfi.mod)