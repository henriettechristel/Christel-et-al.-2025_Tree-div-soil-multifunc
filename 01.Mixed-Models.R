# Cleaning environment
rm(list=ls())

# Packages:
library("tidyverse")
library("readxl")
library("lmerTest")
library("optimx")
library("performance")
library("emmeans")

library("reshape2")
library("car")
library("sjPlot")
library("ggpubr")
library("multcompView")
library("devtools")
library("multifunc")
library("gridExtra")
library("viridis")

library("ggeffects")
library("DHARMa")

# load data ----
MyDiv_data <- read_xlsx("data.xlsx")
biomass_data <- read_xlsx("Tree inventory in MyDiv plots in 2021.xlsx")
neighbourhood <- read_xlsx("MyDiv-neighbours.xlsx")

# handling data ----
MyDiv_data <- MyDiv_data  %>%  filter(!(pooled_number %in% c("174")))

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
    plot_myc == "AE" ~ "AE")) %>% 
  mutate(block = as.factor(block),
         plot = as.factor(plot),
         sampling_date = as.factor(sampling_date), 
         tree_sp = as.factor(tree_sp),
         device = as.factor(device),
         lgsp_nr = log(sp_nr),
         lgcmic = log(cmic),
         lgnag = log(nag), 
         lgphos = log(phos),
         lgcarbon = log(C_content))

neighbourhood$target_ID <- str_c(neighbourhood$plot, "_", neighbourhood$target_tree_ID)
neighbourhood$UID <- str_c(neighbourhood$plot, "_", neighbourhood$target_tree_ID)
neighbours <- neighbourhood %>% 
  dplyr::select(plot, UID, target_ID, pooled_number)

neigh1 <- neighbourhood %>% 
  dplyr::select(plot, neigh1, target_ID, pooled_number)
neigh1$UID <- str_c(neigh1$plot, "_", neigh1$neigh1)
neigh1 <- neigh1 %>% 
  dplyr::select(plot, UID, target_ID, pooled_number)

neigh2 <- neighbourhood %>% 
  dplyr::select(plot, neigh2, target_ID, pooled_number)
neigh2$UID <- str_c(neigh2$plot, "_", neigh2$neigh2)
neigh2 <- neigh2 %>% 
  dplyr::select(plot, UID, target_ID, pooled_number)

neigh3 <- neighbourhood %>% 
  dplyr::select(plot, neigh3, target_ID, pooled_number)
neigh3$UID <- str_c(neigh3$plot, "_", neigh3$neigh3)
neigh3 <- neigh3 %>% 
  dplyr::select(plot, UID, target_ID, pooled_number)

neigh4 <- neighbourhood %>% 
  dplyr::select(plot, neigh4, target_ID, pooled_number)
neigh4$UID <- str_c(neigh4$plot, "_", neigh4$neigh4)
neigh4 <- neigh4 %>% 
  dplyr::select(plot, UID, target_ID, pooled_number)

neigh5 <- neighbourhood %>% 
  dplyr::select(plot, neigh5, target_ID, pooled_number)
neigh5$UID <- str_c(neigh5$plot, "_", neigh5$neigh5)
neigh5 <- neigh5 %>% 
  dplyr::select(plot, UID, target_ID, pooled_number)

neigh6 <- neighbourhood %>% 
  dplyr::select(plot, neigh6, target_ID, pooled_number)
neigh6$UID <- str_c(neigh6$plot, "_", neigh6$neigh6)
neigh6 <- neigh6 %>% 
  dplyr::select(plot, UID, target_ID, pooled_number)

neigh7 <- neighbourhood %>% 
  dplyr::select(plot, neigh7, target_ID, pooled_number)
neigh7$UID <- str_c(neigh7$plot, "_", neigh7$neigh7)
neigh7 <- neigh7 %>% 
  dplyr::select(plot, UID, target_ID, pooled_number)

neigh8 <- neighbourhood %>% 
  dplyr::select(plot, neigh8, target_ID, pooled_number)
neigh8$UID <- str_c(neigh8$plot, "_", neigh8$neigh8)
neigh8 <- neigh8 %>% 
  dplyr::select(plot, UID, target_ID, pooled_number)

neighs <- rbind(neighbours, neigh1, neigh2, neigh3, neigh4, neigh5, neigh6, neigh7, neigh8) %>% 
  mutate(pooled_number = str_pad(pooled_number, width = 3, side = "left", pad = "0"))


#biomass data
biomass_data$dbh <- as.numeric(biomass_data$dbh, na.rm = TRUE)
biomass_data2 <- biomass_data %>% 
  mutate(basal_area = ((dbh^2)*pi)/40000) # divided by 40000 to get m² instead of cm²
biomass_data2$UID <- str_c(biomass_data2$plotID, "_", biomass_data2$position)

tree_biomass <- biomass_data2 %>% 
  right_join(neighs, by = "UID") %>% 
  select(basal_area, UID, target_ID, pooled_number)

biomass <- tree_biomass %>% group_by(pooled_number) %>% 
  summarise(mean_basal_area = sum(basal_area, na.rm =T))

MyDiv_data <- MyDiv_data %>% 
  rename(target_mean_ba = mean_basal_area)

MyDiv <- MyDiv_data %>% 
  left_join(biomass, by = c("pooled_number" = "pooled_number"))

# first normalise functions
normalized_MyDiv <- function_normalization(
  MyDiv,
  fun_cols = c(11:14, 16, 17, 23),
  negative = NULL,
  by_group = NULL 
)

# now calculate MF with q = 0, 1, 2

MF_MyDiv <- MF1_single(func_data = normalized_MyDiv[,c(11:14, 16, 17, 23)], 
                       weight = 1, 
                       species_data = NULL)


#Filter MF values to only keep the correlated case (take into account that functions can be correlated)
#Then reorganise data to merge with original dataset

MF_data <- MF_MyDiv |> 
  filter(Type == "corr_corrected") |> 
  pivot_wider(names_from = Order.q,
              values_from = qMF) |> 
  rename(MF0=`q = 0`,
         MF1=`q = 1`,
         MF2=`q = 2`)

#Merge MF values with original data

MyDiv_data <- cbind(MyDiv, MF_data)


# models ----
# multifunctionality
mean.mod <- lmer(meanFunction ~ plot_myc + lgsp_nr + scale + 
                   plot_myc:lgsp_nr + 
                   plot_myc:scale + 
                   lgsp_nr:scale +
                   (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(mean.mod, type = 1)
plot(simulateResiduals(fittedModel = mean.mod))


anova(mean.mod, type = 3)
emmeans(mean.mod, list(pairwise ~ plot_myc), adjust = "tukey")

ggplot(MyDiv_data) +
  geom_boxplot(aes(x = scale, y = meanFunction))


model_performance(mean.mod)
check_model(mean.mod)


# microbial biomass
lgcmic.mod <- lmer(lgcmic ~ plot_myc + lgsp_nr + scale + 
                     plot_myc:lgsp_nr + 
                     plot_myc:scale + 
                     lgsp_nr:scale +
                     (1|block/plot) + (1|tree_sp) + (1|sampling_date), MyDiv_data)

anova(lgcmic.mod, type = 1)

plot(simulateResiduals(fittedModel = lgcmic.mod))


ggplot(MyDiv_data) +
  geom_smooth(aes(x = sp_nr, y = cmic, col = plot_myc), method = "lm")

summary(cmic.mod)
emmeans(lgcmic.mod, list(pairwise ~ plot_myc), adjust = "tukey")

model_performance(cmic.mod)
check_model(cmic.mod)

# microbial respiration
basal.mod <- lmer(basal ~ plot_myc + lgsp_nr + scale + 
                    plot_myc:lgsp_nr + 
                    plot_myc:scale + 
                    lgsp_nr:scale +
                    (1|block/plot) + (1|tree_sp) + (1|sampling_date), MyDiv_data)
anova(basal.mod, type = 1) 
plot(simulateResiduals(fittedModel = basal.mod))


model_performance(basal.mod)
check_model(basal.mod)

# enzyme bgluc
bgluc.mod <- lmer(bgluc ~ plot_myc + lgsp_nr + scale + 
                    plot_myc:lgsp_nr + 
                    plot_myc:scale + 
                    lgsp_nr:scale +
                    (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(bgluc.mod, type = 1)
plot(simulateResiduals(fittedModel = bgluc.mod))


emmeans(bgluc.mod, list(pairwise ~ plot_myc), adjust = "tukey")


model_performance(bgluc.mod)
check_model(bgluc.mod)

# enzyme xyl 
xyl.mod <- lmer(xyl ~ plot_myc + lgsp_nr + scale + 
                  plot_myc:lgsp_nr + 
                  plot_myc:scale + 
                  lgsp_nr:scale +
                  (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(xyl.mod, type = 1)
plot(simulateResiduals(fittedModel = xyl.mod))


emmeans(xyl.mod, list(pairwise ~ plot_myc), adjust = "tukey")

model_performance(xyl.mod)
check_model(xyl.mod)

# enzyme nag 
lgnag.mod <- lmer(lgnag ~ plot_myc + lgsp_nr + scale + 
                    plot_myc:lgsp_nr + 
                    plot_myc:scale + 
                    lgsp_nr:scale +
                    (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(lgnag.mod, type = 1)
plot(simulateResiduals(fittedModel = lgnag.mod))

emmeans(nag.mod, list(pairwise ~ plot_myc), adjust = "tukey")

model_performance(nag.mod)
check_model(nag.mod)


# enzyme phos 
lgphos.mod <- lmer(lgphos ~ plot_myc + lgsp_nr + scale + 
                     plot_myc:lgsp_nr + 
                     plot_myc:scale + 
                     lgsp_nr:scale +
                     (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(lgphos.mod, type = 1)
plot(simulateResiduals(fittedModel = lgphos.mod))

ggplot(MyDiv_data) +
  geom_smooth(aes(x = sp_nr, y = lgphos, col = scale), method = "lm")

summary(lgphos.mod)#

model_performance(phos.mod)
check_model(phos.mod)

# water-stable aggregates 
wsa.mod <- lmer(WSA_perc ~ plot_myc + lgsp_nr + scale + 
                  plot_myc:lgsp_nr + 
                  plot_myc:scale + 
                  lgsp_nr:scale +
                  (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(wsa.mod, type = 1)
summary(wsa.mod)
plot(simulateResiduals(fittedModel = wsa.mod))

ggplot(MyDiv_data) +
  geom_smooth(aes(x = sp_nr, y = WSA_perc), method = "lm")

emmeans(wsa.mod, list(pairwise ~ plot_myc), adjust = "tukey")

model_performance(wsa.mod)
check_model(wsa.mod)



# mean basal area
area.mod <- lmer(mean_basal_area ~ plot_myc + lgsp_nr + scale + 
                   plot_myc:lgsp_nr + 
                   plot_myc:scale + 
                   lgsp_nr:scale +
                   (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(area.mod, type = 1)
plot(simulateResiduals(fittedModel = area.mod))

emmeans(area.mod, list(pairwise ~ plot_myc), adjust = "tukey")

ggplot(MyDiv_data) +
  geom_boxplot(aes(x = plot_myc, y = mean_basal_area))


# soil water content 
h2o.mod <- lmer(h2o_perc ~ plot_myc + lgsp_nr + scale + 
                  plot_myc:lgsp_nr + 
                  plot_myc:scale + 
                  lgsp_nr:scale +
                  (1|block/plot) + (1|sampling_date), MyDiv_data)
summary(h2o.mod)
anova(h2o.mod, type = 1) #
plot(simulateResiduals(fittedModel = h2o.mod))

emmeans(h2o.mod, list(pairwise ~ sp_nr), adjust = "tukey")

model_performance(h2o.mod)
check_model(h2o.mod)


# C
lgcarbon.mod <- lmer(lgcarbon ~ plot_myc + lgsp_nr + scale + 
                       plot_myc:lgsp_nr + 
                       plot_myc:scale + 
                       lgsp_nr:scale +
                       (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(lgcarbon.mod, type = 1) 
plot(simulateResiduals(fittedModel = lgcarbon.mod))

model_performance(carbon.mod)
check_model(carbon.mod)

# N 
nitrogen.mod <- lmer(N_content ~ plot_myc + lgsp_nr + scale + 
                       plot_myc:lgsp_nr + 
                       plot_myc:scale + 
                       lgsp_nr:scale +
                       (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(nitrogen.mod, type = 1) 
plot(simulateResiduals(fittedModel = nitrogen.mod))
summary(nitrogen.mod)
model_performance(nitrogen.mod)
check_model(nitrogen.mod)

# C/N 
MyDiv_data$CN <- MyDiv_data$C_content/MyDiv_data$N_content

cn.mod <- lmer(CN ~ plot_myc + lgsp_nr + scale + 
                 plot_myc:lgsp_nr + 
                 plot_myc:scale + 
                 lgsp_nr:scale +
                 (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(cn.mod, type = 1) #

model_performance(cn.mod)
check_model(cn.mod)

# pH
pH.mod <- lmer(pH ~ plot_myc + lgsp_nr + scale + 
                 plot_myc:lgsp_nr + 
                 plot_myc:scale + 
                 lgsp_nr:scale +
                 (1|block/plot) + (1|tree_sp), MyDiv_data)
anova(pH.mod, type = 1) #
plot(simulateResiduals(fittedModel = pH.mod))

histemmeans(pH.mod, list(pairwise ~ scale), adjust = "tukey")

model_performance(pH.mod)
check_model(pH.mod)
