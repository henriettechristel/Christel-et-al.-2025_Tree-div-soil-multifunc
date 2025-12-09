rm(list=ls())

library("tidyverse")
library("tidyr")
library("dplyr")
library("readxl")
library("ggplot2")
library("ggpubr")
library("patchwork")

# MyDiv_data <- use data from script 01.Mixed-Models.R
jitter <- position_jitter(width = 0.1)

(MFI_div_type <- MyDiv_data %>% 
   ggplot() +
   geom_point(aes(x = sp_nr, y = meanFunction, col = type, shape = type), alpha = 0.4, position = jitter) +
   geom_smooth(aes(x = sp_nr, y = meanFunction, col = type, fill = type), 
               method = "lm", alpha = 0.2) +
   theme_bw() +
   theme(legend.position = "none", 
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20),
         axis.title.x = element_text(size = 20), 
         axis.title.y = element_text(size = 20), 
         axis.text = element_text(size = 13),
         plot.title = element_text(hjust = 0.5, size = 18)) +
   scale_x_log10(breaks = c(1,2,4)) +
   labs(x= "Tree species richness",  
        y = "MFI\n") +
   scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
   scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                     values = c("#298c8c", "#800074"),
                     name = "") +
   scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c(16, 17),
                      name = "") +
   annotate(geom = "text", 
            x = c(1.1, 1.18, 1.3), 
            y = c(0.73, 0.70, 0.67), 
            label = c("TSR: ** ", "ZONE: ns  ", "ZONE x TSR: *"))
 
)

(cmic_div_type <- MyDiv_data %>% 
   ggplot() +
   geom_point(aes(x = sp_nr, y = cmic, col = type, shape = type), alpha = 0.4, position = jitter) +
   geom_smooth(aes(x = sp_nr, y = cmic, col = type, fill = type), 
               method = "lm", alpha = 0.2) +
   theme_bw() +
   scale_x_log10(breaks = c(1,2,4)) +
   theme(legend.position = "none",  
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20),
         axis.title.x = element_text(size = 20), 
         axis.title.y = element_text(size = 20), 
         axis.text = element_text(size = 13),
         plot.title = element_text(hjust = 0.5, size = 18)) +
   labs(x= "Tree species richness",  
        y = expression(atop(CMIC,"("*µg~Cmic~g^{-1}~dry~soil*")"))) +
   scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
   scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                     values = c("#298c8c", "#800074"),
                     name = "") +
   scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c(16, 17),
                      name = "") +
   annotate(geom = "text", x = c(1.1, 1.18, 1.3), y = c(470, 450, 430), 
            label = c("TSR: ***", "ZONE: ns  ", "ZONE x TSR: ns"))
 
)

(basal_div_type <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = basal, col = type, shape = type), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = basal, col = type, fill = type), 
                method = "lm", alpha = 0.2) +
    theme_bw() +
    scale_x_log10(breaks = c(1,2,4)) +
    theme(legend.position = "none",  
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = expression(atop(BASAL,"("*µl~O[2]~h^{-1}~g^{-1}~dry~soil*")"))) +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.3), y = c(1.6, 1.52, 1.44), 
             label = c("TSR: ns  ", "ZONE: ns   ", "ZONE x TSR: ns"))
  
)

(bgluc_div_type <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = bgluc, col = type, shape = type), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = bgluc, col = type, fill = type), 
                method = "lm", alpha = 0.2) +
    theme_bw() +
    scale_x_log10(breaks = c(1,2,4)) +
    theme(legend.position = "none",  
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = expression(atop(BGLUC,"("*nmol~h^{-1}~g^{-1}~dry~soil*")"))) +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.3), y = c(365, 345, 325), 
             label = c("TSR: ns  ", "ZONE: ns   ", "ZONE x TSR: *"))
  
)

(xyl_div_type <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = xyl, col = type, shape = type), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = xyl, col = type, fill = type), 
                method = "lm", alpha = 0.2) +
    theme_bw() +
    scale_x_log10(breaks = c(1,2,4)) +
    theme(legend.position = "none",  
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = expression(atop(XYL,"("*nmol~h^{-1}~g^{-1}~dry~soil*")"))) +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.3), y = c(37, 34.5, 32), 
             label = c("TSR: ns   ", "ZONE: ns   ", "ZONE x TSR: ns"))
  
)

(nag_div_type <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = nag, col = type, shape = type), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = nag, col = type, fill = type), 
                method = "lm", alpha = 0.2) +
    theme_bw() +
    scale_x_log10(breaks = c(1,2,4)) +
    theme(legend.position = "none",  
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = expression(atop(NAG,"("*nmol~h^{-1}~g^{-1}~dry~soil*")"))) +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.3), y = c(165, 155, 145), 
             label = c("TSR: *  ", "ZONE: ns   ", "ZONE x TSR: ns"))
  
)

(phos_div_type <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = phos, col = type, shape = type), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = phos, col = type, fill = type), 
                method = "lm", alpha = 0.2) +
    theme_bw() +
    scale_x_log10(breaks = c(1,2,4)) +
    theme(legend.position = "none",  
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = expression(atop(PHOS,"("*nmol~h^{-1}~g^{-1}~dry~soil*")"))) +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.3), y = c(150, 142, 134), 
             label = c("TSR: **  ", "ZONE: ns   ", "ZONE x TSR: ns"))
  
)

(WSA_div_type <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = WSA_perc, col = type, shape = type), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = WSA_perc, col = type, fill = type), 
                method = "lm", alpha = 0.2) +
    theme_bw() +
    scale_x_log10(breaks = c(1,2,4)) +
    theme(legend.position = "none",  
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = "WSA\n(%)") +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.3), y = c(95, 92, 89), 
             label = c("TSR: ns   ", "ZONE: ns   ", "ZONE x TSR: ns"))
)

fig.easy <- MFI_div_type + WSA_div_type + 
  cmic_div_type + basal_div_type +
  bgluc_div_type + xyl_div_type + 
  nag_div_type + phos_div_type + 
  plot_layout(ncol = 2, nrow = 4) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect", axis_titles = "collect_x") &
  theme(legend.position='bottom')
