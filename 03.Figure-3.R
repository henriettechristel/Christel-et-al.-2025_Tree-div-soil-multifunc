rm(list=ls())

library("tidyverse")
library("tidyr")
library("dplyr")
library("readxl")
library("ggplot2")
library("ggpubr")
library("patchwork")

MyDiv_data <- read_xlsx("../../02.data/MyDiv_TreeDi_11.12.2022.xlsx")

jitter <- position_jitter(width = 0.1)

# Figure 3 - scale dependency of tree species richness effects ----

MFI_div_type <- MyDiv_data %>% 
   ggplot() +
   geom_point(aes(x = sp_nr, y = meanFunction, col = type, shape = type), alpha = 0.4, position = jitter) +
   geom_smooth(aes(x = sp_nr, y = meanFunction, col = type, fill = type), 
               method = "lm", alpha = 0.2) +
   theme_bw() +
   theme(legend.position = "none", 
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20),
         axis.title.x = element_blank(), 
         axis.title.y = element_text(size = 20), 
         axis.text = element_text(size = 13),
         plot.title = element_text(hjust = 0.5, size = 18)) +
   scale_x_log10(breaks = c(1,2,4)) +
   labs(x= "Tree species richness",  
        y = "Value", 
        title = "MFI") +
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
            x = c(1.1, 1.18, 1.31), 
            y = c(0.73, 0.70, 0.67), 
            label = c("TSR: ns  ", "ZONE: ns   ", "ZONE x TSR: *"))

cmic_div_type <- MyDiv_data %>% 
   ggplot() +
   geom_point(aes(x = sp_nr, y = cmic, col = type, shape = type), alpha = 0.4, position = jitter) +
   geom_smooth(aes(x = sp_nr, y = cmic, col = type, fill = type), 
               method = "lm", alpha = 0.2) +
   theme_bw() +
   scale_x_log10(breaks = c(1,2,4)) +
   theme(legend.position = "none",  
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20),
         axis.title.x = element_blank(), 
         axis.title.y = element_blank(), 
         axis.text = element_text(size = 13),
         plot.title = element_text(hjust = 0.5, size = 18)) +
   labs(x= "Tree species richness",  
        y = "",
        title = "CMIC") +
   scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
   scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                     values = c("#298c8c", "#800074"),
                     name = "") +
   scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c(16, 17),
                      name = "") +
   annotate(geom = "text", x = c(1.1, 1.18, 1.37), y = c(470, 450, 430), 
            label = c("TSR: ns  ", "ZONE: ns   ", "ZONE x TSR: ns"))

basal_div_type <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = basal, col = type, shape = type), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = basal, col = type, fill = type), 
                method = "lm", alpha = 0.2) +
    theme_bw() +
    scale_x_log10(breaks = c(1,2,4)) +
    theme(legend.position = "none",  
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = "", 
         title = "BASAL") +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.37), y = c(1.6, 1.52, 1.44), label = c("TSR: ns  ", "ZONE: ns   ", "ZONE x TSR: ns"))

bgluc_div_type <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = bgluc, col = type, shape = type), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = bgluc, col = type, fill = type), 
                method = "lm", alpha = 0.2) +
    theme_bw() +
    scale_x_log10(breaks = c(1,2,4)) +
    theme(legend.position = "none",  
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = "", 
         title = "BGLUC") +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.31), y = c(365, 345, 325), label = c("TSR: ns  ", "ZONE: ns   ", "ZONE x TSR: *"))

xyl_div_type <- MyDiv_data %>% 
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
         y = "Value", 
         title = "XYL") +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.37), y = c(37, 34.5, 32), label = c("TSR: ns   ", "ZONE: ns   ", "ZONE x TSR: ns"))

nag_div_type <- MyDiv_data %>% 
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
          axis.title.y = element_blank(), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = "", 
         title = "NAG") +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.37), y = c(165, 155, 145), label = c("TSR: ns  ", "ZONE: ns   ", "ZONE x TSR: ns"))

phos_div_type <- MyDiv_data %>% 
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
          axis.title.y = element_blank(), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = "", 
         title = "PHOS") +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
    annotate(geom = "text", x = c(1.1, 1.18, 1.37), y = c(150, 142, 134), label = c("TSR: ns  ", "ZONE: ns   ", "ZONE x TSR: ns"))

WSA_div_type <- MyDiv_data %>% 
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
          axis.title.y = element_blank(), 
          axis.text = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    labs(x= "Tree species richness",  
         y = " ", 
         title = "WSA") +
    scale_color_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c("#298c8c", "#800074"),
                       name = "") +
    scale_fill_manual(labels = c("Interaction zone", "Target tree zone"), 
                      values = c("#298c8c", "#800074"),
                      name = "") +
    scale_shape_manual(labels = c("Interaction zone", "Target tree zone"), 
                       values = c(16, 17),
                       name = "") +
  annotate(geom = "text", x = c(1.1, 1.18, 1.37), y = c(95, 92, 89), label = c("TSR: ns    ", "SCALE: ns   ", "SCALE x TSR: ns"))


# arrange and save ----

fig.easy <- MFI_div_type + cmic_div_type + basal_div_type +
  bgluc_div_type + xyl_div_type + nag_div_type + phos_div_type + WSA_div_type + 
  plot_layout(ncol = 4, nrow = 2) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect", axis_title = "collect_x") &
  theme(legend.position='bottom')