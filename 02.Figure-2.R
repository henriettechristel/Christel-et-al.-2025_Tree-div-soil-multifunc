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
(MFI_myc_div <- MyDiv_data %>% 
   ggplot() +
   geom_point(aes(x = sp_nr, y = meanFunction, col = plot_myc, shape = plot_myc), alpha = 0.4, position = jitter) +
   geom_smooth(aes(x = sp_nr, y = meanFunction, col = plot_myc, fill = plot_myc), 
               method = "lm", alpha = 0.2) + 
   geom_smooth(aes(x = sp_nr, y = meanFunction), method = "lm", colour = "#333333", alpha = 0, linewidth = 0.5) + 
   theme_bw() +
   theme(legend.position = "none", 
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20),
         axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20), 
         axis.text = element_text(size = 13),
         plot.title = element_text(hjust = 0.5, size = 18)) +
   scale_x_log10(breaks = c(1,2,4)) +
   labs(x = "Tree species richness", 
        y = "MFI\n") +
   scale_color_manual(labels = c("AM", "MIX", "EM"),
                      values = c("#93C47D", "#FFB000", "#6D9EEB"),
                      name = "MYC:") +
   scale_fill_manual(labels = c("AM", "MIX", "EM"),
                     values = c("#93C47D", "#FFB000", "#6D9EEB"),
                     name = "MYC:") +
   scale_shape_manual(labels = c("AM", "MIX", "EM"),
                      values = c(16, 15, 17),
                      name = "MYC:") +
   annotate(geom = "text", x = c(1.1, 1.1, 1.25), y = c(0.73, 0.70, 0.67), label = c("TSR: **    ", "MYC: **   ", "TSR x MYC: ns"))
 
)

(cmic_myc_div <- MyDiv_data %>% 
   ggplot() +
   geom_point(aes(x = sp_nr, y = cmic, col = plot_myc, shape = plot_myc), alpha = 0.4, position = jitter) +
   geom_smooth(aes(x = sp_nr, y = cmic, col = plot_myc, fill = plot_myc), 
               method = "lm", alpha = 0.2) +   
   geom_smooth(aes(x = sp_nr, y = cmic), method = "lm", colour = "#333333", alpha = 0, linewidth = 0.5) + 
   theme_bw() +
   theme(legend.position = "none", 
         legend.text = element_text(size = 20),
         legend.title = element_text(size = 20),
         axis.title.x = element_text(size = 20), 
         axis.title.y = element_text(size = 20),  
         axis.text = element_text(size = 13),
         plot.title = element_text(hjust = 0.5, size = 18)) +
   scale_x_log10(breaks = c(1,2,4)) +
   labs(x = "Tree species richness", 
        y = expression(atop(CMIC,"("*µg~Cmic~g^{-1}~dry~soil*")"))) +
   scale_color_manual(labels = c("AM", "MIX", "EM"), 
                      values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                      name = "MYC:") +
   scale_fill_manual(labels = c("AM", "MIX", "EM"), 
                     values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                     name = "MYC:") +
   scale_shape_manual(labels = c("AM", "MIX", "EM"), 
                      values = c(16, 15, 17),
                      name = "MYC:") +
   annotate(geom = "text", x = c(1.1, 1.1, 1.25), y = c(470, 450, 430), label = c("TSR: ***", "MYC: *  ", "TSR x MYC: * "))
)

(basal_myc_div <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = basal, col = plot_myc, shape = plot_myc), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = basal, col = plot_myc, fill = plot_myc), 
                method = "lm", alpha = 0.2) +   
    geom_smooth(aes(x = sp_nr, y = basal), method = "lm", colour = "#333333", alpha = 0, linewidth = 0.5) + 
    theme_bw() +
    theme(legend.position = "none", 
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20),  
          axis.text = element_text(size = 13),       
          plot.title = element_text(hjust = 0.5, size = 18)) +
    scale_x_log10(breaks = c(1,2,4)) +
    labs(x = "Tree species richness", 
         y = expression(atop(BASAL,"("*µl~O[2]~h^{-1}~g^{-1}~dry~soil*")"))) +
    scale_color_manual(labels = c("AM", "MIX", "EM"), 
                       values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                       name = "MYC:") +
    scale_fill_manual(labels = c("AM", "MIX", "EM"), 
                      values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                      name = "MYC:") +
    scale_shape_manual(labels = c("AM", "MIX", "EM"), 
                       values = c(16, 15, 17),
                       name = "MYC:") +
    annotate(geom = "text", x = c(1.1, 1.1, 1.25), y = c(1.6, 1.52, 1.44), label = c("TSR: ns   ", "MYC: ns  ", "TSR x MYC: ns"))
)

(bgluc_myc_div <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = bgluc, col = plot_myc, shape = plot_myc), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = bgluc, col = plot_myc, fill = plot_myc), 
                method = "lm", alpha = 0.2) +   
    geom_smooth(aes(x = sp_nr, y = bgluc), method = "lm", colour = "#333333", alpha = 0, linewidth = 0.5) + 
    theme_bw() +
    theme(legend.position = "none", 
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20),  
          axis.text = element_text(size = 13),      
          plot.title = element_text(hjust = 0.5, size = 18)) +
    scale_x_log10(breaks = c(1,2,4)) +
    labs(x = "Tree species richness", 
         y = expression(atop(BGLUC,"("*nmol~h^{-1}~g^{-1}~dry~soil*")"))) +
    scale_color_manual(labels = c("AM", "MIX", "EM"), 
                       values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                       name = "MYC:") +
    scale_fill_manual(labels = c("AM", "MIX", "EM"), 
                      values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                      name = "MYC:") +
    scale_shape_manual(labels = c("AM", "MIX", "EM"), 
                       values = c(16, 15, 17),
                       name = "MYC:") +
    annotate(geom = "text", x = c(1.1, 1.1, 1.25), y = c(365, 345, 325), label = c("TSR: ns   ", "MYC: **   ", "TSR x MYC: ns"))
)

(xyl_myc_div <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = xyl, col = plot_myc, shape = plot_myc), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = xyl, col = plot_myc, fill = plot_myc), 
                method = "lm", alpha = 0.2) +   
    geom_smooth(aes(x = sp_nr, y = xyl), method = "lm", colour = "#333333", alpha = 0, linewidth = 0.5) + 
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
    scale_color_manual(labels = c("AM", "MIX", "EM"), 
                       values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                       name = "MYC:") +
    scale_fill_manual(labels = c("AM", "MIX", "EM"), 
                      values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                      name = "MYC:") +
    scale_shape_manual(labels = c("AM", "MIX", "EM"), 
                       values = c(16, 15, 17),
                       name = "MYC:") +
    annotate(geom = "text", x = c(1.1, 1.1, 1.25), y = c(37, 34.5, 32), label = c("TSR: ns   ", "MYC: **   ", "TSR x MYC: ns"))
)


(nag_myc_div <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = nag, col = plot_myc, shape = plot_myc), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = nag, col = plot_myc, fill = plot_myc), 
                method = "lm", alpha = 0.2) +   
    geom_smooth(aes(x = sp_nr, y = nag), method = "lm", colour = "#333333", alpha = 0, linewidth = 0.5) + 
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
    scale_color_manual(labels = c("AM", "MIX", "EM"), 
                       values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                       name = "MYC:") +
    scale_fill_manual(labels = c("AM", "MIX", "EM"), 
                      values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                      name = "MYC:") +
    scale_shape_manual(labels = c("AM", "MIX", "EM"), 
                       values = c(16, 15, 17),
                       name = "MYC:") +
    annotate(geom = "text", x = c(1.1, 1.1, 1.25), y = c(165, 155, 145), label = c("TSR: *     ", "MYC: ***  ", "TSR x MYC: ns"))
)

(phos_myc_div <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = phos, col = plot_myc, shape = plot_myc), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = phos, col = plot_myc, fill = plot_myc), 
                method = "lm", alpha = 0.2) +   
    geom_smooth(aes(x = sp_nr, y = phos), method = "lm", colour = "#333333", alpha = 0, linewidth = 0.5) + 
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
    scale_color_manual(labels = c("AM", "MIX", "EM"), 
                       values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                       name = "MYC:") +
    scale_fill_manual(labels = c("AM", "MIX", "EM"), 
                      values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                      name = "MYC:") +
    scale_shape_manual(labels = c("AM", "MIX", "EM"), 
                       values = c(16, 15, 17),
                       name = "MYC:") +
    annotate(geom = "text", x = c(1.1, 1.1, 1.25), y = c(150, 142, 134), label = c("TSR: **    ", "MYC: ns  ", "TSR x MYC: ns"))
)

(WSA_myc_div <- MyDiv_data %>% 
    ggplot() +
    geom_point(aes(x = sp_nr, y = WSA_perc, col = plot_myc, shape = plot_myc), alpha = 0.4, position = jitter) +
    geom_smooth(aes(x = sp_nr, y = WSA_perc, col = plot_myc, fill = plot_myc), 
                method = "lm", alpha = 0.2) +   
    geom_smooth(aes(x = sp_nr, y = WSA_perc), method = "lm", colour = "#333333", alpha = 0, linewidth = 0.5) + 
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
    scale_color_manual(labels = c("AM", "MIX", "EM"), 
                       values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                       name = "MYC:") +
    scale_fill_manual(labels = c("AM", "MIX", "EM"), 
                      values = c("#93C47D", "#FFB000", "#6D9EEB"), 
                      name = "MYC:") +
    scale_shape_manual(labels = c("AM", "MIX", "EM"), 
                       values = c(16, 15, 17),
                       name = "MYC:") +
    annotate(geom = "text", x = c(1.1, 1.1, 1.25), y = c(95, 92, 89), label = c("TSR: ns   ", "MYC: **   ", "TSR x MYC: ns"))
)

(fig.easy <- MFI_myc_div + WSA_myc_div +
    cmic_myc_div + basal_myc_div +
    bgluc_myc_div + xyl_myc_div + 
    nag_myc_div + phos_myc_div +  
    plot_layout(ncol = 2, nrow = 4) +
    plot_annotation(tag_levels = "a") +
    plot_layout(guides = "collect", axis_titles = "collect_x") &
    theme(legend.position='bottom'))
