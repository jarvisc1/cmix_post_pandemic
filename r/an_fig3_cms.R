## Contact matrices


library(data.table)
library(ggplot2)
library(viridis)
library(cowplot)
library(patchwork)


# Source user written scripts ---------------------------------------------

## Load contact matrices
cms_uk <- qs::qread("outputs/cm_data/uk_cm.qs")
cms_be <- qs::qread("outputs/cm_data/be_cm.qs")
cms_nl <- qs::qread("outputs/cm_data/nl_cm.qs")
cms_ch <- qs::qread("outputs/cm_data/ch_cm.qs")

## Load eigs
eigs_uk <- unlist(qs::qread(file = "outputs/cm_data/uk_eigs.qs"))
eigs_ld1_uk <- unlist(qs::qread(file = "outputs/cm_data/uk_ld1_eigs.qs"))
eigs_be <- unlist(qs::qread(file = "outputs/cm_data/be_eigs.qs"))
eigs_nl <- unlist(qs::qread(file = "outputs/cm_data/nl_eigs.qs"))
eigs_ch <- unlist(qs::qread(file = "outputs/cm_data/ch_eigs.qs"))

## Load pmod eigs
pmod_eigs_uk <- unlist(qs::qread(file = "outputs/cm_data/pmod_uk_eigs.qs"))
pmod_eigs_be <- unlist(qs::qread(file = "outputs/cm_data/pmod_be_eigs.qs"))
pmod_eigs_nl <- unlist(qs::qread(file = "outputs/cm_data/pmod_nl_eigs.qs"))
pmod_eigs_ch <- unlist(qs::qread(file = "outputs/cm_data/pmod_ch_eigs.qs"))


rel_eigs_uk <-     eigs_uk/    pmod_eigs_uk
rel_eigs_ld1_uk <- eigs_ld1_uk/pmod_eigs_uk
rel_eigs_be <-     eigs_be/    pmod_eigs_be
rel_eigs_nl <-     eigs_nl/    pmod_eigs_nl
rel_eigs_ch <-     eigs_ch/    pmod_eigs_ch

dt_eigs <- data.table(country = rep(c("UK", "BE", "NL", "CH", "UK"), each = 1000), 
                      period = c(rep("Final CoMix round", 4000), rep("1st Lockdown", 1000)),
           ratio = c(rel_eigs_uk, rel_eigs_be, rel_eigs_nl, rel_eigs_ch, rel_eigs_ld1_uk))

dt_error <- dt_eigs[, .(med = median(ratio), l2.5 = quantile(ratio, 0.025), l97.5 = quantile(ratio, 0.975)),
        by = .(country, period)]


be_lockdown <- data.table(country = "BE", period = "1st Lockdown", med = 0.186, `l2.5` = 0.171, `l97.5` = 0.203)

dt_error <- rbind(dt_error, be_lockdown)

dt_error[, country := factor(country, levels = c("CH", "NL", "BE", "UK"))]

# -------------------------------------------------------------------------


cm_plot <- function(cms_, title_ = "All"){
  my_breaks <- c(0,0.05, 0.1, 0.20, 0.5, 1.00, 2,4,6)
  my_breaks_lab <- c("0","0.05", "0.10", "0.20", "0.5", "1.00", "2.00","4.00", "6.00")
  tick_labels = c("0-4", "5-11", "12-17", "18-29","30-39",
                  "40-49", "50-59", "60-69", "70+")
  ggplot(cms_[[1]], aes(age_group, age_group_cont, fill= means)) + 
    geom_tile() + 
    scale_fill_viridis(discrete=FALSE, name='Mean \nContacts', trans = "log",
                       breaks = my_breaks, labels = my_breaks_lab, limits = c(0.02,6))+ 
    ggtitle(title_) +
    geom_text(aes(label = round(means,1)),
              colour = ifelse(cms_[[1]]$means >3.5, "black", "white")
                  ) +
    ylab('Contact age group') +
    xlab('Participant age group') +
    scale_x_discrete(labels = tick_labels)+
    scale_y_discrete(labels = tick_labels)+
    theme(axis.line=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(), 
          axis.text.x =element_text(angle = 90),
          axis.ticks.x=element_blank(), 
          axis.ticks.y=element_blank(),
          legend.key.size = unit(2, 'cm'),
          legend.key.width = unit(0.5, 'cm'))
    
}


## This works for the moment.
cms_plots <- (cm_plot(cms_uk, title_ = "UK") +
cm_plot(cms_be, title_ = "BE") )/
  (
cm_plot(cms_nl, title_ = "NL") +
cm_plot(cms_ch, title_ = "CH")) +
  patchwork::plot_layout(guides = "collect")


##
ggplot(dt_eigs) +
  geom_boxplot(aes(x = country, y = ratio))


error_plot <- ggplot(dt_error) +
  geom_vline(xintercept = c(0, 0.25, 0.5, 0.75, 1), col = "grey") +
  geom_point(aes(y = country, x = med, col = period)) +
  geom_linerange(aes(y = country, xmin = l2.5, xmax = l97.5, col = period)) +
  scale_x_continuous(limits =  c(0,1), expand = c(0,0.01)) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = expression(Relative~change~"in"~R[0]), y = "Country", title = "B") +
  theme(axis.line = element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(), 
        axis.text.x =element_text(angle = 0),
        axis.ticks.x=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = c(0.12, 0.1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.width = unit(0.2, 'cm'))
  
multi_plot <- (cms_plots) / error_plot + plot_layout(heights = c(4,1))

layout <- "
AABB
AABB
AABB
CCDD
CCDD
CCDD
EEEE
"


multi_plot <-  (cm_plot(cms_uk, title_ = "UK") + cm_plot(cms_be, title_ = "BE") +
  cm_plot(cms_nl, title_ = "NL") + cm_plot(cms_ch, title_ = "CH") +
  patchwork::plot_layout(guides = "collect")) +
  (error_plot + plot_layout(guides = "keep")) +
  plot_layout(design = layout)



ggsave("outputs/fig3_cmatrices.png",plot = cms_plots, height = 15, width = 10)
ggsave("outputs/fig3_cmatrices_err.png",plot = multi_plot, height = 12, width = 10)

tab4_R <- flextable::flextable(dt_error)

flextable::save_as_docx(tab4_R, path = "outputs/tab_rel_r.docx")
