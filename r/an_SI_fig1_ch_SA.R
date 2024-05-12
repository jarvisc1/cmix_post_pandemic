## Contact matrices


library(data.table)
library(ggplot2)
library(viridis)
library(cowplot)
library(patchwork)


# Source user written scripts ---------------------------------------------


## Load eigs
eigs_ch <- unlist(qs::qread(file = "./outputs/cm_data/ch_eigs.qs"))
eigs_ch_prem <- unlist(qs::qread(file = "./outputs/cm_data/ch_prem_eigs.qs"))

## Load pmod eigs
pmod_eigs_ch <- unlist(qs::qread(file = "./outputs/cm_data/pmod_ch_eigs.qs"))
## Load prem eigs
prem_eigs_ch <- unlist(qs::qread(file = "./outputs/cm_data/prem_ch_eig.qs"))

rel_eigs_ch <-     eigs_ch/    pmod_eigs_ch
rel_eigs_ch_prem <-     eigs_ch_prem/    prem_eigs_ch

dt_eigs <- data.table(country = rep(c("CH (pmod)", "CH (prem)"), each = 1000), 
                      period = c(rep("Final CoMix round", 2000)),
                      ratio = c(rel_eigs_ch, rel_eigs_ch_prem))

dt_error <- dt_eigs[, .(med = median(ratio), l2.5 = quantile(ratio, 0.025), l97.5 = quantile(ratio, 0.975)),
                    by = .(country, period)]


dt_error[, country := factor(country, levels = c("CH (pmod)", "CH (prem)"))]


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

ggsave("outputs/fig_SI1_CH_SA.png",plot = error_plot, height = 6, width = 9)