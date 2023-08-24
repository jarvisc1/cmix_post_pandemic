#an_fig1_freq


## Packages
library(data.table)
library(ggplot2)
library(patchwork)
library(socialmixr)


# Load data ---------------------------------------------------------------
cnts <- qs::qread('data/wrapup_contacts.qs')

cnts <- cnts[survey_round == 1000]
country_levs <- c("all", "uk", "be", "nl", "ch")
country_labs <- c("All", "UK", "BE", "NL", "CH")
cnts[, country := factor(country, levels = country_levs, labels = country_labs)]


names(cnts)

table(cnts$cnt_frequency)
table(cnts$cnt_type)
table(cnts$cnt_main_type)
table(cnts$cnt_total_time)
table(cnts$cnt_phys)
table(cnts$cnt_prec_mask)


cnts[, cnt_phys := factor(cnt_phys, levels = c(1,0), labels = c("Yes", "No"))]
cnts[, cnt_prec_2m_plus := factor(cnt_prec_2m_plus, levels = c(1,0), labels = c("Yes", "No"))]
cnts[, cnt_prec_mask := factor(cnt_prec_mask, levels = c(1,0), labels = c("Yes", "No"))]
cnts[, cnt_outside := factor(cnt_outside, levels = c(1,0), labels = c("Yes", "No"))]

# Load Polymod data -------------------------------------------------------

data("polymod")

parts_poly = data.table(polymod$participants)
conts_poly = data.table(polymod$contacts)

## Add in country
conts_poly <- merge(conts_poly, parts_poly[,.(part_id,country)], by = "part_id")

conts_poly <- conts_poly[country %in% c("United Kingdom", "Belgium", "Netherlands")]

## Change names
conts_poly[country == "United Kingdom", country := "UK"]
conts_poly[country == "Belgium", country := "BE"]
conts_poly[country == "Netherlands", country := "NL"]

## Convert to Yes and No
conts_poly[, cnt_phys := factor(phys_contact, levels = c(1,2), labels = c("Yes", "No"))]

## Adapt frequency
conts_poly[, cnt_frequency := factor(frequency_multi, levels = 1:5,
                                     labels = c("1-2 days", "2-3 weeks", "1 month", "occasional", "never met"))]

conts_poly[, cnt_total_time := factor(duration_multi, levels = 1:5, labels = 
                                        c("<5m", "5m-14m", "15m-59m", "60m-4h", "4h+"))]

pmod_phy_freq <- conts_poly[!is.na(cnt_frequency), .(.N, perc = sum(cnt_phys == "No", na.rm = T)/sum(cnt_phys %in% c("Yes", "No"),  na.rm = T)), 
                            by = .(cnt_frequency, country)]

# Frequency ----------------------------------------------------------------
p_freq_phys <- ggplot(cnts[!is.na(cnt_frequency)]) +
  geom_bar(aes(x = cnt_frequency, fill = cnt_phys), position = "fill") +
  geom_point(data = pmod_phy_freq, aes(y = perc, x = cnt_frequency,
                                       pch = "POLYMOD", col = "POLYMOD"),
             size = 3) +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "grey"), name = "") +
  scale_shape_manual(values = c("POLYMOD" =  18), name = "") +
  scale_color_manual(values = c("POLYMOD" =  "black"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "A: Physical contact?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_freq_prec <- ggplot(cnts[!is.na(cnt_frequency) & !is.na(cnt_prec_2m_plus)]) +
  geom_bar(aes(x = cnt_frequency, fill = cnt_prec_2m_plus), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "B: 2 meter distance?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        axis.text.y = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_freq_prec_mask <- ggplot(cnts[!is.na(cnt_frequency)]) +
  geom_bar(aes(x = cnt_frequency, fill = cnt_prec_mask), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "C: Wore a mask?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_freq_prec_outside <- ggplot(cnts[!is.na(cnt_frequency) & !is.na(cnt_outside)]) +
  geom_bar(aes(x = cnt_frequency, fill = cnt_outside), position = "fill") +
  geom_hline(yintercept = seq(0,1, 0.1), col = "white") +
  scale_fill_manual(values = c("darkred", "grey"), name = "") +
  scale_y_continuous(expand = expansion(0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "%", subtitle = "D: Met outside?") +
  facet_wrap(.~country, ncol = 1) + coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        strip.text.x = element_text(angle = 0, hjust = 0))

p_freq <- (p_freq_phys | p_freq_prec | p_freq_prec_mask | p_freq_prec_outside) + 
  plot_layout(
    widths = c(1.01,1,1,1,1),
    guides = "collect")  & theme(legend.position = "bottom")


ggsave(filename = "Outputs/fig1_bar_freq.png", plot = p_freq, height = 10, width = 12) 

