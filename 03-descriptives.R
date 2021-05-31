
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)
library(ggrepel)

import_plex_sans()
theme_set(theme_ipsum_ps())
extrafont::loadfonts(quiet = TRUE, device = 'pdf')

# 9 colors
blue_colors <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0",
                 "#92C5DE", "#4393C3", "#2166AC", "#192841")

blue_colors2 <- c('#bdd7e7', '#eff3ff', "#08519c", "#3182bd", '#6baed6')


# Load Data ---------------------------------------------------------------

load('data/df_pool.Rda')
load('data/cifpm.Rda')

df_pool <- df_pool %>% 
  filter(year != 2018)


# Summary Statistics ------------------------------------------------------


df_pool1 <- df_pool %>% 
  mutate(rev_total = rev_total/100, rev_own = rev_own/100, rev_iptu = rev_iptu/100, 
         gdp = gdp/100, share = rev_own/rev_total, fpm = fpm/100)

df_pool %>% 
  group_by(th) %>%
  mutate(rev_own = rev_iptu/pop) %>% 
  select(rev_own) %>% 
  summarise_all(list(mean, sd)) %>% 
  mutate_if(is.numeric, round, digits = 0) %>% 
  kbl() %>% 
  kable_styling()


df_pool %>% 
  group_by(th) %>%
  mutate(prop = rev_own/rev_total) %>% 
  select(prop) %>% 
  summarise_all(list(mean, sd)) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  kbl() %>% 
  kable_styling()



# FPM ---------------------------------------------------------------------


tiff("figures/fpm-all-bin.tiff", units = "in", width = 7, height = 5, res = 300)
ggplot(df_pool, 
       aes(x = pop, y = log(fpm), color = factor(cifpm))) +
  stat_summary_bin(fun.y = 'mean', bins = 200, geom = 'point', alpha = 0.8) +
  geom_smooth(method = "loess", se = F, size = 0.8) +
  labs(title = "", x = "Population", y = "Log(FPM)") +
  geom_vline(xintercept = c(10188, 13584, 16980, 23772, 30564, 37356, 44148, 50940),
             linetype = "dotted", color = "#333333") +
  scale_x_continuous(breaks = c(10188, 13584, 16980, 23772, 30564, 37356, 44148, 50940),
                     labels = scales::dollar_format(prefix = "")) +
  scale_colour_manual(values = blue_colors, name = '',
                      labels = c('0.6', '0.8', '1.0', '1.2', '1.4', 
                                 '1.6', '1.8', '2.0', '2.2')) +
  theme(legend.position = 'bottom', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        axis.text.x = element_text(size = 7.6))
dev.off()


# FPM per Capita ----------------------------------------------------------

fpm_pc <- df_pool %>% 
  group_by(th) %>% 
  summarise(fpm = mean(fpm/pop)) 

fpm_pc$fpm <- round(fpm_pc$fpm, digits = 0)  


tiff("figures/fpm-per-capita.tiff", units = "in", width = 7, height = 5, res = 300)
fpm_pc %>% 
  ggplot(aes(x = th, y = fpm, fill = th)) +
  geom_col() + 
  geom_text_repel(aes(label = scales::dollar(fpm, prefix = '')), data = fpm_pc, 
                  color = "gray20", size = 3, vjust = 2) +
  labs(title = "", x = "Threshold", y = "FPM Per Capita") +
  scale_fill_brewer(palette = "RdBu", name = '', labels = '') +
  scale_y_continuous(labels = scales::dollar_format(prefix = '')) +
  theme_ipsum_ps() +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12))
dev.off()


# FPM, Total, Own ---------------------------------------------------------

# total revenue
total_plot <- df_pool %>% 
  group_by(th) %>% 
  summarise(rev_total_m = (mean(rev_total))/100, rev_total_sd = sd(rev_total)/100, 
            min = min(rev_total), max = max(rev_total)) %>% 
  ggplot(aes(x = rev_total_m, y = th, color = th)) + 
  geom_point() +
  geom_errorbar(aes(xmin = rev_total_m - rev_total_sd, xmax = rev_total_m + rev_total_sd), 
                width = .4,
                position = position_dodge(0.05)) +
  labs(title = "A", x = "Total Revenue", y = "Thresholds") +
  scale_colour_manual(values = blue_colors, name = '', labels = '') +
  scale_x_continuous(labels = scales::dollar_format(prefix = "")) +
  theme_ipsum_ps() +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12))

# FPM 
fpm_plot <- df_pool %>% 
  group_by(th) %>% 
  summarise(fpm_m = (mean(fpm))/100, fpm_sd = sd(fpm)/100, 
            min = min(fpm), max = max(fpm)) %>% 
  ggplot(aes(x = fpm_m, y = th, color = th)) + 
  geom_point() +
  geom_errorbar(aes(xmin = fpm_m - fpm_sd, xmax = fpm_m + fpm_sd), width = .4,
                position = position_dodge(0.05)) +
  labs(title = "B", x = "FPM", y = "Thresholds") +
  scale_colour_manual(values = blue_colors, name = '', labels = '') +
  scale_x_continuous(labels = scales::dollar_format(prefix = "")) +
  theme_ipsum_ps() +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        plot.title = element_text(size = 12))

# own revenue
own_plot <- df_pool %>% 
  group_by(th) %>% 
  summarise(rev_own_m = (mean(rev_own))/100, rev_own_sd = sd(rev_own)/100, 
            min = min(rev_own), max = max(rev_own)) %>% 
  ggplot(aes(x = rev_own_m, y = th, color = th)) + 
  geom_point() +
  geom_errorbar(aes(xmin = rev_own_m - rev_own_sd, xmax = rev_own_m + rev_own_sd), 
                width = .4,
                position = position_dodge(0.05)) +
  labs(title = "C", x = "Own Revenue", y = "Thresholds") +
  scale_colour_manual(values = blue_colors, name = '', labels = '') +
  scale_x_continuous(labels = scales::dollar_format(prefix = "")) +
  theme_ipsum_ps() +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12))

# prop own/total
prop_plot <- df_pool %>% 
  group_by(th) %>% 
  summarise(prop = mean(rev_own/rev_total), sd = sd(rev_own/rev_total), 
            min = min(prop), max = max(prop)) %>% 
  ggplot(aes(x = prop, y = th, color = th)) + 
  geom_point() +
  geom_errorbar(aes(xmin = prop - sd, xmax = prop + sd), 
                width = .4, position = position_dodge(0.05)) +
  labs(title = "D", x = "% Own Revenue/Total Revenue", y = "Thresholds") +
  scale_colour_manual(values = blue_colors, name = '', labels = '') +
  scale_x_continuous(labels = scales::dollar_format(prefix = "")) +
  theme_ipsum_ps() +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12))

# grid
tiff("figures/revenues.tiff", units = "in", width = 6, height = 8, res = 300)
gridExtra::grid.arrange(total_plot, fpm_plot, own_plot, prop_plot)
dev.off()


# Rev Per Year ------------------------------------------------------------

rev_year <- df_pool %>% 
  select(year, rev_total, rev_own, fpm) %>% 
  pivot_longer(
    cols = (!year),
    names_to = "name",
    values_to = "values") %>% 
  group_by(year, name) %>%
  summarise(total = sum(values)/1000000000) 

# relevel
rev_year$name <- factor(rev_year$name, levels = c("rev_total", "fpm", "rev_own"))

# label 
data_ends <- rev_year %>% filter(year == 2017) %>% 
  mutate_if(is.numeric, round, digits = 0)

# colors  
three <- c("#08519c", "#3182bd", '#6baed6')

tiff("figures/rev-year.tiff", units = "in", width = 7, height = 5, res = 300)
rev_year %>% 
  ggplot(aes(x = year, y = total, group = name, color = name)) +
  geom_line(size = 1) + 
  geom_text_repel(aes(label = scales::dollar(total, prefix = '')), data = data_ends, 
                  color = "gray20", size = 3) +
  labs(title = "", x = "Year", y = "Values") +
  scale_color_manual(values = three, name = '', labels = c('Total Revenue', 'FPM', 
                                                             'Own Revenue','Property Tax')) +
  scale_y_continuous(labels = scales::dollar_format(prefix = '')) +
  theme_ipsum_ps() +
  theme(legend.position = 'bottom', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12))
dev.off()


# Own PC / Region ---------------------------------------------------------

own_pc <- df_pool %>% 
  group_by(region) %>% 
  summarise(pc = mean(rev_own/pop))

own_pc$region <- factor(own_pc$region, levels = c("North", "Northeast", "Central-West",
                                                  "South", "Southeast"))

own_pc$pc <- round(own_pc$pc, digits = 0)  


tiff("figures/rev-per-capita.tiff", units = "in", width = 7, height = 5, res = 300)
own_pc %>% 
  ggplot(aes(x = region, y = pc, fill = region)) +
  geom_col() + 
  geom_text(aes(label = scales::dollar(pc, prefix = '')), data = own_pc, 
                  color = "gray20", size = 3, vjust = 2) +
  labs(title = "", x = "Region", y = "Per Capita Own Revenue") +
  scale_fill_manual(values = blue_colors2 , name = '', labels = '') +
  scale_y_continuous(labels = scales::dollar_format(prefix = '')) +
  theme_ipsum_ps() +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12))
dev.off()
