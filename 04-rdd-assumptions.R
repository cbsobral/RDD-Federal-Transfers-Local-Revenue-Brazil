
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)

import_plex_sans()
theme_set(theme_ipsum_ps())
extrafont::loadfonts(quiet = TRUE, device = 'pdf')

# 9 colors
blue_colors <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0",
                 "#92C5DE", "#4393C3", "#2166AC", "#192841")


# Load Data ---------------------------------------------------------------

load('data/df_pool.Rda')
load('data/cifpm.Rda')

df_pool <- df_pool %>% 
  filter(year != 2018)


# Coefficients --------------------------------------------------------------

tiff("figures/coeffs.tiff", units = "in", width = 7, height = 5, res = 300)
ggplot(df_pool, 
       aes(x = pop, y = factor(cifpm), color = factor(cifpm))) +
  geom_point() + 
  labs(title = "", x = "Population", y = "FPM Coefficient") +
  geom_vline(xintercept = c(10188, 13584, 16980, 23772, 30564, 37356, 44148, 
                            50940), linetype = "dotted", color = "#333333") +
  scale_x_continuous(breaks = c(10188, 13584, 16980, 23772, 30564, 37356, 44148, 
                                50940),
                     labels = scales::dollar_format(prefix = "")) +
  scale_colour_manual(values = blue_colors, name = '',
                      labels = c('0.6', '0.8', '1.0', '1.2', '1.4', '1.6', 
                                 '1.8', '2.0', '2.2')) +
  theme(legend.position = 'bottom', plot.margin = unit(c(0,0.2,0.2,0.2), "cm"), 
        axis.text.x = element_text(size = 7.5))
dev.off()


# McCrary Discontinuity ---------------------------------------------------

# split by thresholds
df_th <- split(df_pool, df_pool$th)

# create df with thresholds
df_density <- as.data.frame(list(unique(df_pool$th)))

# calculate density for each
df_density <- df_density %>% 
  mutate(p_value = map(
    .x = df_th, # iterate over split dfs
    .f = (~ DCdensity(.x$pop_0, plot = FALSE)))) # formula

# unnest list
df_density <- unnest(df_density, p_value) 

# print
df_density %>% 
  rename(threshold = 1) %>% # rename column 1
  kbl() %>% 
  kable_styling()


# Sorting Evidence + McCrary ----------------------------------------------

two_colors <- c("#E41A1C",'#005b96')

th1 <- ggplot(df_pool %>% 
                filter(th == 1), 
              aes(x = pop_0, fill = factor(treat))) +
  geom_histogram(bins = 150) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#333333") +
  labs(title = "1", subtitle = "p-value = .005",x = "Normalized Population", y = "Frequency") +
  scale_fill_manual(values = two_colors, name = '', labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12), plot.subtitle = element_text(size = 8, 
                                                                           hjust = 1))

th2 <- ggplot(df_pool %>% 
                filter(th == 2), 
              aes(x = pop_0, fill = factor(treat))) +
  geom_histogram(bins = 150) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#333333") +
  labs(title = "2", subtitle = "p-value = .990",x = "Normalized Population", y = "Frequency") +
scale_fill_manual(values = two_colors, name = '', labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12), plot.subtitle = element_text(size = 8, 
                                                                           hjust = 1))
th3 <- ggplot(df_pool %>% 
                filter(th == 3), 
              aes(x = pop_0, fill = factor(treat))) +
  geom_histogram(bins = 150) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#333333") +
  labs(title = "3", subtitle = "p-value = .389",x = "Normalized Population", y = "Frequency") +
scale_fill_manual(values = two_colors, name = '', labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12), plot.subtitle = element_text(size = 8, 
                                                                           hjust = 1))
th4 <- ggplot(df_pool %>% 
                filter(th == 4), 
              aes(x = pop_0, fill = factor(treat))) +
  geom_histogram(bins = 150) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#333333") +
  labs(title = "4", subtitle = "p-value = .151",x = "Normalized Population", y = "Frequency") +
scale_fill_manual(values = two_colors, name = '', labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12), plot.subtitle = element_text(size = 8, 
                                                                           hjust = 1))
th5 <- ggplot(df_pool %>% 
                filter(th == 5), 
              aes(x = pop_0, fill = factor(treat))) +
  geom_histogram(bins = 150) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#333333") +
  labs(title = "5", subtitle = "p-value = .984",x = "Normalized Population", y = "Frequency") +
scale_fill_manual(values = two_colors, name = '', labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12), plot.subtitle = element_text(size = 8, 
                                                                           hjust = 1))
th6 <- ggplot(df_pool %>% 
                filter(th == 6), 
              aes(x = pop_0, fill = factor(treat))) +
  geom_histogram(bins = 150) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#333333") +
  labs(title = "6", subtitle = "p-value = .154",x = "Normalized Population", y = "Frequency") +
scale_fill_manual(values = two_colors, name = '', labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12), plot.subtitle = element_text(size = 8, 
                                                                           hjust = 1))
th7 <- ggplot(df_pool %>% 
                filter(th == 7), 
              aes(x = pop_0, fill = factor(treat))) +
  geom_histogram(bins = 150) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#333333") +
  labs(title = "7", subtitle = "p-value = .544",x = "Normalized Population", y = "Frequency") +
scale_fill_manual(values = two_colors, name = '', labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12), plot.subtitle = element_text(size = 8, 
                                                                           hjust = 1))
th8 <- ggplot(df_pool %>% 
                filter(th == 8), 
              aes(x = pop_0, fill = factor(treat))) +
  geom_histogram(bins = 150) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#333333") +
  labs(title = "8", subtitle = "p-value = .801",x = "Normalized Population", y = "Frequency") +
scale_fill_manual(values = two_colors, name = '', labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), 
        plot.title = element_text(size = 12), plot.subtitle = element_text(size = 8, 
                                                                           hjust = 1))

tiff("figures/mc-crary-disc.tiff", units = "in", width = 6, height = 8.6, res = 300)
gridExtra::grid.arrange(th1, th2, th3, th4, th5, th6, th7, th8, ncol = 2)
dev.off()