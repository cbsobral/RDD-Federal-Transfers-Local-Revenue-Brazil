
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)

# Load Data ---------------------------------------------------------------

load('data/df_pool.Rda')
load('data/cifpm.Rda')

df_pool <- df_pool %>% 
  filter(year != 2018)

robustness <- read_delim("data/robustness.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

import_plex_sans()
theme_set(theme_ipsum_ps())
extrafont::loadfonts(quiet = TRUE, device = 'pdf')


# All Estimates -------------------------------------------------------------


three <- c('#005b96',"#E41A1C")

robustness$sub <- factor(robustness$sub, levels = c('Pooled', 'South', 'Southeast', 'Higher GDP'))

robustness$model <- factor(robustness$model , levels = c('Without 2012', 'With 2018', 'P = 3', 
                                                         'P = 2',  'C = - 350', 'C = + 350', 
                                                         'H = 1.5', 'H = 0.5', 'No Movement', 
                                                         'No Fixed Effects',  'Baseline'))


tiff("figures/robust-3.tiff", units = "in", width = 7, height = 3.5, res = 300)
robustness %>%  
  filter(sub != 'Pooled') %>% 
ggplot(aes(x = coef, y = model, color = point)) + 
  geom_vline(xintercept = 0, colour = "black", lty = 2) + 
  geom_point(size = 1) +
  geom_errorbar(aes(xmin = inf, xmax = sup), width = .2, colour = "gray20", 
                size = 0.7, alpha = 0.5, position = position_dodge(0.05)) +
  labs(title = "", x = "Estimated Treatment Effects", y = "") +
  scale_colour_manual(values = three, name = '', labels = '') +
  theme_ipsum_ps() +
  theme(legend.position = 'none', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        strip.text = element_text(size = 8)) +
  facet_grid(~ sub)
dev.off()