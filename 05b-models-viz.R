
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)

# Load Data ---------------------------------------------------------------

load('data/df_pool.Rda')
load('data/cifpm.Rda')

df_pool <- df_pool %>% 
  filter(year != 2018)

outcomes <- read_delim("data/outcomes.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

import_plex_sans()
theme_set(theme_ipsum_ps())
extrafont::loadfonts(quiet = TRUE, device = 'pdf')


# Subsamples --------------------------------------------------------------

# regions
df_regions <- split(df_pool, df_pool$region)

# GDP
df_pool <- df_pool  %>% 
  group_by(th) %>% 
  mutate(develop = if_else(gdp > median(gdp), 1, 0)) %>% 
  ungroup()

df_dev <- split(df_pool, df_pool$develop)


# Pooled ------------------------------------------------------------------


all_p <- rdplot(y = df_pool$logo_rev, x = df_pool$pop_0, p = 2, kernel = "triangular", 
                col.lines = "#E41A1C", col.dots = '#005b96',
                covs = cbind(as.factor(df_pool$uf), as.factor(df_pool$year)))

tiff("figures/rd-plots-pool.tiff", units = "in", width = 7, height = 5, res = 300)
all_p$rdplot + theme_set(theme_ipsum_ps()) +
  labs(title = "", x = "Normalized Population", y = "Log(Own Revenue)") +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), plot.title = element_text(size = 12))
dev.off()


# Region ------------------------------------------------------------------

# north
north_p <- rdplot(y = df_regions[["North"]]$logo_rev, x = df_regions[["North"]]$pop_0, 
                  p = 2, kernel = "triangular", col.lines = "#E41A1C", col.dots = '#005b96',
                  covs = cbind(as.factor(df_regions[["North"]]$uf), 
                               as.factor(df_regions[["North"]]$year)))
summary(north_p)

north <- north_p$rdplot + theme_set(theme_ipsum_ps()) +
  labs(title = "A", x = "Normalized Population", y = "Log(Own Revenue)") +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), plot.title = element_text(size = 12))


# northeast
northeast_p <- rdplot(y = df_regions[["Northeast"]]$logo_rev, x = df_regions[["Northeast"]]$pop_0, 
                  p = 2, kernel = "triangular", col.lines = "#E41A1C", col.dots = '#005b96',
                  covs = cbind(as.factor(df_regions[["Northeast"]]$uf), 
                               as.factor(df_regions[["Northeast"]]$year)))
summary(northeast_p)

northeast <- northeast_p$rdplot + theme_set(theme_ipsum_ps()) +
  labs(title = "B", x = "Normalized Population", y = "Log(Own Revenue)") +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), plot.title = element_text(size = 12))


# central-west
cw_p <- rdplot(y = df_regions[["Central-West"]]$logo_rev, x = df_regions[["Central-West"]]$pop_0, 
                      p = 2, kernel = "triangular", col.lines = "#E41A1C", col.dots = '#005b96',
                      covs = cbind(as.factor(df_regions[["Central-West"]]$uf), 
                                   as.factor(df_regions[["Central-West"]]$year)))
summary(cw_p)

cw <- cw_p$rdplot + theme_set(theme_ipsum_ps()) +
  labs(title = "C", x = "Normalized Population", y = "Log(Own Revenue)") +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), plot.title = element_text(size = 12))

# south
south_p <- rdplot(y = df_regions[["South"]]$logo_rev, x = df_regions[["South"]]$pop_0, 
                  p = 2, kernel = "triangular", col.lines = "#E41A1C", col.dots = '#005b96',
                  covs = cbind(as.factor(df_regions[["South"]]$uf), 
                               as.factor(df_regions[["South"]]$year)))
summary(south_p)

south <- south_p$rdplot + theme_set(theme_ipsum_ps()) +
  labs(title = "D", x = "Normalized Population", y = "Log(Own Revenue)") +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), plot.title = element_text(size = 12))


# southeast
southeast_p <- rdplot(y = df_regions[["Southeast"]]$logo_rev, 
                      x = df_regions[["Southeast"]]$pop_0, p = 2, kernel = "triangular", 
                      col.lines = "#E41A1C", col.dots = '#005b96',
                      covs = cbind(as.factor(df_regions[["Southeast"]]$uf), 
                                   as.factor(df_regions[["Southeast"]]$year)))
summary(southeast_p)

southeast <- southeast_p$rdplot + theme_set(theme_ipsum_ps()) +
  labs(title = "E", x = "Normalized Population", y = "Log(Own Revenue)") +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), plot.title = element_text(size = 12))


tiff("figures/rd-plots-reg.tiff", units = "in", width = 6, height = 8, res = 300)
gridExtra::grid.arrange(north, northeast, cw, south, southeast, ncol = 2)
dev.off()

# GDP ---------------------------------------------------------------------

# not dev
ndev_p <- rdplot(y = df_dev[["0"]]$logo_rev, x = df_dev[["0"]]$pop_0, p = 1, 
                 kernel = "triangular", col.lines = "#E41A1C", col.dots = '#005b96',
                 covs = cbind(as.factor(df_dev[["0"]]$uf), as.factor(df_dev[["0"]]$year)))

ndev <- ndev_p$rdplot + theme_set(theme_ipsum_ps()) +
  labs(title = "A", x = "Normalized Population", y = "Log(Own Revenue)") +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), plot.title = element_text(size = 12))

summary(ndev_p)

# dev
dev_p <- rdplot(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, p = 1, 
                kernel = "triangular", col.lines = "#E41A1C", col.dots = '#005b96',
                      covs = cbind(as.factor(df_dev[["1"]]$uf), as.factor(df_dev[["1"]]$year)))


dev <- dev_p$rdplot + theme_set(theme_ipsum_ps()) +
  labs(title = "B", x = "Normalized Population", y = "Log(Own Revenue)") +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), plot.title = element_text(size = 12))

summary(dev_p)


tiff("figures/rd-plots-dev.tiff", units = "in", width = 7, height = 3.5, res = 300)
gridExtra::grid.arrange(ndev, dev, ncol = 2)
dev.off()


# All Estimates -------------------------------------------------------------

outcomes$label <- factor(outcomes$label, levels = c("Higher GDP", "Lower GDP", 
                                                  "Southeast", "South", "Central-West", 
                                                  "Northeast", "North","Pooled" ))

outcomes$subsample <- factor(outcomes$subsample , levels = c('baseline', 'region', 'gdp'))

three <- brewer.pal(3,"Set1")


tiff("figures/outcomes.tiff", units = "in", width = 7, height = 3.5, res = 300)
outcomes %>%  
ggplot(aes(x = outcome, y = label, color = subsample)) + 
  geom_vline(xintercept = 0, colour = "black", lty = 2) + 
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = inf, xmax = sup), width = .4, colour = "gray20", size = 0.7, alpha = 0.5,
                position = position_dodge(0.05)) +
  labs(title = "", x = "Estimated Treatment Effects", y = "") +
  scale_colour_manual(values = three, name = '', labels = c('Entire Sample', 'Region', 'GDP')) +
  theme_ipsum_ps() +
  theme(legend.position = 'bottom', plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        plot.title = element_text(size = 12))
dev.off()