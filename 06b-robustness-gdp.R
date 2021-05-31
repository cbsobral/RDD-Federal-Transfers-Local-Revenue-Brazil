
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)

# Load Data ---------------------------------------------------------------

load('data/df_pool.Rda')
load('data/cifpm.Rda')


import_plex_sans()
theme_set(theme_ipsum_ps())
extrafont::loadfonts(quiet = TRUE, device = 'pdf')


# GDP Subsample -----------------------------------------------------------

df_pool <- df_pool  %>% 
  group_by(th) %>% 
  mutate(develop = if_else(gdp > median(gdp), 1, 0)) %>% 
  ungroup()

df_dev <- split(df_pool, df_pool$develop)


# Fixed Effects -----------------------------------------------------------

df_dev[["1"]] <- df_dev[["1"]] %>% 
  filter(year != 2018)

nofe <- rdrobust(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, all = T)


# False Cutoffs -----------------------------------------------------------

m350 <- rdrobust(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, c = -350,
         covs = cbind(as.factor(df_dev[["1"]]$uf), as.factor(df_dev[["1"]]$year)))

p350 <- rdrobust(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, c = 350, 
         covs = cbind(as.factor(df_dev[["1"]]$uf), as.factor(df_dev[["1"]]$year)))


# Bandwidth ---------------------------------------------------------------

halfbw <- rdrobust(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, all = TRUE, h = 580,
                 covs = cbind(as.factor(df_dev[["1"]]$uf), as.factor(df_dev[["1"]]$year)))

dbbw <- rdrobust(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, h = 1741, all = T,
                 covs = cbind(as.factor(df_dev[["1"]]$uf),  as.factor(df_dev[["1"]]$year)))

# No Movement -------------------------------------------------------------

# filter municipalities without movement 
no_change <- df_dev[["1"]] %>%  
  group_by(codibge) %>% 
  count(bracket) %>% 
  group_by(codibge) %>% 
  count() %>% 
  filter(n < 2) %>% 
  pull(codibge)

df_no_change <- df_dev[["1"]] %>% 
  filter(codibge %in% no_change)

# model
no_change <- rdrobust(y = df_no_change$logo_rev, x = df_no_change$pop_0, all = T,
                 covs = cbind(as.factor(df_no_change$uf), as.factor(df_no_change$year)))


# Polynomial --------------------------------------------------------------

p2 <- rdrobust(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, p = 2, all = T,
                covs = cbind(as.factor(df_dev[["1"]]$uf), as.factor(df_dev[["1"]]$year)))

p3 <- rdrobust(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, p = 3, all = T,
               covs = cbind(as.factor(df_dev[["1"]]$uf), as.factor(df_dev[["1"]]$year)))


# Years -------------------------------------------------------------------

# with 2018
w2018 <- rdrobust(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, covs = cbind(as.factor(df_dev[["1"]]$uf), 
                                                                                    as.factor(df_dev[["1"]]$year)))

# without 2012
df_2012 <- df_dev[["1"]] %>% 
  filter(year != 2018) %>% 
  filter(year != 2012)

wt2012 <- rdrobust(y = df_2012$logo_rev, x = df_2012$pop_0, covs = cbind(as.factor(df_2012$uf), 
                                                                         as.factor(df_2012$year)))



# Prints ------------------------------------------------------------------


dfs <-  list(nofe, no_change, halfbw, dbbw, p350, m350, p2, p3, w2018, wt2012)

# round to 3 digits
dfstotal_all <- map_dfr(dfs, extract, c("coef", "se", "ci", "pv")) %>%
  round(digits = 3)

dfstotal_bw <- map_dfr(dfs, extract, c("bws")) %>%
  round(digits = 3)

# repair names
tibble(dfstotal_all, .name_repair = "unique")
tibble(dfstotal_bw, .name_repair = "unique")

write.table(dfstotal_all, file = "coef_gdp.csv")
write.table(dfstotal_bw, file = "bw_gdp.csv")
