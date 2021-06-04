
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)

# Load Data ---------------------------------------------------------------

load('data/df_pool.Rda')
load('data/cifpm.Rda')


# Validity ----------------------------------------------------------------

# filter one year only
df_2016 <- df_pool %>% 
  filter(year == 2016) 

# area in km
area <- rdrobust(y = df_2016$area, x = df_2016$pop_0)


# region
df_2016$region <- as.numeric(factor(c(df_2016$region)))
reg <- rdrobust(y = df_2016$region, x = df_2016$pop_0)


# Years -------------------------------------------------------------------

# with 2018
w2018 <- rdrobust(y = df_pool$logo_rev, x = df_pool$pop_0, covs = cbind(as.factor(df_pool$uf), 
                                                                        as.factor(df_pool$year)))

# without 2012
df_2012 <- df_pool %>% 
  filter(year != 2018) %>% 
  filter(year != 2012)

wt2012 <- rdrobust(y = df_2012$logo_rev, x = df_2012$pop_0, 
                   covs = cbind(as.factor(df_2012$uf), as.factor(df_2012$year)))


# Fixed Effects -----------------------------------------------------------

df_pool <- df_pool %>% 
  filter(year != 2018)

nofe <- rdrobust(y = df_pool$logo_rev, x = df_pool$pop_0, all = T)


# Placebo Cutoffs ---------------------------------------------------------

m350 <- rdrobust(y = df_pool$logo_rev, x = df_pool$pop_0, c = -350, 
         covs = cbind(as.factor(df_pool$uf), as.factor(df_pool$year)))

p350 <- rdrobust(y = df_pool$logo_rev, x = df_pool$pop_0, c = 350, 
         covs = cbind(as.factor(df_pool$uf), as.factor(df_pool$year)))


# Bandwidth ---------------------------------------------------------------

halfbw <- rdrobust(y = df_pool$logo_rev, x = df_pool$pop_0, all = TRUE, h = 625,
                 covs = cbind(as.factor(df_pool$uf), as.factor(df_pool$year)))

dbbw <- rdrobust(y = df_pool$logo_rev, x = df_pool$pop_0, h = 1876,
                 covs = cbind(as.factor(df_pool$uf), as.factor(df_pool$year)))


# Polynomial --------------------------------------------------------------

p2 <- rdrobust(y = df_pool$logo_rev, x = df_pool$pop_0, p = 2, all = T,
               covs = cbind(as.factor(df_pool$uf), as.factor(df_pool$year)))

p3 <- rdrobust(y = df_pool$logo_rev, x = df_pool$pop_0, p = 3, all = T,
               covs = cbind(as.factor(df_pool$uf), as.factor(df_pool$year)))


# No Movement -------------------------------------------------------------

# filter without movement 
no_change <- df_pool %>%  
  group_by(codibge) %>% 
  count(bracket) %>% 
  group_by(codibge) %>% 
  count() %>% 
  filter(n < 2) %>% 
  pull(codibge)

df_no_change <- df_pool %>% 
  filter(codibge %in% no_change)


no_change <- rdrobust(y = df_no_change$logo_rev, x = df_no_change$pop_0, all = T,
                 covs = cbind(as.factor(df_no_change$uf), as.factor(df_no_change$year)))

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

write.table(dfstotal_all, file = "coef_pool.csv")
write.table(dfstotal_bw, file = "bw_pool.csv")

