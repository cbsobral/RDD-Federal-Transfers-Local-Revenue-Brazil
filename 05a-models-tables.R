
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)

# Load Data ---------------------------------------------------------------

load('data/df_pool.Rda')
load('data/cifpm.Rda')

df_pool <- df_pool %>% 
  filter(year != 2018)


import_plex_sans()
theme_set(theme_ipsum_ps())
extrafont::loadfonts(quiet = TRUE, device = 'pdf')

# Main Results ------------------------------------------------------------

# all thresholds
all <- rdrobust(y = df_pool$logo_rev, x = df_pool$pop_0, p = 1, all = T,
                covs = cbind(as.factor(df_pool$uf), as.factor(df_pool$year)))


# Regions -----------------------------------------------------------------

# split 
df_regions <- split(df_pool, df_pool$region)

# create data frame
rdd_regions <- data.frame(order = 1:5, region = c('Central-West','North', 
                                                  'Northeast', 'South', 'Southeast'))
# models
rdd_regions <- rdd_regions %>%
  mutate(rdd_obj = map(
    .x = df_regions, # the first object to iterate over
    .f = (~ rdrobust(y = .x$logo_rev, x = .x$pop_0, all = T, 
                     covs = cbind(as.factor(.x$uf), as.factor(.x$year))))))

north <- rdd_regions$rdd_obj %>% 
  pluck(2) 

northeast <- rdd_regions$rdd_obj %>% 
  pluck(3) 

centralw <- rdd_regions$rdd_obj %>% 
  pluck(1) 

south <- rdd_regions$rdd_obj %>% 
  pluck(4) 

southeast <- rdd_regions$rdd_obj %>% 
  pluck(5) 



# GDP ---------------------------------------------------------------------


df_pool <- df_pool  %>% 
  group_by(th) %>% 
  mutate(develop = if_else(gdp > median(gdp), 1, 0)) %>% 
  ungroup()

df_dev <- split(df_pool, df_pool$develop)


# not developed
notdev <- rdrobust(y = df_dev[["0"]]$logo_rev, x = df_dev[["0"]]$pop_0, all = T,
                 covs = cbind(as.factor(df_dev[["0"]]$uf), 
                              as.factor(df_dev[["0"]]$year)))

# developed
dev <- rdrobust(y = df_dev[["1"]]$logo_rev, x = df_dev[["1"]]$pop_0, all = T,
                 covs = cbind(as.factor(df_dev[["1"]]$uf), 
                              as.factor(df_dev[["1"]]$year)))

# Thresholds --------------------------------------------------------------

# split
df_th <- split(df_pool, df_pool$th)

# create data frame
rdd_th <- as.data.frame(list(unique(df_pool$th)))

# models
rdd_th <- rdd_th %>%
  mutate(rdd_obj = map(
    .x = df_th, # the first object to iterate over
    .f = (~ rdrobust(y = .x$logo_rev, x = .x$pop_0, all = T, 
                     covs = cbind(as.factor(.x$uf), as.factor(.x$year))))))


# Thresholds --------------------------------------------------------------


th1 <- rdd_th$rdd_obj %>% 
  pluck(1) 

th2 <- rdd_th$rdd_obj %>% 
  pluck(2) 

th3 <- rdd_th$rdd_obj %>% 
  pluck(3) 

th4 <- rdd_th$rdd_obj %>% 
  pluck(4)

th5 <- rdd_th$rdd_obj %>% 
  pluck(5) 

th6 <- rdd_th$rdd_obj %>% 
  pluck(6) 

th7 <- rdd_th$rdd_obj %>% 
  pluck(7) 

th8 <- rdd_th$rdd_obj %>% 
  pluck(8) 



# Tables ------------------------------------------------------------------


dfs <-  list(all, north, northeast, centralw, south, southeast, notdev, dev)

# round to 3 digits
dfstotal_all <- map_dfr(dfs, extract, c("coef", "se", "ci", "pv")) %>%
  round(digits = 3)

dfstotal_bw <- map_dfr(dfs, extract, c("bws")) %>%
  round(digits = 3)

# repair names
tibble(dfstotal_all, .name_repair = "unique")
tibble(dfstotal_bw, .name_repair = "unique")

# write tables
write.table(dfstotal_all, file = "coef_main.csv")
write.table(dfstotal_bw, file = "bw_main.csv")



# Limitations -------------------------------------------------------------


df_3 <- df_pool %>% 
  filter(th != 3)

df_6 <- df_pool %>% 
  filter(th != 6)

df_7 <- df_pool %>% 
  filter(th != 7)


# without 3
summary(rdrobust(y = df_3$logo_rev, x = df_3$pop_0, all = T, 
                 covs = cbind(as.factor(df_3$uf), as.factor(df_3$year))))


# without 6
summary(rdrobust(y = df_6$logo_rev, x = df_6$pop_0, all = T, 
                 covs = cbind(as.factor(df_6$uf), as.factor(df_6$year))))

# without 7
summary(rdrobust(y = df_7$logo_rev, x = df_7$pop_0, all = T, 
                 covs = cbind(as.factor(df_7$uf), as.factor(df_7$year))))
