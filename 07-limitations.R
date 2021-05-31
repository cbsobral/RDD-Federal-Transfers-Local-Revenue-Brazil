
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)

# Load data ---------------------------------------------------------------

load('data/df_pool.Rda')
load('data/cifpm.Rda')

df_pool <- df_pool %>% 
  filter(year != 2018)


# Re-estimate -------------------------------------------------------------


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


