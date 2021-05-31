
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)

# Load data ---------------------------------------------------------------

# main data
load('data/df_main.Rda')

# coefficients and population data
load('data/cifpm.Rda')


# Process -----------------------------------------------------------------

# filter max population
df_prep <- df_main %>% 
  filter(pop < 61128)

# label brackets
df_prep <- df_prep %>% 
  mutate(bracket = if_else(pop <= 10188, 0.6,
                           if_else(pop > 10188 & pop <= 13584, 0.8,
                           if_else(pop > 13584 & pop <= 16980, 1,
                           if_else(pop > 16980 & pop <= 23772, 1.2,
                           if_else(pop > 23772 & pop <= 30564, 1.4,
                           if_else(pop > 30564 & pop <= 37356, 1.6,
                           if_else(pop > 37356 & pop <= 44148, 1.8,
                           if_else(pop > 44148 & pop <= 50940, 2,
                           if_else(pop > 50940 & pop <= 61128, 2.2, 
                           2.4))))))))))

#  municipalities that do not conform with coefficient
table(df_prep$cifpm, df_prep$bracket)

# 129 values || df_prep should have 35096 observations
# 24+25+20+22+7+9+10+5+7

# remove the ones that have cifpm different from bracket
df_prep <- subset(df_prep, df_prep$cifpm == df_prep$bracket)  # 35,096 observations



# Create Multiple DFs -----------------------------------------------------

th_1 <- df_prep %>% 
  filter(pop >=  0 & pop <= 13584) %>% 
  mutate(pop_0 = pop - 10188) %>% 
  mutate(treat = ifelse(pop > 10188, 1, 0)) %>% 
  mutate(th = paste(1))


th_2 <- df_prep %>% 
  filter(pop > 10188 & pop <= 16980) %>% 
  mutate(pop_0 = pop - 13584) %>% 
  mutate(treat = ifelse(pop > 13584, 1, 0)) %>% 
  mutate(th = paste(2))

th_3 <- df_prep %>% 
  filter(pop > 13584 & pop <= 23772) %>% 
  mutate(pop_0 = pop - 16980) %>% 
  mutate(treat = ifelse(pop > 16980, 1, 0)) %>% 
  mutate(th = paste(3))

th_4 <- df_prep %>% 
  filter(pop > 16980 & pop <= 30564) %>% 
  mutate(pop_0 = pop - 23772) %>% 
  mutate(treat = ifelse(pop > 23772, 1, 0)) %>% 
  mutate(th = paste(4))

th_5 <- df_prep %>% 
  filter(pop > 23772 & pop <= 37356) %>% 
  mutate(pop_0 = pop - 30564) %>% 
  mutate(treat = ifelse(pop > 30564, 1, 0)) %>% 
  mutate(th = paste(5))

th_6 <- df_prep %>% 
  filter(pop > 30564 & pop <= 44148) %>% 
  mutate(pop_0 = pop - 37356) %>% 
  mutate(treat = ifelse(pop > 37356, 1, 0)) %>% 
  mutate(th = paste(6))

th_7 <- df_prep %>% 
  filter(pop > 37356 & pop <= 50940) %>% 
  mutate(pop_0 = pop - 44148) %>% 
  mutate(treat = ifelse(pop > 44148, 1, 0)) %>% 
  mutate(th = paste(7))

th_8 <- df_prep %>% 
  filter(pop > 44148 & pop <= 61128) %>% 
  mutate(pop_0 = pop - 50940) %>% 
  mutate(treat = ifelse(pop > 50940, 1, 0)) %>% 
  mutate(th = paste(8))

# save with duplicated values
df_dpl <- rbind(th_1, th_2, th_3, th_4, th_5, th_6, th_7, th_8) # not used


# Filter (Brollo) ---------------------------------------------------------

th_1 <-  th_1 %>% 
  filter(pop <= 11885) 

th_2 <-  th_2 %>% 
  filter(pop >= 11886 & pop <= 15281)

th_3 <-  th_3 %>% 
  filter(pop >= 15282 & pop <= 20375)

th_4 <-  th_4 %>% 
  filter(pop >= 20376 & pop <= 27167)

th_5 <-  th_5 %>% 
  filter(pop >= 27168 & pop <= 33959)

th_6 <-  th_6 %>% 
  filter(pop >= 33960 & pop <= 40751)

th_7 <-  th_7 %>% 
  filter(pop >= 40752 & pop <= 47543)

th_8 <-  th_8 %>% 
  filter(pop >= 47544)

# merge
df_pool <- rbind(th_1, th_2, th_3, th_4, th_5, th_6, th_7, th_8) # according to Brollo (2013)



# Save --------------------------------------------------------------------

save(df_dpl, file = 'data/df_dpl.Rda')
save(df_pool, file = 'data/df_pool.Rda')
