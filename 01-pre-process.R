
# Libraries ---------------------------------------------------------------

source("packages.R")
options(scipen = 999)


# Load Data ---------------------------------------------------------------

# revenues
rev_total <- read_csv("raw_data/rev_total.csv")
rev_own <- read_csv("raw_data/rev_own.csv")
rev_iptu <- read_csv("raw_data/rev_iptu.csv")

## merge revenues
revenue <- merge(rev_total, rev_own, by = 'codibge')
revenue <- merge(revenue, rev_iptu, by = 'codibge')


# transfers 12-18
tcu_2012 <- read_csv("raw_data/tcu_2012.csv", col_types = cols(indice = col_skip()))
tcu_2013 <- read_csv("raw_data/tcu_2013.csv", col_types = cols(indice = col_skip()))
tcu_2014 <- read_csv("raw_data/tcu_2014.csv", col_types = cols(indice = col_skip()))
tcu_2015 <- read_csv("raw_data/tcu_2015.csv")
tcu_2016 <- read_csv("raw_data/tcu_2016.csv")
tcu_2017 <- read_csv("raw_data/tcu_2017.csv")
tcu_2018 <- read_csv("raw_data/tcu_2018.csv", col_types = cols(indice = col_skip()))


#gdp 12-18
gdp <- read_csv("raw_data/gdp.csv", col_types = cols(`Município` = col_skip()))

# area 
area <- read_csv("raw_data/area.csv", col_types = cols(`Município` = col_skip()))

# fpm 
fpm <- read_delim("raw_data/fpm.csv", ";", 
                  escape_double = FALSE, col_types = cols(mun = col_skip(), 
                                                               transfer = col_skip(), 
                                                               siafi = col_skip()), 
                  locale = locale(decimal_mark = ",", grouping_mark = ""), 
                  trim_ws = TRUE)

# Shape -------------------------------------------------------------------

# add year column
tcu_2012$year <- '2012'
tcu_2013$year <- '2013'
tcu_2014$year <- '2014'
tcu_2015$year <- '2015'
tcu_2016$year <- '2016'
tcu_2017$year <- '2017'
tcu_2018$year <- '2018'

# merge revenue with transfers (tcu)
tcu_2012 <- revenue %>% 
  select(codibge, total_2012, own_2012, iptu_2012) %>% 
  rename(rev_total = total_2012, rev_own = own_2012, rev_iptu = iptu_2012) %>% 
  merge(tcu_2012, by = 'codibge') 

tcu_2013 <- revenue %>% 
  select(codibge, total_2013, own_2013, iptu_2013) %>% 
  rename(rev_total = total_2013, rev_own = own_2013, rev_iptu = iptu_2013) %>% 
  merge(tcu_2013, by = 'codibge') 

tcu_2014 <- revenue %>% 
  select(codibge, total_2014, own_2014, iptu_2014) %>% 
  rename(rev_total = total_2014, rev_own = own_2014, rev_iptu = iptu_2014) %>% 
  merge(tcu_2014, by = 'codibge') 

tcu_2015 <- revenue %>% 
  select(codibge, total_2015, own_2015, iptu_2015) %>% 
  rename(rev_total = total_2015, rev_own = own_2015, rev_iptu = iptu_2015) %>% 
  merge(tcu_2015, by = 'codibge') 

tcu_2016 <- revenue %>% 
  select(codibge, total_2016, own_2016, iptu_2016) %>% 
  rename(rev_total = total_2016, rev_own = own_2016, rev_iptu = iptu_2016) %>% 
  merge(tcu_2016, by = 'codibge') 

tcu_2017 <- revenue %>% 
  select(codibge, total_2017, own_2017, iptu_2017) %>% 
  rename(rev_total = total_2017, rev_own = own_2017, rev_iptu = iptu_2017) %>% 
  merge(tcu_2017, by = 'codibge')  

tcu_2018 <- revenue %>% 
  select(codibge, total_2018, own_2018, iptu_2018) %>% 
  rename(rev_total = total_2018, rev_own = own_2018, rev_iptu = iptu_2018) %>% 
  merge(tcu_2018, by = 'codibge') 


# join dfs
df_main <- rbind(tcu_2012, tcu_2013, tcu_2014, tcu_2015, tcu_2016, tcu_2017, tcu_2018)


# GDP
## pivot

summary(gdp)
gdp <- gdp %>%
  pivot_longer(!codibge, names_to = "year", values_to = "gdp")

## remove last digit
gdp$codibge <- strtrim(gdp$codibge, 6)
gdp$codibge <- as.numeric(gdp$codibge)

## join with df_main
df_main <- merge(df_main, gdp, by = c("codibge", "year"))

# area
## remove last digit
area$codibge <- strtrim(area$codibge, 6)
area$codibge <- as.numeric(area$codibge)

# joing with df_main
df_main <- merge(df_main, area, by = c("codibge"))


# New Variables -----------------------------------------------------------

df_main <- df_main %>% 
  mutate(logt_rev = log(rev_total)) %>%
  mutate(logo_rev = log(rev_own)) %>%
  mutate(logi_rev = log(rev_iptu)) %>%
  mutate(gdp = gdp*1000) %>% # current values
  mutate(log_gdp = log(gdp)) %>% # log of gdp
  mutate(gdp_pc = gdp/pop) # gdp per capita


# Add Region --------------------------------------------------------------

df_main <- df_main %>% 
  mutate(region = if_else(uf == 'PR' | uf == 'SC' | uf == 'RS', "South",
                          ifelse(uf == 'RO' | uf == 'AC' | uf == 'AM' | uf == 'RR' | uf == 'PA' | uf == 'AP' | uf == 'TO', "North",
                          ifelse(uf == 'MA' | uf == 'PI' | uf == 'CE' | uf == 'RN' | uf == 'PB' | uf == 'PE' | uf == 'AL' | uf == 'SE' | uf == 'BA', "Northeast",
                          ifelse(uf == 'MG' | uf == 'ES' | uf == 'RJ' | uf == 'SP', "Southeast",
                          "Central-West")))))  


# Add FPM -----------------------------------------------------------------

## remove last digit
fpm$codibge <- strtrim(fpm$codibge, 6)
fpm$codibge <- as.numeric(fpm$codibge)

# joing with df_main
df_main <- merge(df_main, fpm, by = c("codibge", "year"))

# Save --------------------------------------------------------------------

# re-order
col_order <- c("codibge", "uf", "region", "area", "pop", "year", "cifpm", "part_uf", 
               "rev_total", "rev_own","rev_iptu","logt_rev", "logo_rev", "logi_rev",
               "gdp", "log_gdp", "gdp_pc", "fpm")

df_main <- df_main[, col_order]


# save
save(df_main, file = "data/df_main.Rda")
