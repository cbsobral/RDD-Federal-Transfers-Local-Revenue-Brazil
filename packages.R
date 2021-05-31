# install packages from CRAN
p_needed <- c(#'texreg', # load first bc conflict with magrittr/tidyr
              'tidyverse','haven', 'lubridate', # tidverse tools
              'magrittr', 'janitor', 'broom', # clean up stuff
              'stargazer', 'xtable', 'summarytools', 'kableExtra', 'table1', 'kableExtra', # format stuff
              'ggthemes', 'hrbrthemes',  'RColorBrewer', 'gridExtra', # plot stuff
              'rdd', 'rdrobust', 'rdmulti' # stats stuff
              
)

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)
