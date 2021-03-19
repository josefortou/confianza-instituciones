# libraries

library(tidyverse)
library(broom)

# data ####

wvs <- read_rds("output/wvs.rds")

# model data ####

# SES

mod <- lm(conf_xbar ~ q260 + q262 + higher_ed + marital_status + q274 + q288 + religious +
            victim + qh + q50,
          data = wvs)

mod %>%
  lmtest::coeftest(vcov. = sandwich::vcovHC)


                   