install_packages("tidyverse")

library(tidyverse) # the 'new' normal
library(haven) # part of tidyverse, imports Stata dta as tibble

data <- read_dta("../2_data/2_haiti2012wDurablesECVMASI.dta")
data <- filter(data, complete.cases(data)) # we may want to comment this out to explore is.na() first


ggplot(data.frame(x = c(0, 50)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = 8))


ggplot(data.frame(x = c(0, 50)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = 25))
