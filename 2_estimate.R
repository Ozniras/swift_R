setwd('~/Dropbox/dataProjects/swift/haiti2/r')

library(tidyverse)
library(haven)
library(leaps)

data <- read_dta("./2_haiti2012wDurablesECVMASI_wFolds.dta")
data <- filter(data, complete.cases(data))

data <- select(data, -one_of("hhd_labEmpl15pRt", "hhd_labLF15pRt", "hhd_eduLevlNoneRt", "hhd_rooms1",
               "hhd_size1", "hoh_labUnem15p", "hoh_eduLvlComPrim", "hoh_workSmllFrml", "dwl_energyOther",
               "dwl_wallOther", "dwl_roofOther", "dwl_floorOther", "dwl_lightHomeNone")) 

folds <- 10
maxModel <- 100

out <- matrix(NA, nrow = folds, ncol = maxModel)

# folds <- as.integer(runif(nrow(data), 0, 10)) + 1

for (i in 1:folds) {
  train <- filter(data, fold != i)
  test <- filter(data, fold == i)
  y.train <- transmute(train, pcexpLog = log(pcexp))
  y.test <-  transmute(test, pcexpLog = log(pcexp))
  x.train <- train %>% 
    mutate(region1 = region == 1, 
           region2 = region == 2, 
           region3 = region == 3, 
           region4 = region == 4, 
           region5 = region == 5) %>% 
    select(starts_with('dwl_'), 
           starts_with('ast_'), 
           starts_with('hoh_'), 
           starts_with('hhd_'), 
           starts_with('hhd_'), 
           starts_with('region')) %>% 
    select(-region, -region1)
  x.test <- test %>% 
    mutate(region1 = region == 1, 
           region2 = region == 2, 
           region3 = region == 3, 
           region4 = region == 4, 
           region5 = region == 5) %>% 
    select(starts_with('dwl_'), 
           starts_with('ast_'), 
           starts_with('hoh_'), 
           starts_with('hhd_'), 
           starts_with('hhd_'), 
           starts_with('region')) %>% 
    select(-region, -region1)
  result <- regsubsets(as.matrix(x.train), as.matrix(y.train), weights = as.matrix(train$hhd_size),
                       method = 'forward', nvmax = maxModel, really.big = FALSE)

  lenModel <- result$np - 1
  for (j in 1:lenModel) {
    regressors <- names(coef(result, j))[2:(j + 1)]
    model.frmla <- as.formula(paste('pcexpLog ~  ', paste(regressors, collapse = ' + '), sep = ''))
    model.train <- lm(model.frmla, data = train, weights = hhd_size)
    model.test <- predict(model.train, newdata = test)
    model.rmse <- sqrt(mean((test$pcexpLog - model.test) ^ 2))
    out[i, j] <- model.rmse
    }
}

out <- as_tibble(out[1:lenModel])
out.graph.data <- 
  tibble(modSize = 1:(result$np - 1), 
         rmse = colMeans(out), 
         sd = apply(out, 2, sd), 
         min = apply(out, 2, min), 
         max = apply(out, 2, max)) %>% 
  mutate(uci = rmse + 1.96 * sd, lci = rmse - 1.96 * sd)

ggplot(data = out.graph.data, aes(x = modSize, y = rmse)) +
  geom_line(se = FALSE) +
  geom_smooth(se = FALSE) +
  geom_linerange(aes(ymin = lci, ymax = uci), alpha = 0.1)

opt.size <- as.integer(filter(out.graph.data, rmse >= min(uci)) %>% 
  summarize(max(modSize)))

opt.regressors <- names(coef(result, opt.size))[2:(opt.size + 1)]
opt.frmla <- as.formula(paste('pcexpLog ~  ', paste(opt.regressors, collapse = ' + '), sep = ''))
lm(opt.frmla, data = data)


opt.regressors.all <- names(coef(result, 36))[2:(36 + 1)]
opt.frmla.all <- as.formula(paste('pcexpLog ~  ', paste(opt.regressors.all, collapse = ' + '), sep = ''))
lm(opt.frmla.all, data = data)
