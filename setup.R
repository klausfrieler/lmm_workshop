library(tidyverse)
library(sjPlot)
library(lmerTest)
source("utils.R")
source("plot_utils.R")
source("model_comp.R")
source("simulate_lmm.R")


setup_workspace <- function(){
  covox <- readRDS("data/covox.rds")
  mer <- readRDS("data/MER.rds")
  names(mer)[11:21] <- names(mer)[11:21] %>% sprintf("MIR.%s", .)
  mer_mir_features <- names(mer)[11:21]
  mer <- mer %>% filter(!is.na(MIR.tempo))
  mer <- mer %>% mutate(across(all_of(mer_mir_features), function(x) scale(x) %>% as.numeric()))
  top_emo <- mer %>% select(s_id, all_of(mer_emo_vars)) %>% pivot_longer(-s_id) %>% group_by(s_id, name) %>% summarise(m = mean(value)) %>% summarise(emo = name[which.max(m)], .groups = "drop") %>% ungroup()
  mer <- mer %>% left_join(top_emo, by = "s_id")
  assign("covox", covox, globalenv())
  assign("mer", mer, globalenv())
  assign("mer_mir_features", mer_mir_features, globalenv())
  assign("mer_emo_vars", c("angry", "sad", "happy", "peacef", "tender", "fearf"), globalenv())
  
}