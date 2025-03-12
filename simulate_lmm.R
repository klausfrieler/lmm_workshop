library(tidyverse)
library(truncnorm)
messagef <- function(...) message(sprintf(...))

no_ranef <- list(sd = list(raters = 0, items = 0))

simulate_lmm <- function(n_raters = 10, 
                         n_items = 10, 
                         n_conditions = 2,
                         condition_labels = c("AO", "AV"),
                         fixef_beta = c(1),
                         ranef = list(sd = list(raters = .5, items = .5)),
                         y0 = 3,
                         error = .5){
  n_total <- n_raters * n_items
  browser()
  # if(n_items %% n_conditions ){
  #   stop("Items should be multiple of conditions")
  # }
  if(n_conditions != 2){
    stop("More than 2 conditions not implemented yet")
  }
  if(length(fixef_beta) < (n_conditions - 1)){
    browser()
    stop("Too few condition betas")
    
  }
  fixef_beta <- fixef_beta[1:(n_conditions - 1)]
  if(is.null(condition_labels)){
    conditions_labels <- as.character(1:n_conditions)
  }
  browser()
  
  y0_raters <- rnorm(n_raters, 0, ranef$sd$raters)
  y0_items <- rnorm(n_items, 0, ranef$sd$items)
  err <- rnorm(n_raters * n_items, 0, error)
  design <- expand_grid(rater = 1:n_raters, item = 1:n_items, condition = rep(1:n_conditions) - 1)
  model_matrix <- design %>% 
    mutate(betas = fixef_beta,
           y1 = y0 + y0_raters[rater] + y0_items[item]) %>% 
    mutate(liking = y1 + betas * condition + err) %>% 
    mutate(rater = sprintf("r%02d", rater), 
           item =  sprintf("i%02d", item),
           condition = condition_labels[condition + 1]
           )
  messagef("True SD raters: %.2f, True SD items %.2f, True err = %.2f, mean(betas) = %.2f", sd(y0_raters), sd(y0_items), sd(err), mean(model_matrix$betas))
  return(model_matrix)
}