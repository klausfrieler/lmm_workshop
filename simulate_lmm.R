
no_ranef <- list(
  intercept = list(raters = .0, items = .0),
  beta = list(raters = .0, items = .0),
  corr = list(raters = .0, items = .0)
)

weak_ranef_no_slope <- list(intercept = list(raters = .2, items = .1),
                            beta = list(raters = 0, items = 0),
                            corr = list(raters = 0, items = 0))

strong_ranef_no_slope <- list(intercept = list(raters = .5, items = .5),
                              beta = list(raters = 0, items = 0),
                              corr = list(raters = 0, items = 0))

weak_ranef_with_slope <- list(intercept = list(raters = .2, items = .1),
                              beta = list(raters = .2, items = .1),
                              corr = list(raters = .5, items = .5))

strong_ranef_with_slope <- list(intercept = list(raters = .5, items = .5),
                                beta = list(raters = .5, items = .5),
                                corr = list(raters = .5, items = .5))


simulate_lmm <- function(n_raters = 10, 
                         n_items = 10, 
                         n_conditions = 2,
                         condition_labels = c("AO", "AV"),
                         fixef_beta = c(1),
                         ranef = strong_ranef_with_slope,
                         y0 = 3,
                         error = .5, 
                         contrasts = contr.treatment){
  n_total <- n_raters * n_items
  if(n_conditions != 2){
    stop("More than 2 conditions not implemented yet")
  }
  if(length(fixef_beta) < (n_conditions - 1)){
    stop("Too few condition betas")
  }
  
  fixef_beta <- fixef_beta[1:(n_conditions - 1)]
  
  
  cor_raters <- matrix(c(1, ranef$corr$raters, ranef$corr$raters, 1), nrow = 2, ncol = 2) 
  cor_items <- matrix(c(1, ranef$corr$items, ranef$corr$items, 1), nrow = 2, ncol = 2) 
  sigma_raters <- cor2cov(cor_raters, c(ranef$intercept$raters, ranef$beta$raters))
  sigma_items <- cor2cov(cor_raters, c(ranef$intercept$items, ranef$beta$items))
  
  
  ranef_raters <- mvtnorm::rmvnorm(n_raters, mean = rep(0, 2), sigma = sigma_raters) %>% as.data.frame() %>% set_names(c("y0", "beta"))
  ranef_items <- mvtnorm::rmvnorm(n_items, mean = rep(0, 2), sigma = sigma_items) %>% as.data.frame() %>% set_names(c("y0", "beta"))
  err <- rnorm(n_raters * n_items, 0, error)
  #browser()
  design <- expand_grid(rater = 1:n_raters, item = 1:n_items, condition = rep(1:n_conditions) - 1)
  model_matrix <- design %>% 
    mutate(betas = fixef_beta  + ranef_raters$beta[rater] + ranef_items$beta[item]) %>% 
    mutate(y1 = y0 + ranef_raters$y0[rater] +  ranef_items$y0[item]) %>% 
    mutate(liking = y1 + betas * condition + err) %>% 
    mutate(rater = sprintf("r%02d", rater), 
           item =  sprintf("i%02d", item),
           condition = factor(condition_labels[condition + 1])
    )
  contrasts(model_matrix$condition) <- contrasts(n_conditions)
  model_matrix <- model_matrix %>% mutate(condition = as.character(condition)) 
  
  messagef("True SD raters: (%.2f, %.2f), True SD items (%.2f, %.2f), True err = %.2f, mean(betas) = %.2f", 
           sd(ranef_raters$y0), sd(ranef_raters$beta), sd(ranef_items$y0), sd(ranef_items$beta), 
           sd(err), mean(model_matrix$betas))
  return(model_matrix)
}