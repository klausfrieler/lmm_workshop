simulate_lmm <- function(n_raters = 10, n_items = 10, 
                         fixef_beta = c(raters = 2, items = 1),
                         ranef_sd = c(raters = 1, items = 1, err = 1),
                         ranef_mean = c(raters = 1, items = 1)){
  y0_raters <- rnorm(n_raters, 0, ranef_sd["raters"])
  y0_items <- rnorm(n_items, 0, ranef_sd["items"])
  res <- rnorm(n_raters * n_items, 0, ranef_sd["err"])
  browser()
  design <- expand_grid(rater = 1:n_raters, item = 1:n_items)
  model_matrix <- design %>% mutate(y = y0_raters[rater] + y0_items[item] + res)
  y <- xt + zt
}