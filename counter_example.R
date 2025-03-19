simulate_counter_example <- function(n_rater = 100, n_samples = 10, sigma_beta = 2, rater_eps = 2){
  x <- seq(0, 1, .01)
  n_samples <- min(max(n_samples, 10), length(x))
  ret <- 
    map_dfr(1:n_rater, function(p_id) {
    offset <- (p_id %% 10) - 5
    
    x_tmp <- sample(x, n_samples)
    beta <- rnorm(1, 1, sigma_beta)
    y <- x_tmp * beta  - offset + rnorm(1, 0, rater_eps)
    offset_temp <- rnorm(1, offset/10, .2)
    tibble(p_id = factor(p_id), x = x_tmp + offset_temp, y = y)})
  ret
}

counter_example_demo <- function(sigma_beta = 5, rater_eps = 5){
  test_data <- simulate_counter_example(sigma_beta = sigma_beta, rater_eps = rater_eps) 
  lmm <- lmer( y ~ x + (1|p_id), data =  test_data) %>% broom::tidy() %>% mutate(type = "lmm")
  lm <- lm( y ~ x , data =  test_data) %>% broom::tidy() %>% mutate(type = "lm")
  q <- test_data %>% ggplot(aes(x = x, y = y))
  q <- q + geom_point() 
  q <- q + geom_smooth(aes(group = factor(p_id)), method = "lm", color = "indianred4",alpha = .02) 
  q <- q + geom_smooth(method = "lm", color = "lightblue", linewidth = 2)  
  q <- q + theme_bw() 
  q <- q + theme(legend.position = "none") 
  print(q)
  bind_rows(lmm, lm)
}