source("simulate_lmm.R")
source("simulate_lmm_slope.R")
library(sjPlot)

get_sig <- function(p_values, levels = 3){
  if(levels < 1) levels <- 1
  if(levels > 3) levels <- 3
  
  stars <- rep("n.s.", length(p_values))
  stars[p_values < .05] <- "*"
  if(levels > 1) stars[p_values < .01] <- "**"
  if(levels > 2) stars[p_values < .001] <- "***"
  stars
}

model_cmp <- function(n_raters = 10, n_items = 10, fixef = 1, with_slope = T, n_runs = 1, ranef = strong_ranef_with_slope){
  map_dfr(1:n_runs, function(batch){
    if(with_slope){
      sim <- simulate_lmm_with_slope(n_raters = n_raters, n_items = n_items, fixef_beta = fixef, ranef = ranef)
    }
    else{
      sim <- simulate_lmm(n_raters = n_raters, n_items = n_items, fixef_beta = fixef)
    }
    m_full <- sim %>% 
      lmerTest::lmer(liking ~ condition + (1 + condition|rater) + (1+ condition|item), data = .)
    m_partial <- sim %>% 
      lmerTest::lmer(liking ~ condition + (1 |rater) + (1|item), data = .)
    m_simple <- sim %>% 
      lm(liking ~ condition, data = .)
    results <- bind_rows(
      broom.mixed::tidy(m_full) %>% filter(effect == "fixed", term != "(Intercept)") %>% select(-c(effect, group)) %>% mutate(type = "full"), 
      broom.mixed::tidy(m_partial) %>% filter(effect == "fixed", term != "(Intercept)") %>% select(-c(effect, group)) %>% mutate(type = "partial"),
      broom::tidy(m_simple) %>% filter(term != "(Intercept)")  %>% mutate(type = "simple"),
      rater_wise_lms(sim) 
    ) %>% mutate(batch = batch)
    if(batch == 1){
      sjPlot::plot_models(m_full, m_partial, m_simple,  m.labels = c("full", "partial", "simple"))
    }    
    results
  })

}

rater_wise_lms <-  function(data){
  raters <- unique(data$rater)
  ret <- map_dfr(raters, function(r){
    tmp <- data %>% filter(rater == r)
    mod <- lm(liking ~ condition, data = tmp) %>% broom::tidy() %>% mutate(rater = r)
  }) 
  base <- ret %>% filter(term != "(Intercept)") %>% summarise(term = term[1], m = mean(estimate), std.error = sd(estimate)/sqrt(nrow(.)))
  #browser()
  t_stat <- ret %>% filter(term != "(Intercept)") %>% rstatix::t_test(estimate ~ 1, mu = 0)  %>% select(statistic, df, p.value = p)
  base %>% rename(estimate = m) %>% bind_cols(t_stat) %>% mutate(type = "singles")
}