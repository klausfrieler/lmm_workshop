source("simulate_lmm.R")
library(sjPlot)

three_methods <- function(data){
  m_full <- data %>% 
    lmerTest::lmer(liking ~ condition + (1 + condition|rater) + (1+ condition|item), data = .)
  
  m_partial <- data %>% 
    lmerTest::lmer(liking ~ condition + (1 |rater) + (1|item), data = .)
  
  m_simple <- data %>% 
    lm(liking ~ condition, data = .)
  #m_re_by_rater <- data %>% re_wise_lms()
  list(full = m_full, partial = m_partial, simple = m_simple)
}

method_cmp <- function(data){
  m_full <- data %>% 
    lmerTest::lmer(liking ~ condition + (1 + condition|rater) + (1+ condition|item), data = .)
  
  m_partial <- data %>% 
    lmerTest::lmer(liking ~ condition + (1 |rater) + (1|item), data = .)
  
  m_simple <- data %>% 
    lm(liking ~ condition, data = .)
  
  m_sandwich <- m_simple %>% lmtest::coeftest(vcov = sandwich::vcovCL, cluster = ~rater + item, df = Inf)

  
  results <- bind_rows(
    broom.mixed::tidy(m_full) %>% filter(effect == "fixed", term != "(Intercept)") %>% select(-c(effect, group)) %>% mutate(type = "Full LMM"), 
    broom.mixed::tidy(m_partial) %>% filter(effect == "fixed", term != "(Intercept)") %>% select(-c(effect, group)) %>% mutate(type = "Partial LMM"),
    broom::tidy(m_simple) %>% filter(term != "(Intercept)")  %>% mutate(type = "Simple LM"),
    broom.mixed::tidy(m_sandwich) %>% filter(term != "(Intercept)")  %>% mutate(type = "Sandwich vcovCL"),
    re_wise_lms(data, "rater") %>% mutate(type = "Single LM (rater)"), 
    re_wise_lms(data, "item") %>% mutate(type = "Single LM (items)")
  ) 
  results %>% mutate(ci95_low = estimate  - 1.96 * std.error, ci95_hi = estimate  + 1.96 * std.error)
}

model_cmp <- function(n_raters = 10, n_items = 10, fixef = 1, with_slope = T, n_runs = 1, ranef = strong_ranef_with_slope){

  map_dfr(1:n_runs, function(batch){
    
    sim <- simulate_lmm(n_raters = n_raters, n_items = n_items, fixef_beta = fixef, ranef = ranef)
    results <- method_cmp(sim) %>% mutate(batch = batch)

    results
  })

}

re_wise_lms <-  function(data, axis = "rater", only_models = F){
  elts <- unique(data[[axis]])
  ret <- map_dfr(elts, function(r){
    tmp <- data %>% filter(!!sym(axis) == r)
    if(length(unique(tmp$condition)) == 1){
      return(NULL)
    }
    mod <- lm(liking ~ condition, data = tmp) %>% broom::tidy() %>% mutate(!!sym(axis) := r)
  }) 
  if(nrow(ret) == 0){
    return(NULL)
  }
  if(only_models){
    return(ret)
  }
  base <- ret %>% filter(term != "(Intercept)") %>% summarise(term = term[1], m = mean(estimate), std.error = sd(estimate)/sqrt(nrow(.)))
  #browser()
  t_stat <- ret %>% filter(term != "(Intercept)") %>% rstatix::t_test(estimate ~ 1, mu = 0)  %>% select(statistic, df, p.value = p)
  base %>% rename(estimate = m) %>% bind_cols(t_stat) %>% mutate(type = sprintf("singles_%s", axis))
}

center_then_average_lm <- function(data, z_axis = "rater"){
  avg_axis = "rater"
  if(z_axis == "rater"){
    avg_axis = "item"
  }
  lm_form <- as.formula(sprintf("liking ~ condition + (1|%s)", avg_axis))
  tmp <- data %>% group_by(!!sym(z_axis)) %>% mutate(liking = scale(liking, scale = F) %>% as.numeric()) %>% ungroup()
  tmp <- tmp %>% group_by(!!sym(avg_axis), condition) %>% summarise(liking = mean(liking, na.rm = T), .groups = "drop")
  if(tmp %>% count(!!sym(avg_axis), condition) %>% count(nn = n) %>% nrow() == 1){
    return(NULL)
  }
  if(tmp %>% count(!!sym(z_axis), condition) %>% count(nn = n) %>% nrow() == 1){
    return(NULL)
  }
  mod <- lmerTest::lmer(lm_form, data = tmp) %>% broom.mixed::tidy()
  
  mod %>% filter(effect == "fixed", term != "(Intercept)") %>% mutate(type = sprintf("z_%s_a_%s", z_axis, avg_axis)) %>% select(-c(effect, group))
}

power_demo_plot <- function(model_coeficients, true_value = 0.0){
  error_stats <- model_coeficients %>% 
    group_by(type) %>% 
    summarise(type_I_error = mean(p.value < .05), 
              type_II_error = mean(p.value > .05), 
              .groups = "drop") %>% 
    mutate(true_value = true_value)
  if(true_value > 0){
    error_stats <- error_stats %>% rename(error = type_II_error)
  }
  else{
    error_stats <- error_stats %>% rename(error = type_I_error)
  }
  model_coeficients <- model_coeficients %>% left_join(error_stats, by = "type") %>% 
    mutate(error_str = sprintf(" %.1f%%", 100 * round(error, 3)))
  q <- model_coeficients %>% ggplot(aes(x = type, y = estimate, color = get_sig(p.value, 1))) 
  q <- q + geom_pointrange(aes(ymin = ci95_low, ymax = ci95_hi), 
                           position = position_jitter(width = 0.1), 
                           linetype = 'dotted') 
  q <- q + geom_label(aes(x = type, y = true_value  + 1, label = error_str, group = type))
  q <- q + theme_bw() 
  q <- q + geom_hline(yintercept = true_value)
  q <- q + scale_color_brewer(palette = "Set1") 
  q <- q +  labs(color = "", x = "Method")
  q 
}