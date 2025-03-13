library(tidyverse)

plot_sim <- function(n_raters = 25, n_items = 25, ranef = strong_ranef_with_slope, fixef = .5, facet_type = c("none", "item", "rater")){
  facet_type <- match.arg(facet_type)
  
  tmp <- simulate_lmm(n_raters = n_raters, n_items = n_items, fixef_beta = fixef, ranef = ranef)
  q <- tmp %>% ggplot(aes(x = condition, y = liking)) 
  q <- q + geom_path(aes(group = rater), alpha = .05)  
  q <- q + theme_bw()  
  q <- q +  geom_boxplot(width = .2, alpha = 2) 
  q <- q + geom_violin(alpha = .2)
  q <- q + stat_summary(fun.data = "mean_cl_boot", color = "indianred") 
  
  if(facet_type == "item"){
    q <- q + facet_wrap(~item)
  }
  if(facet_type == "rater"){
    q <- q + facet_wrap(~rater)
  }
  q
}

beta_sig_plot <- function(mod_cmp, true_beta = 0, title = NULL){
  q <- mod_cmp %>%
    mutate(sig = get_sig(p.value, 1, labels = "p")) %>%
    ggplot(aes(x = type, y = estimate, color = sig))

  q <- q + geom_pointrange(
    aes(ymin = ci95_low, ymax = ci95_hi),
    position = position_jitter(width = 0.1),
    linetype = 'dotted'
  )
  q <- q + scale_color_brewer(palette = "Set1")
  q <- q + labs(x = "Method", y = "beta(Condition)", color = "Sign.")
  q <- q + geom_hline(yintercept = true_beta)
  q <- q + theme_bw()
  if(!is.null(title)){
    q <- q + ggtitle(title)
  }
  q
}

mer_re_plot <- function(data = mer, y_var = "AROUSAL", x_var = "MIR.rms_mean"){
  q <- data %>% ggplot(aes(x = !!sym(x_var), y = !!sym(y_var))) 
  q <- q + geom_point() 
  q <- q + geom_smooth(method = "lm", aes(color = factor(p_id))) 
  q <- q + facet_wrap(~p_id) 
  q <- q  + theme_bw() 
  q <- q + theme(legend.position = "none")
  q
}