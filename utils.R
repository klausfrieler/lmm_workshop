
messagef <- function(...) message(sprintf(...))

cor2cov <- function(V, sd) {
  V * tcrossprod(sd)
}

sample_by_var <- function(data, var = "rater", frac = 1){
  elts <- unique(data[[var]])
  n_elts <- round(length(elts) * frac)
  if(n_elts < 2){
    stop("Too few elements")
  }
  s_elts <- sample(elts, n_elts)
  data %>% filter(!!sym(var) %in% s_elts)
}

sample_rater_item <- function(data, rater_frac = 1, item_frac = 1){
  sample_by_var(data, "rater", rater_frac) %>% 
    sample_by_var("item", item_frac)
}

get_sig <- function(p_values, levels = 3, labels = "stars"){
  if(levels < 1) levels <- 1
  if(levels > 3) levels <- 3
  sig_labels <- c("n.s.", "p<.05", "p<.01", "p<-.001")
  if(labels == "starts"){
    sig_labels <- c("n.s.", "*", "**", "***")
  }
  stars <- rep(sig_labels[1], length(p_values))
  stars[p_values < .05] <- sig_labels[2]
  if(levels > 1) stars[p_values < .01] <- sig_labels[3]
  if(levels > 2) stars[p_values < .001] <- sig_labels[4]
  stars
}
