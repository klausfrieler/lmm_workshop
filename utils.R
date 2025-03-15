
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

lm_by_hand <- function(data, nrows = 3){
  if(is.null(nrows)){
    nrows <- nrow(data)
  }
  data <- data %>% slice(1:nrows)
  mm <- data %>% select(x, z) %>% as.matrix()
  mm <- cbind(rep(1, nrows), mm)
  y <- data %>% pull(y)
  betas_man <- inv(t(mm) %*% mm) %*% t(mm) %*% y
  betas_lm <- lm(y ~ x + z, data = data) %>% coef()
  mean((betas_man - betas_lm)^2)
}


euclid_dist <- function(x, y){
  sqrt(scalar_prod(x - y))
}
norm_vec <- function(x){
  sqrt(sum(x * x))
}

unit_vec <- function(x){
  x/norm_vec(x)
}

scalar_prod <- function(x, y){
  sum(x * y)
}


center <- function(x){
  x - mean(x)
}

sd_by_hand <- function(x){
  n <- length(x)
  norm_vec(center(x))/sqrt(n-1)
}

scale_by_hand <- function(x){
  n <- length(x)
  sqrt(n-1) * unit_vec(center(x))
}


cor_by_hand <- function(x, y){
  scalar_prod(unit_vec(center(x)), unit_vec(center(y)))
}