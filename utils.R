
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


describe_data_md <- function(data_set){
  names <- names(data_set)
  types <- sapply(data_set, class)
  
  values <- sapply(data_set, function(x){
    if(class(x) == "factor"){
      if(length(levels(x)) <= 5){
        sprintf("[ %s ]", paste(levels(x), collapse = " &vert; "))
      }
      else{
        sprintf("[ %s ]", paste(c(levels(x)[1:4], "..."), collapse = " &vert; "))
      }
    }    
    else if(class(x) == "integer"){
      sprintf("%d&ndash;%d", min(x), max(x))
    } 
    else if(class(x) == "logical"){
      "[TRUE &vert; FALSE]"
    } 
    else{ "" 
      }
  })
  meta <- tibble(Variable = sprintf("**%s**", names), Type = types, Values = values)
  meta
}

scale_by_hand <- function(x){
  n <- length(x)
  sqrt(n-1) * unit_vec(center(x))
}


cor_by_hand <- function(x, y){
  scalar_prod(unit_vec(center(x)), unit_vec(center(y)))
}

multi_col_demo <- function(N = 100, seed = NULL, error = 2, r_xy = .5){
  set.seed(seed)
  if(r_xy == 0){
    sigma_y <- 1
  }
  else{
    sigma_y <- sqrt((1 - r_xy^2)/r_xy^2)
  }
  x <- rnorm(N, 0, 1)
  y <- x + rnorm(N, 0, sigma_y)
  u <- rnorm(N, 0, 1)
  z <- 1  + x + y + u + rnorm(N, 0, error)
  data <- tibble(z = z, x = x, y = y) 
  mod <- data %>% lm(z ~ x + y + u, data = .)
  #summary(mod)
  #mod %>% car::vif()
  #cor(x, y)
  broom::tidy(mod) %>% mutate(sig = p.value >= .05, 
                              eps = error, 
                              r_xy = r_xy, N = N)
    
}

plot_multi_col_demo <- function(N = 100, r_xy = .9, error = 2, iter = 100, alpha = .5, as_error = F){
  simu_data <- map_dfr(1:iter, function(x){
    multi_col_demo(N = N, r_xy = r_xy, error = error) %>% 
      mutate(iter = x)
  }) %>% 
    mutate(sig = factor(sig, labels = c("p < .05", "n.s.")[1:length(unique(sig))]))
  y0 <- 1
  if(as_error){
    simu_data <- simu_data %>% mutate(estimate = (estimate - 1)/estimate) %>% filter(abs(estimate) < 2)
    y0 <- 0
    
  }
  q <- simu_data %>% ggplot(aes(x = term, y = estimate, color = sig))
  q <- q + geom_boxplot(aes(group = term), width = .25, outliers = F) 
  q <- q + geom_jitter(width = .1, alpha = alpha)  
  q <- q + geom_hline(yintercept = y0) 
  q <- q + theme_bw() 
  q <- q + labs(color = "")
  q <- q + scale_color_brewer(palette = "Set1")  
  if(as_error){
    q <- q + scale_y_continuous(labels = scales::percent)
  }
  q <- q + theme(text = element_text(size = 11), axis.text = element_text(size = 11))
  q 
}
