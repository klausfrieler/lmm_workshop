#Code from Martinez ez al., 2020 - Quantifying idiosyncratic and shared contributions to judgment  (https://osf.io/q28g6/)

#Function for grabbing the variance components for random effect clusters
#Code modified from: https://github.com/jslefche/rsquared.glmm/blob/master/rsquaredglmm.R#L129
#https://github.com/jslefche/piecewiseSEM/blob/master/R/sem.model.fits.R

varmod <- function(x, ...) {
  # return value
  var_ <- var.lme4(x)
  # check if we have multiple parameters
  if (nargs() > 1) {
    # get input list
    params_ <- list(...)
    var_ <- list(var_)
    for (p_ in params_) {
      var_[[length(icc_) + 1]] <- var.lme4(p_)
    }
    names(var_) <- NULL
  }
  return(var_)
}


var.lme4 <- function(fit) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  # ------------------------
  # check object class
  # ------------------------
  if (any(class(fit) == "glmerMod") || any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
    # ------------------------
    # random effects variances
    # ------------------------
    #reva <- summary(fit)$varcor
    reva = VarCorr(fit)
    # retrieve all variances
    vars <- lapply(reva, diag)
    # residual variances
    if (any(class(fit) == "glmerMod")) {
      # for logistic models, we use pi / 3
      resid_var <- (pi^2) / 3
    } else {
      # for linear models, we have a clear
      # residual variance
      resid_var <- attr(reva, "sc")^2
    }
    vars = c(vars, setNames(resid_var, 'residual'))
    vars = sapply(vars, sum)#just unlists them, doesn't sum anything.
    
    
    return(vars)
  } else {
    warning("Function 'var' currently only supports 'merMod' objects (package 'lme4').", call. = F)
  }
}


#Estimates the variance explained by each grouping factor (summing across all the random intercepts and slopes)
#https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q2/022033.html
#https://github.com/jslefche/piecewiseSEM/blob/master/R/sem.model.fits.R
#http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12225/epdf
#https://github.com/jslefche/rsquared.glmm/blob/master/rsquaredglmm.R#L129

vpc = function(fit, ...){
  reva = VarCorr(fit)
  #calculate the variance
  vars =  lapply(reva, diag)
  if (any(class(fit) == "glmerMod")) {
    # for logistic models, we use pi / 3
    resid_var <- (pi^2) / 3
  } else {
    # for linear models, we have a clear
    # residual variance
    resid_var <- attr(reva, "sc")^2
  }
  #Get fixed effect variance multiply coefficient by design matrix
  varFix = var(as.vector(lme4::fixef(fit) %*% t(fit@pp$X)))
  total_var <- sum(sapply(vars, sum), resid_var, varFix)
  #Calulcate vpc
  vpc1 <- sapply(vars, function(x) x[1]) / total_var #only random intercepts
  names(vpc1) <- names(reva)
  return(vpc1)
}


#Estimates the marginal and conditional r squared
#total variance = Var(fix) + Var(random: int and slopes) + Var(residual)... + Var(dispersion for non-normal models)
#marginal = Var(fix)/Var(total)
#conditional = Var(fix) + Var(random)/total
#https://github.com/jslefche/rsquared.glmm/blob/master/rsquaredglmm.R#L129
#https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/
#https://github.com/jslefche/piecewiseSEM/blob/master/R/sem.model.fits.R

r2 = function(fit, ...){
  reva = VarCorr(fit)
  #grab the random variances, intercepts and slopes
  vars =  lapply(reva, diag)
  if (any(class(fit) == "glmerMod")) {
    # for logistic models, we use pi / 3
    resid_var <- (pi^2) / 3
  } else {
    # for linear models, we have a clear
    # residual variance
    resid_var <- attr(reva, "sc")^2
  }
  # dispersion variance
  disp_var <-unlist(VarCorr(fit)[sapply(unique(unlist(strsplit(names(lme4::ranef(fit)),":|/"))), function(l) length(unique(fit@frame[,l])) == nrow(fit@frame))])
  if(is.null(disp_var)) disp_var = 0 else disp_var = disp_var
  
  #Get fixed effect variance multiply coefficient by design matrix
  varFix = var(as.vector(lme4::fixef(fit) %*% t(fit@pp$X)))
  #Add all variances to get total
  total_var <- sum(sapply(vars, sum), resid_var, varFix, disp_var)
  #Calulcate Marginal and Conditional
  Rm <- varFix / total_var #only random intercepts
  Rc <- sum(varFix,sapply(vars, sum)) / total_var #Takes random slopes into account
  rdat = c(Rm, Rc)
  names(rdat) = c('Marginal', 'Conditional')
  return(rdat)
}

#############################################################################################
#Honekopp method for calculating variance proportions between
#shared and private tastes
#Code modified from: https://github.com/jslefche/rsquared.glmm/blob/master/rsquaredglmm.R#L129
#https://github.com/jslefche/piecewiseSEM/blob/master/R/sem.model.fits.R
bi <- function(x, sub, stim, inter, ...) {
  # return value
  hp_ <- bi.lme4(x, sub, stim, inter)
  return(hp_)
}


bi.lme4 <- function(fit, sub, stim, inter) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  # ------------------------
  # check object class
  # ------------------------
  if (any(class(fit) == "glmerMod") || any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
    # ------------------------
    # random effects variances
    # ------------------------
    reva = VarCorr(fit)
    # retrieve only intercepts
    vars <- lapply(reva, function(x) x[[1]])
    #separate shared/private components
    private1 = c(vars[inter][[1]])
    private2 = c(vars[inter][[1]], vars[sub][[1]])
    shared =  vars[stim][[1]]
    
    #B1
    b1 = setNames(sum(private1)/sum(private1, shared), 'b1')
    
    #B2
    b2 = setNames(sum(private2)/sum(private2, shared), 'b2')
    
    ri.bi = c(b1, b2)
    return(ri.bi)
  } else {
    warning("Function 'bi' currently only supports 'merMod' objects (package 'lme4').", call. = F)
  }
}

