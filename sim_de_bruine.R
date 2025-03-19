library(faux)
library(lme4)
library(lmerTest)

sim_lmer <- function( sub_n = 200,
                      sub_sd = 100,
                      sub_version_sd = 20, 
                      sub_i_version_cor = -0.2,
                      stim_n = 50,
                      stim_sd = 50,
                      stim_version_sd = 10,
                      stim_cond_sd = 30,
                      stim_cond_version_sd = 15,
                      stim_i_cor = -0.4,
                      stim_s_cor = +0.2,
                      grand_i = 400,
                      hard_congr = -25,
                      hard_incon = +25,
                      easy_congr = -50,
                      easy_incon = +50,
                      error_sd = 25) {
  sub <- rnorm_multi(
    n = sub_n, 
    vars = 2, 
    r = sub_i_version_cor,
    mu = 0, # means of random intercepts and slopes are always 0
    sd = c(sub_sd, sub_version_sd),
    varnames = c("sub_i", "sub_version_slope")
  ) %>%
    mutate(
      sub_id = 1:sub_n,
      sub_cond = rep(c("easy","hard"), each = sub_n/2) # between-subjects factor
    )
  
  stim_cors <- c(stim_i_cor, stim_i_cor, stim_i_cor,
                 stim_s_cor, stim_s_cor,
                 stim_s_cor)
  stim <- rnorm_multi(
    n = stim_n, 
    vars = 4, 
    r = stim_cors, 
    mu = 0, # means of random intercepts and slopes are always 0
    sd = c(stim_sd, stim_version_sd, stim_cond_sd, stim_cond_version_sd),
    varnames = c("stim_i", "stim_version_slope", "stim_cond_slope", "stim_cond_version_slope")
  ) %>%
    mutate(
      stim_id = 1:stim_n
    )
  
  # mean difference between easy and hard conditions
  sub_cond_eff     <- (easy_congr + easy_incon)/2 -
    (hard_congr + hard_incon)/2
  # mean difference between incongruent and congruent versions
  stim_version_eff <- (hard_incon + easy_incon)/2 - 
    (hard_congr + easy_congr)/2  
  # interaction between version and condition
  cond_version_ixn <- (easy_incon - easy_congr) -
    (hard_incon - hard_congr) 
  
  trials <- crossing(
    sub_id = sub$sub_id, # get subject IDs from the sub data table
    stim_id = stim$stim_id, # get stimulus IDs from the stim data table
    stim_version = c("congruent", "incongruent") # all subjects see both congruent and incongruent versions of all stimuli
  ) %>%
    left_join(sub, by = "sub_id") %>% # includes the intercept, slope, and conditin for each subject
    left_join(stim, by = "stim_id")   # includes the intercept and slopes for each stimulus
  
  dat <- trials %>%
    mutate(
      # effect-code subject condition and stimulus version
      sub_cond.e = recode(sub_cond, "hard" = -0.5, "easy" = +0.5),
      stim_version.e = recode(stim_version, "congruent" = -0.5, "incongruent" = +0.5),
      # calculate trial-specific effects by adding overall effects and slopes
      cond_eff = sub_cond_eff + stim_cond_slope,
      version_eff = stim_version_eff + stim_version_slope + sub_version_slope,
      cond_version_eff = cond_version_ixn + stim_cond_version_slope,
      # calculate error term (normally distributed residual with SD set above)
      err = rnorm(nrow(.), 0, error_sd),
      # calculate DV from intercepts, effects, and error
      dv = grand_i + sub_i + stim_i + err +
        (sub_cond.e * cond_eff) + 
        (stim_version.e * version_eff) + 
        (sub_cond.e * stim_version.e * cond_version_eff)
    )
  return(dat)
  mod <- lmer(dv ~ sub_cond.e * stim_version.e +
                (1 + stim_version.e | sub_id) + 
                (1 + stim_version.e*sub_cond.e | stim_id),
              data = dat)
  
  mod.sum <- summary(mod)
  
  return(mod.sum)
}