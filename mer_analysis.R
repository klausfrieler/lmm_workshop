#
get_top_cors <- function(data = mer, target_var = "AROUSAL", predictors = mer_mir_features, n = 5){
  cor_list <- mer %>% 
    select(all_of(c(target_var, predictors))) %>% 
    corrr::correlate() %>% 
    corrr::focus(!!sym(target_var)) %>% 
    arrange(desc(abs(!!sym(target_var)))) %>% 
    top_n(n) %>% 
    pull(term)
  cor_list
}

mer_model_selection <- function(){
  mir_mod_0 <- lmer(AROUSAL ~ (1 | p_id) + (1  | s_id), data = mer, control = lmerControl(optimizer = "bobyqa"))
  mir_mod_1 <- lmer(AROUSAL ~ MIR.tempo + (1 + MIR.tempo| p_id) + (1  | s_id), data = mer, , control = lmerControl(optimizer = "bobyqa"))
  mir_mod_2 <- lmer(AROUSAL ~ MIR.tempo + MIR.rms_mean + (1 + MIR.tempo + MIR.rms_mean| p_id) + (1  | s_id), data = mer, control = lmerControl(optimizer = "bobyqa"))
  mir_mod_3 <- lmer(AROUSAL ~ MIR.tempo + MIR.rms_mean + MIR.pitch_mean +  (1 + MIR.rms_mean + MIR.pitch_mean + MIR.tempo| p_id) + (1  | s_id), data = mer, control = lmerControl(optimizer = "bobyqa"))
  mir_mod_4 <- lmer(AROUSAL ~ MIR.tempo + MIR.rms_mean + MIR.pitch_mean +  MIR.mode_mean + (1 + MIR.rms_mean + MIR.pitch_mean + MIR.tempo + MIR.mode_mean| p_id) + (1  | s_id), data = mer, control = lmerControl(optimizer = "bobyqa"))
  form_full <- sprintf("AROUSAL ~ %s + (1 + MIR.tempo + MIR.rms_mean| p_id) + (1|s_id)", 
                       paste(mer_mir_features, collapse = " + ")) 
  mir_mod_full <- lmer(as.formula(form_full), data = mer, control = lmerControl(optimizer = "bobyqa"))
  
  assign("mir_mod_best", mir_mod_full, globalenv())
  anova(mir_mod_0, mir_mod_1, mir_mod_2, mir_mod_3, mir_mod_4, mir_mod_full)  
}

improve_mer_model <- function(best_model = mir_mod_best, predictors){
  #mir_mod_new <- lmer(AROUSAL ~ MIR.tempo + MIR.rms_mean + MIR.pitch_mean + (1 | s_id) + (MIR.rms_mean + MIR.pitch_mean | p_id), data = mer)
  mir_mod_new <- lmer(AROUSAL ~ MIR.low_energy_mean + MIR.pitch_mean + MIR.rms_mean + MIR.pulse_clarity_mean + MIR.attacktime_std + MIR.tempo + (1 + MIR.tempo + MIR.rms_mean | p_id) + (1 | s_id), data = mer)
  mir_mod_new2 <- lmer(AROUSAL ~ MIR.low_energy_mean + MIR.pitch_mean + MIR.rms_mean *  MIR.tempo + MIR.pulse_clarity_mean + MIR.attacktime_std  + (1 + MIR.tempo + MIR.rms_mean | p_id) + (1 | s_id), data = mer)
  assign("mir_mod_best", mir_mod_new, globalenv())
  anova(best_model, mir_mod_new, mir_mod_new2)
}