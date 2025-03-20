---
title: LMM Worksop
author: Klaus Frieler, Max Planck Institute for Empirical Aesthetics, Frankfrut/Main
date: 20.03.2025
---

Materials for a Linear Mixed Effects Models Workshop

## Data Sets
The project includes several data sets that are used during the workshop.

### Simulating random effects data

Central are simulated data, though, which can be generated using `simulate_lmm()`. The generated data are coming from a 
fully crossed desihn of `n_rater` rating `n_item` with respect to `liking`. The items are presented in two `condition`s, audio-only 
(`AO`) and audio-visual `AV`. The effect of condition on liking can be set be with the parameter `fixef_beta`, 
the residual error with the parameter `eps`, the global intercept with `y0`. The random effects are set with the parameter `ranef` which needs to be a nested list.
On the top level, the need to be three lists with names `intercept`, `beta`,  and `cor`, which  themselves need to be lists with entries `raters`and 
`ìtems`, containing the corresponding random effects parameters.


| Variable      | Type    | Values                                              |
|:--------------|:--------|:----------------------------------------------------|
| **rater**     | factor  | [ r01 &vert; r02 &vert; r03 &vert; r04 &vert; ... ] |
| **item**      | factor  | [ i01 &vert; i02 &vert; i03 &vert; i04 &vert; ... ] |
| **condition** | factor  | [ AO &vert; AV ]                                    |
| **betas**     | numeric |                                                     |
| **y1**        | numeric |                                                     |
| **liking**    | numeric |                                                     |

There are four other data sets, three real and one simulated, included with the project, which will be described shortly in the following.

### `mer`
This is a abridged version of the original data from Lange & Frieler, 2016. The data come from an experiment where 20 raters
(column `p_id`) rated 60 items (column `s_id`) on several scales. The data set is filtered to included only cases which have a 
definite tempo (column `MIR.tempo`), which are 53 in total.

The main ratings were perceived emotional expressions (columns `angry`, `sad`, `happy`, `peacef`, `tender`, `fearf`), These were 
transformed with a factor analysis to two variables for valence and arousal (columns `AROUSAL` and `VALENCE`.)

Besides that, for each item there are 11 audio features extracted, where MIR.tempo was manually annotated (columns 
`MIR.low_energy_mean`, `MIR.pitch_mean`, `MIR.rms_mean`, `MIR.regularity_mean`, `MIR.keyclarity_mean`, `MIR.mode_mean`, 
`MIR.pulse_clarity_mean`, `MIR.spectral_novelty_mean`, `MIR.mode_std`, `MIR.attacktime_std`, `MIR.tempo`).

Finally, there is a categorial variables (column `emo`), which contains the emotional label, derived as the emotion variable 
with the highest mean across participants. 

| Variable                      | Type    | Values                                                       |
|:------------------------------|:--------|:-------------------------------------------------------------|
| **p_id**                      | integer | 1&ndash;20                                                   |
| **s_id**                      | integer | 1&ndash;60                                                   |
| **AROUSAL**                   | numeric |                                                              |
| **VALENCE**                   | numeric |                                                              |
| **angry**                     | integer | 1&ndash;7                                                    |
| **sad**                       | integer | 1&ndash;7                                                    |
| **happy**                     | integer | 1&ndash;7                                                    |
| **peacef**                    | integer | 1&ndash;7                                                    |
| **tender**                    | integer | 1&ndash;7                                                    |
| **fearf**                     | integer | 1&ndash;7                                                    |
| **MIR.low_energy_mean**       | numeric |                                                              |
| **MIR.pitch_mean**            | numeric |                                                              |
| **MIR.rms_mean**              | numeric |                                                              |
| **MIR.regularity_mean**       | numeric |                                                              |
| **MIR.keyclarity_mean**       | numeric |                                                              |
| **MIR.mode_mean**             | numeric |                                                              |
| **MIR.pulse_clarity_mean**    | numeric |                                                              |
| **MIR.spectral_novelty_mean** | numeric |                                                              |
| **MIR.mode_std**              | numeric |                                                              |
| **MIR.attacktime_std**        | numeric |                                                              |
| **MIR.tempo**                 | numeric |                                                              |
| **emo**                       | factor  | [ angry &vert; fearf &vert; happy &vert; peacef &vert; ... ] |

### `covox`

This is a selection of the Covox dataset (Bruder et al., 2024). The data comes from a rating experiment, where 60 raters 
(column `rater`), assessed 330 items (column `item`) in total which respect to their liking (column `score`). 

The items were short monophonic audio excerpts by 22 singers which rendered 3 different melodies (column `melody`) in 
5 different styles (column `style`), 3 singing (`opera`, `pop`, and `lullaby`) and 2 speech styles (infant directed `ID` and
adult directed `AD`, see also column `speech`).

Each participant rated all items on two different occasions (column `time`).

| Variable   | Type      | Values                                                                                                  |
|:-----------|:----------|:--------------------------------------------------------------------------------------------------------|
| **singer** | factor    | [ singer01 &vert; singer02 &vert; singer03 &vert; singer04 &vert; ... ]                                 |
| **rater**  | character | [ r01 & vert; r02 &vert; r03 & vert; r04& ... ]                                                                                                         |
| **style**  | factor    | [ AD &vert; ID &vert; lullaby &vert; opera &vert; pop ]                                                 |
| **melody** | factor    | [ CH &vert; MS &vert; NN ]                                                                              |
| **time**   | factor    | [ T1 &vert; T2 ]                                                                                        |
| **score**  | integer   | 1&ndash;9                                                                                               |
| **item**   | factor    | [ singer01_CH_AD &vert; singer01_CH_ID &vert; singer01_CH_lullaby &vert; singer01_CH_opera &vert; ... ] |
| **speech** | logical   | [TRUE &vert; FALSE] 


### `kids_beta`
This is a subset of the raw from Will et al. 2024. In this experiment, 188 participants from 5 different age groups, 
had to tap to  a beat in two different settings and with  two different tempos. Precision (`precision`) was measured as the negative 
logarihm of the standard deviation `log_sd_ioi`) of produced inter-beat intervals (the higher, the better). Furthermore, beat 
perception and production skills were assessed with a different battery.


| Variable         | Type    | Values                                                                      |
|:-----------------|:--------|:----------------------------------------------------------------------------|
| **p_id**         | factor  | [ i_e_05_02 &vert; i_e_06_02 &vert; i_e_07_02 &vert; i_e_09_02 &vert; ... ] |
| **age_group**    | factor  | [ 5yo &vert; 6yo &vert; 7yo &vert; 8yo &vert; adults ]                      |
| **setting**      | factor  | [ metronome &vert; social ]                                                 |
| **experimenter** | factor  | [ i &vert; l &vert; w ]                                                     |
| **tempo**        | factor  | [ fast &vert; slow ]                                                        |
| **beat_perc**    | numeric |                                                                             |
| **beat_prod**    | numeric |                                                                             |
| **log_sd_ioi**   | numeric |                                                                             |
| **beat_ability** | numeric |                                                                             |
| **precision**    | numeric |                                                                             |

### `scenario2`
This is simulated data with heteroscedastic and measurement error. Outcome is `y`, with two predictors `x` and `z`. True coefficients
are intercept = 1, beta_x = .4, and beta_z = .5. The original values, before applying measurement errors `xe`, `ye` and `ze', 
are `xt`, `yt` and `zt'.  The level of measurement error can be `high` or `low`.


| Variable | Type      | Values     |
|:---------|:----------|:-----------|
| **y**    | numeric   |            |
| **x**    | numeric   |            |
| **xe**   | numeric   |            |
| **xt**   | numeric   |            |
| **zt**   | numeric   |            |
| **ze**   | numeric   |            |
| **z**    | numeric   |            |
| **p_id** | integer   | 1&ndash;50 |
| **me**   | character | [ low &vert; vert ]            |