source("bcf-iv.R")
source("bcf-itt.R")

library(tidyverse)
library(xtable)

# 1. Preprocessing
data = read.csv("anon_2018_data.csv", header=T, quote="")

df = data %>% mutate(is_control = if_else(is_control == 'True', 1, 0)) %>% 
  mutate(messaged = if_else(messaged == 'True', 1, 0)) %>% 
  mutate(voted = if_else(voted == "True", 1, 0)) %>% 
  mutate(across(vb.tsmart_state, as.factor)) %>% 
  mutate(vb.voterbase_gender = dplyr::recode(vb.voterbase_gender, 
                                             'Male' = 1, 'Female' = 0, 'Unknown' = -1)) %>% # 522 unknowns
  mutate(across(vb.voterbase_gender, as.factor)) %>% 
  mutate(across(vb.voterbase_race, as.factor)) %>% 
  mutate(vb.voterbase_marital_status = dplyr::recode(vb.voterbase_marital_status, 
                                                     'Married' = 1, 'Unmarried' = 0, 'Unknown' = -1)) %>% # 883 unknowns
  mutate(across(vb.voterbase_marital_status, as.factor)) %>%
  mutate(across(vb.vf_party, as.factor)) %>% 
  mutate(vb.vf_race = dplyr::recode(vb.vf_race, 'Unavailable' = 'Uncoded')) %>% 
  mutate(across(vb.vf_race, as.factor)) %>%
  mutate(vb.voterbase_voter_score = dplyr::recode(vb.voterbase_voter_score,
                                                  'Never Voted' = 'Never_Voted',
                                                  'Infrequent Voter' = 'Infrequent_Voter',
                                                  'Frequent Voter' = 'Frequent_Voter',
                                                  'Super Voter' = 'Super_Voter')) %>% 
  mutate(vb.voterbase_voter_score = factor(vb.voterbase_voter_score, ordered=TRUE, 
                                           levels = c('Never_Voted', 'Infrequent_Voter', 'Frequent_Voter', 'Super_Voter'))) %>% 
  mutate(vb.education = ifelse(is.na(vb.education), 0, vb.education)) %>%
  mutate(vb.education = dplyr::recode(vb.education,
                                      '0' = 'Unknown',
                                      '6' = 'Less_Than_High_School',
                                      '1' = 'High_School',
                                      '2' = 'Some_College',
                                      '3' = 'College_Graduate',
                                      '4' = 'Post_Graduate')) %>%
  mutate(vb.education = factor(vb.education, ordered=TRUE, 
                               levels=c('Unknown', 'Less_Than_High_School', 'High_School', 'Some_College', 'College_Graduate', 'Post_Graduate'))) %>% 
  mutate(vb.household_income_range = factor(vb.household_income_range, ordered=TRUE)) %>% 
  mutate(vb.household_net_worth = factor(vb.household_net_worth, ordered=TRUE)) %>% 
  mutate(ts.tsmart_urbanicity = factor(ts.tsmart_urbanicity, ordered=TRUE)) %>% 
  mutate(ts.reg_urbanicity = factor(ts.reg_urbanicity, ordered=TRUE)) %>% 
  mutate(across(ts.tsmr_race, as.factor))
  # mutate(across(vb.vf_p2016_party, as.factor)) %>%
  # mutate(across(vb.vf_p2014_party, as.factor)) %>%
  # mutate(across(vb.vf_p2012_party, as.factor))

rm(data)

# Urbanicity: only keep those with matching info
df = df %>% mutate(urban_equal = if_else(ts.tsmart_urbanicity_rank == ts.reg_urbanicity_rank, 1, 0)) %>% 
  filter(urban_equal == 1) %>% 
  select(-urban_equal)

# States 
summary(df$vb.tsmart_state)

df$vb.tsmart_state = fct_collapse(df$vb.tsmart_state, Purple = c('AZ', 'CO', 'FL', 'GA', 'MI', 'ME', 'MN', 'NC', 'NH', 'OH', 'PA', 'VA', 'WI'),
                                  Blue = c('AL', 'CA', 'CT', 'DC', 'DE', 'HI', 'IL', 'MA', 'MD', 'NJ', 'NM', 'NY', 'OR', 'RI', 'VT', 'WA'),
                                  Red = c('AR', 'AK', 'IA', 'ID', 'IN', 'KS', 'KY', 'LA', 'MO', 'MS', 'MT', 'ND', 'NE', 'NV', 'OK', 'SC', 'SD', 'TN', 'TX', 'UT', 'WY', 'WV'))

# 2. Get the final dataframe ready for fitting
myvars = c('voted', 'messaged', 'is_control', 
           'vb.voterbase_age', 'vb.voterbase_gender', 'vb.voterbase_marital_status',
           'vb.education', 'vb.household_income_range', 'ts.tsmart_urbanicity_rank',
           'vb.voterbase_voter_score', 'vb.vf_party', 'voted_g2014', 'voted_g2016', 'vb.tsmart_state',
           'ts.tsmart_presidential_general_turnout_score', 'ts.tsmart_midterm_general_turnout_score',
           'ts.tsmart_midterm_general_enthusiasm_score', 'ts.tsmart_presidential_primary_turnout_score',
           'ts.tsmart_non_presidential_primary_turnout_score', 
           'ts.tsmart_offyear_general_turnout_score',
           'ts.tsmart_partisan_score',
           'ts.tsmart_ideology_score', 'ts.tsmart_activist_score', 'ts.tsmart_trump_resistance_score',
           'predictwise.authoritarianism_score', 'predictwise.populism_score', 'predictwise.racial_resentment_score')

df = df[myvars]

df = df %>% na.omit()

dim(df)

# 3. Get y, z, w, x ready
attach(df)

x = cbind(vb.voterbase_age, vb.voterbase_gender, vb.voterbase_marital_status,
          vb.education, vb.household_income_range, ts.tsmart_urbanicity_rank,
          vb.voterbase_voter_score, vb.vf_party, voted_g2014, voted_g2016, vb.tsmart_state,
          ts.tsmart_presidential_general_turnout_score, ts.tsmart_midterm_general_turnout_score,
          ts.tsmart_midterm_general_enthusiasm_score, ts.tsmart_presidential_primary_turnout_score,
          ts.tsmart_non_presidential_primary_turnout_score, 
          ts.tsmart_offyear_general_turnout_score,
          ts.tsmart_partisan_score,
          ts.tsmart_ideology_score, ts.tsmart_activist_score, ts.tsmart_trump_resistance_score,
          predictwise.authoritarianism_score, predictwise.populism_score, predictwise.racial_resentment_score)
          
y = as.matrix(voted) 
w = as.matrix(messaged)
z = as.matrix(is_control) 

detach(df)

# 4. Fit BCF-IV 
print('Fitting BCF-IV...')
bcf_iv_results = bcf_iv(y, w, z, x, binary = TRUE, n_burn = 1000, n_sim = 1000, inference_ratio = 0.5)

xtable(bcf_iv_results, digits=2)

print('Fitting BCF-ITT for robustness check...')
bcf_itt(y, w, z, x, binary = TRUE, max_depth = 2, n_burn = 1000, n_sim = 1000, inference_ratio = 0.5)
