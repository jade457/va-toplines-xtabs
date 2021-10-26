library(civis)
library(surveys.sdk)
library(glue)
library(tidyr)
library(dplyr)

source("utils.R")
source("config.R")

# PROJECT_ID=92905
abortion_seq = '2021-10-18 Political Tracking [VA Postmortem Questions]_October 22, 2021_08.36.csv'

###### INITIALIZATION ######
df_1 <- read.csv(abortion_seq)[-c(1:2),c(9,295)] 
df_1$PostMortem.AbortionMobilizationRCT_DO = gsub("Q80", "vapm_abortion_standard", df_1$PostMortem.AbortionMobilizationRCT_DO)
df_1$PostMortem.AbortionMobilizationRCT_DO = gsub("Q81", "vapm_abortion_effect_on_enthusiasm", df_1$PostMortem.AbortionMobilizationRCT_DO)
df_1 = separate(df_1, 2, c('first_view','second_view'), sep = '\\|')

urna_survey_data <- SurveyData(PROJECT_ID)

# pull in first_view and second_view
urna_survey_data$wide_table <- merge(df_1, urna_survey_data$wide_table, by.x = 'ResponseId', by.y = 'response_id')
urna_survey_data$wide_table$first_view <- sub("^$", "NA", urna_survey_data$wide_table$first_view)


###### POST-PROCESSING ######

# clean up HTML elements from text
clean_data_func(urna_survey_data)

# keep only completes?

# drop all open text columns -> throws an error when creating xtabs
urna_survey_data$wide_table <- dplyr::select(urna_survey_data$wide_table, -contains(c("_TEXT", "box_for_feels", "open_end", "open_end_web")))

# bins from age - omit in favor of generation recoding later 
# age_bin_from_year_func(urna_survey_data, c('yearborn_web')) # new col name = age_bin

# recode columns
urna_survey_data$recode( #to-do: fix later
  column=c('partisanship_web2_push'),
  mapping=list("Lean more toward Democrats" = "Democrat",
               "Lean more toward Republicans" = "Republican"),
  keep_original=FALSE
)


# coalesce columns
primary_cols <- c("race_civis2_web", "partisanship_web2", "potus2020_votechoice_recall", 
                  "state_house_seat_1", "gov_2021_matchup_1")
secondary_cols <- c("race_civis2_other_race_follow_up_web", "partisanship_web2_push_recoded", 
                    "potus2020_votechoice_recall_push", "state_house_seat_1_push", 
                    "gov_2021_matchup_1_push")
coalesce_on <- c(list("Other race"), list("Independent"), list("I did not vote"), 
                 list("Don't Know", "Someone Else"), list("Don't Know", "Someone Else"))
coalesce_cols_func(urna_survey_data, primary_cols, secondary_cols, coalesce_on, keep_original = TRUE) # new col names = primary_cols + _coalesced



# new columns (collapsing columns, newly created columns)
urna_survey_data$wide_table <- dplyr::mutate(urna_survey_data$wide_table,
              home_languages = case_when(other_language == 'No' ~ 'English',
                                           home_languages_1 == 1 ~ 'Spanish',
                                           home_languages_2 == 1 ~ 'Chinese',
                                           home_languages_3 == 1 ~ 'French',
                                           home_languages_4 == 1 ~ 'Tagalog',
                                           home_languages_5 == 1 ~ 'Vietamese',
                                           home_languages_6 == 1 ~ 'Other'))

urna_survey_data$wide_table <- dplyr::mutate(urna_survey_data$wide_table,
                                             media_use_4way_2 = case_when(media_use_4way_2_2 == 1 ~ 'National news like ABC, CBS, or NBC',
                                                                          media_use_4way_2_3 == 1 ~ 'CNN',
                                                                          media_use_4way_2_4 == 1 ~ 'MSNBC',
                                                                          media_use_4way_2_5 == 1 ~ 'Local news',
                                                                          media_use_4way_2_6 == 1 ~ 'Facebook',
                                                                          media_use_4way_2_7 == 1 ~ 'None of these',
                                                                         media_use_4way_2_13 == 1 ~ 'Other',
                                                                         media_use_4way_2_14 == 1 ~ 'Twitter',
                                                                         media_use_4way_2_16 == 1 ~ 'One America News Network',
                                                                         media_use_4way_2_17 == 1 ~ 'Newsmax'))

urna_survey_data$wide_table <- dplyr::mutate(urna_survey_data$wide_table,
                                             generation = case_when(between(age,18,24) ~ 'Gen Z',
                                                                     between(age,24,40) ~ 'Millennial',
                                                                     between(age,40,56) ~ 'Gen X',
                                                                     between(age,56,75) ~ 'Boomers',
                                                                     age >= 75 ~ 'Silent/Greatest'))

###### MULTIPLIER XTABBING ######

# prep - rename columns for ease of crosstabbing
old_columns = c('gss_bible', 'gss_trust')
new_columns = c('bible_gss', 'trust_gss')
rename_cols_func(urna_survey_data, old_columns, new_columns)



# prep - simplify columns
urna_survey_data$wide_table <- mutate(urna_survey_data$wide_table, 
                                      race_ethnicity_4way = case_when(race_civis2_web == 'Asian' ~ 'POC',
                                                                      race_civis2_web == 'White' ~ 'White',
                                                                      race_civis2_web == 'Black or African American' ~ 'Black',
                                                                      race_civis2_web == 'American Indian or Alaska Native' ~ 'POC',
                                                                      race_civis2_web == 'Hispanic or Latino/a' ~ 'Latino',
                                                                      race_civis2_web == 'Other race' ~ 'POC'))

urna_survey_data$wide_table <- mutate(urna_survey_data$wide_table, 
                                      education_2way = case_when(education_2 == 'No high school diploma' ~ 'Non-College',
                                                                      education_2 == 'Bachelor\'s degree' ~ 'College',
                                                                      education_2 == 'Advanced degree (such as Master\'s, Professional, or Doctorate degree)' ~ 'College',
                                                                      education_2 == 'Associate\'s degree' ~ 'College',
                                                                      education_2 == 'High school diploma or equivalent' ~ 'Non-College',
                                                                      education_2 == 'Some college, but no degree' ~ 'Non-College'))

urna_survey_data$wide_table <- mutate(urna_survey_data$wide_table, 
                                      income_web_3way = case_when(income_web == '$100,001 - $150,000' ~ 'Over $100k',
                                                                  income_web == '$50,001 - $75,000' ~ '$50k-$100k',
                                                                  income_web == 'More than $150,000' ~ 'Over $100k',
                                                                  income_web == 'Under $25,000' ~ 'Under $50k',
                                                                  income_web == '$25,000 - $50,000' ~ 'Under $50k',
                                                                  income_web == '$75,001 - $100,000' ~ '$50k-$100k'))

urna_survey_data$wide_table <- mutate(urna_survey_data$wide_table, 
                                      ideology_web_3way = case_when(ideology_web == 'Somewhat liberal' ~ 'Liberal',
                                                                    ideology_web == 'Moderate' ~ 'Moderate',
                                                                    ideology_web == 'Very conservative' ~ 'Conservative',
                                                                    ideology_web == 'Very liberal' ~ 'Liberal',
                                                                    ideology_web == 'Somewhat conservative' ~ 'Conservative'))

urna_survey_data$wide_table <- mutate(urna_survey_data$wide_table, 
                                      vapm_crt_persuasion = case_when(vapm_crt_persuasion_a == 1 ~ 'Republican first',
                                                                      vapm_crt_persuasion_b == 1 ~ 'Democrat first'))

urna_survey_data$wide_table <- mutate(urna_survey_data$wide_table, 
                                      vapm_covid_management_persuasion = case_when(vapm_covid_management_persuasion_a == 1 ~ 'Democrat first',
                                                                                   vapm_covid_management_persuasion_b == 1 ~ 'Republican first'))

urna_survey_data$wide_table <- mutate(urna_survey_data$wide_table, 
                                      vapm_control_persuasion = case_when(vapm_control_persuasion_a == 1 ~ 'election_reminded',
                                                                          vapm_control_persuasion_a == 'NA' ~ 'election_not_reminded'))

  

# multiplier x-tab columns
primary = c('race_ethnicity_4way', 'gender_web_twoway', 'income_web_3way', 'education_2way')
secondary = list(c('education_2way', 'partisanship_web2_coalesced', 'income_web_3way', 'ideology_web_3way','bible_gss', 'trust_gss', 'gun_in_house', 'gender_web_twoway', 'generation'),
              c('trust_gss', 'bible_gss', 'generation'),
              c('trust_gss', 'gender_web_twoway', 'partisanship_web2_coalesced', 'ideology_web_3way', 'generation'),
              c('trust_gss', 'bible_gss', 'generation'))
multi_xtab_columns <- c()

# dynamically create multiplier x-tab columns
for (i in 1:length(primary)){
  for (j in 1:length(secondary[[i]])){
    xtab_col_name = paste0(sub("\\_.*", "", primary[i]), "_by_", sub("\\_.*", "", secondary[[i]][j]))
    multi_xtab_columns <- c(multi_xtab_columns, xtab_col_name)
    urna_survey_data$wide_table <- dplyr::mutate(urna_survey_data$wide_table, 
                                                 (!!xtab_col_name) := paste0((!!as.name(primary[i])),'|',(!!as.name(secondary[[i]][j])))) 
  }
}



###### WEIGHTING ######
# to-do: join with real weighting later 
urna_survey_data$wide_table$weight = as.numeric(1)

###### CREATE FUTURE OBJECT ######

# define crosstab columns 
single_crosstab_columns <- list(
                      #demos
                     'gender_web_twoway', 'race_civis2_web_coalesced', 'partisanship_web2_coalesced',
                     'education_2', 'income_web', 'CPS_employment', 'CPS_MARST', 'generation', 'ideology_web',
                     'bible_gss', 'trust_gss', 'home_languages', 'english_speak_well', 
                     'media_use_4way_2', 'gss_spanking', 'gun_in_house', 'support_system',
                     
                     #covid
                     'vapm_covid_vax_status', 'vapm_covid_vax_status_reason', 'vapm_covid_booster', 'vapm_covid_shot_count',
                     
                     #vote choice
                     'poli_favorability_joe_biden', 'poli_job_approval_joe_biden_5pt', 'potus2016_votechoice_recall4',
                     'potus2020_votechoice_recall_coalesced', 'state_house_seat_1_coalesced', 'gov_2021_matchup_1_coalesced',
                     
                     #experiment/persuasion testing
                     'first_view', 'vapm_crt_persuasion', 'vapm_covid_management_persuasion', 'vapm_control_persuasion'
                     )

all_xtab_columns <- c(single_crosstab_columns, multi_xtab_columns)

future <- urna_survey_data$create_crosstabs_toplines(
  crosstab_columns=all_xtab_columns,
  weight_column="weight"
)

file_link <- future$outputs()[[1]]$link


# scratch

# xtabs_sampling <- list('race_civis2_web_coalesced', 'race_by_education')
# 
# future_sample <- urna_survey_data$create_crosstabs_toplines(
#   crosstab_columns=list('gender_web_twoway', 'race_civis2_web_coalesced'),
#   weight_column="weight"
# )

# create multiple future objects for each crosstab
# for (xtab in crosstab_columns){
#   future <- urna_survey_data$create_crosstabs_toplines(
#     crosstab_columns=list('first_view'),
#     weight_column="weight"
#   )
#   outputs <- c(outputs, xtab=future)
# }

# ulst <- lapply(urna_survey_data$wide_table, unique)

# write_xlsx(urna_wide,"urna_wide.xlsx")
# ulst <- lapply(urna_wide[1:200], unique)
# 
# write_xlsx(data.frame(name=names(ulst), val=unlist(ulst,use.names=FALSE)),"ulst.xlsx")
# urna_survey_data_template <- SurveyData(76342)
# 
# urna_wide_template <- urna_survey_data_template$wide_table
# urna_fields_template <- urna_survey_data_template$fields
# urna_survey_data_template$wide_table$weight <- as.numeric(1)
# 
# future <- urna_survey_data_template$create_crosstabs_toplines(
#   crosstab_columns=list('gender_web_twoway'),
#   weight_column="weight"
# )
# outputs <- c(outputs, xtab=future)

# add new stuff to fields list for first_view and second_view

# wide_table_2 <- processed_survey_data$wide_table
# fields_2 <- processed_survey_data$fields
# 
# fields_2[[length(fields_2)+1]] = fields_2[[206]]
# fields_2[[length(fields_2)]]$field_code = 'first_view'
# fields_2[[length(fields_2)]]$data_type = 'object'
# fields_2[[length(fields_2)]]$field_id = 300000
# fields_2[[length(fields_2)]]$options = c('Control', 'Treatment')
# fields_2[[length(fields_2)]]$root_field_id = 400000
# 
# fields_2[[length(fields_2)+1]] = fields_2[[length(fields_2)]]
# fields_2[[length(fields_2)]]$field_code = 'second_view'
# fields_2[[length(fields_2)]]$field_id = 300001
# fields_2[[length(fields_2)]]$root_field_id = 400001
# processed_survey_data$fields <- fields_2



# rearrange wide table
# do first_view and second_view need to come before the last two columns?

# wide_table_2 <- wide_table_2 %>% dplyr::relocate(c('first_view','second_view'), .after='vapm_important_issues')
# processed_survey_data$wide_table <- wide_table_2

# urna_survey_data$wide_table <- cbind(urna_wide[1:18], urna_wide[159:160], urna_wide[242:244])
