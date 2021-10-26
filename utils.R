

# create a SurveyData object for our project
init_survey_data_obj <- function(PROJECT_ID){
  survey_data <- SurveyData(PROJECT_ID)
}



# coalesce columns
coalesce_cols_func <- function(survey_data, coalesce_cols_1, coalesce_cols_2, coalesce_on, keep_original=FALSE){
  for (i in 1:length(coalesce_cols_1)){
    survey_data$coalesce(
      primary_column=coalesce_cols_1[i],
      secondary_column=coalesce_cols_2[i],
      coalesce_on=coalesce_on[i],
      keep_original=keep_original
    )  
  }
}

# clean data - html elements
clean_data_func <- function(survey_data, clean_data_flag=TRUE){
  if (clean_data_flag){
    survey_data$clean_data()
  }
}


# recode columns
recode_cols_func <- function(survey_data, recode_cols, mapping, keep_original = FALSE){
  for (i in 1:length(recode_cols)){
    survey_data$recode(
      column=recode_cols[i],
      mapping=mapping[i],
      keep_original=keep_original
    )
  }
}


# age bin from year
age_bin_from_year_func <- function(survey_data, age_bin_cols){
  for (i in 1:length(age_bin_cols)){
    survey_data$age_bin_from_year(column=age_bin_cols[[i]], keep_original=FALSE)    
  }
}


# rename cols?
rename_cols_func <- function(survey_data, old_columns, new_columns, keep_original=FALSE){
  for (i in 1:length(old_columns)){
    survey_data$rename_column(
      old_column=old_columns[i],
      new_column=new_columns[i],
      keep_original=keep_original
    )
  }
}


# Filter out the under_18 age_bin (specific to this example project -- not always necessary)
weighting <- function(){
  survey_data$wide_table <- 
    survey_data$wide_table %>%
    filter(age_bin != "under_18")
  
  # Initialize the sdk client
  sdk <- SurveysSDKClient()
  
  # Run recoding
  # recoding 154 is based on the preset ACS recoding, customized for this survey project
  # See here for an example of creating a custom recoding:
  # https://platform.civisanalytics.com/spa/#/notebooks/21383
  recode <- sdk$recode_survey_by_recoding_id(
    survey_data$wide_table, 
    recoding_id=154,
    keep_originals=TRUE
  )
  
  # Wait for recoding job to finish and grab the outputs
  outputs <- recode$outputs()
  
  # Get file_id of recoded survey
  recoded_file_id <- outputs[[1]]$value$recoded_file_id
  
  # Weight to age (just for the sake of example)
  target_variables <- list("age_bucket")
  
  df <- surveys.sdk::civis_feather_file_to_dataframe(recoded_file_id)
  df_weighting <- df[c("age_bucket", "response_id", "project_id")]
  
  weight <- 
    sdk$weight_to_strata_based_targets(
      survey_data=df_weighting,
      survey_primary_key="response_id",
      microdata_source=16, # ACS microdata source has id # 16
      target_variables=target_variables,
      microdata_weighting_column='pop_weight'
    )
  
  # Wait for weighting job to finish and grab the outputs
  outputs <- weight$outputs()
  
  # Grab the weighted file ID from the output
  weighted_df_file_id <- outputs[[2]]$value$weighted_df_fileid
  
  # Read in weighted data
  weighted_df <- civis::read_civis(weighted_df_file_id)
  
  # Merge weights into full dataset
  survey_data$wide_table <- 
    survey_data$wide_table %>%
    left_join(
      weighted_df[c("response_id", "weight")],
      by="response_id"
    )
}

# make toplines
make_toplines_ct <- function(crosstaab_columns){
  future <- survey_data$create_crosstabs_toplines(
    crosstab_columns=crosstab_columns, 
    weight_column="weight"
  )
}


