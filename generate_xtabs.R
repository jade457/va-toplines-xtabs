library(readxl)
library(writexl)
library(civis)
library(civis.deckR2)
library(zoo)
library(purrr)
library(dplyr)
library(openxlsx)
library(scales)

source("generate_future_objs.R")

path <- 'ToplinesCrosstabs_raw.xlsx'

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  sheets <- sheets[sheets != 'Table of Contents']
  # take sample for now
  sheets <- sheets[30:35]
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets(path)

# reformatting excel sheets -> named list of dfs and other metadata 
xtab_groupings = list(
  
  # topline = c('Topline'),
  
  single_demos = c('gender_web_twoway', 'race_civis2_web_coalesced', 'partisanship_web2_coalesced',
                   'education_2', 'income_web', 'CPS_employment', 'CPS_MARST', 'generation', 'ideology_web',
                   'bible_gss', 'trust_gss', 'home_languages', 'english_speak_well', 
                   'media_use_4way_2', 'gss_spanking', 'gun_in_house', 'support_system'),
  
  #covid
  covid = c('vapm_covid_vax_status', 'vapm_covid_vax_status_reason', 'vapm_covid_booster', 'vapm_covid_shot_count'),
  
  #vote choice
  vote_choice = c('poli_favorability_joe_biden', 'poli_job_approval_joe_biden_5pt', 'potus2016_votechoice_recall4',
                  'potus2020_votechoice_recall_coalesced', 'state_house_seat_1_coalesced', 'gov_2021_matchup_1_coalesced'),
  
  persuasion_testing = c('first_view', 'vapm_crt_persuasion', 'vapm_covid_management_persuasion', 'vapm_control_persuasion'),
  
  multi_xtabs = c(multi_xtab_columns))

master_list <- list()
temp_df <- data.frame()
for (xtab in unlist(xtab_groupings)){
  print(xtab)
  all_qs_list = list()
  #xtab='gender_web_twoway'
  for(i in 1:length(mysheets)){
    #i=40
    templist <- list()
    
    og_df <- mysheets[[i]]
    templist[["question_text"]] <- names(og_df)[1]
    tempdf <- og_df %>%
      mutate(pmap_df(., ~ na.locf(c(...)[-1]))) # to-do: more robust, only on first row
    temp_df <- tempdf[-(grep('NA', tempdf[1,]))]
    names(tempdf) <- tempdf[1,]
    tempdf <- tempdf[-(1:1),]  
    tempdf <- cbind(tempdf[1], tempdf[ , grepl( paste0("^", xtab,'$') , names( tempdf ) ) ])
    colnames(tempdf)[1] <- 'Response'
    
    cleaned_df <- tempdf[complete.cases(tempdf),]
    # cleaned_df[,-1] <- sapply(cleaned_df[-1],as.numeric)
    
    metadata_tempdf <- anti_join(tempdf, cleaned_df, by=c('Response'))
    metadata_tempdf <- metadata_tempdf[,-1] 
    metadata_tempdf <- metadata_tempdf[complete.cases(metadata_tempdf),]
    
    cleaned_df$Response <- sapply(lapply(cleaned_df$Response, strwrap, width=30), paste, collapse="\n")
    
    cleaned_df[,-1] <- sapply(cleaned_df[,-1], function(x) ifelse(grepl("[0-9]+", x),percent(as.numeric(x), accuracy=1),x))
    
    templist[["df"]] <- cleaned_df
    templist[["n_size"]] <- unlist(metadata_tempdf[2, ], use.names = FALSE)
    templist[["moe"]] <- unlist(percent(as.numeric(metadata_tempdf[4, ]), accuracy=1), use.names = FALSE)
    all_qs_list[[i]] <- templist
}
master_list[[xtab]] <- all_qs_list
}


# final push to make the nicely formatted excel toplines 
sheet_tab_dfs <- list() # list of df, each element = combined df of each xtab grouping
xtab_df <- data.frame()
xtab_grouping_df <- data.frame()

for(i in 1:length(xtab_groupings)){
  xtab_grouping_df <- data.frame()
  for (j in 1:length(xtab_groupings[[i]])){
    xtab_df <- data.frame()
    xtab <- xtab_groupings[[i]][j]
    xtab_allqs <- master_list[names(master_list) == xtab]
    
    for (k in 1:length(xtab_allqs[[1]])){
      current_q <- xtab_allqs[[1]][[k]]
      unit_df <- rbind(current_q$df, c("n-size",current_q$n_size))
      unit_df <- rbind(unit_df, c("MOE",current_q$moe))
      q_col <- c(NA, current_q$question_text)
      length(q_col) <- nrow(unit_df)
      unit_df <- cbind(Question=q_col, unit_df)
      if(nrow(xtab_df) == 0){
        xtab_df <- unit_df
      }else{
        xtab_df <- rbind(xtab_df, unit_df[-1,])
      }
    }
    if(nrow(xtab_grouping_df) == 0){
      xtab_grouping_df <- xtab_df
    }else{
      xtab_grouping_df <- cbind(xtab_grouping_df, xtab_df[,-(1:2)])
    }
  }
  sheet_tab_dfs[[names(xtab_groupings)[i]]] <- xtab_grouping_df
}


write.xlsx(sheet_tab_dfs, file = "output_sample.xlsx", overwrite=TRUE)



# scratch





# templist <- list()
# # templist[["field_code"]] <- field_code 
# tempdf <- mysheets[[3]]
# templist[["question_text"]] <- names(tempdf)[1]
# tempdf <- tempdf %>%
#   mutate(pmap_df(., ~ na.locf(c(...)[-1]))) # to-do: more robust, only on first row
# 
# cleaned_df <- tempdf[complete.cases(tempdf),]
# cleaned_df[,-1] <- sapply(cleaned_df[-1],as.numeric)
# 
# metadata_tempdf <- anti_join(tempdf, cleaned_df, by=c('Response'))
# metadata_tempdf <- metadata_tempdf[,-1] 
# metadata_tempdf <- metadata_tempdf[complete.cases(metadata_tempdf),]
# 
# cleaned_df$Response <- sapply(lapply(cleaned_df$Response, strwrap, width=30), paste, collapse="\n")
# templist[["df"]] <- cleaned_df
# templist[["n_size"]] <- unlist(metadata_tempdf[2, ], use.names = FALSE)
# templist[["moe"]] <- unlist(metadata_tempdf[4, ], use.names = FALSE)
# 
# 
# test <- data[[111]]
# test_df <- test$df
# tempdf <- rbind(test_df, c("n",test$n_size))
# tempdf <- rbind(tempdf, c("moe",test$moe))



