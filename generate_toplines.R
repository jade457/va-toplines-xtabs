library(readxl)
library(civis)
library(civis.deckR2)



path <- '/Users/jwu2/toplines-ct-auto/ToplinesCrosstabs.xlsx'

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  sheets <- sheets[sheets != 'Table of Contents']
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets(path)

# reformatting excel sheets -> named list of dfs and other metadata 
data <- list()
for (i in 1:length(mysheets)){
  templist <- list()
  # templist[["field_code"]] <- field_code 
  tempdf <- mysheets[[i]]
  templist[["question_text"]] <- names(tempdf)[1]
  names(tempdf) <- tempdf[2,]
  tempdf <- tempdf[-(1:2),]  
  
  cleaned_df <- tempdf[complete.cases(tempdf),]
  cleaned_df[,-1] <- sapply(cleaned_df[-1],as.numeric)
  
  metadata_tempdf <- anti_join(tempdf, cleaned_df, by=c('Response'))
  metadata_tempdf <- metadata_tempdf[,-1] 
  metadata_tempdf <- metadata_tempdf[complete.cases(metadata_tempdf),]
  
  cleaned_df$Response <- sapply(lapply(cleaned_df$Response, strwrap, width=30), paste, collapse="\n")
  templist[["df"]] <- cleaned_df
  templist[["n_size"]] <- metadata_tempdf[2,1]
  templist[["moe"]] <- metadata_tempdf[4,1]
  data[[i]] <- templist 
}



# export toplines to a deliverable
d <- deliverable("Toplines") 

topline_plot <- function(df){
  plot <- ggplot(data = df, aes(x = Response, y = Percent, fill = Response)) +
    geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
    geom_text(aes(label = scales::percent(Percent %>% round(2))), 
              vjust = 1, size = 5, fontface = "bold", family = "Lato", nudge_y=0.03) + coord_flip() +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_line( size=.1, color="black" )  ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_blank()
    )
  return(plot)
}


for (i in 1:length(data)) {
  p <- topline_plot(data[[i]]$df)
  print(data[[i]]$question_text)
  d <- d %u% unit(data[[i]]$question_text, 
                  paste("n-size: ",data[[i]]$n_size, "\nmargin of error: ", 
                        data[[i]]$moe), panel_1=p)
}

# finish exporting deliverable
to_ppt(d, "test")
