library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(gt)
library(stringr)
library(report)

sum_and_hist <- function(x) {
  x %>% pivot_longer(-user, 
               names_to = "Vars", 
               values_to = "Values") %>%
    nest_by(Vars) %>%
    mutate(N = nrow(data %>% filter(!is.na(data$Values))),
           M = round(mean(data$Values,na.rm=T), 2), 
           SD = round(sd(data$Values,na.rm=T), 2), 
           Median = round(median(data$Values,na.rm=T), 2), 
           Min = round(min(data$Values,na.rm=T), 2), 
           Max = round(max(data$Values,na.rm=T), 2), 
           plots = list(ggplot(data, aes(Values)) + geom_histogram() + theme_void()), .keep = "unused") %>%
    ungroup %>%
    mutate(Histogram = NA) %>%
    {dat <- .
    dat %>%
      select(-plots) %>%
      gt() %>%
      text_transform(locations = cells_body(c(Histogram)),
                     fn = function(x) {
                       map(dat$plots, ggplot_image, height = px(20))
                     }
      )
    }
}


preview_cols <- function(dataset) {
  str_replace((report(dataset[,],distribution = F ,digits = 2,) %>% str_split("\n"))[[1]][3:(ncol(dataset)+2)],"  - ","") %>%
    as.data.frame() %>% 
    separate(1,sep = ": ", into= c("Column","Values"),extra = "merge") %>% mutate(Values = str_replace(Values," entries", " distinct entries")) %>% gt::gt()
} 

summary_df  <- function(dataset, symbol = "*", explanations = list(c(NULL, NULL)), ignore = rep.int(T,nrow(dataset))) {
  for (col_index in seq_along(dataset)) {
    col_name <- names(dataset)[col_index]
    col_type <- ifelse(!is.null(explanations[[col_index]][2]), explanations[[col_index]][2], NULL)
    switch(
      col_type,
      "double" = dataset[[col_name]] <- as.numeric(gsub(",", ".", dataset[[col_name]])),
      "character" = dataset[[col_name]] <- as.character(dataset[[col_name]]),
      "integer" = dataset[[col_name]] <- as.integer(dataset[[col_name]])
    )
  }
  lista <- str_replace((report(dataset) %>% summary() %>%
               str_split("\n"))[[1]][3:(ncol(dataset)+2)], "  - ", "") %>%
  as.data.frame() %>% 
  separate(1,sep = ": ", into= c("Column","Values"), extra = "merge") %>%
  mutate(Values = str_replace(Values," entries", " distinct entries")) %>% 
  mutate(Values = str_replace(Values," Skewness = .*,", ""))  %>%
  mutate(Values = str_replace(Values,"Mean =,", "M ="))   %>%
  mutate(Values = str_replace(Values,"; NA$", ""))  
  for (li in 1:nrow(lista)) {
    cat(paste0("  ", symbol, "  **",lista[li,"Column"], "**: ", ifelse(is.null(explanations[[li]][1]),"",explanations[[li]][1]) ,
               ifelse(ignore[li],"",lista[li,"Values"]),
               ".\n"))
  }
} 


rep <- function(x) {
  y = (str_replace(x,",","."))
  y = (str_replace(y,"^-\\.","-0."))
  y = (str_replace(y,"^\\.","0."))
  as.numeric(y)
}
