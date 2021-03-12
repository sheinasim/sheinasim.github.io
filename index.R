## ----setup, include = F--------------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(plyr)
library(broom)
library(dplyr)
library(ggplot2)
library(readxl)
library(flexdashboard)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sem = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
 return(data_sum)
}


## ----Read in file--------------------------------------------------------------------------------------------------------------------------------------------------------

data_df <- read_tsv("test.txt", col_names=T, trim_ws = TRUE, skip_empty_rows = TRUE)



## ----plot line-----------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=data_df, aes(x=X, y=Y1)) +
  geom_line()+
  geom_point()



## ----plot hist-----------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(data_df, aes(x=Y1)) + geom_histogram()



## ----plot lolli----------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(data_df, aes(x=X, y=Y2)) +
  geom_point() + 
  geom_segment( aes(x=X, xend=X, y=0, yend=Y2))

