## ----setup, include = F-------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(plyr)
library(broom)
library(dplyr)
library(ggplot2)
library(readxl)
library(flexdashboard)
library(wesanderson)

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

pal1 <- wes_palette("Darjeeling1", 5, type = "continuous")
pal2 <- wes_palette("FantasticFox1", 5, type = "continuous")
pal3 <- wes_palette("Cavalcanti1", 5, type = "continuous")


## ----Read in file-------------------------------------------------------------------------------------------------------------------

data_xl_df <- readxl::read_xlsx("qlb_colony_spreadsheet.xlsx", col_names=T, sheet="lifestages") %>% 
  gather(key = "Life stage", value = "Count", -Date) %>% 
  mutate(`Life stage` = fct_relevel(`Life stage`, "Larvae", "Pupae", "Adult female", "Adult male", "Adult mating"))

View(data_xl_df)



## ----plot stackedbar----------------------------------------------------------------------------------------------------------------

ggplot(data_xl_df, aes(fill=`Life stage`, y=Count, x=Date)) + 
    geom_bar(position="stack", stat="identity") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),) +
    xlab("") +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) + 
    scale_fill_manual(values=pal3)
    



## ----results = hide, echo = FALSE, include = FALSE----------------------------------------------------------------------------------

# ggplot(data_df, aes(x=X, y=Y2)) +
#   geom_point() + 
#   geom_segment( aes(x=X, xend=X, y=0, yend=Y2))

knitr::purl("index.Rmd")


