library(boxr)
library(dplyr)
library(magrittr)

install.packages(c("Rcpp", "devtools"), dependencies=TRUE)
library(devtools)
install_github("awalker89/openxlsx")

install.packages("openxlsx", dependencies=TRUE)

install.packages('openxlsx', type='source', repos='http://cran.us.r-project.org') 

box_auth()
box_read()
box_search('mariculture_fish_province.xlsx') %>%
  box_read()

data<-box_read_excel(140781870465,sheet=2)
data<-box_read(140781870465)

dim(data)
box_read_excel
box_read
