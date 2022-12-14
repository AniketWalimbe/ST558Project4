library(tidyverse)



getData <- function(){
  heart_data <- read_csv("framingham_heart_disease.csv")
  
  return(heart_data)
}
