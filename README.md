# ST558Project4

## Framingham Heart Disease 

The main purpose of the app is to predict the effect of various different factors like age, sex, glucose level, smoking, cholesterol and many others on diabetes. This app will help to determine what levels of different factors can be dangerous and a user can monitor his/her reports based on the reference level provided. 

Libraries required to run this project are : 
```{r}
library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(ggplot2)
library(caret)
library(shinycssloaders)
```

To install these libraries run the following code : 

```{r}
install.packages(c("shiny","tidyverse", "DT", "shinydashboard", "ggplot2", "caret", "shinycssloaders"))
```

To run this app, run the following code : 

```{r}
shiny::runGitHub("ST558Project4", "AniketWalimbe", ref="main")
```
