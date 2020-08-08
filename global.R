# library(rvest)
library(shinythemes)
library(plotly)


layout_params <- list(
    showgrid = TRUE,
    showticklabels = TRUE,
    gridcolor = 'rgb(41, 41, 41)'
)
layout_font<- list(
    family = "sans serif",
    size = 14,
    color = 'white')

radiobutton_trends <- c("Total Cases", "Daily New Cases", "Total Deaths", "Daily Deaths")


df_wom <-  read.csv("https://raw.githubusercontent.com/oluvvafemi/COVID-19-Dashboard/master/data/df_worldometer.csv")

country_vec <- df_wom[['Country']]

#-----------------------------------------------------------
df_timeSeries_total <- read.csv("https://raw.githubusercontent.com/oluvvafemi/COVID-19-Dashboard/master/data/df_confirmed_cases_time_series.csv")
df_death_timeSeries <- read.csv("https://raw.githubusercontent.com/oluvvafemi/COVID-19-Dashboard/master/data/df_deaths_time_series.csv")

date_df <- as.Date(colnames(df_timeSeries_total)[5:dim(df_timeSeries_total)[2]],
                   format = "X%m.%d.%y")


df_chloropleth <- read.csv("https://raw.githubusercontent.com/oluvvafemi/COVID-19-Dashboard/master/data/df_confirmed_cases_choropleth.csv")
