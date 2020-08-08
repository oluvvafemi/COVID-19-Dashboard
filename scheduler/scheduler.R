library(xml2)
library(rvest)
library(plotly)
library(RCurl)
library(countrycode)

library(gh)
library(caTools)
library(httr)
source("./tokens.R")

#be careful with this
Sys.setenv(GITHUB_PAT = git_token)
#--------Data Cleaning scheduled for 11.30pm everyday------------------------------


        #---------Scraping date from worldometer

worldometer_pg <- xml2::read_html("https://www.worldometers.info/coronavirus/#countries")
wom_tabless <- rvest::html_nodes(worldometer_pg, "table")
head(wom_tabless)
tbls_ls <- worldometer_pg %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)


df_wom <- tbls_ls[[1]][c("Country,Other", "TotalCases", "ActiveCases", "TotalRecovered", "TotalDeaths")][8:(dim(tbls_ls[[1]])[1]-8),]

colnames(df_wom) <- c("Country", "TotalCases", "ActiveCases", "TotalRecovered", "TotalDeaths")



# because we get data from 2 different sources,
#below we make sure the naming conventions are the same
source_names_wom <- c('CAR','Congo', 'DRC', 'Ivory Coast','Vatican City', 'St. Vincent Grenadines', 'Taiwan',
                  'UAE','UK', 'USA', 'Myanmar', 'Canada', 'Australia', 'China' )
unif_names_wom <- c('Central African Republic', 'Congo (Brazzaville)', 'Congo (Kinshasa)', "Cote d'Ivoire", 
                "Holy See", "Saint Vincent and the Grenadines", "Taiwan*", "United Arab Emirates",
                "United Kingdom", "US", "Burma", "Canada*", "Australia*", "China*")

for(i in 1:length(source_names_wom)){
    df_wom$Country[df_wom$Country==source_names_wom[i]] <- unif_names_wom[i]
}


#----------------getting data from John Hopkins------------------------------

repo_timeSeries_exists <- RCurl::url.exists("https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
total_timeSeries_data_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
death_timeSeries_data_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

if(repo_timeSeries_exists){
    df_timeSeries_total <- read.csv(total_timeSeries_data_url)
    df_death_timeSeries <- read.csv(death_timeSeries_data_url)
}


# because we get data from 2 different sources,
#below we make sure the naming conventions are the same
source_names_jh <- c('Faroe Islands', 'Greenland', 'French Guiana', 'French Polynesia', 'Guadeloupe', 'Mayotte',
          'New Caledonia', 'Reunion', 'Saint Barthelemy', 'St Martin', 'Martinique', 'Aruba',
          'Curacao', 'Sint Maarten', 'Bermuda', 'Cayman Islands', 'Channel Islands', 'Gibraltar',
          'Isle of Man', 'Montserrat', 'Anguilla', 'British Virgin Islands', 'Turks and Caicos Islands',
          'Falkland Islands (Malvinas)', 'Saint Pierre and Miquelon', 'Hong Kong', 'Macau', 'Bonaire, Sint Eustatius and Saba')
unif_names_jh <- c('Faeroe Islands', 'Greenland', 'French Guiana', 'French Polynesia', 'Guadeloupe', 'Mayotte',
            'New Caledonia', 'Réunion', 'St. Barth', 'Saint Martin', 'Martinique', 'Aruba', 'Curaçao',
            'Sint Maarten', 'Bermuda', 'Cayman Islands', 'Channel Islands', 'Gibraltar', 'Isle of Man',
            'Montserrat', 'Anguilla', 'British Virgin Islands', 'Turks and Caicos', 'Falkland Islands',
            'Saint Pierre Miquelon', 'Hong Kong', 'Macao', 'Caribbean Netherlands')

df_timeSeries_total$Country.Region <- as.character(df_timeSeries_total$Country.Region)
df_death_timeSeries$Country.Region <- as.character(df_death_timeSeries$Country.Region)

for(i in 1:length(source_names_jh)){
    df_timeSeries_total$Country.Region[df_timeSeries_total$Province.State==source_names_jh[i]] <- unif_names_jh[i]
    df_death_timeSeries$Country.Region[df_death_timeSeries$Province.State==source_names_jh[i]] <- unif_names_jh[i]
    
}


# Creating a row for World

df_timeSeries_total <- rbind(df_timeSeries_total, data.frame(Province.State="",Country.Region = "World", Lat='', Long='',
                                                             t(colSums(subset(df_timeSeries_total, select = -c(Province.State,Country.Region,
                                                                                                               Lat, Long))
                                                             ))))
df_death_timeSeries <- rbind(df_death_timeSeries, data.frame(Province.State="",Country.Region = "World", Lat='', Long='',
                                                             t(colSums(subset(df_death_timeSeries, select = -c(Province.State,Country.Region,
                                                                                                               Lat, Long))
                                                             ))))


# taking the main observations for these 3 countries

countries_lat_long <- list(list("Canada", "52.9399", "-73.5491"), 
                        list("Australia", "-31.950500", "115.860500"), 
                        list("China", "40.182400", "116.414200"))

for(i in 1:length(countries_lat_long)){
    df_death_timeSeries <- rbind(df_death_timeSeries, data.frame(Province.State="",Country.Region = paste0(countries_lat_long[[i]][[1]], '*'), 
                                                                 Lat=countries_lat_long[[i]][[2]], Long=countries_lat_long[[i]][[3]],
                                                                 t(colSums(subset(df_death_timeSeries, select = -c(Province.State,Country.Region,
                                                                                                                   Lat, Long), Country.Region==countries_lat_long[[i]][[1]])
                                                                 ))))
    df_timeSeries_total <- rbind(df_timeSeries_total, data.frame(Province.State="",Country.Region = paste0(countries_lat_long[[i]][[1]], '*'), 
                                                                 Lat=countries_lat_long[[i]][[2]], Long=countries_lat_long[[i]][[3]],
                                                                 t(colSums(subset(df_timeSeries_total, select = -c(Province.State,Country.Region,
                                                                                                                   Lat, Long), Country.Region==countries_lat_long[[i]][[1]])
                                                                 ))))
    df_timeSeries_total <- df_timeSeries_total[df_timeSeries_total$Country.Region!=countries_lat_long[[i]][[1]],]
    df_death_timeSeries <- df_death_timeSeries[df_death_timeSeries$Country.Region!=countries_lat_long[[i]][[1]],]
    
    }

# Removing commas from some names.
df_timeSeries_total$Country.Region[df_timeSeries_total$Country.Region=='Korea, South'] <- "S. Korea"
df_death_timeSeries$Country.Region[df_death_timeSeries$Country.Region=='Korea, South'] <- "S. Korea"

df_timeSeries_total$Province.State <- as.character(df_timeSeries_total$Province.State)
df_death_timeSeries$Province.State <- as.character(df_death_timeSeries$Province.State)

df_timeSeries_total$Province.State[df_timeSeries_total$Province.State=='Bonaire, Sint Eustatius and Saba'] <- "Bonaire Sint Eustatius and Saba"
df_death_timeSeries$Province.State[df_death_timeSeries$Province.State=='Bonaire, Sint Eustatius and Saba'] <- "Bonaire Sint Eustatius and Saba"


date_df <- as.Date(colnames(df_timeSeries_total)[5:dim(df_timeSeries_total)[2]],
                   format = "X%m.%d.%y")



total_confirmed <-  as.numeric(subset(df_timeSeries_total, select = c(dim(df_timeSeries_total)[2]))[,1])
total_deaths <- as.numeric(subset(df_death_timeSeries, select = c(dim(df_death_timeSeries)[2]))[,1])

#----create df to be used for map and adding country code

df_choropleth <- cbind(subset(df_timeSeries_total, select = c(2, 3, 4)), total_confirmed, total_deaths)

df_choropleth <- df_choropleth[df_choropleth$Country.Region!='World',]


df_choropleth$Code <-countrycode::countrycode(df_choropleth$Country.Region, origin = 'country.name', destination = 'iso3c') 

col_to_format <- c("TotalCases", "ActiveCases", "TotalRecovered", "TotalDeaths")

for(i in col_to_format){
    df_wom[[i]] <- as.numeric(gsub(",", "", df_wom[[i]]))
    df_wom[[i]] <- format(df_wom[[i]], big.mark = " ")
}


dfs_to_commit <- list(df_choropleth, df_timeSeries_total, df_death_timeSeries, df_wom )
url_df <- c("https://api.github.com/repos/oluvvafemi/COVID-19-Dashboard/contents/data/df_confirmed_cases_choropleth.csv",
            "https://api.github.com/repositories/285729177/contents/data/df_confirmed_cases_time_series.csv",
            "https://api.github.com/repositories/285729177/contents/data/df_deaths_time_series.csv",
            "https://api.github.com/repositories/285729177/contents/data/df_worldometer.csv")
paths_df <- c("data/df_confirmed_cases_choropleth.csv", "data/df_confirmed_cases_time_series.csv",
              "data/df_deaths_time_series.csv", "data/df_worldometer.csv")
names_df <- c("df_confirmed_cases_choropleth.csv", "df_confirmed_cases_choropleth.csv", 
              "df_deaths_time_series.csv","df_worldometer.csv")

for (i in 1:length(dfs_to_commit)){
    encode_df <-paste(capture.output(write.table(dfs_to_commit[[i]], quote=FALSE, row.names=FALSE , sep=",")),
                      collapse="\n")
    encode_df <- caTools::base64encode(encode_df)
    
    sha_df <- GET(url_df[i])
    sha_df <- sha_df[[4]][[1]][[3]][[7]][1]
    sha_df <- gsub("[[:punct:]]", "",  strsplit(sha_df, 'W/')[[1]][2])
    gh("PUT /repos/:owner/:repo/contents/:path", owner="oluvvafemi", repo = "COVID-19-Dashboard",
       path =paths_df[i], message = paste0("automatically updated ",names_df[i]),
       sha= sha_df, content=encode_df)
    
}

