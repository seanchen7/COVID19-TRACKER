# COVID-19 Data Analysis
# Author: Sean Chen
# Data Source: New York Times, Census Bureau

# 0. Environment Setup  
library(data.table)
library(ggplot2)
library(stringr)
library(leaflet)
library(tigris)
library(htmlwidgets)
library(RColorBrewer)
library(leaftime)
library(RCurl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #Automatically set working directory
input_path <- "./Data//" #Input control
output_path <- "./Shiny//"

# 1. Geo Data -------------------------------------------------------------------------------------------------
# The following code is to pre-process geography data and doesn't need to be run every time
# # Census shape file for mapping  #(https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
# # get_counties <- function(states_list) {
# #   #Hold result for first state since arguement "state" cannot be blank
# #   counties_all <-  counties(state = states_list[[1]], cb=TRUE)
# #   #Iteration to get results for other states
# #   if (length(states_list)>1) {
# #     for (i in c(2:length(states_list))) {
# #       print(paste("Download shapefiles for state ", states_list[[i]]))
# #       counties_temp <- counties(state = states_list[[i]], cb=TRUE)
# #       counties_all <- rbind_tigris(counties_all,counties_temp)
# #     }
# #   }
# #   #Create FIPSCOUNTY code
# #   counties_all$FIPSCOUNTY <- paste0(str_pad(counties_all$STATEFP, 2, "left", pad="0"),
# #                                   str_pad(counties_all$COUNTYFP,3,"left", pad="0"))
# #   return(counties_all)
# # }
# # 
# # state_list = c(1, 4,5,6,8,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
# #                41,42,44,45,46,47,48,49,50,51,53,54,55,56) #48 states plus DC
# # counties_all <- get_counties(states_list)
# # states_all <- states(cb=TRUE)
# # save(counties_all, states_all,file= paste0(input_path, "geo_shape.RData"))
# load(paste0(input_path, "geo_shape.RData"))
# 
# # load(paste0(input_path,"Geo_data.Rdata")) 
# # load(paste0(input_path,"GEOCODEs.Rdata")) #this is not needed
# 
# 
# # County coordinates (https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2019_Gazetteer/2019_Gaz_cousubs_national.zip)
# county_geo <- data.table(read.csv(paste0(input_path, "2019_Gaz_counties_national.txt"), sep = "\t", 
#                                    header=TRUE, stringsAsFactors = FALSE))
# county_geo[,FIPSCOUNTY:= str_pad(GEOID,5,"left", pad="0")]
# 
# # MSA/MD/COUNTY crosswalk 
# #(https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls)
# data_temp <- read.csv(paste0(input_path, "list1_Sep_2018.csv"),stringsAsFactors = FALSE)
# MSAMD <- data.table(data_temp[,c("CBSA.Code","Metropolitan.Division.Code","CBSA.Title","Metropolitan.Division.Title",
#                                  "FIPS.State.Code","FIPS.County.Code")])
# setnames(MSAMD,c("msa","md","msa_name","md_name","state_code","county_code"))
# MSAMD[,msamd:=ifelse(is.na(md),msa,md)][,msamd_name:=ifelse(is.na(md),msa_name,md_name)][,msamd_id:=ifelse(is.na(md),"MSA","MD")]
# MSAMD[,FIPSCOUNTY := paste0(str_pad(state_code, 2, "left", pad="0"),str_pad(county_code,3,"left", pad="0"))]
# MSAMD[,(c("msa","msamd","md","state_code","county_code")) := lapply(.SD,as.character),.SDcols=c("msa","msamd","md","state_code","county_code")]
# 
# #FIPS code (https://www.census.gov/geographies/reference-files/2017/demo/popest/2017-fips.html)
# FIPS <- data.table(read.csv(paste0(input_path, "all-geocodes-v2017.csv"),stringsAsFactors = FALSE))
# setnames(FIPS, c("summary_level", "state_code", "county_code", "county_sub_code", "place_code", "city_code", "name"))
# FIPS[,FIPSCOUNTY := paste0(str_pad(state_code, 2, "left", pad="0"), str_pad(county_code,3,"left", pad="0"))]
# 
# #Census data (https://www.ffiec.gov/census/censusInfo.aspx)
# # census <-  read.csv(paste0(input_path, "Census2019.csv"),header=FALSE,stringsAsFactors = FALSE)
# # census <- data.table(census[,c(2:5, 15:19)])
# # setnames(census,c("MSAMD","state_code","county_code","census_tract_number",
# #                   "total_persons", "total_families" ,"total_household", "total_female", "total_male"))
# # census[,FIPSCOUNTY := paste0(str_pad(state_code, 2, "left", pad="0"),str_pad(county_code,3,"left", pad="0"))]
# # census[,GEOID:= paste0(FIPSCOUNTY,str_pad(census_tract_number,6,"left", pad="0"))]
# # save(census,file= paste0(input_path, "census.RData"))
# load(paste0(input_path, "census.RData"))
# 
# # Aggregate to county level
# census_agg <- census[, .(total_persons = sum(total_persons)), by = .(FIPSCOUNTY, state_code, county_code)]
# census_agg[, sqrt_persons:= sqrt(total_persons)]
# # census_agg <- merge(census_agg, unique(county_geo[, .(FIPSCOUNTY, NAME)]), all.x = T, by="FIPSCOUNTY")
# 
# 
# save(counties_all, states_all, county_geo, MSAMD, FIPS, census_agg,file= paste0(input_path, "GEODATA.RData"))

load(paste0(input_path, "GEODATA.RData"))
#----

# 2. COVID Data --------------------------------------------------------------------------------------------------

#NYT COVID-19 data

# case_raw <- data.table(read.csv("~/Documents/GitHub/covid-19-data/us-counties.csv", stringsAsFactors = FALSE))
case_raw <- data.table(read.csv(text = getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"), stringsAsFactors = FALSE))


# Processing data
case_raw[, date:= as.Date(date, tz = "UTC")]
case_raw[,FIPSCOUNTY:= str_pad(fips,5,"left", pad="0")]
# Fixing unknown counties
case_raw[county=="New York City", FIPSCOUNTY:="36061"] # New York City data exception
case_raw[county=="Kansas City", FIPSCOUNTY:="29095"] #Kansas city MO


#Date range and stats
last_update <- max(case_raw[,date])
date_range <- seq(min(case_raw[,date]), last_update, by="days")
total_case <- case_raw[date==last_update, sum(cases)]
total_deaths <- case_raw[date==last_update, sum(deaths)]

#Fill in NAs between dates
DT1 <- case_raw
setkey(DT1, FIPSCOUNTY, date)
DT2 <- DT1[DT1[, .(date = date_range), by=.(FIPSCOUNTY)],
              on=.(FIPSCOUNTY, date), roll = TRUE]
case_all <- na.omit(DT2[, .(date, FIPSCOUNTY, county, state, cases, deaths)])
case_all <- case_all[order(FIPSCOUNTY, date)]

# Calculate the daily increase
case_all[, case_delta:= cases - shift(cases), by = .(FIPSCOUNTY)]
case_all[is.na(case_delta), case_delta:=cases]
case_all[, death_delta:= deaths - shift(deaths), by = .(FIPSCOUNTY)]
case_all[is.na(death_delta), death_delta:=deaths]

# # Rolling delta
# case_all[, rolling_case:=cumsum(case_delta)/(1:.N), by = FIPSCOUNTY]

# # Check data
# summary(case_raw)
# case_all[FIPSCOUNTY=="36061"]


# Processing state level data
# state_raw <- data.table(read.csv("~/Documents/GitHub/covid-19-data/us-states.csv", stringsAsFactors = FALSE))
state_raw <- data.table(read.csv(text = getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"), stringsAsFactors = FALSE))

state_raw[, date:= as.Date(date, tz = "UTC")]

#Fill in NAs between dates
DT1 <- state_raw
setkey(DT1, fips, date)
DT2 <- DT1[DT1[, .(date = date_range), by=.(fips)],
           on=.(fips, date), roll = TRUE]
case_state <- na.omit(DT2)
case_state <- case_state[order(fips, date)]
# Calculate the daily increase
case_state[, case_delta:= cases - shift(cases), by = .(fips)]
case_state[is.na(case_delta), case_delta:=cases]
case_state[, death_delta:= deaths - shift(deaths), by = .(fips)]
case_state[is.na(death_delta), death_delta:=deaths]


# ----

# 3. Map Data ----

#Define geographies
state_list = c(1, 4,5,6,8,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
               41,42,44,45,46,47,48,49,50,51,53,54,55,56) #48 states plus DC
# state_list = c(34, 36, 42)
area <- FIPS[state_code %in% state_list]
counties_list = unique(area[,FIPSCOUNTY])

#Subset COVID data to specific area

# Latest data
DT1 <- case_all[FIPSCOUNTY %in% counties_list & date==last_update]
DT2 <- merge(x=DT1, y=county_geo[, .(FIPSCOUNTY, INTPTLAT, INTPTLONG)], all.x=TRUE, by="FIPSCOUNTY") #Merge with coordinates info
setnames(DT2, old=c("INTPTLAT","INTPTLONG"),new=c("lat","long"))
count1 <- as.data.frame(DT2)

# Time series
count2 <- case_all[FIPSCOUNTY %in% counties_list]
count2 <- merge(x=count2, y=county_geo[, .(FIPSCOUNTY, INTPTLAT, INTPTLONG)], all.x=TRUE, by="FIPSCOUNTY") #Merge with coordinates info
setnames(count2, old=c("INTPTLAT","INTPTLONG"),new=c("lat","long"))


#Subset spatial data
counties_shape <- counties_all[counties_all$FIPSCOUNTY %in% counties_list,]
states_shape <- states_all[states_all$STATEFP %in% str_pad(state_list,2,"left", pad="0"),]

# Combine county level data
DT1 = as.data.table(count1)
DT2 <- merge(census_agg[, .(FIPSCOUNTY, total_persons, sqrt_persons)], DT1[, .(FIPSCOUNTY, cases, deaths)],
             all.x = T, by = "FIPSCOUNTY")
geo_county <-  geo_join(counties_shape, DT2, "FIPSCOUNTY", "FIPSCOUNTY") #merged with census data

# Timestamp
radius_control <- 6
min_date <- "2020-03-01"
max_date <- "2020-05-01"

count3 <- count2[date>=min_date & date<max_date, .(lat, long, date, cases, deaths)]
count3[, date:=date+1] #adjusted for timezone difference
n_intervals1 = difftime(max(count3[, date]), min(count3[, date]), tz="UTC", units = c("hours"))
count3[, radius:= sqrt(cases)/radius_control]
count3[, start:=date][, end:= date]
count3 <- geojsonio::geojson_json(count3,lat="lat",lon="long")

count4 <- count2[date>=max_date, .(lat, long, date, cases, deaths)]
count4[, date:=date+1] #adjusted for timezone difference
n_intervals2 = difftime(max(count4[, date]), min(count4[, date]), tz="UTC", units = c("hours"))
count4[, radius:= sqrt(cases)/radius_control]
count4[, start:=date][, end:= date]
count4 <- geojsonio::geojson_json(count4,lat="lat",lon="long")


# ----

# 4. Export data for Shiny app ----
case_all <- case_all[date==last_update] #Limit memory use
case_all <- merge(case_all, census_agg[, .(FIPSCOUNTY, total_persons)], by = 'FIPSCOUNTY', all.x = T)
case_all[, case_pc:=cases/total_persons]

save(case_all, case_state, last_update, total_case, total_deaths, file= paste0(output_path, "COVID_Case.RData"))

save(geo_county, counties_shape, states_shape, count1, count3, count4,  n_intervals1, n_intervals2, 
     file= paste0(output_path,"MAP_data.RData"))

#----

