#### Load libraries ####
library(shinythemes)
library(tidyr)
library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("leaflet")
library(leaflet)
library(magrittr)
library(sp)
library(RColorBrewer)
#install.packages("DT")
library(DT)
#install.packages("highcharter")
#library(highcharter)
library(plyr)
library(scales)
#library(shinyjs)
#install.packages("plotrix")
library(plotrix)

if(!exists("crime_by_city_long")){
  load("env_data.RData")
}

# or_red_pal_1 = c("#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026")
# or_red_pal_2 = c("#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026")
# 
# us_states_coordinates = read.csv("us_states_coord.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# # require(devtools)
# # if(!suppressWarnings(requireNamespace('albersusa', quietly = TRUE))) {
# #   devtools::install_github('hrbrmstr/albersusa')
# # }
# library(albersusa)
# library(dplyr)
# library(tidyr)
# library(rvest)
# library(geojsonio)
# library(tm)
# 
# #### Load Data ####
# state_abbr = read.csv("city_state_abbr.csv", check.names = FALSE, stringsAsFactors = FALSE)
# hate_crime_table = read.csv("table13_cities.csv", header = TRUE, stringsAsFactors = FALSE)
# zero_crime_table = read.csv("table14_cities.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# #View(hate_crime_table)
# 
# #### Load Statutes ####
# statutes_by_state_by_enhancement = read.csv("statutes_by_state_by_penalty_enhancement.csv", header = TRUE, stringsAsFactors = FALSE)
# statutes_by_state_by_enhancement_2 = read.csv("statutes_by_state_by_penalty_enhancement_all.csv", header = TRUE, stringsAsFactors = FALSE)
# statutes_by_state_other_types = read.csv("statutes_by_state_other_types.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# statutes_by_state_by_enhancement$state = gsub("New \nHampshire", "New Hampshire", statutes_by_state_by_enhancement$state)
# statutes_by_state_by_enhancement$state = gsub("DC", "District of Columbia", statutes_by_state_by_enhancement$state)
# 
# statutes_by_state_other_types$state = gsub("New \nHampshire", "New Hampshire", statutes_by_state_other_types$state)
# statutes_by_state_other_types$state = gsub("DC", "District of Columbia", statutes_by_state_other_types$state)
# 
# statutes_by_state_by_enhancement$state = stripWhitespace(statutes_by_state_by_enhancement$state)
# statutes_by_state_other_types$state = stripWhitespace(statutes_by_state_other_types$state)
# 
# hate_crime_table$state = stripWhitespace(hate_crime_table$state)
# zero_crime_table$state = stripWhitespace(zero_crime_table$state)
# 
# trimWhitespace = function (x) gsub("^\\s+|\\s+$", "", x)
# 
# statutes_by_state_by_enhancement$state = trimWhitespace(statutes_by_state_by_enhancement$state)
# statutes_by_state_other_types$state = trimWhitespace(statutes_by_state_other_types$state)
# 
# hate_crime_table$state = trimWhitespace(hate_crime_table$state)
# zero_crime_table$state = trimWhitespace(zero_crime_table$state)
# 
# hate_crime_table$state = gsub("District Of Columbia", "District of Columbia", hate_crime_table$state)
# zero_crime_table$state = gsub("District Of Columbia", "District of Columbia", zero_crime_table$state)
# 
# for(i in 1: length(unique(hate_crime_table$state))){
#   cat("\n", i, grep(unique(hate_crime_table$state)[i], statutes_by_state_by_enhancement$state, value = TRUE))
#   cat(", ", grep(unique(hate_crime_table$state)[i], statutes_by_state_other_types$state, value = TRUE))
# }
# 
# for(i in 1: length(unique(zero_crime_table$state))){
#   cat("\n", i, grep(unique(zero_crime_table$state)[i], statutes_by_state_by_enhancement$state, value = TRUE))
#   cat(", ", grep(unique(zero_crime_table$state)[i], statutes_by_state_other_types$state, value = TRUE))
# }
# 
# #### Get All States ####
# states = c("All states", unique(statutes_by_state_by_enhancement$state))
# 
# set.seed(1)
# 
# # Shuffle the vector of colors (this palette is color blind friendly)
# state_colors = sample(c("#5b7fee", "#93c049", "#5b2282", "#46ca79", "#a43f9f", "#6ac262",
#                         "#293892", "#b0b243", "#5b56bc", "#4d912c", "#bc7ae2", "#4d7522",
#                         "#e177d4", "#49c48e", "#a83987", "#7cbf76", "#70226a", "#a8bd69",
#                         "#3e2d71", "#d19b2e", "#5888e0", "#c36825", "#1fe1fb", "#d4424c",
#                         "#45c1a6", "#c8345c", "#347435", "#df6bb9", "#797220", "#9f85e3",
#                         "#c89a45", "#7b51a7", "#d1a261", "#455da4", "#bc452b", "#7297de",
#                         "#e48055", "#8e59a0", "#955923", "#d196e2", "#802716", "#d383c2",
#                         "#c46d53", "#c34587", "#e77162", "#852956", "#d56467", "#de79a4",
#                         "#872232", "#ce3f78", "#ca5e71"))
# 
# #### Get All Years ####
# years = sort(as.integer(as.character(unique(hate_crime_table$date))))
# 
# #### Create a Custom Table Container for Box 3 (Tab 2) ####
# sketch = htmltools::withTags(table(
#   class = 'display',
#   thead(
#     tr(
#       th(rowspan = 2, "State"),
#       th(rowspan = 2, "Penalty Enhancement"),
#       th(colspan = 6, "Penalty Enhancement for Crimes Motivated by:")
#     ),
#     tr(
#       lapply(c('Race/ Religion/ Ethnicity', 'Sexual Orientation', 'Gender', 'Gender Identity',
#                'Disability', 'Other'), th)
#     )
#   )
# ))
# 
# #print(sketch)
# 
# #### Load and merge data ####
# 
# anti_jewish_muslim = read.csv("Anti-Jewish-Muslim-Data.csv", header = TRUE, stringsAsFactors = FALSE)
# merged = merge(hate_crime_table, anti_jewish_muslim, by = c("date", "state", "city", "latitude", "longitude"), all = TRUE)
# #View(merged)
# 
# #[, c(1:4,7)]
# 
# missing = unique(merged[which(is.na(merged$state)), "city"])
# 
# for(i in 1:length(missing)){
#   print(grep(missing[i], hate_crime_table$city)[1])
#   merged[grep(missing[i], merged$city), "state"] = hate_crime_table[grep(missing[i], hate_crime_table$city)[1], "state"]
#   merged[grep(missing[i], merged$city), "latitude"] = hate_crime_table[grep(missing[i], hate_crime_table$city)[1], "latitude"]
#   merged[grep(missing[i], merged$city), "longitude"] = hate_crime_table[grep(missing[i], hate_crime_table$city)[1], "longitude"]
# }
# 
# # missing = unique(merged[intersect(which(is.na(merged$latitude)), which(!is.na(merged$Anti_Jewish))), "city"])
# #
# # for(i in 1:length(missing)){
# #   #print(grep(missing[i], hate_crime_table$city)[1])
# #   merged[grep(missing[i], merged$city), "latitude"] = anti_jewish_muslim[grep(missing[i], anti_jewish_muslim$city)[1], "latitude"]
# #   merged[grep(missing[i], merged$city), "longitude"] = anti_jewish_muslim[grep(missing[i], anti_jewish_muslim$city)[1], "longitude"]
# # }
# 
# hate_crime_table = merged
# 
# rm(missing, merged)
# 
# #View(hate_crime_table)
# 
# state_hate_crime_totals_final = read.csv("state_hate_crime_totals_final.csv", header = TRUE, stringsAsFactors = FALSE)
# #View(state_hate_crime_totals_final)
# 
# #View(hate_crime_table[, c(1:3, 32, 33)])
# 
# summed_anti_jewesh_muslim = aggregate(cbind(Anti_Jewish, Anti_Islamic) ~ date + state, data = hate_crime_table[, c(1:3, 32, 33)], sum)
# 
# #View(summed_anti_jewesh_muslim)
# 
# merged = merge(state_hate_crime_totals_final, summed_anti_jewesh_muslim, by = c("date", "state"), all = TRUE)
# #View(merged)
# 
# merged = merged[, -4]
# 
# state_hate_crime_totals_final = merged
# #View(state_hate_crime_totals_final)
# 
# #View(hate_crime_table)
# 
# #### Vector of Unique Crime Types ####
# colnames(hate_crime_table)[c(32, 33, 9, 16, 15, 13, 11, 10, 17)] = c("Anti Jewish", "Anti Muslim", "Disability", "Gender",
#                                                                    "Gender Identity", "Race", "Sexual Orientation",
#                                                                    "Ethnicity", "Race/Ethnicity")
# 
# crime_types = colnames(hate_crime_table)[c(32, 33, 9, 16, 15, 13, 11, 10, 17)]
# 
# #### Data Frame With Crime Types & Colors ####
# type_colors = data.frame(crime_type = crime_types, color = brewer.pal(9, "Set3"), stringsAsFactors = FALSE)
# 
# #### Aggregate all data from table13 by city and by year ####
# crime_by_city = hate_crime_table[,c(1:5, 9:11, 13, 15:17, 32:33)]
# #View(crime_by_city)
# 
# crime_by_city = crime_by_city[,c(1:5,13:14,6,11:7,12)]
# 
# # crime_by_city_summed = cbind(crime_by_city[, c(1:5)],
# #                              total_crimes = rowSums(crime_by_city[, crime_types],
# #                                                     na.rm = TRUE))
# 
# #View(crime_by_city_summed)
# #View(crime_by_city)
# #View(crime_by_city_long)
# crime_by_city_long = gather(crime_by_city,
#                             key = crime_type,
#                             value = crime_count,
#                             na.rm = FALSE,
#                             `Anti Jewish`, `Anti Muslim`, Disability, Gender,
#                             `Gender Identity`, Race, `Sexual Orientation`,Ethnicity, `Race/Ethnicity`)
# 
# #View(crime_by_city_long)
# 
# #nchar(crime_by_city_long$city[129])
# 
# crime_by_city_long = crime_by_city_long[-which(nchar(crime_by_city_long$city) < 2), ]
# 
# #### Read in the 2004-2009 Anti-Jewish/Anti-Islamic Data ####
# additional_data = read.csv("Anti_Jewish_Islamic_summary_long_4.csv", header = TRUE, stringsAsFactors = FALSE)
# head(additional_data)
# 
# temp = additional_data[, c(4, 6, 1, 8, 9, 3, 5)]
# head(temp)
# 
# colnames(temp) = c("date", "state", "city", "latitude", "longitude", "crime_type", "crime_count")
# head(temp)
# 
# temp$crime_type = gsub("Anti-Jewish", "Anti Jewish", temp$crime_type)
# temp$crime_type[grep("Anti-Islamic", temp$crime_type)] = "Anti Muslim"
# head(temp)
# 
# additional_data = temp
# head(crime_by_city_long)
# 
# crime_by_city_long[crime_by_city_long$crime_type == "Anti Jewish", ]
# crime_by_city_long[crime_by_city_long$crime_type == "Anti Muslim", ]
# 
# only_jewish_muslim_before_2010 = intersect(which(crime_by_city_long$date < 2010),
#                                            union(which(crime_by_city_long$crime_type == "Anti Jewish"),
#                                                  which(crime_by_city_long$crime_type == "Anti Muslim")))
# 
# temp = rbind(additional_data, crime_by_city_long[-only_jewish_muslim_before_2010, ])
# 
# crime_by_city_long = temp
# 
# head(crime_by_city_long)
# 
# # row.names(temp) = paste0("row_", c(1:nrow(temp)))
# # nrow(temp)
# # unique_df = unique(temp[, c(1:3, 6)])
# # keep = as.integer(unlist(strsplit(row.names(unique_df), split = "row_"))[which(unlist(strsplit(row.names(unique_df), split = "row_")) != "")])
# 
# #### Aggregate all data from table13 by state and by year ####
# 
# head(state_hate_crime_totals_final)
# 
# crime_by_state = state_hate_crime_totals_final[, c(1, 2, 10, 11, 6, 7, 8, 3, 4, 5, 9)]
# head(crime_by_state)
# 
# colnames(crime_by_state)[c(3:11)] = c("Anti Jewish", "Anti Muslim", "Disability", "Gender",
#                                       "Gender Identity", "Race", "Sexual Orientation", "Ethnicity", "Race/Ethnicity")
# 
# crime_by_state$Gender = as.integer(crime_by_state$Gender)
# 
# crime_by_state_summed = cbind(crime_by_state[, c(1,2)],
#                               total_crimes = rowSums(crime_by_state[, crime_types],
#                                                      na.rm = TRUE))
# 
# crime_by_state_long = gather(crime_by_state,
#                              key = crime_type,
#                              value = crime_count,
#                              `Anti Jewish`, `Anti Muslim`, Disability, Gender,
#                              `Gender Identity`, Race, `Sexual Orientation`,Ethnicity, `Race/Ethnicity`)
# 
# head(crime_by_state_long)
# 
# crime_national = read.csv("Nationwide Hate Crime Totals 2015_2004.csv", check.names = F, stringsAsFactors = F)
# crime_national_long = gather(crime_national,
#                              key = crime_type,
#                              value = crime_count,
#                              Total, `Anti Jewish`, `Anti Muslim`, Disability, Gender,
#                              `Gender Identity`, Race, `Sexual Orientation`,Ethnicity, `Race/Ethnicity`)
# 
# 
# additional_data_by_state = aggregate(crime_count ~ date + state + crime_type, data = additional_data, sum)
# 
# head(additional_data_by_state)
# 
# only_jewish_muslim_before_2010 = intersect(which(crime_by_state_long$date < 2010),
#                                            union(which(crime_by_state_long$crime_type == "Anti Jewish"),
#                                                  which(crime_by_state_long$crime_type == "Anti Muslim")))
# 
# temp = rbind(additional_data_by_state, crime_by_state_long[-only_jewish_muslim_before_2010, ])
# 
# crime_by_state_long = temp
# 
# rm(temp, additional_data_by_state, additional_data, only_jewish_muslim_before_2010)
# 
# #### Get composite map of USA ####
# composite_usa_map = usa_composite()
# #bounds = c(-125, 24 ,-75, 45)
# 
# #### US States for Dashboard 1 ####
# us_states_geo = geojsonio::geojson_read("stateData.geojson", what = "sp")
# 
# #### Additional data ####
# map_df = data.frame(state = states[-1],
#                     stringsAsFactors = FALSE)
# 
# map2_df = data.frame(state = composite_usa_map@data$name,
#                      stringsAsFactors = FALSE)
# 
# state_coordinates = read.csv("us_states_coord.csv", header = TRUE, stringsAsFactors = FALSE)
# state_bounding_boxes = read.csv("us_states_bounding_boxes.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# statutes_statistics = data.frame(state = statutes_by_state_by_enhancement$state,
#                                  YES = rep(0, nrow(statutes_by_state_by_enhancement)),
#                                  NO = rep(0, nrow(statutes_by_state_by_enhancement)),
#                                  stringsAsFactors = FALSE)
# 
# for(i in 1:nrow(statutes_statistics)){
#   statutes_statistics[i, "YES"] = length(which(statutes_by_state_by_enhancement[i,] == "YES"))
#   statutes_statistics[i, "NO"] = length(which(statutes_by_state_by_enhancement[i,] == "NO"))
# }
# 
# yes_pal = data.frame(YES = c(0:6),
#                      yes_color = c("#FF6666", "#FFB266", "#FFFF66", "#B2FF66", "#66FF66", "#66FFB2", "#66FFFF"),
#                      stringsAsFactors = FALSE)
# 
# statutes_statistics = join(statutes_statistics, yes_pal, by = "YES")
# 
# #### Data Frame With Crime Types & Colors ####
# 
# crime_types_table_3 = c("All", "Disability", "National Origin", "Gender", "Gender Identity", "Other", "Race", "Religion", "Sexual Orientation")
# type_colors_2 = data.frame(crime_type = crime_types_table_3, color = brewer.pal(9, "Set3"), stringsAsFactors = FALSE)
# 
# statute_types = data.frame(type = c(type_colors_2$crime_type),
#                            statute_column = c(9, 7, 3, 5, 6, 8, 3, 3, 4),
#                            statute_color = "#00FF00",
#                            #statute_color = c("#8DD3C7", "#FB8072", "#BEBADA", "#FB8072", "#FB8072", "#FDB462", "#B3DE69", "#FB8072", "#009999"),
#                            stringsAsFactors = FALSE)
# 
# dnr_zero = read.csv("DNR_zero.csv", header = TRUE, stringsAsFactors = FALSE)
# zero_select = data.frame(dnr_zero[grep("Reported 0 in at least 1 quarter", dnr_zero$dnr_zero),c(1,5,6)], 1)
# zero_summary = ddply(zero_select[,c(1,2,4)], .(date, state), colwise(sum))
# colnames(zero_summary) = c("date", "state", "total_crimes")
# 
# 
# population_est = read.csv("pop_estimates_by_state.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# population_est = gather(population_est,
#                         key = year,
#                         value = population,
#                         -c(id, state))
# 
# population_est$year = gsub("X", "", as.character(population_est$year))
# population_est$year = as.integer(population_est$year)
# 
# population_est$population = gsub(",", "", population_est$population)
# population_est$population = as.integer(population_est$population)
# 
# statues_tooltip_2 = data.frame(label = rep("", length(map2_df$state)),
#                                num_yes = rep(0, length(map2_df$state)),
#                                stringsAsFactors = FALSE)
# 
# colors_for_map_3 = data.frame(num_yes = c(0:8),
#                               color = c("#E52955", "#E529A6", "#C629E5", "#7A29E5", "#292FE5", "#2981E5", "#29DFE5", "#29E5B9", "#29E548"),
#                               stringsAsFactors = FALSE)
# 
# for(i in 1:length(map2_df$state)){
#   #i = 1
#   tooltip_data = statutes_by_state_by_enhancement[statutes_by_state_by_enhancement$state == as.character(map2_df$state[i]),]
#   all_yes = grep("YES", tooltip_data)
#   statues_tooltip = unlist(lapply(all_yes, function(x){
#     statute_types$type[statute_types$statute_column == x]
#   }))
# 
#   statues_tooltip_2$label[i] = paste(statues_tooltip, collapse = "<br/>")
#   statues_tooltip_2$num_yes[i] = length(statues_tooltip)
# }
# 
# statues_tooltip_2 = join(statues_tooltip_2, colors_for_map_3, by = "num_yes")
# 
# statues_tooltip_2$label[which(map2_df$state == "Utah")] = "The Utah statute ties penalties </br>for hate crimes to violations of </br>the victim's constitutional or </br>civil rights"
# statues_tooltip_2$color[which(map2_df$state == "Utah")] = "#A0A0A0"
# 
# save.image(file = "env_data.RData")
