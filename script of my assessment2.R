#Setup
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(httr, jsonlite, dplyr, ggplot2, purrr, sf, lubridate, tidyr, osmdata)


library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(purrr)
library(sf)
library(lubridate)
library(tidyr)
library(osmdata)



#Collect data of police stations' location
#Use the wrapper from r package
bb <- getbb ("london uk")
london_rst <- opq(bb)%>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf() 

london_police_points <- london_rst$osm_points %>% 
  select(name, geometry) 

london_police_polygons <- london_rst$osm_polygons %>%
  select(name, geometry)


st_write(london_police_points, "data/london_police_points.geojson")

st_write(london_police_polygons, "data/london_police_polygons.geojson")

#inspect the data
head(london_police_points)
head(london_police_polygons)




#Create London polygon for data collection with longitude and latitude

lat_min <- 51.38
lat_max <- 51.67
lng_min <- -0.56
lng_max <- 0.28

make_grid_polys <- function(lat_min, lat_max, lng_min, lng_max, step = 0.05) {
  
  lats <- seq(lat_min, lat_max, by = step)
  lngs <- seq(lng_min, lng_max, by = step)
  
  polys <- list()
  
  for (i in seq_len(length(lats) - 1)) {
    for (j in seq_len(length(lngs) - 1)) {
      
      poly <- paste(
        paste(lats[i],   lngs[j],   sep = ","),
        paste(lats[i],   lngs[j+1], sep = ","),
        paste(lats[i+1], lngs[j+1], sep = ","),
        paste(lats[i+1], lngs[j],   sep = ","),
        sep = ":"
      )
      
      polys <- append(polys, poly)
    }
  }
  
  polys
}

london_polys <- make_grid_polys(
  lat_min = 51.38,
  lat_max = 51.67,
  lng_min = -0.56,
  lng_max = 0.28,
  step = 0.07
)

length(london_polys)





#Collect London crime data

#Use a function to safely request JSON data from the API, and avoid crashing when errors or empty responses return
safe_get_json <- function(url) {
  res <- GET(url)
  status <- status_code(res)
  txt <- content(res, "text", encoding = "UTF-8")
  
  if (status != 200) {
    message("Status ", status, " for URL: ", url)
    return(data.frame())
  }
  
  if (grepl("^\\s*<", txt)) {
    message("HTML returned for URL: ", url)
    return(data.frame())
  }
  
  if (nchar(txt) < 2) {
    message("Empty response for URL: ", url)
    return(data.frame())
  }
  
  tryCatch(
    fromJSON(txt),
    error = function(e) {
      message("JSON parse error for URL: ", url)
      return(data.frame())
    }
  )
}

#Build the API request for a given polygon and month, then use the previous function to collect crime data
get_crime_poly <- function(poly, date) {
  url <- paste0(
    "https://data.police.uk/api/crimes-street/all-crime?",
    "poly=", URLencode(poly),
    "&date=", date
  )
  safe_get_json(url)
}

#Create a sequence of months needed
dates <- seq(as.Date("2020-01-01"), as.Date("2024-12-01"), by = "month")
dates <- format(dates, "%Y-%m")


#Loop through every polygon and month, collect the data and store it
all_results <- list()
counter <- 1

for (p in london_polys) {
  for (d in dates) {
    
    message("Fetching poly ", counter, "/", length(london_polys), " for date ", d)
    
    df <- get_crime_poly(p, d)
    
    if (nrow(df) > 0) {
      df$poly_id <- counter
      df$date <- d
      all_results[[length(all_results) + 1]] <- df
    }
    
    Sys.sleep(0.5)
  }
  
  counter <- counter + 1
}

crime_data <- bind_rows(all_results)

write.csv(crime_data, "data/london_crime_data.csv", row.names = FALSE)


#inspect the data
crime_data <- read.csv("data/london_crime_data.csv")
head(crime_data)





# data wrangling
# define the focused crime categories
crime_categories <- c(
  "all-crime",
  "anti-social-behaviour",
  "bicycle-theft",
  "criminal-damage-arson",
  "drugs",
  "possession-of-weapons",
  "public-order",
  "robbery",
  "violent-crime"
)

#get trend_data
crime_2024 <- crime_data %>%
  filter(
    category %in% crime_categories,
    substr(date, 1, 4) == "2024"
  ) %>%
  mutate(
    month = factor(substr(date, 6, 7), levels = sprintf("%02d", 1:12))
  )


trend_data <- crime_2024 %>%
  group_by(category, month) %>%
  summarise(count = n(), .groups = "drop")

write.csv(trend_data, "data/trend_data.csv", row.names = FALSE)

head(trend_data)


#get map_data
crime_2024_clean <- crime_2024 %>%
  rename(
    latitude = location.latitude,
    longitude = location.longitude
  ) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )


map_data <- crime_2024_clean %>%
  select(longitude, latitude, category, month)

write.csv(map_data, "data/map_data.csv", row.names = FALSE)

head(map_data)


#get ranking_data
ranking_data <- crime_2024_clean %>%
  mutate(
    street_clean = gsub("On or near ", "", location.street.name, perl = TRUE)
  ) %>%
  select(
    category,
    latitude,
    longitude,
    month,
    date,
    street_clean
  )

write.csv(ranking_data, "data/ranking_data.csv", row.names = FALSE)

head(ranking_data)



#function to rank the street/area
get_street_ranking <- function(crime_type,
                               month = "all",
                               top_n = 10) {
  
  df <- ranking_data
  
  df <- df %>% filter(category == crime_type)
  
  df %>%
    count(street_clean, sort = TRUE) %>%
    slice_head(n = top_n)
}





