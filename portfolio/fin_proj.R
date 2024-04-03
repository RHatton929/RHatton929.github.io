library(tidyverse)
library(janitor)
library(GGally)
library(leaflet)
library(readxl)
library(modelr)
library(easystats)

# Read in the data####

df <- read_csv("./ufo_data.csv") %>% 
  clean_names()

# Make sure all states/territories accounted for ####
continental_us_bounds <- list(
  xmin = -125.0,  # Westernmost point of the US (including Hawaii)
  xmax = -66.93457,  # Easternmost point of the US (excluding some territories)
  ymin = 24.396308,  # Southernmost point of the US (including Puerto Rico)
  ymax = 49.384358  # Northernmost point of the US (including Alaska)
)

alaska_hawaii_bounds <- list(
  xmin = -178.2166,  # Westernmost point of the US (including Hawaii)
  xmax = -129.9943,  # Easternmost point of the US (excluding some territories)
  ymin = 18.9117,  # Southernmost point of the US (excluding Puerto Rico)
  ymax = 71.5388  # Northernmost point of the US (excluding Alaska)
)

continental_us_data <- df %>%
  filter(latitude >= continental_us_bounds$ymin & latitude <= continental_us_bounds$ymax &
           longitude >= continental_us_bounds$xmin & longitude <= continental_us_bounds$xmax)

alaska_hawaii_data <- df %>%
  filter(latitude >= alaska_hawaii_bounds$ymin & latitude <= alaska_hawaii_bounds$ymax &
           longitude >= alaska_hawaii_bounds$xmin & longitude <= alaska_hawaii_bounds$xmax)

combined_data <- rbind(continental_us_data, alaska_hawaii_data)

view(filtered_data)


df <- df %>%
  mutate(country = case_when(
    state %in% c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "dc", "fl", 
                 "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me", 
                 "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", 
                 "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", 
                 "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy") ~ "us",
    TRUE ~ "other"
  ))

# Then select only the US

df <- df %>% 
  filter(country == "us")

unique(df$shape)

# Make sure the shapes are condensed and readable####

df <- df %>% 
  mutate(shape = case_when(
  shape %in% c("light", "fireball", "flash", "flare") ~ "light",
  shape %in% c("circle", "sphere", "egg", "oval", "disk", "round") ~ "spherical",
  shape %in% c("cylinder", "rectangle", "cigar") ~ "rectangular",
  shape %in% c("triangle", "pyramid") ~ "triangular",
  is.na(shape) | shape %in% c("unknown", "other", "changing", "changed") ~ "other",
  shape %in% c("delta", "chevron") ~ "delta",
  shape %in% c("diamond", "hexagon") ~ "diamond",
  shape %in% c("cone", "dome") ~ "cone",
  shape == "crescent" ~ "crescent",
  TRUE ~ as.character(shape)))

# Drop unecessary columns####

df <- df %>% 
  select(-duration_hours_min, -comments, -date_posted) #maybe drop city

# separate datetime into columns month/day/year/time

df <- df %>%
  separate(datetime, into = c("month", "day", "year", "time"), sep = "[ /]")

# which state has the most sightings?
 
df %>% 
  mutate(state = toupper(state)) %>% 
  filter(state!=is.na(state)) %>% 
  group_by(state) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(x = reorder(state, -N),
             y = N)) +
  geom_col(aes(fill = state)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4, 
                                   hjust = 1),
        plot.title = element_text(face = "bold", size = (15))) +
  labs(title = "Number of UFO Sightings by US State/Territory",
       x = "State",
       y = "Number of UFO Sightings") +
  guides(fill = "none")

# Which state has most sightings per capita?

state_data <- df %>% 
  mutate(state = toupper(state)) %>% 
  filter(state!=is.na(state)) %>% 
  group_by(state) %>% 
  summarize(N = n())

state_pop <- data.frame(
  state = toupper(c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "dc", "fl", 
                    "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me", 
                    "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", 
                    "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", 
                    "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy")),
  pop = c(4779736, 710231, 6392017, 2915918, 37253956, 5029196, 3574097, 897934, 601723, 18801310,
          9687653, 1360301, 1567582, 12830632, 6483802, 3046355, 2853118, 4339367, 4533372, 1328361,
          5773552, 6547629, 9883640, 5303925, 2967297, 5988927, 989415, 1826341, 2700551, 1316470,
          8791894, 2059179, 19378102, 9535483, 672591, 11536504, 3751351, 3831074, 12702379, 1052567,
          4625364, 814180, 6346105, 25145561, 2763885, 625741, 8001024, 6724540, 1852994, 5686986, 563626))

state_merge <- state_data %>% 
  left_join(state_pop, by = 'state')

state_merge <- state_merge %>% 
  mutate(percent_per_capita = (N/pop)*100)

state_merge %>% 
  ggplot(aes(x = reorder(state, -percent_per_capita),
             y = percent_per_capita)) +
  geom_col(aes(fill = state)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        plot.title = element_text(face = "bold", size = 15)) +
  labs(title = "UFO Sightings per Capita by US State/Territory",
       x = "State",
       y = "UFO Sightings per Capita") +
  guides(fill = "none")

# what shape of ufo is most commonly seen in each state? (add shape over state on map)
most_common_shape_by_state <- df %>%
  group_by(state, shape) %>%
  summarise(count = n()) %>%
  slice(which.max(count))

# with above, what is the most common shape in the us overall?
most_common_shape <- df %>%
  count(shape) %>%
  arrange(desc(n))

# what is the avg duration of a sighting in each state?
df %>%
  group_by(state) %>%
  summarize(avg_duration = mean(duration_seconds, na.rm = TRUE))

mean(df$duration_seconds, na.rm = TRUE)

# what month has the most sightings?
sightings_by_month <- df %>% 
  group_by(month) %>% 
  summarize(count = n())

ggplot(sightings_by_month, aes(x = month, y = count, fill = month)) +
  geom_col() +
  labs(title = "UFO Sightings per Month",
       x = "Month",
       y = "Number of Sightings") +
  theme_minimal() +
  theme(legend.position = "none")

# do sightings peak at certain times of year? times of month?

get_season <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

df <- df %>%
  mutate(season = sapply(month, get_season))

sightings_by_season <- df %>%
  group_by(season) %>%
  summarize(count = n())

ggplot(sightings_by_season, aes(x = season, y = count, fill = season)) +
  geom_col() +
  theme_minimal() +
  labs(title = "UFO Sightings by Season",
       x = "Season",
       y = "Number of Sightings",
       fill = "Season") +
  theme(legend.position = "none")

# how has frequency of UFO sightings changed over the years?
df %>%
  mutate(year = as.integer(year)) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  labs(title = "Frequency of UFO Sightings Over the Years")

# when appending to map, see if there is a relationship between rural/urban and sighting num

m <- leaflet(data = df) %>% 
  addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>%
  addCircleMarkers(lng = df$longitude,
                   lat = df$latitude,
                   radius = 5,
                   color = "lightgreen",
                   popup = ~paste("Shape:", str_to_title(df$shape),"<br>",
                                  "Duration (seconds):", df$duration_seconds, "<br>",
                                  "Duration (minutes):", df$duration_seconds/60, "<br>",
                                  "Latitude: ", df$latitude, "<br>",
                                  "Longitude: ", df$longitude))
m

# Add in military bases

mil_dat <- read_xlsx("military-bases.xlsx") %>% 
  clean_names()

names(mil_dat)

mil_dat <- mil_dat %>% 
  select(geo_point, component, site_name, oper_stat)

mil_dat <- mil_dat %>% 
  separate(geo_point, into = c("latitude", "longitude"), sep = ", ", convert = TRUE)

# Make another leaflet

# Start modeling ####

# create a model for sightings by year and state

sight_by_year <- df %>% 
  group_by(year, state) %>% 
  summarize(N = n())

mod1 <- glm(data = sight_by_year, 
            formula = N ~ year + state)

mod2 <- glm(data = sight_by_year, 
            formula = N ~ year * state)

compare_performance(mod1, mod2) %>% plot()

# model for sightings by year and lat/long

# is there a correlation between shape and duration?

# create a model to predict what the frequency would look like in 2024
