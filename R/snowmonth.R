library(rprojroot)
library(stringi)
library(hrbrthemes)
library(tidyverse)

pre <- find_rstudio_root_file()

# Get and read in Maine precip ------------------------------------------------------

URL <- "http://cdiac.ornl.gov/ftp/ushcn_daily/state17_ME.txt.gz"
fil <- file.path(pre, "data", basename(URL))
if (!file.exists(fil)) download.file(URL, fil)

read_fwf(file = fil,
         col_positions = fwf_widths(c(6, 4, 2, 4, rep(c(5, 1, 1, 1), 31)),
                                    col_names = c("coop_id", "year", "month", "element",
                                                  flatten_chr(map(1:31, ~paste("r_", c("v", "fm", "fq", "fs"),
                                                                               .x, sep=""))))),
         col_types = paste0("ciic", paste0(rep("iccc", 31), collapse=""), collapse=""),
         na = c("", "NA", "-", "-9999")) %>%
  gather(day, value, starts_with("r_v")) %>%
  select(-starts_with("r_")) %>%
  mutate(day = as.numeric(stri_replace_first_fixed(day, "r_v", ""))) %>%
  mutate(date = sprintf("%s-%02d-%02d", year, month, day)) -> daily_wx

# Read in stations ------------------------------------------------------------------

URL <- "http://cdiac.ornl.gov/ftp/ushcn_daily/ushcn-stations.txt"
fil <- file.path(pre, "data", basename(URL))
if (!file.exists(fil)) download.file(URL, fil)

read_fwf(file = file.path(pre, "data", "ushcn-stations.txt"),
         col_positions = fwf_widths(c(6, 9, 10, 7, 3, 31, 7, 7, 7, 3),
                                    col_names = c("coop_id", "latitude", "longitude",
                                                  "elevation", "state", "name",
                                                  "component_1", "component_2",
                                                  "component_3", "utc_offset")),
         col_types = "cdddcccccc") -> stations

closestStation <- function(stations, lat, lon, restrict_to = NULL) {
  if (!is.null(restrict_to)) stations <- filter(stations, state == restrict_to)
  index <- which.min(sqrt((stations$latitude-lat)^2 +
                            (stations$longitude-lon)^2))
  stations[index,]
}

# compute total snow amounts per month ----------------------------------------------

(near_me <- closestStation(stations, 43.2672, -70.8617, restrict_to="ME"))

filter(daily_wx, coop_id == near_me$coop_id, element=="SNOW", value>0) %>%
  count(year, month, wt=value) %>%
  ungroup() %>%
  mutate(
    n = n / 10, # readings are in 10ths of inches
    date = as.Date(sprintf("%s-%02d-01", year, month)),
    month_name = lubridate::month(date, TRUE, FALSE)
  ) %>%
  ggplot(aes(x=date, y=n)) +
  geom_segment(aes(xend=date, yend=0), size=0.75, color="#9ecae1") +
  scale_y_continuous(limits=c(0, 65)) +
  facet_wrap(~month_name, ncol=3, drop=FALSE, scales="free") +
  labs(x=NULL, y="inches", title="Total snowfall in a given month by year",
       subtitle="Data for Station id 176905 — Portland (Maine) Jetport") +
  theme_ipsum_rc(grid="Y", axis_text_size=8)

filter(daily_wx, coop_id == near_me$coop_id, element=="SNOW", value>0) %>%
  count(year, month, wt=value) %>%
  ungroup() %>%
  mutate(n = n / 10) %>%
  complete(year, month=1:12) %>%
  mutate(
    date = as.Date(sprintf("%s-%02d-01", year, month)),
    month_name = factor(lubridate::month(date, TRUE, FALSE), levels=rev(month.name))
  ) %>%
  ggplot(aes(year, month_name)) +
  geom_tile(aes(fill=n), color="#b2b2b2", size=0.15) +
  scale_x_continuous(expand=c(0,0.15), position="top") +
  viridis::scale_fill_viridis(name = "Total inches", na.value="white") +
  labs(x=NULL, y=NULL, title="Total snowfall in a given month by year",
       subtitle="Data for Station id 176905 — Portland (Maine) Jetport") +
  theme_ipsum_rc(grid="", axis_text_size = 10) +
  guides(fill=guide_colourbar(label.position = "top", direction = "horizontal", title.vjust = 0)) +
  theme(legend.title = element_text(size=10)) +
  theme(legend.key.height = unit(0.5, "lines")) +
  theme(legend.position = c(0.9, 1.25))

