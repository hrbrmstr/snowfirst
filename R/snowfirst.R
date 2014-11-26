library(pbapply)
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(stringi)

# United States Historical Climatology Network Daily Dataset Archive:
# http://cdiac.ornl.gov/ftp/ushcn_daily/

# Station file format:
# http://cdiac.ornl.gov/ftp/ushcn_daily/station_file_format.txt

stations <- read.fwf("data/ushcn-stations.txt",
                     widths=c(6, 9, 10, 7, 3, 31, 7, 7, 7, 3),
                     col.names=c("coop_id", "latitude", "longitude", "elevation",
                                 "state", "name", "component_1", "component_2",
                                 "component_3", "utc_offset"),
                     colClasses=c("character", "numeric", "numeric", "numeric",
                                  "character", "character", "character", "character",
                                  "character", "character"),
                     comment.char="", strip.white=TRUE)

# not a great circle, but it gets the job done here
closestStation <- function(stations, lat, lon) {
  index <- which.min(sqrt((stations$latitude-lat)^2 +
                          (stations$longitude-lon)^2))
  stations[index,]
}

# what's the closest station?
closestStation(stations, 43.2672, -70.8617)

# hrm...what about the closest station in Maine?
closestStation(stations %>% filter(state=="ME"), 43.2672, -70.8617)

# we'll eventually show both, but go with the actual closes one first

snow <- readLines("data/state17_ME.txt")

# we aren't using read.fwf since the records are wide and we want them long
# so we just read in the records line-by-line
# snow <- readLines("data/state27_NH.txt")

# we're only looking for SNOW records
snow <- grep("SNOW", snow, value=TRUE)

# and, we're only looking for a particular station
snow <- grep("^176905", snow, value=TRUE)

# now that we have those, we'll make our data.table by iterating over each
# SNOW record and build a data.table from the daily records, then combine
# all of the SNOW records into one data.table

snow_dat <- rbindlist(pblapply(snow, function(x) {

  rbindlist(lapply(1:31, function(i) {

    # record format described here:
    # http://cdiac.ornl.gov/ftp/ushcn_daily/data_format.txt

    start <- 17 + (i-1)*8

    list(coop_id=substr(x, 1, 6),
         date=sprintf("%s-%02d-%02d", substr(x, 7, 10), as.numeric(substr(x, 11, 12)), i),
         element=substr(x, 13, 16),
         value=as.numeric(substr(x, start, start+4)),
         mflag=substr(x, start+5, start+5),
         qflag=substr(x, start+6, start+6),
         sflag=substr(x, start+7, start+7))

  }))

}))

# -9999 indicates no data or invalid data, so ignore those records
# we could also make them NA, but that's not necessary for this exercise

snow_dat <- snow_dat %>% filter(value != -9999)

# since the data file has 31 days for each records regardless of whether
# that's valid or not we do a shortcut to remove invalid dates by doing the
# a vectorized Date conversion, then removing records with NA dates

snow_dat$date <- as.Date(snow_dat$date)
snow_dat <- snow_dat %>% filter(!is.na(date))

# having the year extracted is handy for filtering

snow_dat$year <- format(snow_dat$date, "%Y")

# since the Winter season in the US spans years, we have to work with the
# "day of year" and then munge it for Jan-May dates (we assume the first snowfall
# was never in June, but is prbly not not be true for Alaska, so user-beware)
snow_dat$doy <- as.numeric(format(snow_dat$date, "%j"))
snow_dat$doy <- ifelse(snow_dat$doy<=180,
                       snow_dat$doy + as.numeric(format(as.Date(sprintf("%s-12-31", snow_dat$year)), "%j")),
                       snow_dat$doy)

# now the fun begins

first <- snow_dat %>%
  filter(value>0) %>%                           # ignore 0 values
  filter(date>=as.Date("1950-01-01")) %>%       # start at 1950 (arbitrary)
  merge(stations, by="coop_id", all.x=TRUE) %>% # merge station details
  group_by(coop_id, year) %>%                   # group by station and year
  arrange(doy) %>%                              # sort by our munged day of year
  filter(row_number(doy) == 1) %>%              # grab the first entry by group
  select(name, state, date, value, doy)         # we only need some variables

# make a nice title for the visualization
title_1 <- sprintf("First observed snowfall (historical) at %s, %s", stri_trans_totitle(unique(first$name)), unique(first$state))

# an overused plot by me, but I really like the dot-line charts and we
# add a twist by using a snowflake for the point and use icy blue colors
gg <- ggplot(first, aes(y=year, x=doy))
gg <- gg + geom_segment(aes(xend=min(first$doy)-20, yend=year), color="#9ecae1", size=0.25)
gg <- gg + geom_point(aes(color=coop_id), shape=8, size=3, color="#3182bd")
gg <- gg + geom_text(aes(label=format(date, "%b-%d")), size=3, hjust=-0.2)
gg <- gg + scale_x_continuous(expand=c(0, 0), limits=c(min(first$doy)-20, max(first$doy)+20))
gg <- gg + labs(x=NULL, y=NULL, title=title_1)
gg <- gg + theme_bw()
gg <- gg + theme(legend.position="none")
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.ticks.x=element_blank())
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(axis.text.y=element_text(color="#08306b"))
by_year <- gg

# we're going to pair the dot-line plot with a boxplot, but I also want to
# give some indication of the most likely range for the first snowfall, so
# we grab the quartiles via summary and use them to annotate the second graph
wx_range <- summary(as.Date(format(first$date, "2013-%m-%d")))
names(wx_range) <- NULL
min_wx <- gsub("2013-", "", wx_range[2])
max_wx <- gsub("2013-", "", wx_range[5])
title_2 <- sprintf("Most likely first snowfall will be between %s & %s", min_wx, max_wx)

# we use a trick to line up the box plot with the dot-line plot by
# using the same character width y-axis labels but making them the background
# color (in this case, white) and keeping the x-axis limits the same. there
# may be another way to do this but this is quick, and easy to remember.
# a violin plot would work here as well
gg <- ggplot(first %>% mutate(name="0000"), aes(name, doy))
gg <- gg + geom_boxplot(fill="#3182bd", color="#08306b", outlier.colour="#08306b")
gg <- gg + scale_y_continuous(expand=c(0, 0),
                              limits=c(min(first$doy)-20, max(first$doy)+20))
gg <- gg + coord_flip()
gg <- gg + labs(x=NULL, y=NULL, title=title_2)
gg <- gg + theme_bw()
gg <- gg + theme(legend.position="none")
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.ticks.x=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(axis.ticks.y=element_line(color="white"))
gg <- gg + theme(axis.text.y=element_text(color="white"))
gg <- gg + theme(plot.title=element_text(size=11))
box_wx <- gg

# final presentation
grid.arrange(by_year, box_wx, nrow=2, heights=unit(c(0.9, 0.1), "npc"))


# REPEAT THE ANALYSIS FOR MAINE AND THEN DO THE FOLLOWING
# nh <- arrangeGrob(by_year, box_wx, nrow=2, heights=unit(c(0.9, 0.1), "npc"))
# me <- arrangeGrob(by_year, box_wx, nrow=2, heights=unit(c(0.9, 0.1), "npc"))
# grid.arrange(nh, me, ncol=2)


