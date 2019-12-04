require(lubridate)
require(tidyverse)
require(rgdal)
require(sp)
require(sf)

# reading maps

indcities = readOGR("Ind_mega","Ind_mega")
indmap = readOGR("India","India_2011")


indcitiesfort = fortify(indcities)
indmapfort = fortify(indmap)

plotcitiesmap = ggplot() +
  geom_polygon(data = indmap, aes(x=long, y=lat, group=group), colour = 'black', fill = "white")+  
  geom_polygon(data = indcities, aes(x=long, y=lat, group=group), colour = 'black', fill = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  coord_map()


################ read eBird data


rawpath = "ebd_IN_relSep-2019.txt"
sensitivepath = "Sensitive_India_may 2019.csv"

# select only necessary columns
preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

# CATEGORY - species, subspecies, hybrid, etc.; COMMON.NAME - common name of species;
# SCIENTIFIC NAME - scientific name; OBSERVATION.COUNT - count of each species observed in a list;
# LOCALITY.ID - unique location ID; LOCALITY.TYPE - hotspot, etc.;
# LATITUDE and LONGITUDE - coordinates; OBSERVATION.DATE - checklist date; 
# TIME.OBSERVATIONS.STARTED - checklist start time; OBSERVER ID - unique observer ID;
# PROTOCOL TYPE - stationary, traveling, historical, etc.; DURATION.MINUTES - checklist duration;
# EFFORT.DISTANCE.KM - distance traveled; NUMBER.OBSERVERS - no. of birders;
# ALL.SPECIES.REPORTED - indicates whether a checklist is complete or not;
# GROUP.IDENTIFIER - unique ID for every set of shared checklists (NA when not shared);
# SAMPLING.EVENT.IDENTIFIER - unique checlist ID

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

# read sensitive species data
nms = nms[-47]
sesp = read.csv(sensitivepath, colClasses = nms, stringsAsFactors = F)
stdformat = data.frame(date = as.character(sesp$OBSERVATION.DATE))
stdformat = stdformat %>%
  separate(date, c("month","day","year"), "/")
stdformat$year = as.numeric(stdformat$year)
sesp$OBSERVATION.DATE = paste(stdformat$year,"-",stdformat$month,"-",stdformat$day, sep = "")
sesp = sesp %>% mutate(GROUP.IDENTIFIER = ifelse(GROUP.IDENTIFIER == "", NA, GROUP.IDENTIFIER))

# merge both data frames
data = rbind(data,sesp)

imp = c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
        "LOCALITY.ID", "REVIEWED","APPROVED",
        "LOCALITY.TYPE",
        "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
        "PROTOCOL.TYPE",
        "DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "ALL.SPECIES.REPORTED","group.id")

days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data = data %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  dplyr::select(imp) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month], 
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(day <= 151, cyear-1, cyear)) %>%
  group_by(group.id) %>% mutate(no.sp = n_distinct(COMMON.NAME)) %>%
  ungroup

data = data %>% filter(year < 2019)


########## mapping polygons on to data


library(rgeos)

temp = data %>% group_by(LOCALITY.ID) %>% slice(1) # same group ID, same attributes

rownames(temp) = temp$LOCALITY.ID # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,indcities) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$LOCALITY.ID = rownames(temp) # add column to join with the main data
temp = temp[colSums(!is.na(temp)) > 0]
temp = temp[rowSums(!is.na(temp)) > 1,]
data = data %>%
  filter(LOCALITY.ID %in% temp$LOCALITY.ID)
data = left_join(temp,data)



########### plot stuff from eBird data on map


save(data,file = "citydataset.RData")
