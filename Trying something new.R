count_blank = sum(Dummy_data1 == "" , na.rm = TRUE)
count_blank

train_ltfs[train_ltfs == ""] = NA

count_NA = sum(is.na(Dummy_data1))
count_NA
any(is.na(ltfs))

train_ltfs %>% drop_na()

##############################

options(scipen=999)  # to remove the exponential
options(scipen=0)    # to get back to default
getOption("scipen")

##############################
# to tilt the words in x axis in vertical direction

theme(axis.text.x = element_text(angle = 90, hjust = 1))

###############################

ltfs = data.frame(v1 = sample(c("Y","N"), 100, T),
                 v2 = sample(c("Y","N"), 100, T),
                 v3 = sample(c("Y","N"), 100, T),
                 v4 = sample(c("Y","N"), 100, T),
                 v5 = sample(c("Y","N"), 100, T))

ltfs[] = lapply(ltfs,as.integer)
str(ltfs)


install.packages("sjPlot")
library(sjPlot)
sjp.corr(ltfs)
sjt.corr(ltfs)

##########################################
# how to subset
M = subset(club, select = -c(reservation_id,member_age_buckets,memberid,cluster_code,reservationstatusid_code,resort_id))

##########################################

cr = cor(correlation, method = c("kendall","spearman","pearson"))

corrplot(cr, type = "lower", method = "number")
corrplot(cr, type = "lower",  na.label = "square", na.label.col = "orange")

###################################################

# Breaking the loation in latitude and longitude

{
  require(stringr)
  
  regex = "\\((\\d+\\.\\d+), (-\\d+\\.\\d+)\\)"
  lat_long = str_match(boston$Location , regex)[,2:3]
  lat_long = apply(lat_long, 2, as.numeric)
  colnames(lat_long) = c("y", "x")
  boston_latlong = cbind(boston, lat_long)
}

######################################################

#install.packages("ggmap")
#library("ggmap")

# install dev version of ggmap
devtools::install_github("dkahle/ggmap")

library(ggmap)
#> Loading required package: ggplot2
#> Google Maps API Terms of Service: http://developers.google.com/maps/terms.
#> Please cite ggmap if you use it: see citation("ggmap") for details.

# save api key
register_google(key = "AIzaSyCJfpTjCxZoic03AQq7WhDHBwlyg1_apfs")

# check if key is saved
#has_goog_key()
#ggmap::has_goog_key()

#> [1] TRUE

ggmap(
  ggmap = get_map(
    "Dayton",
    zoom = 13, scale = "auto",
    maptype = "satellite",
    source = "google"),
  extent = "device",
  legend = "topright"
)

#######################################################################
# variable reduction method
#install.packages("Boruta")
library("Boruta")

set.seed(123)
boruta.train = Boruta(Upvotes ~ ., data = train, doTrace = 2)

################################################################
