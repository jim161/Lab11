library(ncdf4)
library(reshape2)

#set study area
lonmax <- -78 #top northern most coordinate
lonmin <- -100.00
latmax <- 31 #left eastern coordinate
latmin <- 17 #right western coordinate

# identify the variable you want to extract data for

#open Netcdf
data <- nc_open(f)

var <- "chlor_a"


#list netCDF files
f <- list.files(pattern=".nc",full.names=F) 
#What pattern can you use to identify all the netCDF files?

d <- plyr::adply(f, 1, function(file) {

  # open netCDF file
  data<-nc_open(file)

  # extract data
  lon<-ncvar_get(data,"lon")
  lat<- ncvar_get(data,"lat")
  tmp.array <- ncvar_get(data, data$var[[var]])
  dunits <- ncatt_get(data, var, "units")$value
  fillvalue <- NA

  dim(tmp.array)

  # remove the missing data
  tmp.array[tmp.array == fillvalue] <- NA

  #  matrix to data.frame
  dimnames(tmp.array)<-list(lon=lon,lat=lat)
  dat.var<-melt(tmp.array,id="lon")

  # select data from the study area
  dat.varSAtmp<-subset(dat.var, lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin)

  # extract date information
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  
  year <- format(as.Date(datemean,"%Y-%m-%dT%H:%M:%OSZ"), "%Y" ) # get the year
  month <- format(as.Date(datemean,"%Y-%m-%dT%H:%M:%OSZ"), "%m") # get the month
  day <-  format(as.Date(datemean,"%Y-%m-%dT%H:%M:%OSZ"), "%d") # get the day

   # prepare final data set. Include the day (it is missing in the code below)
  dat.varSA<-data.frame(rep(as.integer(year,nrow(dat.varSAtmp))),
                        rep(as.integer(month,nrow(dat.varSAtmp))), 
                        rep(as.integer(day,nrow(dat.varSAtmp))), dat.varSAtmp$lon, dat.varSAtmp$lat,
                        dat.varSAtmp$value, rep(dunits,nrow(dat.varSAtmp)), 
                        rep(var, nrow(dat.varSAtmp)))
  names(dat.varSA)<-c("year","month","day","lon","lat", "value","unit","var")
  

  # close connection
  nc_close(data)

  return(dat.varSA)

}, .progress="text", .inform = T)

d <- d[,-1]

# save csv file
csvfile<- "Chlor_a_data.csv"
write.table(na.omit(d), csvfile, row.names = FALSE, sep=",")

mean <- mean(Chlor_a_data$value)
variance <- var(Chlor_a_data$value)
stdev <-sd(Chlor_a_data$value)

stats <- data.frame(mean, stdev,variance)

csv <- "Chlor_a_statsummary"
write.table(na.omit(stats), csv, row.names = FALSE, sep = ",")

