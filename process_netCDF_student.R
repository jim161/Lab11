library(ncdf4)
library(reshape2)
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf)

ncin <- nc_open(f)
print(ncin)
#set study area
lonmax <- -78 #top northern most coordinate
lonmin <- -100.00
latmax <- 31 #left eastern coordinate
latmin <- 17 #right western coordinate

# identify the variable you want to extract data for

data <- nc_open(file)#open Netcdf

var <- ncvar_get(data, "chlor_a")


#list netCDF files
f <- list.files(pattern=".nc",full.names=F) 
#What pattern can you use to identify all the netCDF files?

d <- plyr::adply(file, 1, function(file) {

  # open netCDF file
  data<-nc_open(f)

  # extract data
  lon<-ncvar_get(data,"lon")
  lat<- ncvar_get(data,"lat")
  tmp.array <- ncvar_get(data, data$var[[var]])
  dunits <- ncatt_get(data, var, "units")$value
  fillvalue <- "NA"

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
  day <-  format(as.Date(datemean,"%Y-%m-%dT%H:%M:%OSZ"), "%dT") # get the day

   # prepare final data set. Include the day (it is missing in the code below)
  dat.varSA<-data.frame(rep(as.integer(year,nrow(dat.varSAtmp))),
                        rep(as.integer(month,nrow(dat.varSAtmp))), 
                        rep(as.integer(day,nrow(dat.varSAtmp))),
                        rep(dunits,nrow(dat.varSAtmp)), 
                        rep(var, nrow(dat.varSAtmp)))
  names(dat.varSA)<-c("year","month","day","lon","value","unit","var")
  

  # close connection
  nc_close(data)

  return(dat.varSA)
head
}, .progress="text", .inform = T)

d <- d[,-1]

# save csv file

