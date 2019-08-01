# Data visualization contest with R: first edition
# Preprocessing
# Antonio Diaz

#----------------------------------------------------------------------
# Load - Data and Packages
#----------------------------------------------------------------------
#Load packages
library(data.table) #work with tables
library(lubridate) #work with dates
library(ggmap) #get missing latitude and longitude from google
library(stringr) #work with address vector

#Load data
load('input/data w hour.RData') #Original datas
categorias = fread('input/categoria_multa.csv',sep=';',header=FALSE, col.names=c('infraction','categoria'),encoding = "UTF-8") #Manual classification based on infraction field

#----------------------------------------------------------------------
# Formats
#----------------------------------------------------------------------
#Convert to data.table 
dd = data.table(dd)

#Encodings - char to factor
cols_char = sapply(dd,is.character)
cols_char = names(which(cols_char))
dd[, cols_char] = dd[, lapply(.SD, iconv), .SDcols=cols_char]

#New category column "categoria" 
dd = merge(dd, categorias, by='infraction',all.x = TRUE)
#New column "type2", which combines type and categoria
dd[,type2:=ifelse(type=='Velocidad'|type=='Cimadevilla',type,categoria)]
#Translate "otros", "velocidad" and "cimadevilla"
dd$type2 = tolower(dd$type2)
dd[type2=="otros",type2:="other"]
dd[type2=="velocidad",type2:="speed"]
dd[type2=="cimadevilla",type2:="restricted area"]
rm(categorias)

#Columns 'type' and 'calification' to factor
cols_factor = c('type','calification')
dd[, cols_factor] = dd[, lapply(.SD, factor), .SDcols=cols_factor]

#Latitude and longitude to numeric
dd$latitude = as.numeric(scan(text=dd$latitude, dec=",", sep="."))
dd$longitude = as.numeric(scan(text=dd$longitude, dec=",", sep="."))

#hour and minute to numeric
dd$hour_num = hour(dd$hour)
dd$minute_num = minute(dd$hour)

rm(cols_char,cols_factor)

#----------------------------------------------------------------------
# Get longitude and latitude from Google API 
#----------------------------------------------------------------------
#load('output/direcciones.RData') #Slow process, uncomment this row to directly load the table 
#Log in google API
register_google(key ='put your google api key here')

#Unique address table (direcciones)
direcciones = unique(dd[,.(place,number)])
direcciones[,direccion:=(paste(place,ifelse(is.na(number),"",number),"GIJON"))]

#Get coordinates of addresses table (direcciones)
i = nrow(direcciones)
direcciones$lat_google = 0
direcciones$lon_google = 0

for (j in 1:i) {
  direccion = direcciones$direccion[j]
  print(j)   
  print(direccion)
  #call geocode function gives longitude and latitude
  cord = geocode(direccion,source = "google")
  direcciones[j,lat_google:=cord$lat]
  direcciones[j,lon_google:=cord$lon]
}

#----------------------------------------------------------------------
# New features
#----------------------------------------------------------------------
#Update coordinates in main table
dd = merge(dd, direcciones, all.x=TRUE, by=c('place','number') )
dd$latitude = ifelse(!is.na(dd$latitude),dd$latitude, dd$lat_google)
dd$longitude = ifelse(!is.na(dd$longitude),dd$longitude, dd$lon_google)

#Merge 'year','month', 'day','hour' and 'minute' in "date" column
dd$date = ymd_hm(paste(dd$year, dd$month, dd$day,dd$hour_num,dd$minute_num))

#New numeric features: speed and speed limit
dd[!is.na(speed),speed_num:=regmatches(speed, regexpr("[[:digit:]]+", speed))]
dd[!is.na(speed_limit),speed_limit_num:=regmatches(speed_limit, regexpr("[[:digit:]]+", speed_limit))]
dd$speed_num = as.numeric(dd$speed_num)
dd$speed_limit_num = as.numeric(dd$speed_limit_num)

#Drop unnecesary fields
dd$lat_google=NULL
dd$lon_google=NULL
dd$speed=NULL
dd$speed_limit=NULL
dd$direccion=NULL
dd$year=NULL
dd$month=NULL
dd$day=NULL
dd$hour_num=NULL
dd$minute_num=NULL
dd$hour=NULL
dd$infraction=NULL
dd$type=NULL
dd$categoria=NULL

#----------------------------------------------------------------------
# Cleaning
#----------------------------------------------------------------------
#"number" NA's to "S/N"
dd[,number:=ifelse(is.na(number),"S/N",number)]

#Remove rows with NA in latitude and longitude (43 rows deleted)
dd = dd[!is.na(latitude),]

#Remove out of Asturias locations (5 rows deleted)
dd = dd[latitude>42.96 & latitude<43.66 & longitude> -7.03 & longitude< -4.51,]

#Remove NA's in place (as we don't know were the infraction occurrs) (21 rows deleted)
dd= dd[!is.na(place)]

#Translate calification
dd[,calification:=ifelse(calification=='Leve','Minor',
                         ifelse(calification=='Grave','Major','Serious'))]

#----------------------------------------------------------------------
# Save table (.rds and .csv)
#----------------------------------------------------------------------
write.table(dd,'output/dd.csv', sep='|',dec='.',quote = FALSE, row.names = FALSE,na="")
saveRDS(dd,file='app/data/dd.rds')

