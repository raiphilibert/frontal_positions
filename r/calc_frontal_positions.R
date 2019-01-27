library(stringr)
library(dplyr)
library(ggplot2)
library(akima)
library(viridis)
options(scipen=99999)

dataPath <- './data/SANAE 56_XBT/SANAE 56_Leg1_XBT/'
files=list.files(dataPath)

xbtData <- NULL
for(f in files){
  res <- unlist(read.csv(paste0(dataPath,f),header=FALSE,sep="\t",stringsAsFactors = FALSE)[1:10,],use.names = FALSE)
  
  df <- NULL
  for (var in c('latitude','longitude')){
  lat <- res[grepl(var,res,ignore.case=TRUE)]
  lat <- gsub(",","",lat)
  lat <- strsplit(lat,"=")[[1]][2]
  lat <- str_trim(lat,side='both')
  lat <- strsplit(lat," ")
  degree <- lat[[1]][1]
  minute <- lat[[1]][2]
  direction <- lat[[1]][4]
  lat <- as.numeric(degree) + as.numeric(minute)/60
  if(direction=="W"|direction=="S"){
    lat=lat*-1
  }
  df[var] <- lat
  }
  
  date <- res[grepl('date',res,ignore.case=TRUE)]
  date <- gsub(",","",date)
  date <- strsplit(date,"=")[[1]][2] 
  date <- str_trim(date,side="both")
  time <- res[grepl('time',res,ignore.case=TRUE)]
  time <- gsub(",","",time)
  time <- strsplit(time,"=")[[1]][2] 
  time <- str_trim(time,side="both")
  date <- paste(date,time,sep=" ")
  date <- as.POSIXct(date,format="%d/%m/%Y %H:%M",tz="GMT")
  df['date'] <- date
  values <- read.csv(paste0(dataPath,f),skip=10,sep="\t",stringsAsFactors = FALSE)
  values['latitude'] <- df['latitude']
  values['longitude'] <- df['longitude']
  values['date'] <- df['date']
  values <- values %>% mutate(Depth.=as.numeric(gsub(",","",Depth.)),
                    Temp = as.numeric(Temp)) %>% rename(depth=Depth.,temp=Temp)
  
  xbtData <- bind_rows(xbtData,values)
}

ggplot(data=xbtData) + geom_point(aes(x=latitude,y=-Depth.,color=Temp, group=latitude)) 
write.table(file='./data/xbtDatacompiled.txt',x=xbtData,sep='\t',row.names=FALSE)

##INTERPOLATE DATA
fld <- with(xbtData, interp(x = latitude, y = depth, z = temp))
xbtInterp <- as.data.frame(interp2xyz(fld))  # the xy.est parameter (data.frame)
names(xbtInterp) <- c('latitude','depth','temp')


front_names <- c("STF","SAF","PF")
front_axial_t <-c(10,6,2)
front_exp <- NULL

for   (i in 1:length(front_names)){  
  datAxial <- xbtData[round(xbtData$depth)>195 & round(xbtData$depth)<205,]
  ind <- which(datAxial$temp<front_axial_t[i]+0.5 & datAxial$temp>front_axial_t[i]-0.5)
  dat_front <- datAxial[ind,]
  lats <- unique(dat_front$lat)
  temp <- dat_front$temp
  depth <- dat_front$depth
 # lon <- dat_front$longitude
  lat <- dat_front$latitude
  dat_front <- cbind(temp,depth,lat) %>% as.data.frame()
  front_pos <- dat_front %>% 
              group_by(lat) %>% 
              summarise(temp=mean(temp),depth=mean(depth)) %>% mutate(
                front_name = front_names[i]
              )
  closestVal <- which(abs(front_axial_t[i]-front_pos$depth)==min(abs(front_axial_t[i]-front_pos$depth)))
  front_pos = front_pos[closestVal,]
  front_exp <- rbind(front_exp,front_pos)
}

ggplot(xbtInterp, aes(x=latitude, y=depth, z=temp)) + geom_raster(aes(fill=temp),interpolate = TRUE)  +
  scale_x_reverse() + scale_y_reverse() + geom_vline(data=front_exp,aes(xintercept=lat)) + 
  scale_fill_gradientn(colours=viridis(256))

##Print front_exp to get data
front_exp
