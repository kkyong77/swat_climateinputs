## daymet R packages
#install.packages("daymetr")
# download dayment data based on lat and log
# reformat the data for SWAT model inputs
# reading yakima headwater subbasin lat and log

library(daymetr)
library(chron)
library(lubridate)
library(zoo)

getwd()


xyz<-read.delim("C:/Project/Columbia_SWAT/Yakima_SWAT/climate/subbasin_yakima_headwater.txt",header=T,sep=",")
xy<-xyz[,c("Lat","Long_")]

write.csv(xy,file="yakima_header_site.csv")


ya_daymet<-download_daymet_batch("yakima_header_site.csv",
                      start = 1980,
                      end = 2019,
                      internal = TRUE)


df<-ya_daymet

## writing the location values for SWAT model inputs----
dataPath_swat <- "C:/Project/Columbia_SWAT/Yakima_SWAT/climate"

setwd(dataPath_swat)

datalist=list()

###########################################################
# calculating laps rate of temperature and precipitation

for (i in 1:nrow(xyz)){
  tmp1<-xyz[i,]
  nam_pcp <- paste(as.numeric(substr(xyz[i,"HydroID"],4,7)),"pcp", sep = "")
  myfile_pcp <- file.path(paste0("ya",nam_pcp))
#  myfile_name <- file.path(paste0(nam_pcp,"py"))
  header=c("ID","NAME","LAT","LONG","ELEVATION")
  tmp1<-cbind(i,myfile_pcp,tmp1[,c("Lat","Long_","Elev")])
  colnames(tmp1)<-header
  datalist[[i]]<-tmp1
  
}

pcp_loc<-do.call(rbind,datalist)

write.csv(pcp_loc, file="locations_ya_PCP.txt", row.names=FALSE)

## writing precipitation as SWAT model input format----


# number of stations (nrow(xyz))
for (i in 1:nrow(xyz)) { # 
  
  pcp<-data.frame(cbind(df[[i]]$data$year,df[[i]]$data$prcp..mm.day))
  colnames(pcp)=c("year","mm")
  years<-unique(pcp$year)
  
  for (y in 1:length(years)) {
    data_pcp <-pcp[pcp$year==years[y],]
    # All Daymet years, including leap years, have 1 - 365 days. For
    # leap years, the Daymet database includes leap day. Values for
    # December 31 are discarded from leap years to maintain a 365-day year.
     if (y==1){ # first year
        if(leap.year(data_pcp$year[1])){
          data_pcp$doy<-c(1:365)
          data_pcp_new<-matrix(0,nrow=366,ncol=3)
          data_pcp_new[1:365,]=as.matrix(data_pcp)
          data_pcp_new[366,]=c(years[y],round(mean(data_pcp$mm[363:365]),1),366) # previous three days averaged 
          colnames(data_pcp_new)=c("year","mm","doy")
          }
          else {
            data_pcp$doy<-c(1:365)
            data_pcp_new=as.matrix(data_pcp)
          }
      }
     else { # after first year 
       if(leap.year(data_pcp$year[1])){
          data_pcp$doy<-c(1:365)
          data_pcp_new2<-matrix(0,nrow=366,ncol=3)
          data_pcp_new2[1:365,]=as.matrix(data_pcp)
          data_pcp_new2[366,]=c(years[y],round(mean(data_pcp$mm[363:365]),1),366) # previous three days averaged
          colnames(data_pcp_new2)=c("year","mm","doy")
          }
         else {
           data_pcp$doy<-c(1:365)
           data_pcp_new2=as.matrix(data_pcp)
           }
       data_pcp_new=rbind(data_pcp_new,data_pcp_new2) 
       } 
       
      
      }
    
  nam_pcp <- paste(as.numeric(substr(xyz[i,"HydroID"],4,7)),"pcp", sep = "")
  myfile_pcp <- file.path(paste0("ya",nam_pcp))
  #  myfile_name <- file.path(paste0(nam_temp,"py"))
  header = sprintf("%d %d %d", 1980,01,01)
  write(header,file=paste0(myfile_pcp,".txt"))
  write.table(data_pcp_new[,c(2)], file=paste0(myfile_pcp,".txt"), append=T, col.names=F, row.names=F, quote=F)
}




## writing tmax and tmin as SWAT model input format----
datalist=list()

for (i in 1:nrow(xyz)){
  tmp1<-xyz[i,]
  nam_temp <- paste(as.numeric(substr(xyz[i,"HydroID"],4,7)),"tmp", sep = "")
  myfile_temp <- file.path(paste0("ya",nam_temp))
  #  myfile_name <- file.path(paste0(nam_temp,"py"))
  header=c("ID","NAME","LAT","LONG","ELEVATION")
  tmp1<-cbind(i,myfile_temp,tmp1[,c("Lat","Long_","Elev")])
  colnames(tmp1)<-header
  datalist[[i]]<-tmp1
  
}

temp_loc<-do.call(rbind,datalist)

write.csv(temp_loc, file="locations_ya_temp.txt", row.names=FALSE)



for (i in 1:nrow(xyz)){
  tmp<-data.frame(cbind(df[[i]]$data$year,df[[i]]$data$tmax..deg.c.,df[[i]]$data$tmin..deg.c.))
  colnames(tmp)=c("year","tmax","tmin")

  years<-unique(tmp$year)

  for (y in 1:length(years)) {
    data_tmp <-tmp[tmp$year==years[y],]
  # All Daymet years, including leap years, have 1 - 365 days. For
  # leap years, the Daymet database includes leap day. Values for
  # December 31 are discarded from leap years to maintain a 365-day year.
    if (y==1){ # first year
      if(leap.year(data_tmp$year[1])){
        data_tmp$doy<-c(1:365)
        data_tmp_new<-matrix(0,nrow=366,ncol=4)
        data_tmp_new[1:365,]=as.matrix(data_tmp)
        data_tmp_new[366,]=c(years[y],round(mean(data_tmp$tmax[363:365]),1),round(mean(data_tmp$tmin[363:365]),1),366) # previous three days averaged 
        colnames(data_tmp_new)=c("year","tmax","tmin","doy")
      }
    else {
      data_tmp$doy<-c(1:365)
      data_tmp_new=as.matrix(data_tmp)
        }
      }
  
    else { # after first year 
      if(leap.year(data_tmp$year[1])){
        data_tmp$doy<-c(1:365)
        data_tmp_new2<-matrix(0,nrow=366,ncol=4)
        data_tmp_new2[1:365,]=as.matrix(data_tmp)
        data_tmp_new2[366,]=c(years[y],round(mean(data_tmp$tmax[363:365]),1),round(mean(data_tmp$tmin[363:365]),1),366) # previous three days averaged 
        colnames(data_tmp_new2)=c("year","tmax","tmin","doy")
        }
      else {
      data_tmp$doy<-c(1:365)
      data_tmp_new2=as.matrix(data_tmp)
        }
      data_tmp_new=rbind(data_tmp_new,data_tmp_new2) 
    } 
  
  
  } # end of full year data for each station


  nam_temp <- paste(as.numeric(substr(xyz[i,"HydroID"],4,7)),"tmp", sep = "")
  myfile_temp <- file.path(paste0("ya",nam_temp))
  #myfile_name <- file.path(paste0(nam_temp,"py"))
  header = sprintf("%d %d %d", 1980,01,01)
  write(header,file=paste0(myfile_temp,".txt"))
  write.table(data_tmp_new[,c(2,3)], file=paste0(myfile_temp,".txt"), append=T, col.names=F, row.names=F, quote=F,sep=",")
}



############################################################
# adding solar radiation data
# daily solar radiation=daylength*
#Incident shortwave radiation flux density in watts per square meter, taken as an average over the daylight period of the day.
#NOTE: Daily total radiation (MJ/m2/day) can be calculated as follows: ((srad (W/m2) * dayl (s/day)) / l,000,000)


datalist=list()

for (i in 1:nrow(xyz)){
  tmp1<-xyz[i,]
  nam_trad <- paste(as.numeric(substr(xyz[i,"HydroID"],4,7)),"trad", sep = "")
  myfile_trad <- file.path(paste0("ya",nam_trad))
  #  myfile_name <- file.path(paste0(nam_temp,"py"))
  header=c("ID","NAME","LAT","LONG","ELEVATION")
  tmp1<-cbind(i,myfile_trad,tmp1[,c("Lat","Long_","Elev")])
  colnames(tmp1)<-header
  datalist[[i]]<-tmp1
  
}

trad_loc<-do.call(rbind,datalist)

write.csv(trad_loc, file="locations_ya_trad.txt", row.names=FALSE)


for (i in 1:nrow(xyz)){
  rad<-df[[i]]$data$srad..W.m.2.
  dayl<-df[[i]]$data$dayl..s.
  trad<-(rad*dayl)
  trad<-round(trad/1000000,2)
  year<-df[[i]]$data$year
  trad<-data.frame(cbind(year,trad))
  colnames(trad)<-c("year","trad")
  years<-unique(trad$year)
  
  for (y in 1:length(years)) {
    data_trad <-trad[trad$year==years[y],]
    # All Daymet years, including leap years, have 1 - 365 days. For
    # leap years, the Daymet database includes leap day. Values for
    # December 31 are discarded from leap years to maintain a 365-day year.
    if (y==1){ # first year
      if(leap.year(data_trad$year[1])){
        data_trad$doy<-c(1:365)
        data_trad_new<-matrix(0,nrow=366,ncol=3)
        data_trad_new[1:365,]=as.matrix(data_trad)
        data_trad_new[366,]=c(years[y],round(mean(data_trad$trad[363:365]),2),366) # previous three days averaged 
        colnames(data_trad_new)=c("year","trad","doy")
      }
      else {
        data_trad$doy<-c(1:365)
        data_trad_new=as.matrix(data_trad)
      }
    }
    
    else { # after first year 
      if(leap.year(data_trad$year[1])){
        data_trad$doy<-c(1:365)
        data_trad_new2<-matrix(0,nrow=366,ncol=3)
        data_trad_new2[1:365,]=as.matrix(data_trad)
        data_trad_new2[366,]=c(years[y],round(mean(data_trad$trad[363:365]),2),366) # previous three days averaged 
        colnames(data_trad_new2)=c("year","trad","doy")
      }
      else {
        data_trad$doy<-c(1:365)
        data_trad_new2=as.matrix(data_trad)
      }
      data_trad_new=rbind(data_trad_new,data_trad_new2) 
    } 
    
    
  } # end of full year data for each station
  
  nam_trad <- paste(as.numeric(substr(xyz[i,"HydroID"],4,7)),"trad", sep = "")
  myfile_trad <- file.path(paste0("ya",nam_trad))
  header = sprintf("%d %d %d", 1980,01,01)
  write(header,file=paste0(myfile_trad,".txt"))
  write.table(trad[,2], file=paste0(myfile_trad,".txt"), append=T, col.names=F, row.names=F, quote=F)
}
