# using daymet data to prepare for the swat climate input data
## daymet R packages
#install.packages("daymetr")
# download dayment data based on lat and log
# reformat the data for SWAT model inputs
# reading yakima headwater subbasin lat and log

library(daymetr)

home<-getwd()


setwd("C:/Project/Columbia_SWAT/CR_SWAT250m/climate")

xyz<-read.delim("C:/Project/Columbia_SWAT/CR_SWAT250m/climate/CR_subwatersheds.txt",header=T,sep=",")
xy<-xyz[,c("Lat","Long_")]

xyz_v2<-xyz[]


write.csv(tmp_xy,file="CR_header_site_tmp.csv")


CR_daymet_tmp<-download_daymet_batch("CR_header_site_tmp.csv",
                                 start = 1980,
                                 end = 2018,
                                 internal = TRUE)
tmp<-xy[-c(1378,1399,1438,1439),]

xyz_v2<-xyz[-c(1378,1399,1438,1439),]

#1378 1399 1438 1439

write.
CR_daymet_v2<-download_daymet_batch("CR_header_site_v2.csv",
                      start = 1980,
                      end = 2018,
                      internal = TRUE)
csv(tmp,file="CR_header_site_v2.csv")




Cr_daymet[[1]]$data$tmax..deg.c.
CR_daymet[[1]]$data$tmin..deg.c.

df<-CR_daymet


df<-CR_daymet_v2



## writing the location values for SWAT model inputs----
dataPath_swat <- "C:/Project/Columbia_SWAT/CR_SWAT250m/climate"

setwd(dataPath_swat)

datalist=list()


###########################################################
# calculating laps rate of temperature and precipitation

for (i in 1:nrow(xyz_v2)){
  tmp1<-xyz_v2[i,]
  nam_pcp <- paste(as.numeric(xyz_v2[i,"Subbasin"]),"pcp", sep = "")
  myfile_pcp <- file.path(paste0("CR",nam_pcp))
#  myfile_name <- file.path(paste0(nam_pcp,"py"))
  header=c("ID","NAME","LAT","LONG","ELEVATION")
  tmp1<-cbind(i,myfile_pcp,tmp1[,c("Lat","Long_","Elev")])
  colnames(tmp1)<-header
  datalist[[i]]<-tmp1
  
}

pcp_loc<-do.call(rbind,datalist)

write.csv(pcp_loc, file="locations_CR_PCP.txt", row.names=FALSE)

## writing precipitation as SWAT model input format----
for (i in 1:nrow(xyz_v2)){
  pcp<-df[[i]]$data$prcp..mm.day.
  nam_pcp <- paste(as.numeric(xyz_v2[i,"Subbasin"]),"pcp", sep = "")
  myfile_pcp <- file.path(paste0("CR",nam_pcp))
  #  myfile_name <- file.path(paste0(nam_temp,"py"))
  header = sprintf("%d %d %d", 1980,01,01)
  write(header,file=paste0(myfile_pcp,".txt"))
  write.table(pcp, file=paste0(myfile_pcp,".txt"), append=T, col.names=F, row.names=F, quote=F)
  print(i)
}





## writing tmax and tmin as SWAT model input format----
datalist=list()

for (i in 1:nrow(xyz_v2)){
  tmp1<-xyz_v2[i,]
  nam_temp <- paste(as.numeric(xyz_v2[i,"Subbasin"]),"tmp", sep = "")
  myfile_temp <- file.path(paste0("CR",nam_temp))
  #  myfile_name <- file.path(paste0(nam_temp,"py"))
  header=c("ID","NAME","LAT","LONG","ELEVATION")
  tmp1<-cbind(i,myfile_temp,tmp1[,c("Lat","Long_","Elev")])
  colnames(tmp1)<-header
  datalist[[i]]<-tmp1
  
}

temp_loc<-do.call(rbind,datalist)

write.csv(temp_loc, file="locations_CR_temp.txt", row.names=FALSE)


 
for (i in 1:nrow(xyz_v2)){
tmax<-df[[i]]$data$tmax..deg.c.
tmin<-df[[i]]$data$tmin..deg.c.
tmp<-cbind(tmax,tmin)
nam_temp <- paste(as.numeric(xyz_v2[i,"Subbasin"]),"tmp", sep = "")
myfile_temp <- file.path(paste0("CR",nam_temp))
#myfile_name <- file.path(paste0(nam_temp,"py"))
header = sprintf("%d %d %d", 1980,01,01)
write(header,file=paste0(myfile_temp,".txt"))
write.table(tmp, file=paste0(myfile_temp,".txt"), append=T, col.names=F, row.names=F, quote=F,sep=",")
print(i)
}

  
tmp2 <- download_daymet("1378",
                      lat = 46.36877 ,
                      lon = -123.5671,
                      start = 1980,
                      end = 2018,
                      internal = TRUE,
                      simplify = TRUE) # return tidy data !!



   tmax<-tmp$data$tmax..deg.c.
   tmin<-tmp$data$tmin..deg.c.
   tmp<-cbind(tmax,tmin)
   nam_temp <- paste(as.numeric(xyz[i,"Subbasin"]),"tmp", sep = "")
   myfile_temp <- file.path(paste0("CR",nam_temp))
   #myfile_name <- file.path(paste0(nam_temp,"py"))
   header = sprintf("%d %d %d", 1980,01,01)
   write(header,file=paste0(myfile_temp,".txt"))
   write.table(tmp, file=paste0(myfile_temp,".txt"), append=T, col.names=F, row.names=F, quote=F,sep=",")
   print(i)
 }

# 
# ############################################################
# # adding solar radiation data
# # daily solar radiation=daylength*
# #Incident shortwave radiation flux density in watts per square meter, taken as an average over the daylight period of the day.
# #NOTE: Daily total radiation (MJ/m2/day) can be calculated as follows: ((srad (W/m2) * dayl (s/day)) / l,000,000)
# 
# 
# datalist=list()
# 
# for (i in 1:nrow(xyz)){
#   tmp1<-xyz[i,]
#   nam_trad <- paste(as.numeric(substr(xyz[i,"Subbasin"],4,7)),"trad", sep = "")
#   myfile_trad <- file.path(paste0("CR",nam_trad))
#   #  myfile_name <- file.path(paste0(nam_temp,"py"))
#   header=c("ID","NAME","LAT","LONG","ELEVATION")
#   tmp1<-cbind(i,myfile_trad,tmp1[,c("Lat","Long_","Elev")])
#   colnames(tmp1)<-header
#   datalist[[i]]<-tmp1
#   
# }
# 
# trad_loc<-do.call(rbind,datalist)
# 
# write.csv(trad_loc, file="locations_CR_trad.txt", row.names=FALSE)
# 
# 
# for (i in 1:nrow(xyz)){
#   rad<-df[[i]]$data$srad..W.m.2.
#   dayl<-df[[i]]$data$dayl..s.
#   trad<-(rad*dayl)
#   trad<-trad/1000000
#   nam_trad <- paste(as.numeric(substr(xyz[i,"Subbasin"],4,7)),"trad", sep = "")
#   myfile_trad <- file.path(paste0("CR",nam_trad))
#   header = sprintf("%d %d %d", 1980,01,01)
#   write(header,file=paste0(myfile_trad,".txt"))
#   write.table(trad, file=paste0(myfile_trad,".txt"), append=T, col.names=F, row.names=F, quote=F)
# }
