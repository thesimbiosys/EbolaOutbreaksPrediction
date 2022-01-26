#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("The script needs the boundaries and max_t", call.=FALSE)
} else if (length(args)<2) {
  # default output file
  args[1] = 10 #i longs horizontal
  args[2] = 100   #j lats vertical
}

options(digits = 15)

distance_km=0.1

n_longs<-as.numeric(args[1])
n_lats<-as.numeric(args[2])

index=1

longs<-head(seq(1,n_longs,by=(distance_km*10000/1000000)),n_longs)
lats<-head(seq(1,n_lats,by=(distance_km*10000/1000000)),n_lats)

res<-data.frame()

for(j in 1:(n_lats)){
  for(i in 1:(n_longs)){
    
    lon<-i*(distance_km*10000)/1000000
    lat<-j*(distance_km*10000)/1000000
    
    #vecinos
    n_nord<-ifelse(index+n_longs > 0 && index+n_longs <= n_longs*n_lats, index+n_longs, NA)
    n_south<-ifelse(index-n_longs > 0, index-n_longs, NA)
    n_east<-ifelse(index+1 > 0 & index+1 < (j*n_longs)+1, index+1, NA)
    n_west<-ifelse(index-1 > 0 & index-1 >= n_longs*(j-1)+1, index-1, NA)
    
    if(i==1 || j==1 || j==n_lats || i==n_longs){
      res<-rbind(res,cbind("Latitude"=lat, "Longitude"=lon, "Mean.K0"=0, "id"=index, "j"=j, "i"=i, 
                           "n_nord"=n_nord, "n_south"=n_south, "n_east"=n_east, "n_west"=n_west, "isZero"=TRUE, "isBorder"=FALSE))
    }else if(i==2 || j==2 || i==n_longs-1 || j==n_lats-1){
      res<-rbind(res,cbind("Latitude"=lat, "Longitude"=lon, "Mean.K0"=1500, "id"=index, "j"=j, "i"=i, 
                           "n_nord"=n_nord, "n_south"=n_south, "n_east"=n_east, "n_west"=n_west, "isZero"=FALSE, "isBorder"=TRUE))
    }else{
      res<-rbind(res,cbind("Latitude"=lat, "Longitude"=lon, "Mean.K0"=1500, "id"=index, "j"=j, "i"=i, 
                           "n_nord"=n_nord, "n_south"=n_south, "n_east"=n_east, "n_west"=n_west, "isZero"=FALSE, "isBorder"=FALSE))
    }
    index=index+1
  }
  
}

write.csv(res,paste("grids/grid_",n_longs,"-",n_lats,".csv",sep = ""), row.names = FALSE)
