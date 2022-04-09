############
#GG distance map
############
rm( list = ls()) #clear env

library(tidyverse)
library(dplyr)
library(ggplot2)
library(spacetools)
library(deltamapr)
library(sf)
library(raster)
library(RColorBrewer)
library(PostGIS)


#create a fishnet of points using WW_Delta from deltamapr
st_crs(WW_Delta)

time_start<-Sys.time()
grid<-st_make_grid(WW_Delta,cellsize = .005,what="centers")
time_end<-Sys.time()
time1<-time_end-time_start
time1
plot(grid)

grid%>%st_set_crs(st_crs(WW_Delta))

#calculate distance from GG
grid<-as.data.frame(grid)
grid$ID<-grid%>%rownames(1:nrow(grid_df))
grid$X<-unlist(map(grid$geometry,1))
grid$Y<-unlist(map(grid$geometry,2))
saveRDS(grid,"Outputs/grid.rds")
grid_df<-readRDS("Outputs/grid.rds")

time_start<-Sys.time()
WW_trans<-Maptransitioner(st_buffer(WW_Delta,dist=.001))
time_end<-Sys.time()
time2<-time_end-time_start
time2

saveRDS(WW_trans,"Outputs/transitioned_WW.rds")
WW_trans<-readRDS("Outputs/transitioned_WW.rds")

time_start<-Sys.time()
grid_dist<-GGdist(Water_map=st_buffer(WW_Delta,dist=.0001),Points=grid,Latitude_column = Y, Longitude_column = X,PointID_column = ID,Water_map_transitioned = WW_trans)
time_end<-Sys.time()
time<-time_end-time_start
#time #1.92 hours
saveRDS(grid_dist,"Outputs/grid_dist.rds")
grid_dist<-readRDS("Outputs/grid_dist.rds")

#join in grid_dist to grid_df
grid_df<-grid_df%>%inner_join(grid_dist)
saveRDS(grid_df,"Outputs/grid_df.rds")
grid_df<-readRDS("Outputs/grid_df.rds")

#grid_dist<-st_distance(gg_cord,grid)
df <- data.frame(Distance = as.vector(grid_df$Distance)/1000,
                 sf::st_coordinates(grid))
str(df)
df<-st_as_sf(df,coords=c("X","Y"))
df<-df%>%st_set_crs(st_crs(WW_Delta))
df$X<-unlist(map(df$geometry,1))
df$Y<-unlist(map(df$geometry,2))
#colors 
col_dist <- brewer.pal(11, "RdYlBu")

ggplot(grid_df, aes(X,Y,fill = Distance),color=NA)+ #variables
  geom_sf(lwd=0)+
  geom_tile()+
  scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
  labs(fill = "Distance (km)")+ #legend name
  theme_bw()+ #map theme
  theme(legend.position = "bottom") #legend position
ggsave("Figures/dist_map.png",width = 10,height = 10)

#export as a raster
ext <- extent(as(grid, "Spatial"))
ext
#raster destination
r <- raster(resolution = .002, ext = ext)

#convert the points to a spatial object class sf
dist_sf <- st_as_sf(df, coords = c("X", "Y")) %>%
  st_set_crs(st_crs(WW_Delta))

#create the distance raster
dist_raster <- rasterize(dist_sf, r, "Distance", fun = mean)
plot(dist_raster)
writeRaster(dist_raster, file = "Outputs/dist_delta.tif", format = "GTiff", overwrite = TRUE)

#lets clip to just the extent of the emp sampling
e <- as(extent(-122.2,-121.6, 38.0,38.3), 'SpatialPolygons')
e<-st_as_sf(e)%>%
  st_set_crs(st_crs(WW_Delta))

r <- st_crop(df, extent(e))

ggplot(r, aes(X,Y,fill = Distance),color=NA)+ #variables
  geom_sf(lwd=0)+
  geom_tile()+
  scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
  labs(fill = "Distance (km)")+ #legend name
  theme_bw()+ #map theme
  theme(legend.position = "bottom") #legend position
ggsave("Figures/crop_map.png",width = 10,height = 10)

#Lets try to pull in just the distance raster so we can calculate distances from
#stations using just the raster
writeRaster(dist_raster, file = "Outputs/dist_delta.tif", format = "GTiff", overwrite = TRUE)

