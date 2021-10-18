library(sf)
library(spData)
library(ggplot2)
library(cowplot)
library(rcartocolor)
library(raster)
library(RStoolbox)
library(landsat8)
library(ggspatial)
library(grid)
library(png)
band4 <- raster("Raster/Bolivia/2021/GRANULE/L1C_T20KNE_A022916_20210726T142714/IMG_DATA/T20KNE_20210726T141739_B08.jp2")
band3 <- raster("Raster/Bolivia/2021/GRANULE/L1C_T20KNE_A022916_20210726T142714/IMG_DATA/T20KNE_20210726T141739_B04.jp2")
band2 <- raster("Raster/Bolivia/2021/GRANULE/L1C_T20KNE_A022916_20210726T142714/IMG_DATA/T20KNE_20210726T141739_B03.jp2")
Area1 <- st_read ("Raster/Bolivia/Area.shp") 
Area  <- shapefile("Raster/Bolivia/Area.shp")
Poligono     <- shapefile("Raster/Bolivia/Sitio.shp")
# Combinancion de bandas agricultura
Sentinel_Natu = stack(band4, band3, band2)
##### A nivel de estudio 
#ambito <- mapedit::drawFeatures()       # Creamos el objeto
#ambito <- ambito %>% st_as_sf()         # Convertimos el objeto sf_ee

Poligonox  <-spTransform(Poligono, CRS=crs(band4))
PoligonoxDataFrame <- Poligonox %>% fortify

Poligonox  <-spTransform(Area, CRS=crs(band4))
PoligonoxDataFrame <- Poligonox %>% fortify
#cortar con la zona de estudio
paute17n  <- spTransform(Area , CRS=crs(band4))
bandas1   <- crop(Sentinel_Natu , extent(paute17n))
bandas    <- mask(bandas1,paute17n)
ventana= extent(510000, 521000, 7994000, 8000000)

# Cargamos data
Bol_dep    <- getData('GADM', country='Bolivia', level=1) %>%st_as_sf() 
Bolivia    <- getData('GADM', country='Bolivia', level=0) %>%st_as_sf() 
Bol=ggplot()+
  geom_sf(data = Bol_dep, fill="gray", color="white")+
  geom_sf(data = Bolivia, fill=NA)+
  geom_sf(data = Area1, fill="black", size=0.3)+
  theme_void()+
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

Map=ggRGB(bandas, r=1,g=2,b=3, stretch="lin", ext = ventana)+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  geom_polygon(col = 'gold3',
               fill = NA,
               data = PoligonoxDataFrame,
               aes(x = long, y = lat, group = group))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.8),linetype = "dashed", size = 0.5),
        axis.text = element_text(colour = "black"),
        # plot.background = element_rect(colour = "gray",size = 2),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8))+
  coord_equal()+
  geom_vline(xintercept = c(510000, 513000,516000,519000, 521000), color = "gray50",linetype = "dashed", size = 0.05)+ 
  geom_hline(yintercept = c(7994000, 7996000, 7998000, 8000000), color = "gray50",linetype = "dashed", size = 0.05)+
  labs(x = NULL, y = NULL)


im= ggdraw() +
  coord_equal(xlim = c(0, 21), ylim = c(0, 10), expand = FALSE) +
  draw_plot(Map, width = 18, height = 18,x = 2.1, y = -4)+
  draw_plot(Bol, width = 3, height =3 ,x = 16, y = 1)+
  annotate(geom = "text", x = 19.2, y = 3, label = "Ing.Gorky Florez Castillo", angle = 90,
           family="serif", color = "black", size = 3,fontface = "bold")+
  annotate(geom = "text", x = 3.2, y = 5, label = "Esta imagen, capturada por la \nmisión Copernicus Sentinel-2", 
           family="serif", color = "black", size = 3,fontface = "bold", angle = 90)
# Exportacion
ggsave(plot = im ,"MAPAS/bolivia_area.png",
       units = "cm", width = 21,height = 10, dpi = 900)# guardar grafico  








