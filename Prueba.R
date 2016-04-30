# Script de prueba, para hacer un mapa a la vez
#La meta es programar una funci√≥n que haga un mapa con s√≥lo dos inputs
#la pregunta y el n√∫mero de la respuesta a mapear,
#tambi√©n pod

library("readr")
library("dplyr")
library("stringr")
library("downloader")
library("rgeos")
library("rgdal") 
library("ggplot2")
library("viridis")
library("raster")
library("leaflet")+
library("htmltools")
library("htmlTable")
library("scales")
library("broom")
library("tidyr")
library("maptools")

URL <- "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=microdatos_archivos/encuesta_intercensal/2015/eic2015_"
#Download the encuesta intercensal microdata
for(i in 1:32) {
            download(str_c(URL, str_pad(i, 2, "left", "0"), "_csv.zip&ht=02"),
                     str_c("data/", i), mode = "wb")
            unzip(str_c("data/", i), exdir = "data")
            file.remove(str_c("data/", i))
}



LISTAS<- read.csv(file = "TR_VIVIENDA01.CSV", fileEncoding="latin1", colClasses = c("ENT"="factor", "MUN"="factor")) #para explorar los datos
LISTAS$id <- str_c(as.character(LISTAS$ENT), as.character(LISTAS$MUN))

getwd()
setwd("e:/Intercensal/Intercensal/VIVIENDA")
LISTAS<-read.csv(file = lista[1])


lista<- list.files("./VIVIENDA")
lista[2]

for (i in 1:32) {
            print(i)
            df<- read.csv(lista[i])
}


setwd("e:/Intercensal/Intercensal")
VIVIENDA <- data.frame()
for (i in 1:32) {
            print(i)
            df <- read.csv(str_c("data/TR_VIVIENDA", str_pad(i,2, "left", "0"), ".CSV"), fileEncoding = "latin1", colClasses = c("ENT"="factor", "MUN"="factor"))
            
            VIVIENDA_STATE <- df %>%
                        group_by(ENT,NOM_ENT, MUN, NOM_MUN) %>%
                        summarise(total = sum(FACTOR)) %>%
                        left_join(
                                    df %>%
                                    group_by(ENT,NOM_ENT, MUN,NOM_MUN) %>%
                                    filter(INGR_PEROTROPAIS %in% c(1)) %>%
                                    summarise (VAR = sum (FACTOR))
                        )   %>%
                        mutate(PER_VAR = VAR/total) %>%
                        arrange(desc(PER_VAR))
            VIVIENDA <- rbind(VIVIENDA, VIVIENDA_STATE)
            rm(df)
            gc()
}

VIVIENDA$id<- str_c(VIVIENDA$ENT, VIVIENDA$MUN)
VIVIENDA$VAR[is.na(VIVIENDA$VAR)]<-0
VIVIENDA$PER_VAR[is.na(VIVIENDA$PER_VAR)] <- 0
VIVIENDA$concat<-VIVIENDA$id

class(VIVIENDA$concat)

write_csv(VIVIENDA, "data/VIVIENDA1.csv") 

#si ya se hizo lo anterior antes se puede empezar desde aquÌ
#setwd("e:/Intercensal/Intercensal")
#VIVIENDA <- read_csv("./data/VIVIENDA1.csv")

muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2") 
states <- readOGR("map/mge2013v6_2.shp", "mge2013v6_2")

states_df <- fortify(states)  #id da el estado
bb <- bbox(as(extent(muns) , "SpatialPolygons" ) )
muns@data$id = as.numeric(muns@data$concat)
muns@data <- plyr::join(muns@data, VIVIENDA, by = "id")
muns_df <- fortify(muns,region = "concat")
muns_df <- plyr::join(muns_df, VIVIENDA, by="id")

theme_bare <-theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.background=element_blank(),
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())

####Mapa nacional 
MAPA <- ggplot()+
            geom_map(data = muns_df, map = muns_df, 
                     aes(map_id = id, x = long, y = lat, group = group, fill= PER_VAR),
                     color = "white", size = 0.04) +
            geom_polygon(data = states_df,
                         aes(long, lat, group = group),
                         color = "#aaaaaa", fill = NA, size = 0.3) +
            scale_fill_viridis("Porcentaje", trans = "sqrt", labels = percent) +
            coord_map() +
            labs(x = "", y = "", title = "Porcentaje de hogares en los que respondiÛ <<SÌ>> a la pregunta <<¡lguna persona que vive en esta vivienda recibe dinero de alguien que vive en otro paÌs>>")
coord_map("albers", lat0 = bb[2,1], lat1 = bb[2,2]) +
            theme_bw() +
            theme(legend.key = element_rect(fill = NA)) +
            theme_bare
ggsave("graphs/DINERO_EXTERIOR.png", plot = MAPA, dpi=500, width =14, height = 11)
MAPA

#Municipios m[as altos]
top<-head(VIVIENDA[order(-VIVIENDA$PER_VAR),20])

head(VIVIENDA[order(-VIVIENDA$PER_VAR)],)

head(VIVIENDA)

top <- head(VIVIENDA[order(-VIVIENDA$PER_VAR),],20)[,c(2,4,5,7)]

head(top)


