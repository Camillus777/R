library(tidyverse)
library(readxl)
library(stringr)
library(scales)
library(ggmap)
library(sf)
library(rmapshaper) #for polygon simplification
library(viridis)
library(grid)
library(gridExtra)
library(ggrepel)


my_map_theme = theme_minimal(base_size = 13)+ 
               theme(text = element_text(color="grey70"),
                     plot.title = element_text(color="#11457E", face="bold", size=14),
                     plot.caption = element_text(size=10),
                     #legend
                     legend.position="bottom",
                     #legend.text=element_text(size=12),
                     legend.box.spacing = unit(c(0,0,0,0), "cm"),
                     legend.margin=margin(-12,0,0,0),
                     #axix
                     axis.text=element_blank(),
                     #axis.text=element_blank(color="grey70")
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     #margins
                     plot.margin=unit(c(0.1,0.1,0,0.1),"cm") #top, right, bottom,left
                   
                    )

mygrob = textGrob("Kamil Franek (kamilfranek.com)", x=0.1,  y=0.95, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic"))
mytext = "kamilfranek.com"

natural_colors = c("#009F6B","#C40233","#FFD300","#0087BD") #green, red, yellow, blue
cz_colors = "#11457E"


#### Data Loading ####
setwd("C:/Users/Camillus/Docs/04 Analytics-Projects/17-08 Age structure and Population Distribution in CZ")
file_m = "input/1300641705.xlsx"
file_w = "input/1300641706.xlsx"

population_m =  read_excel(file_m, range  = "Y3:CW107") %>%
  filter(row_number() > 3) %>%
  cbind (read_excel(file_m, range  = "A7:A107", col_names = FALSE)) %>%
  setNames(str_sub(names(.),1,6)) %>%
  rename(age = X__1) %>%
  gather(key=okres,value=count,-age) %>%
  mutate(gender="M")

population_w =  read_excel(file_w, range  = "Y3:CW107") %>%
  filter(row_number() > 3) %>%
  cbind (read_excel(file_w, range  = "A7:A107", col_names = FALSE)) %>%
  setNames(str_sub(names(.),1,6)) %>%
  rename(age = X__1) %>%
  gather(key=okres,value=count,-age) %>%
  mutate(gender="W")

population = rbind(population_m,population_w) %>%
  mutate(age=as.numeric(age))%>%
  mutate(count=count/10^3)%>%
  arrange(age)

pop_okres = population %>%
  spread(gender, count)%>%
  group_by(okres)%>%
  summarize(M=sum(M),W=sum(W))%>%
  mutate(count=M+W)

obce_population  = read_excel("input/pocet_obyvatel_v_obcich_2017-01-01.xlsx", range  = "A6:I10000") %>%
  filter(!is.na(Total)) %>%
  rename(LAU2 = `LAU 2`,
         LAU1 = `LAU 1`) %>%
  mutate(LAU2 = as.integer(LAU2))

#obce_population %>% filter(Total>20000) %>% summarize(total = sum(Total))

#### polygon preparation ####
# read original source shapefile transform and save as RDS Okresy

st_read("shp/5514.xml", layer="AdministrativeUnit") %>%
  st_set_crs(5514) %>%
  st_transform(4326) %>%
  filter(LocalisedCharacterString!="Obec")%>%
  #select(geometry,identifier)%>%
  mutate(okres=factor(str_sub(identifier,1,6))) %>%
  st_as_sf() %>%
  as(Class = 'Spatial') %>%
  ms_simplify(keep = 0.05, weighting = 0.99)%>%
  st_as_sf()%>%
  saveRDS("shp/cr.rds")


st_read("shp/5514.xml") %>%
  st_set_crs(5514) %>%
  st_transform(4326) %>%
  filter(LocalisedCharacterString=="Obec")%>%
  #mutate(LocalisedCharacterString = factor(as.character(LocalisedCharacterString)))%>%
  #select(geometry,identifier)%>%
  mutate(okres=factor(str_sub(identifier,1,6))) %>%
  st_as_sf()%>%
  as(Class = 'Spatial') %>%
  ms_simplify(keep = 0.05, weighting = 0.99)%>%
  st_as_sf()%>%
  saveRDS("shp/cr_obec.rds")


### MAPS ####

#shp_border = readRDS("shp/cr_border.rds")

# plg_cr_border = shp_border %>% 
#   #filter(LocalisedCharacterString == "St?t") %>% 
#   mutate(row = 1:n()) %>% 
#   st_as_sf()%>%
#   st_coordinates()%>%
#   data.frame() %>% 
#   rename(x=X,y=Y) %>%
#   mutate(id=L1)

shp_data = readRDS("shp/cr.rds")
  
  map_polyg_cr = shp_data %>% 
    filter(is.na(LocalisedCharacterString)) %>% 
    #mutate(row = 1:n()) %>% 
    st_as_sf()%>%
    st_coordinates()%>%
    data.frame() %>% 
    rename(x=X,y=Y) %>%
    mutate(id=L2)
  
  map_polyg_okres = shp_data %>% 
    filter(LocalisedCharacterString=="Okres")%>% 
    #select(okres,geometry)%>% 
    #mutate(row = 1:n()) %>% 
    st_as_sf()%>%
    st_coordinates()%>%
    data.frame() %>% 
    rename(x=X,y=Y) %>%
    mutate(id=L2)
  
  map_polyg_kraj = shp_data %>% 
    filter(LocalisedCharacterString=="Kraj")%>% 
    #select(okres,geometry)%>% 
    #mutate(row = 1:n()) %>% 
    st_as_sf()%>%
    st_coordinates()%>%
    data.frame() %>% 
    rename(x=X,y=Y) %>%
    filter(L2!=2)%>%
    mutate(id=L2)
  
shp_obec = readRDS("shp/cr_obec.rds") %>% 
    filter(LocalisedCharacterString=="Obec")%>% 
    #select(okres,geometry)%>% 
    mutate(row = 1:n(),
           area = (function(x) as.numeric(units::set_units(st_area(x), km^2)))(geometry)
           ) %>% 
    left_join((obce_population),by=c("nationalCode"="LAU2"))%>%
    mutate (density = Total/area)%>%
    mutate (density_cat = cut(density, include.lowest = TRUE,ordered_result = TRUE,right = FALSE,dig.lab = 4, breaks = c(0,100,250,500,1000,2600))) %>%
    mutate (population_cat = cut(Total/1000, include.lowest = TRUE,ordered_result = TRUE,right = FALSE,dig.lab = 4, breaks = c(0,10,20,150,1300))) %>%
    mutate (population_cat2 = cut(Total/1000, include.lowest = TRUE,ordered_result = TRUE,right = FALSE,dig.lab = 4, breaks = c(0,10,20,150,1000,1500))) %>%  
  st_as_sf() 

shp_obec = cbind(shp_obec,st_coordinates(st_centroid(st_geometry(shp_obec))))

table_summary = shp_obec %>%
  group_by(population_cat2)%>%
  summarize(Total = sum(Total),
            count = n(), 
            pop_perc = sum(Total)/sum(shp_obec$Total), 
            count_perc=n()/nrow(shp_obec))%>%
  arrange(desc(population_cat2))%>%
  mutate(pop_perc_cum = cumsum(Total)/sum(shp_obec$Total),
         count_perc_cum = cumsum(count_perc))

map_polyg_obec = shp_obec%>%
    st_coordinates()%>%
    data.frame() %>% 
    rename(x=X,y=Y) %>%
    mutate(id=L3)
    #shp_data %>%  spline.poly(shp_data$geometry, 100, k=3)
  
# ploting 
  map_cr_all = ggplot() +  
    geom_polygon(data = map_polyg_cr,  aes(x,y,id), fill = "white", color="#11457E",  size=2.5)+ 
    expand_limits(map_polyg_cr$x,map_polyg_cr$y)+ 
    annotate("text", x=13.7,y=49.95,label="Population: 10.6 millions", color="#D7141A", alpha=1, size=11,hjust = 0)+
    annotate("text", x=13.7,y=49.65,label="Area: 78 866 km²", color="#D7141A", alpha=1, size=11,hjust = 0)+
    annotate("text", x=13.7,y=49.35,label="Density: 134 people per km²", color="#D7141A", alpha=1, size=11,hjust = 0)+
    #show prague
    #geom_point(aes(x=14.4378005, y=50.0755381),color="#D7141A", size=3)+
    coord_map()+ 
    theme_nothing()
  
  map_cr_okres = ggplot() +  
    geom_polygon( data = map_polyg_okres, aes(x,y,group=id),fill = "white", color="#11457E",  size=0.5)+ 
    geom_polygon( data = map_polyg_kraj, aes(x,y,group=id),fill = NA, color="#11457E",  size=2)+ 
    expand_limits(map_polyg_okres)+ 
    #show prague
    geom_point(aes(x=14.4378005, y=50.0755381),color="#D7141A", size=3)+
    coord_quickmap()+
    #coord_map()+
    theme_nothing()
  
  #obec polygon
  map_cr_obec_plg = ggplot() +  
    geom_polygon(data = map_polyg_obec,  aes(x,y,group=id), fill = "white", color="#11457E",  size=0.5)+ 
    expand_limits(map_polyg_obec)+ 
    #show prague
    geom_point(aes(x=14.4378005, y=50.0755381),color="#D7141A", size=1)+
    coord_map()+
    theme_nothing()

  annotat = data.frame(x=c(18,17),
                      y=c(50,50),
                      text=c("test1","test2")
                      )

  #cities bubbles (velikost bubles optimalizovana pro A4 PDF export)
  map_cr_obec_bubles = ggplot()+#, aes(group=row)) +  
    geom_map(data=shp_obec, map = map_polyg_obec, aes(map_id=row), fill="white",color="grey99")+
    geom_polygon(data = map_polyg_cr,  aes(x,y,id), fill = "white", color="#11457E",  size=2, alpha=0)+
    #annotate("text", x=15.7,y=49.75,label=mytext, color="grey70", alpha=0.15, size=20)+
    #geom_map(data=(shp_obec%>%filter(Total>=20000)), map = map_polyg_obec, aes(map_id=row), fill="blue", color="white")+
    geom_point(data=(shp_obec%>%filter(Total>=10000)), aes(X,Y, size=Total/1000, color=population_cat))+
    scale_color_manual(values=c('#fdcc8a','#fc8d59','#D7141A'))+
    scale_size_area(max_size = 20)+ # velikost optimalizovana pro A4 pdf export
    #geom_text_repel(data=annotat, aes(x,y,label=text),min.segment.length=0, segment.color = "grey70", nudge_x = 0.1)+
    geom_text_repel(data = (shp_obec%>%filter(!(X__1 %in% c("Ústí nad Labem","Pardubice")))%>%filter(Total>90000 & Total <500000)), aes(label = X__1, x=X,y=Y), min.segment.length=0, segment.size = 0.3, nudge_x = -0.15,nudge_y = -0.08,color="grey70", size =3)+ #,
    geom_text_repel(data = (shp_obec%>%filter((X__1 %in% c("Ústí nad Labem","Pardubice")))%>%filter(Total>90000 & Total <500000)), aes(label = X__1, x=X,y=Y), min.segment.length=0, segment.size = 0.3, nudge_x = 0.4,nudge_y = -0.03,color="grey70", size =3)+ #,
    geom_text(data = (shp_obec%>%filter(Total>1000000)), aes(label = X__1, x=X,y=Y),color="grey70", size =5)+
    
    #annotation_custom(mygrob,xmin=17, ymax=17,ymin=49.5)+
    #geom_polygon(data = map_polyg_obec,  aes(x,y,group=id), fill = "white", color="#11457E",  size=0.5)+ 
    expand_limits(map_polyg_obec)+ 
    #show prague
    #geom_point(aes(x=14.4378005, y=50.0755381),color="#D7141A", size=1)+
    coord_quickmap()+
    my_map_theme+
    guides(size=FALSE,color=guide_legend(override.aes = list(size=7)))+
    labs(x="",
         y="", 
         color = "Categories by city size (thousands) ", 
         #size = "Size of City (in thousands)",
         title = "50% of Czechs Live In Cities With More Than 10 000 Inhabitants",
         subtitle = "Showing all cities over 10k inhabitants. Size of circles represent population size.",
         caption = "Kamil Franek (kamilfranek.com) - SOURCE: czso.cz   ")
  #Kamil Franek (kamilfranek.com)       
  
 # pro vypocet pod?lu na poctu a podilu na obyvatelstvu plati 80/20  
  (shp_obec%>%filter(Total>=10000)%>%summarize(sum(Total)))[[1]]/(shp_obec%>%summarize(sum(Total)))[[1]]
  (shp_obec%>%filter(Total>=10000)%>%summarize(n()))[[1]]/(shp_obec%>%summarize(n()))[[1]]
  
  sum(shp_obec$Total)
  

  
  #continuous fill
  map_cr_obec_hustota = ggplot()+#, aes(group=row)) +  
    geom_map(data=shp_obec, map = map_polyg_obec, aes(map_id=row, fill=density))+
    scale_fill_continuous(limit=(c(0,1000)), low="white", high="#D7141A", oob=squish)+
    geom_polygon(data = map_polyg_cr,  aes(x,y,id), fill = "white", color="#11457E",  size=2, alpha=0)+ 
    #geom_map(data=(shp_obec%>%filter(Total>=90000)), map = map_polyg_obec, aes(map_id=row), fill="#D7141A")+
    #geom_map(data=(shp_obec%>%filter(Total>20000 & Total<90000)), map = map_polyg_obec, aes(map_id=row), fill="#11457E")+
    #geom_polygon(data = map_polyg_obec,  aes(x,y,group=id), fill = "white", color="#11457E",  size=0.5)+ 
    expand_limits(map_polyg_obec)+ 
    #show prague
    #geom_point(aes(x=14.4378005, y=50.0755381),color="#D7141A", size=1)+
    coord_quickmap()+
    my_map_theme+
    labs(x="",y="")+
   
  
  #categorical fill
  map_cr_obec_hustota_cat = ggplot()+#, aes(group=row)) +  
    geom_map(data=shp_obec, map = map_polyg_obec, aes(map_id=row, fill=density_cat))+
    #scale_fill_discrete(limit=(c(0,1000)), low="white", high="#D7141A", oob=squish)+
    #scale_fill_manual(values=c('white','#fcbba1','#fc9272','#fb6a4a','#de2d26','#D7141A'))+
    scale_fill_manual(values=c('grey97','#fef0d9','#fdcc8a','#fc8d59','#D7141A'))+
    #scale_fill_manual(values=c('white','#f7f7f7','#cccccc','#969696','#636363','#252525'))+
    geom_polygon(data = map_polyg_cr,  aes(x,y,id), fill = "white", color="#11457E",  size=2, alpha=0)+ 
    #geom_map(data=(shp_obec%>%filter(Total>=90000)), map = map_polyg_obec, aes(map_id=row), fill="#D7141A")+
    #geom_map(data=(shp_obec%>%filter(Total>20000 & Total<90000)), map = map_polyg_obec, aes(map_id=row), fill="#11457E")+
    #geom_polygon(data = map_polyg_obec,  aes(x,y,group=id), fill = "white", color="#11457E",  size=0.5)+ 
    expand_limits(map_polyg_obec)+ 
    
    #show prague
    #geom_point(aes(x=14.4378005, y=50.0755381),color="#D7141A", size=1)+
    coord_quickmap()+
    my_map_theme+
  labs(x="",
       y="", 
       fill=bquote("Population per "~km^2 ),
       #size = "Size of City (in thousands)",
       title = "Population density of Czech Republic",
       subtitle = "Showing city level density as a number of inhabitants per square kilometer",
       caption = "Kamil Franek (kamilfranek.com) - SOURCE: czso.cz    ")
    #theme_nothing()+theme()
    
  
#### Charts Population Trees####
# population tree
  
poptree = ggplot()+
  stat_summary(data=population %>% filter(gender=="M"),aes(x=age, y=-count,fill=gender),geom="bar", fun.y = sum, alpha=0.7)+
  stat_summary(data=population %>% filter(gender=="W"),aes(x=age, y=count,fill=gender),geom="bar", fun.y = sum, alpha=0.7)+
  scale_y_continuous()+
  #scale_fill_manual(values=c("forestgreen","springgreen3"))+
  scale_fill_manual(values=c("steelblue3","red1"))+
  theme_minimal()+
  coord_flip()

#### Charts polygon maps ####
### pozn?mky k map?m
# existuj? dva z p?soby geom_polygon ale pak je nutne pred tim spojit vse do jednoho dataframe - coz znamena 
#  opakovat hodnoty pro kazdy polygon bod - nebo pouzit geom_map.
#  pozor_ze u geom_map je nutn? pouzit expand_limits, protoze jinak se sice mapa nakresli ale my ji nevidime. 
# dataframe z mapou musim mit sloupec id a rovnez bud x, y nebo long lat

## pokud chci spojovat

#shp_data = st_read("test_shp/HraniceAdministrativnihoUzemi.shp") %>%
#  st_set_crs(5514 ) %>%
#  st_transform(4326) #%>%
#centr = st_centroid(shp_data)
#shp_data$centroid =centr$geometry



shp_data = readRDS("shp/cr.rds")

shp_data = shp_data %>% 
  filter(LocalisedCharacterString == "Okres") %>% 
  mutate(row = 1:n()) %>% 
  st_as_sf()



map_polyg = data.frame(st_coordinates(shp_data))  %>% 
  left_join((shp_data%>%mutate(id=okres)),by=c("L2"="row"))%>%
  rename(x=X,y=Y)



centr = map_polyg %>% 
  group_by(id)%>%
  summarize(centr_x=(max(x)-min(x))/2+min(x),centr_y=(max(y)-min(y))/2+min(y))

pop_okres2 = pop_okres %>% 
  left_join(centr,by=c("okres"="id"))%>% 
  left_join(shp_data,by=c("okres"="okres"))

## full CR map




map1 = ggplot(data=pop_okres2,aes(fill=count,group=okres)) +
  #geom_point(aes(x=okres,y=count))
  geom_map(map = map_polyg, aes(map_id=okres), color="white")+ 
  geom_text(aes(label = text, x = centr_x, y = centr_y),color="grey70", size =3)+
  geom_text(aes(label = round(count,0), x = centr_x, y = centr_y),color="grey70", size =3,nudge_y = -0.03)+
  #scale_fill_viridis(limit=(c(0,500)),oob=squish, direction=1)+
  scale_fill_continuous(limit=(c(100,300)), low="white", high="steelblue", oob=squish)+
  expand_limits(map_polyg)+
  #theme_minimal()+
  coord_quickmap()+
  theme_nothing() + 
  theme(legend.position = "right", 
        legend.key.size =  unit(0.5, "in")
        )

# pro testovac? zobrazen? jen samotn?ho polygonu
#plot(shp_data)

map_polyg_ = 
ggplot() +  geom_polygon(data = map_polyg,  aes(x,y,id), color="grey90")+ expand_limits(map_polyg$x,map_polyg$y)+ coord_map()+ theme_nothing()
  #geom_point(aes(x=okres,y=count))
 

#### NOTEST a TEST ####


## overlay of ggmap pokus
mapaCR = get_map(location = "Ceska republika", source="google", zoom=7, maptype="roadmap",color="bw") 
map_polyg_adj = map_polyg %>% mutate(x=x,y=y-0.045)

ggmap(mapaCR) +
  geom_map(inherit.aes = FALSE, aes(group=okres,map_id=okres, fill=count), alpha=0.9,color="grey90",map = map_polyg_adj, data=pop_okres2)+ 
  scale_fill_continuous(limit=(c(0,200)), low="white", high="steelblue", oob=squish)+
  geom_text(data = pop_okres2, aes(label = nazok_a, x = centr_x, y = centr_y),color="grey90", size =3)+
  geom_text(data = pop_okres2, aes(label = round(count,0), x = centr_x, y = centr_y),color="grey90", size =3,nudge_y = -0.03)+
  coord_quickmap()

######

ddd = world%>%filter(region=="Czech Republic")
dddm = map_data("world", region=="Czech Republic")

rr = fortify(d, Region = "kodnuts")
d$kodnuts = rownames(d$geometry)
d@data$id = rownames(utah@data)

world <- map_data("world")
ggplot(data=world%>%filter(region=="Czech Republic"), aes(long, lat))+geom_polygon(aes(group=group), fill=NA, color="black")+
  coord_map()+ theme_nothing()

ff = world.cities %>% filter(country.etc=="Czech Republic", pop>100000)

+geom_map(data=(world%>%filter(region=="Czech Republic")), map=world%>%filter(region=="Czech Republic"), aes(long, lat, map_id=group), color="black")

test_data = st_read("shp/1.xml") %>%
  st_set_crs(5514 ) %>%
  st_transform(4326) 

shp_data = test_data %>% filter(LocalisedCharacterString=="Okres") %>% st_as_sf()

shp_data = st_read("cr-shp-jtsk/plochy/okresy_pseudo.shp") %>%
  st_set_crs(5514 ) %>%
  st_transform(4326) %>%
  #shp_data = aggregate(shp_data, list(shp_data$kn), FUN=function(x) x[1]) %>%
  mutate(kodnuts = as.character(kodnuts))%>%
  mutate(kodnuts  = str_replace(kodnuts,"CZ061","CZ063"))%>% 
  mutate(kodnuts  = str_replace(kodnuts,"CZ062","CZ064"))%>% 
  arrange(kodnuts)%>% 
  st_as_sf() #%>% 
#st_simplify(preserveTopology=TRUE, dTolerance=0.05) nefunguje v sf package pro mapy, ale da se pouzit jiny package pokud bude potreba


spline.poly <- function(xy, vertices, k=3, ...) {
  # Assert: xy is an n by 2 matrix with n >= k.
  
  # Wrap k vertices around each end.
  n <- dim(xy)[1]
  if (k >= 1) {
    data <- rbind(xy[(n-k+1):n,], xy, xy[1:k, ])
  } else {
    data <- xy
  }
  
  # Spline the x and y coordinates.
  data.spline <- spline(1:(n+2*k), data[,1], n=vertices, ...)
  x <- data.spline$x
  x1 <- data.spline$y
  x2 <- spline(1:(n+2*k), data[,2], n=vertices, ...)$y
  
  # Retain only the middle part.
  cbind(x1, x2)[k < x & x <= n+k, ]
}

#obec standard jenom pro info nepouzivam v exportu
map_cr_obec_standard = ggplot()+#, aes(group=row)) +  
  geom_map(data=shp_obec, map = map_polyg_obec, aes(map_id=row), fill="white",color="grey99")+
  geom_map(data=(shp_obec%>%filter(Total>=90000)), map = map_polyg_obec, aes(map_id=row), fill="#D7141A", color="white")+
  geom_map(data=(shp_obec%>%filter(Total>20000 & Total<90000)), map = map_polyg_obec, aes(map_id=row), fill="#11457E",color="white")+
  geom_map(data=(shp_obec%>%filter(Total<100)), map = map_polyg_obec, aes(map_id=row), fill="grey",color="white")+
  geom_polygon(data = map_polyg_cr,  aes(x,y,id), fill = "white", color="#11457E",  size=2.5, alpha=0)+
  #geom_point(data=(shp_obec%>%filter(Total>=90000)), aes(X,Y), fill="#D7141A", color="white")+
  geom_text(data = (shp_obec%>%filter(Total>20000)), aes(label = X__1, x=X,y=Y),color="grey70", size =3)+
  #geom_polygon(data = map_polyg_obec,  aes(x,y,group=id), fill = "white", color="#11457E",  size=0.5)+ 
  expand_limits(map_polyg_obec)+ 
  #show prague
  #geom_point(aes(x=14.4378005, y=50.0755381),color="#D7141A", size=1)+
  coord_quickmap()+
  theme_nothing()
