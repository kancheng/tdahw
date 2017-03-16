﻿getwd()
library(leaflet)
dt = read.csv( "yellow_tripdata_2015-08.csv"
, stringsAsFactors = FALSE)

head(dt)

# dim(dt[(dt$dropoff_longitude != 0 )&(dt$dropoff_latitude != 0 )&(dt$pickup_longitude != 0 )&(dt$pickup_latitude != 0 ), ])
dt = dt[(dt$dropoff_longitude != 0 )&(dt$dropoff_latitude != 0 )&(dt$pickup_longitude != 0 )&(dt$pickup_latitude != 0 ), ]

# Q1: What are the most pickups and drop offs region?
# hint: use one of cluster algorithms and count number data points of each cluster.

pkdt = dt[,6:7]
head(pkdt)
test  = kmeans( x = pkdt, center = 5, nstart = 25)
head(test$cluster)
newdt = cbind(dt,kclu = test$cluster)
head(newdt)


newdtk1 = newdt[newdt$kclu == 1,]
newdtk2 = newdt[newdt$kclu == 2,]
newdtk3 = newdt[newdt$kclu == 3,]
newdtk4 = newdt[newdt$kclu == 4,]
newdtk5 = newdt[newdt$kclu == 5,]

dtleaf = data.frame( 
kclu =c(1, 2, 3, 4, 5),
peo = c( 4891798, 2306042, 290710, 346717,3124384),
pklog = c( -73.98322, -73.96069, -73.77907, -73.87234, -73.99686),
pklat = c( 40.75502, 40.7795, 40.64559, 40.76956, 40.72543),
drlog = c( -73.97659, -73.9649, -73.93191, -73.9471, -73.98239),
drlat = c( 40.75201, 40.76886, 40.73064, 40.74938, 40.73649))

# summary

map = leaflet(dtleaf) %>% addTiles()
map %>%  addCircles(lng = ~pklog, lat = ~pklat, radius = ~sqrt(peo)) 

map %>%  addCircles(lng = ~drlog, lat = ~drlat, radius = ~sqrt(peo), color = c('red'))

#leaflet(cities) %>% addTiles() %>%
# addCircles(lng = ~Long, lat = ~Lat, weight = 1,
#    radius = ~sqrt(Pop) * 30, popup = ~City
#  )

# leaflet(quakes) %>% addTiles() %>% addMarkers(
#  clusterOptions = markerClusterOptions()
# )

# Q2: What is the best time to take taxi?  
# hint: count number of pickups in different hour.
# Picktime: 2015-07-02 14:04:06 Droptime: 2015-12-09 07:33:08
# tem =  str_detect(string = dt$, "[][][][][]")

# 抓日期
temdata =  newdt$tpep_pickup_datetime
temtime =  str_sub(temdata, 1,10)
temDay =  str_sub(temdata, 9,10)
temHour =  str_sub(temdata, 12,13)
temindex = duplicated(temtime)
tem = temtime[!temindex]

newd2t = data.frame(day =  as.numeric(temDay), hour =  as.numeric(temHour),
passenger_count = as.numeric(newdt$passenger_count))

sumdf = data.frame()
for(i in 1:length(tem)) {
	temvtor = numeric()
	nam = paste0("q2byd", i)
	comdtext = paste0("newd2t[newd2t$day == ", i, ", ]")
	rcomds = eval(parse(text = comdtext))
		assign( nam, 
		rcomds)
	for(c in 0:23){
		nam2 = paste0( nam, "h", c)
		comdtext2 = paste0( nam, "[", nam, "$hour == ", c, ", ]")
		rcomds2 = eval(parse(text = comdtext2))
			assign( nam2, 
			rcomds2)	
		textsum = paste0(nam2, "$passenger_count")
		rsum = sum(eval(parse(text = textsum)))
		temvtor = c(temvtor, rsum)
	}
	sumdf = rbind( sumdf, temvtor)
}
colnames(sumdf) = c("h0", "h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h10",
 "h11", "h12", "h13", "h14", "h15", "h16", "h17", "h18", "h19", "h20", "h21", "h22", "h23")

meanvtor = numeric()
for( g in 1:NCOL(sumdf)){

	text.vtor =  colnames(sumdf)
	meantext = paste0( "sumdf$", text.vtor[g] )
	rmeacmd = mean(eval(parse(text = meantext)))
	meanvtor = c( meanvtor, rmeacmd)
}

tmsumdf = as.data.frame(t(as.matrix(sumdf)))

ggpic = data.frame(count = meanvtor, hour = colnames(sumdf))
ggpic = cbind( ggpic, nm = 1:NROW(ggpic))
plot(ggpic$nm,ggpic$count, type = "b")

# Q3: Whether weather affects customers to take taxi or not?
# Q3 : 什麼天氣會影響到顧客搭計程車


# Q4: Does long distance trip imply more tip?
# Q4 : 長距離搭乘是否會影響到小費的多寡 

## Test
# 
# kan = leaflet()
# kan = addTiles(kan)
#
# kan = addMarkers(kan, lng = c(-73.99981,-74.99981),
# lat = c(40.74334,42.74334), popup ="kantest"
# )
# m <- leaflet() %>% setView(lng = dt$pickup_longitude, lat = dt$pickup_latitude, zoom = 12)
# m %>% addTiles()
#
# library(ggmap)
# map <- get_map(location = 'Taiwan', zoom = 7)
# ggmap(map) + geom_point(aes(x = lon, y = lat, size = UVI), data = uv)
#