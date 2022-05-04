###### Dark Kangaroo Mouse Species Habitat Distribution Model ######
# This code will be used to create a SHDM for the Dark Kangaroo Mouse
# Data was acquired from 
# Code Written by Christine Nguyen

##### IMPORT AND ORGANIZE DATA ######
wd <- "/Users/han/Documents/Research/SHDM/Dark_Kangaroo_Mouse"
setwd(wd)

library(readr)
library(foreign) 
library(sp)  
library(rgdal) 
library(raster)
library(rgeos)

gov.data<- read_csv("Source/All_known_May19.csv") #import raw data
arctos.data <- read_csv("Source/ArctosData_DKM.csv")#import raw data

##### Explore Data

names(gov.data) #examine column names
table(gov.data$SNAME) #examine species

#determine if any coordinates observations are missing
table(is.na(gov.data$UTMX))
table(is.na(gov.data$UTMY))

names(arctos.data) #examine column names
table(arctos.data$SCIENTIFIC_NAME)#examine species
arctos.data<- subset(arctos.data, SCIENTIFIC_NAME != "Microdipodops pallidus ruficollaris") #remove different species from data
table(arctos.data$SCIENTIFIC_NAME) #check to see if different species was removed

table(is.na(arctos.data$DEC_LAT)) #check to see if coordinates are missing
arctos.data<- subset(arctos.data, DEC_LAT != "NA") #remove observations with missing coordinates

table(is.na(arctos.data$DEC_LONG)) #check to see if observations without coordinates are removed

##### Format Date 

gov.data$date.r <- gov.data$SURVEYDATE #create a new column for observation dates
gov.data$date.r <- ifelse(is.na(gov.data$date.r), gov.data$OBSDATE1, gov.data$date.r) #combine observation dates into one column 

sum(is.na(gov.data$date.r)) # check for observations without dates

gov.data$date.r <- as.Date(gov.data$date.r, format = "%m/%d/%y") #change format of dates
arctos.data$date.r <- as.Date(arctos.data$VERBATIM_DATE, format = "%d-%b-%y") #change format of dates

###### Subset Data to Create Minimalist Data Set
head(gov.data,2)
gov.data2 <- gov.data[c(1:3,5,9,19)]
dim(gov.data2)
 
head(arctos.data,2)
arctos.data2 <- arctos.data[c(3,8:11)]
head(arctos.data2)
 

##### Create Presence Data #####
gov.data2$presence <- 1
arctos.data2$presence <- 1

##### Add TSN and Species Code #####
 
gov.data2$ITIS <- "TSN 180252"
arctos.data2$ITIS<- "TSN 180252"

gov.data2$SpeciesCode <- "mime"
arctos.data2$SpeciesCode<- "mime"

##### ASSIGN COMMON REFERENCE SYSTEM#####


prj.utmN83z12 <- "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" #proj4string for gov data
# http://spatialreference.org/ref/epsg/4326/ => EPSG 4326
prj.wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # project4string for arctos data

prj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #proj4string for chosen common reference system

gov.pts.in <-gov.data2[,c("UTMX", "UTMY")] # create data frame or presence coordinates
coordinates(gov.pts.in) <- ~UTMX + UTMY # convert to spatial points
proj4string(gov.pts.in) <- CRS(prj.utmN83z12) # input projection here
gov.pts.outSP <- spTransform(gov.pts.in, CRS(prj.USGS)) # output pts as spatial object
head(gov.pts.outSP)
plot(gov.pts.outSP)

arctos.pts.in <-arctos.data2[,c("DEC_LONG", "DEC_LAT")]# create data frame or presence coordinates
coordinates(arctos.pts.in) <- ~DEC_LONG + DEC_LAT # convert to spatial points
proj4string(arctos.pts.in) <- CRS(prj.wgs84) # input projection here; assume lat-long EPSG 4269
arctos.pts.outSP <- spTransform(arctos.pts.in, CRS(prj.USGS)) # output pts as spatial object
head(arctos.pts.outSP)
plot(arctos.pts.outSP)

class(arctos.pts.outSP)

##### BUILD R SPATIAL AND R DATAFRAME OBJECTS #####
gov.DF <- as.data.frame(gov.pts.outSP) #create gov data frame from spatial points
names(gov.DF) <- c("caeac_x", "caeac_y") #rename columns with correct coordinate system
str(gov.DF)   

arctos.DF <- as.data.frame(arctos.pts.outSP) 
names(arctos.DF) <- c("caeac_x", "caeac_y")  #rename columns with correct coordinate system
str(arctos.DF)


# build minimalist data structure
gov.data3 <- cbind(gov.data2, gov.DF)  #combine new coordinates with old dataframe
names(gov.data3)  
gov.presDF <- gov.data3[c("ITIS", "SpeciesCode", "date.r", "caeac_x", "caeac_y", "presence")] #subset columns of data for minimalist data set
head(gov.presDF, 2)  

arctos.data3 <- cbind(arctos.data2, arctos.DF)  
names(arctos.data3)  
arctos.presDF <- arctos.data3[c("ITIS", "SpeciesCode", "date.r", "caeac_x", "caeac_y", "presence")] 
head(arctos.presDF, 2)  


#Create spatial data frame
gov.presSP <- SpatialPointsDataFrame(gov.pts.outSP, gov.presDF) 
class(gov.presSP)
#writeOGR(gov.presabsSP, dsn=".", layer="gov_presabs", driver="ESRI Shapefile")
#save(gov.presabsSP, gov.presabsDF, file = "gov.presabs.RData") 
arctos.presSP <- SpatialPointsDataFrame(arctos.pts.outSP, arctos.presDF)
class(arctos.presSP)

#Merge Dataframes and Spatial Points

mime.presDF <- rbind(gov.presDF, arctos.presDF)
mime.pts.outSP <- rbind(gov.pts.outSP, arctos.pts.outSP)
plot(mime.pts.outSP)
mime.presSP <- SpatialPointsDataFrame(mime.pts.outSP, mime.presDF) 
class(mime.presSP)

#writeOGR(mime.presabsSP, dsn=".", layer="mime_presabs", driver="ESRI Shapefile")

save(mime.presSP, mime.presDF, file = "mime.pres.RData") 



##### CREATE MODELLING FRAME #####
mime.pres <- get(load("mime.pres.RData"))

# Option 1: Create Presence Bouding box

tru.ext <- extent(min(mime.pres$caeac_x), max(mime.pres$caeac_x), 
                  min(mime.pres$caeac_y), max(mime.pres$caeac_y)) # true extent of presence points
tru.box <- raster(tru.ext, crs = prj.USGS)  
tru.box


# Option 2: Buffered Bounding Boxes

tru.extb <- extent(min(mime.pres$caeac_x) - 50000, max(mime.pres$caeac_x) + 50000,                  min(mime.pres$caeac_y) - 50000, max(mime.pres$caeac_y) + 50000) #buffered true extent by 50 km
tru.extb  # examine buffer extent
extent(tru.box)  # compare true extent to buffer extent above 

# assign projection extent
tru.buf <- raster(tru.extb, crs = prj.USGS)  # convert extent to raster w/wgs84 projection
tru.buf 

##### Convex hull bouding box
tru.c1 <- chull(mime.presDF[c(4:5)])  # identifies vertices of convex polygon
# note start of head() and end of tail() don't match
head(tru.c1, 2);  tail(tru.c1, 2)
tru.c2 <- c(tru.c1, tru.c1[1])  # closes polygon ie links tail to head
# now start of head() and end of tail() match
head(tru.c2, 2);  tail(tru.c2, 2)  
tru.c3 <- mime.presDF[tru.c2, ]  # extract x,y of vertices ONLY; drop all other obs
head(tru.c3, 2)
tru.xy <- tru.c3[c(4:5)]  # keep only lat(x) long(y)
head(tru.xy, 2)  # examine final [X,Y]s of convex hull points


tru.p1 <- Polygon(tru.xy)  # creates object spatial class obj; pkg sp
tru.p2 <- Polygons(list(tru.p1), "p1")  # weird catechism; accept at face value
tru.p3 <- SpatialPolygons(list(tru.p2))  # ditto
tru.p3  # examine; note SpatialPolygons class

#savePlot(filename = "mod2.5fig05.pdf", type = "pdf")

#### OPTION #4:  build point-buffered bounding box
tru.xy <- (mime.presDF[c(4:5)])  # extract presence data
tru.sp <- SpatialPoints(tru.xy)  # convert to spatial object


# buffer each presence and dissolve all buffers into polygon 
library(rgeos)  # load if needed


tru.pb <- gBuffer(tru.sp, width = 50000, quadsegs = 3)  # higher quadsegs, more "round" is buffer circle

#NOT RUN; code to keep individual polygons
#tru.allpoly <- gBuffer(tru.sp, width = .25, byid = T, quadsegs = 3) 
tru.pb  # examine; note SpatialPolygons class
#plot(tru.pb) # NOT RUN; polygon before rasterizing 


##### START ASSIGN RESOLUTION & VALUES TO BOUNDING BOXES #####
# obtain desired resolution from existing GIS data layer; ex. is elevation
#  usually a "base" layer with defined resolution for all data layers

path.root <- "~/Documents/sdmR-V2019.2"
path.gis <- paste(path.root, "/data/gis_layers", sep = "")
setwd(path.gis)

elev <- raster("elev_1k_aea.img")  # load a GIS predictor layer 

setwd(wd)

res(elev)  # resolution of existing GIS data layer

# resolution and value for bounding box tru.box
res(tru.box) <- res(elev)  # assign resolution to bbox extent
values(tru.box) <- 1  # assign a value to bbox cells; arbitrary
tru.box  # examine; final raster of tru.box

# resolution and value for buffered box tru.buf
res(tru.buf) <- res(elev)  # assign resolution to bbox extent
values(tru.buf) <- 1  # assign a value to bbox cells; arbitrary
tru.buf  # examine; final raster of tru.buf

# assign resolution for convex polygon tru.con
tru.con <- rasterize(tru.p3, tru.buf)  # rasterize polygon to spatial extent 
tru.con  # examine; final raster of tru.con
#plot(tru.con) # NOT RUN; polygon after rasterizing 

# assign resolution for buffered points polygon tru.poly
tru.poly <- rasterize(tru.pb, tru.buf)  # rasterize polygon to spatial extent
tru.poly  #examine; final raster of tru.poly 
#plot(tru.poly) # NOT RUN; polygon after rasterizing 

# save rasterized frames
#setwd(path.gis)
#save("tru.box", "tru.buf", "tru.con", "tru.poly", file = "spatial.frames.RData")
######## END ASSIGN RESOLUTION & VALUES TO BOUNDING BOXES
##### START PLOT BOUNDING BOXES#####
## add boundaries for pretty

setwd(path.gis)
states <- readOGR(dsn = ".", layer = "na_states_aea")  # import shapefile 
setwd(wd)
####
# plot OPTION #1 bounding box w/presence points
plot(tru.box, col = "gray90", legend = F, 
     main = "Bounding box on presence points")  # main plot
# add points to plot
points(mime.presDF$caeac_x, mime.presDF$caeac_y, pch = 20, col = "darkgreen")  
plot(states, add = T, lwd = 1.5)  # make pretty w/boundaries
plot(tru.ext, col = "red", add = T)  # plot extent w/out buffer 


# save polygon as .img  and .pdf if desired
#setwd(path.gis)
#writeRaster(tru.buf, "tru_buf", format = "HFA", overwrite = T) # save as .img file
# outfile plot
#setwd(path.figs)
#savePlot(filename = "mod2.5fig01.pdf", type = "pdf")
####


####
# plot OPTION #2 buffer bounding box w/presence points
plot(tru.buf, col = "gray90", legend = F, 
     main = "Presence and Absence Points on Modelling Frame")  # main plot
# add points to plot
points(mime.presDF$caeac_x, mime.presDF$caeac_y, pch = 20, col = "darkgreen")  
plot(states, add = T, lwd = 1.5)  # make pretty w/boundaries
plot(tru.ext, col = "red", add = T)  # plot extent w/out buffer 
points(psu.points$tr.caeac_x, psu.points$tr.caeac_y, col = "blue") 
legend(1, 95, legend=c("Presence", "Psudo Absence"),
       col=c("darkgreen", "blue"), lty=1:2, cex=0.8)

# save polygon as .img  and .pdf if desired
#setwd(path.gis)
#writeRaster(tru.buf, "tru_buf", format = "HFA", overwrite = T) # save as .img file
# outfile plot
#setwd(path.figs)
#savePlot(filename = "mod2.5fig02.pdf", type = "pdf")
####


####
# plot OPTION #3 convex bounding box
plot(tru.con, col = "gray90", legend = F, 
     main = "Geographic Range of DKM")  # main plot
# add points to plot
points(mime.presDF$caeac_x, mime.presDF$caeac_y, pch = 20, col = "darkgreen") 
points(tru.c3, pch = 20, col = "red")  # add convex vertices
plot(tru.p3, add = T, border = "red")  # plot convex boundary
plot(states, add = T, lwd = 1.5)  # make pretty w/state boundaries 

# save polygon as .img  and .pdf if desired
#setwd(path.gis)
#writeRaster(tru.con, "tru_con", format = "HFA", overwrite = T)  # save as .img file
# outfile plot
#setwd(path.figs)
#savePlot(filename = "mod2.5fig03.pdf", type = "pdf")
####


####
# plot OPTION #4 point-buffered bounding box
plot(tru.poly, col = "gray90", legend = F, 
     main = "Buffered (~50 km) presence points")  # main plot
plot(tru.pb, add = T, border = "red")  # add border to buffer pnts
# add points to plot
points(mime.presDF$caeac_x, mime.presDF$caeac_y, pch = 20, col = "darkgreen")  
plot(states, add = T, lwd = 1.5)  # make pretty w/boundaries


# #test space
# 
# xy.test <- coordinates(mime.presabsDF[,c("caeac_x", "caeac_y")])

## save polygon as .img  and .pdf if desired
#setwd(path.gis)
#writeRaster(tru.con, "tru_con", format = "HFA", overwrite = T)  # save as .img file
## outfile plot
#setwd(path.figs)
#savePlot(filename = "mod2.5fig04.pdf", type = "pdf")
####
######## END PLOT BOUNDING BOXES
##### BUILD FISHNET #####

# examine frames
tru.box  # rectangle based on spp points; no buffer
tru.buf  # rectangle based on 50km buffer of spp points
tru.con  # rectangle based on 50km buffer; convex poly
tru.poly  # rectangle based on 50km buffer; spp point-based polygons

f1 <- coordinates(tru.buf)  # set spatial coords from tru.buf
f2 <- cellFromXY(tru.buf, f1)  # extract cell number from buffered extent
tru.bufcc <- as.data.frame(cbind(f1, f2))  # build datframe of x,y & cell number
# some dataframe clean-up
names(tru.bufcc)[1:3] <- c("cell.caeac_x", "cell.caeac_y", "FNETID")  # assign names
tru.bufcc <- tru.bufcc[c("FNETID", "cell.caeac_x", "cell.caeac_y")]  # reorder
# examine fishnet data structure
dim(tru.bufcc)  # dimension; rows is maximum number of cells in fishnet 
names(tru.bufcc)  # names in fishnet
head(tru.bufcc, 2)  # note first FNETID number
tail(tru.bufcc, 2)  # note last FNETID number matches dimension size

# convert dataframe to spatial object
pts.in <- tru.bufcc[, c("cell.caeac_x", "cell.caeac_y")]  # subset [x,y]
coordinates(pts.in) <- ~cell.caeac_x + cell.caeac_y # where x=longitude and y=latitude

prj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(pts.in) <- CRS(prj.USGS) # input projection here

pts.outSP <- spTransform(pts.in, CRS(prj.USGS)) # output pts as spatial obj
pts.outSP # examine
pts.outDF <- as.data.frame(pts.outSP) # output pts as data.frame
fnetDF <- cbind("FNETID" = tru.bufcc$FNETID, pts.outDF)  # add FNETID to dataframe
head(fnetDF, 2)  # examine

# build the spatial dataset
fnetSP.mime <- SpatialPointsDataFrame(pts.outSP, fnetDF)  # create spatial dataframe
fnetSP.mime  # examine

# save the dataframe and spatial objects
#setwd(path.mod2)  # output path
#writeOGR(fnetSP.spp106, dsn=".", layer="fnet_spp106", driver="ESRI Shapefile") # output shapefile
#writeOGR(fnetSP.spp106, dsn=".", layer="fnet_spp106", driver="ESRI Shapefile", overwrite = T) # output shapefile
fnetDF.mime <- tru.bufcc  # change object name
save("fnetDF.mime", "fnetDF.mime", file = "fnetDF.mime")
######## END BUILD FISHNET OF BOUNDING BOXES
#### Link species data to fishnet ####
########   NOTE THIS CODE ALSO WORKS WITH TRUE PRESENCE:ABSENCE DATA AS WELL
# convert species x,y to spatial coordinate
trup.xy <- mime.presDF[c("caeac_x", "caeac_y")]  # get x,y of species presences
p1 <- coordinates(trup.xy)  # set spatial coords from tru.buf

# extract FNETID of presence from different modelling frames
#pres.fnetid <- cellFromXY(tru.box, p1)  # FNETID from non-buffered box
pres.fnetid <- cellFromXY(tru.buf, p1)  # FNETID from buffered box
#pres.fnetid <- cellFromXY(tru.con, p1)  # FNETID from non-buffered convex
#res.fnetid <- cellFromXY(tru.poly, p1)  # FNETID of presence from pt-buffered poly
head(pres.fnetid)  # FNETID numbers associated with species x,y

# some internal checking
length(pres.fnetid)  # should match number of presences ??
length(mime.presDF$presence)  # it does ... life is good !!

# create species dataframe with FNETIDs
tru.presFNET <- cbind(pres.fnetid, mime.presDF)  # add fishnet ID to tru.pres
names(tru.presFNET)[1] <- "FNETID"  # name change 
head(tru.presFNET)  # examine
#save("tru.presFNET", "tru.presFNET", file = "tru.presFNET")

# giggle plots
plot(tru.buf, col = "gray70", legend = F, main = "Points in modelling frame")  # main plot
points(mime.presDF$caeac_x, mime.presDF$caeac_y, pch = 20, col = "darkgreen") # add spp locations
#setwd(path.figs)
#savePlot(filename = "mod0203.1_fig04.pdf", type = "pdf")
######## END LINK SPECIES DATA TO FISHNET
##### START LINK MODELLING FRAMES TO FISHNET
# extract FNETID of modelling frame; many different frames can be used
#poly.fnetid <- extract(tru.box, fnetDF.spp106[c("cell.wgs_x", "cell.wgs_y")])
#poly.fnetid <- extract(tru.buf, fnetDF.mime[c("caeac_x", "caeac_y")])
#poly.fnetid <- extract(tru.con, fnetDF.spp106[c("cell.wgs_x", "cell.wgs_y")])
poly.fnetid <- extract(tru.buf, fnetDF.mime[c("cell.caeac_x", "cell.caeac_y")])
head(poly.fnetid)  # examine

# create modelling dataframe with FNETIDs
tru.polyFNET <- cbind(fnetDF.mime, poly.fnetid)  # bind modelling frame w/FISHNET
head(tru.polyFNET, 2)  # examine
save("tru.polyFNET", "tru.polyFNET", file = "tru.polyFNET")
# some internal checking
length(tru.polyFNET$poly.fnet)  # number FNETIDs in extent tru.poly extent
table(tru.polyFNET$poly.fnet)[[1]]  # number of FNETIDs in tru.poly modelling frame

#change NA in poly.fnetid to 0, TG

# tru.polyFNET$poly.fnetid.01 <- 0
# tru.polyFNET[which(tru.polyFNET$poly.fnetid == 1), "poly.fnetid.01"] <- 1
# fish.test <- tru.buf
# fish.test <- setValues(tru.buf, tru.polyFNET$poly.fnetid.01)

# giggle plots: modelling frames nested in fishnet
plot(tru.buf, col = "gray90", legend = F, main = "Modelling frame on FISHNET")  # main plot
plot(tru.buf, col = "gray50", legend = F, add=T)  # main plot
#setwd(path.figs)
#savePlot(filename = "mod0203.1_fig05.pdf", type = "pdf")

# giggle plots: spp loaction nested in modelling frames nested in fishnet
plot(tru.buf, col = "gray90", legend = F, main = "SP locations nested in modelling frame in FISHNET")  # main plot
plot(tru.buf, col = "gray50", legend = F, add=T)  # main plot
points(mime.presDF$caeac_x, mime.presDF$caeac_y, pch = 20, col = "darkgreen") # add spp locations
#setwd(path.figs)
#savePlot(filename = "mod0203.1_fig06.pdf", type = "pdf")
######## START LINK MODELLING FRAMES TO FISHNET 
##### START MERGE SPP LOCATION, MODELLING FRAME, ALL BY FISHNET ID'S 
# examine dataframes
head(fnetDF.mime, 2)  # fishnet dataframe
dim(fnetDF.mime)[1]
head(tru.polyFNET, 2)  # modelling dataframe
table(tru.polyFNET$poly.fnetid)[1]
dim(tru.polyFNET)
head(tru.presFNET, 2)  # spp locations dataframe
dim(tru.presFNET)

# begin merge: NOTE merge by=c("sort vars") & all.y=T options
m1 <- merge(fnetDF.mime, tru.polyFNET, by = c("FNETID","cell.caeac_x","cell.caeac_y"), all.y = T)
head(m1, 2)  # examine
length(m1$FNETID)
indexFNET.mime <- merge(m1, tru.presFNET, by = c("FNETID"), all = T)  # final merge: assign DF name
head(indexFNET.mime, 2)  # final merge: FNETID now ranked
tail(indexFNET.mime, 2)
names(indexFNET.mime)[4] <- "in.modFR"  # change some names
names(indexFNET.mime)  # examine names

# internal checking
length(indexFNET.mime$FNETID)  # does it match w/above ?? no, extra 368
table(indexFNET.mime$in.modFR)[[1]]  # does it match w/above ?? no, extra 368
table(indexFNET.mime$SpeciesCode)[[1]]  # does it match w/above ?? yes 
names(indexFNET.mime)  # names make sense ?? yes 


table(duplicated(indexFNET.mime$FNETID)) 
table(duplicated(indexFNET.mime$in.modFR[NA]))
table(duplicated(indexFNET.mime$SpeciesCode))

indexFNET.mime.dup <- indexFNET.mime

#removing duplicates
indexFNET.mime <-indexFNET.mime.dup[!duplicated(indexFNET.mime$FNETID), ]

length(indexFNET.mime$FNETID)
table(indexFNET.mime$in.modFR)[[1]] 
table(indexFNET.mime$SpeciesCode)[[1]]  # 156 species presence points

table(is.na(indexFNET.mime$in.modFR))

#change NA in fishnet to 0

indexFNET.mime$presence <- 0
indexFNET.mime[which(indexFNET.mime$SpeciesCode == "mime"), "presence"] <- 1

table(is.na(indexFNET.mime$presence))
table(indexFNET.mime$presence)
head(indexFNET.mime,2)

# outfile final dataframe
#setwd(path.mod2)
#save(indexFNET.spp106, file = "indexFNET.spp106.RData")
######## END MERGE SPP LOCATION, MODELLING FRAME, ALL BY FISHNET ID'S 

##### GENERATE PSUEDO ABSENCES####
p1 <- indexFNET.mime
p2.mime <- subset(p1$FNETID, p1$presence == 1)  # FNETIDs of spp locations
p2.modFR <- subset(p1$FNETID, p1$in.modFR == 1)  # FNETIDs in modelling frame
length(p2.mime)  # should equal N of spp locations
length(p2.modFR)  # should equal N modelling frame

# drop presence cell FNETIDs; remaining are possible psuedo-abs cell FNETIDs
p3 <- p1[!p1$FNETID %in% p2.mime, ]  # background from fishnet
p4 <- p1[!p1$FNETID %in% p2.mime & p1$FNETID %in% p2.modFR, ] # background from modelling frame
psu.poly <- p4  # new name to dataframe: this dataframe used from now on

# internal checking
length(p1$FNETID)  # N of FISHNET
dim(p3)[1] # N of FISHNET - N spp locations
table(p1$presence)[2] # N spp locations
dim(p3)[1] + table(p1$presence)[[2]] # should equal N of FISHNET
length(p2.modFR)  # N of modelling frame
dim(p4)[1] # N modelling frame - N spp locations
dim(p4)[1] + table(p1$presence)[[2]] # should equal N modelling frame

# recall ...
table(p1$presence)[[2]] # N of spp locations
head(psu.poly, 2) # dataframe from which samples drawn

# start the draws; multiple optios shown below
#   NOTE: set seed if desire repeatability of srs
set.seed(1234) # set seed to ensure repeatability: make sure you rememebr what it is !!
psu.srs1 <- psu.poly[sample(1:nrow(psu.poly), table(p1$presence)[2], replace = F), ]
psu.srs1$presence <- 0  # assign 0 to psu.abs
psu.srs1$in.modFR <- 1  # assign 1 to in.modFR
dim(psu.srs1) # dim[1] should equal N spp locations, dim[2] the No. variables
head(psu.srs1, 2)  # examine

# start the draws; multiple options shown below
#   N=2*No. pres
set.seed(1234) # set seed to ensure repeatability
psu.srs2 <- psu.poly[sample(1:nrow(psu.poly), 2 * table(p1$presence)[[2]], replace = F), ]
psu.srs2$presence <- 0  # assign 0 to psu.abs  
psu.srs2$in.modFR <- 1  # assign 1 to in.modFR
# #   N=4*No. pres
# set.seed(1234) # set seed to ensure repeatability
# psu.srs4 <- psu.poly[sample(1:nrow(psu.poly), 4 * table(p1$SPPRES106)[[1]], replace = F), ]  
# psu.srs4$SPPRES106 <- 0  # assign 0 to psu.abs
# psu.srs4$in.modFR <- 1  # assign 1 to in.modFR
# #   N=10*No. pres
# set.seed(1234) # set seed to ensure repeatability
# psu.srs10 <- psu.poly[sample(1:nrow(psu.poly), 10 * table(p1$SPPRES106)[[1]], replace = F), ]
# psu.srs10$SPPRES106 <- 0  # assign 0 to psu.abs
# psu.srs10$in.modFR <- 1  # assign 1 to in.modFR

#### Combine Psuedoabsences with Presence DF ####
head(tru.presFNET, 2)
head(psu.srs1, 2)  # examine; both MUST have FNETID
mime.PA <- merge(tru.presFNET, psu.srs1, by = c("FNETID", "presence", "caeac_x", "caeac_y"), all = T)  # merge
mime.PA$in.modFR <- NULL  # drop in.modFR index no longer needed
dim(mime.PA)  # examine
head(mime.PA, 2)  # dim => No. pres + No. psu.abs


# create new vars caeac_x & caeac_y; used later in raster stack extraction
mime.PA$tr.caeac_x <- ifelse(mime.PA$presence== 0, mime.PA$cell.caeac_x, mime.PA$caeac_x)
mime.PA$tr.caeac_y <- ifelse(mime.PA$presence == 0, mime.PA$cell.caeac_y, mime.PA$caeac_y) 

head(mime.PA, 2) 

length(mime.PA$FNETID)

mime.PresAbs <- mime.PA [, c(1,2,3,4,8,9,13,14)]
head(mime.PresAbs)
##### UPLOAD PREDICTOR LAYERS ####
require(sf)
#shape <- read_sf(dsn = "./Source/Landfire_ExistVegType", layer = "SHAPEFILE")

setwd(wd)
asp <- raster("./Source/Aspect_1km.tif") #aspect 30m
slp <- raster("./Source/Slope_1km.tif") #slope 30m
lf.veg <- raster("./Source/lf_veg.tif")
# Landfire vegetation cover 30m
nlcd<- raster("./Source/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img") # nlcd 30 m
perc.sand<- raster("./Source/percentSand_30cmdepth.tif") # 1000m data
perc.clay<- raster("./Source/percentClay_30cmdepth.tif") # 1000m data
perc.silt<- raster("./Source/percentSilt_30cmdepth.tif") # 1000m data
precip <- raster("./Source/ppt_1_100m.tif")
sagebrush <- raster("./Source/Sagebrush_Biome.tif")
t.max<- raster("./Source/tmax_1_100m.tif")

##### Convert predictor layers to same resolution and projection #####

##### ELEV

elev_crop <- crop(elev, tru.buf)

#### Aspect
asp_crop <-crop(asp, tru.buf)

asp <- projectRaster(asp_crop, elev_crop)


##### NLCD

nlcd_crop <- crop(nlcd, tru.buf)
dim(nlcd_crop)

projection(nlcd_crop)
projection(elev)

res(nlcd_crop)
res(elev)


nlcd <- projectRaster(nlcd_crop, elev_crop, method = "ngb") 
projection(nlcd)
res(nlcd)
dim(nlcd)


#### Slope

projection(slp) #checking projection
projection(elev)

res(slp) # check resolution
res(elev)

slp.1km<- slp

slp <- crop(slp, tru.buf)

slp <- projectRaster(slp, elev_crop) 
slp

#### Landfire Vegetation Cover

projection(lf.veg) #checking projection
projection(elev)

res(lf.veg) # check resolution
res(elev)

lfveg.30 <- lf.veg

lf.veg <- projectRaster(lfveg.30, elev)
projection(lf.veg)
res(lf.veg)

lf.veg <- crop(lf.veg, tru.buf)
lf.veg <- projectRaster(lf.veg, elev_crop, method = "ngb")

#### Avg. Max Temperature

projection(t.max) #checking projection
projection(elev)

res(t.max) # check resolution
res(elev)

tmax.100 <- t.max

t.max_crop <- crop(tmax.100, tru.buf)

t.max<- projectRaster(t.max_crop, elev_crop) 
projection(t.max)
res(t.max)

#### Precipitation

precip_crop <- crop(precip, tru.buf)
precip<- projectRaster(precip_crop, elev_crop) 

#### Sage Brush

sage_crop <- crop(sagebrush, tru.buf)
sage<- projectRaster(sage_crop, elev_crop, method = "ngb")

#### Percent Sand

perc.sand_crop <- crop(perc.sand, tru.buf)
perc.sand<- projectRaster(perc.sand_crop, elev_crop)

### Percent Clay

perc.clay_crop <- crop(perc.clay, tru.buf)
perc.clay<- projectRaster(perc.clay_crop, elev_crop)

#### Percent Silt

perc.silt_crop <- crop(perc.silt, tru.buf)
perc.silt<- projectRaster(perc.silt_crop, elev_crop)

pred.dom = stack(elev_crop, asp,nlcd, t.max,lf.veg, slp, precip, sage, perc.sand, perc.clay, perc.silt)
##### EXTRACTING DATA FROM RASTERS #####
#### Elevation ####
e1 <- ifelse(mime.PresAbs$presence == 0, 
             extract(elev_crop, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
             extract(elev_crop, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(e1)  # examine length of vector
length(which(is.na(e1)))  # NAs gone

# bind to new training dataframe
mime.tr <- cbind(mime.PresAbs, elev = e1)  # bind values to presabs
head(mime.tr, 2)

#### Aspect ####
a <- ifelse(mime.PresAbs$presence == 0, 
             extract(asp, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
             extract(asp, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(a)  # examine length of vector
length(which(is.na(a)))  # NAs gone

# bind to new training dataframe
mime.tr <- cbind(mime.tr, asp = a)  # bind values to presabs
head(mime.tr, 2)

#### Slope ####
s <- ifelse(mime.PresAbs$presence == 0, 
             extract(slp, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
             extract(slp, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(s)  # examine length of vector
length(which(is.na(s)))  

table(is.na(s))

# bind to new training dataframe
mime.tr <- cbind(mime.tr, slp = s)  # bind values to presabs
head(mime.tr, 2)

#### Perc Sand ####

sa <- ifelse(mime.PresAbs$presence == 0, 
             extract(perc.sand, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
             extract(perc.sand, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(sa)  # examine length of vector
length(which(is.na(sa))) 

# bind to new training dataframe
mime.tr <- cbind(mime.tr, perc.sand = sa)  # bind values to presabs
head(mime.tr, 2)

#### Perc Clay ####

c <- ifelse(mime.PresAbs$presence == 0, 
                extract(perc.clay, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
                extract(perc.clay, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(c)  # examine length of vector
length(which(is.na(c))) 

# bind to new training dataframe
mime.tr <- cbind(mime.tr, perc.clay = c)  # bind values to presabs
head(mime.tr, 2)

#### Perc Silt ####
si <- ifelse(mime.PresAbs$presence == 0, 
                extract(perc.silt, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
                extract(perc.silt, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(si)  # examine length of vector
length(which(is.na(si)))  

# bind to new training dataframe
mime.tr <- cbind(mime.tr, perc.silt = si)  # bind values to presabs
head(mime.tr, 2)

#### 

#### landfire vegetation ####

lfv <- ifelse(mime.PresAbs$presence == 0, 
               extract(lf.veg, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
               extract(lf.veg, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(lfv)  # examine length of vector
length(which(is.na(lfv)))  # NAs gone

# bind to new training dataframe
mime.tr <- cbind(mime.tr, lf.veg = lfv)  # bind values to presabs
head(mime.tr, 2)

#### Avg Max Temp ####
mt <- ifelse(mime.PresAbs$presence == 0, 
               extract(t.max, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
               extract(t.max, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(mt)  # examine length of vector
length(which(is.na(mt)))  # NAs gone

# bind to new training dataframe
mime.tr <- cbind(mime.tr, t.max = mt)  # bind values to presabs
head(mime.tr, 2)

#### Avg Precipitation ####
r <- ifelse(mime.PresAbs$presence == 0, 
               extract(precip, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
               extract(precip, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(r)  # examine length of vector
length(which(is.na(r)))  # NAs gone

# bind to new training dataframe
mime.tr <- cbind(mime.tr, precip = r)  # bind values to presabs
head(mime.tr, 2)

#### Sagebrush ####
sg <- ifelse(mime.PresAbs$presence == 0, 
            extract(sage, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
            extract(sage, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(sg)  # examine length of vector
length(which(is.na(sg)))  # NAs gone

# bind to new training dataframe
mime.tr <- cbind(mime.tr, sage = sg)  # bind values to presabs
head(mime.tr, 2)

#### NLCD ####
n <- ifelse(mime.PresAbs$presence == 0, 
            extract(nlcd, mime.PresAbs[, c("cell.caeac_x", "cell.caeac_y")]), 
            extract(nlcd, mime.PresAbs[, c("caeac_x", "caeac_y")]))
length(n)  # examine length of vector
length(which(is.na(n)))  # NAs gone

# bind to new training dataframe
mime.tr <- cbind(mime.tr, nlcd = n)  # bind values to presabs
head(mime.tr, 2)




mime.tr$lf.veg <- as.factor(mime.tr$lf.veg)
mime.tr$nlcd <- as.factor(mime.tr$nlcd)
mime.tr$sage<- as.factor(mime.tr$sage)
##### EXPLORE TRANING DATA ####

library(modeest)

#check correlation between variables

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{ usr <- par("usr"); on.exit(par(usr)) 
par(usr = c(0, 1, 0, 1)) 
r <- abs(cor(x, y, use = "pairwise.complete.obs")) 
txt <- format(c(r, 0.123456789), digits=digits)[1] 
txt <- paste(prefix, txt, sep="") 
if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 

test <- cor.test(x,y) 
# borrowed from printCoefmat
Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                 cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                 symbols = c("***", "**", "*", ".", " ")) 

text(0.5, 0.5, txt, cex = cex * r)
text(.8, .8, Signif, cex=cex, col=2) 
}

quartz(10,10)
pairs(mime.tr[, c(2,9,10,11,12,13,14,15,16,17,18,19)], lower.panel = panel.smooth, 
      upper.panel = panel.cor, main = "Predictor Variables") 

mime.tr.final1 <- mime.tr[, c(1,2,7,8,9,11,12,15,16,19)] 
#elimiated extra coordinate columns: pec.clay, per.silt

mime.tr.final1$lf.veg <- as.factor(mime.tr.final1$lf.veg)
mime.tr.final1$nlcd <- as.factor(mime.tr.final1$nlcd)
mime.tr.final1$sage<- as.factor(mime.tr.final1$sage)

##### MODEL CREATION ####

# load libraries now if desired; loaded below when needed
library(rJava)
library(dismo)
library(raster)
library(PresenceAbsence)
library(maptools)
library(rgdal)


#### Start Initializing####
# build model formula as alternative hard code
mod.form <- function(dat ,r.col, p.col) {
  # generic formula construction function; inputs as:
  #  resp =>col 1 in dataframe such that r.col=1, 
  #  preds=>col 2 thru ncol in dataframe such that p.col=2
  #  NOTE: predictor vars as factors; coerce PRIOR to formula construction
  # example call: mod.form(dat1,1,2)
  n.col <- ncol(dat)  # No. columns in dataframe
  resp <- colnames(dat[r.col])  # assign resp column name
  resp <- paste("as.factor(", colnames(dat[r.col]), ")", sep = "")  # assign resp column name
  pred <- colnames(dat[c(p.col:n.col)])  # assign preds column names
  mod.formula <- as.formula(paste(resp, "~", paste(pred, collapse = "+")))  # build formula 
}

#### START MAXENT MODEL## ####

dat1 <- mime.tr.final1  # import training data
table(is.na(dat1))
dat1 <- na.omit(dat1)  # remove NAs - MAX is picky
dim(dat1)
table(dat1$presence)
head(dat1, 2)  # examine    
n.col <- ncol(dat1)
n.col  # number of columns
names(dat1)  # verify col No.s for model entry
colnames(dat1) <- c("FNETID", "presence","tr.caeac_x", "tr.caeac_y", "elev_1k_aea", "Slope_1km", "percentSand_30cmdepth", "lf_veg", "tmax_1_100m", "NLCD_2016_Land_Cover_L48_20190424")


mod.MAX.PresAbs1 <- maxent(dat1[5:n.col], dat1[2]) # call for true pres:abs
mod.MAX.PresAbs1


response(mod.MAX.PresAbs1) 
plot(mod.MAX.PresAbs1)


mod.pred <- predict(mod.MAX.PresAbs1, dat1[5:n.col])  # maxent prediction
modl <- "mod.MAX"  # var placeholder
dat2 <- cbind(modl, dat1[2], mod.pred)  # build dataframe w/mod1 predictions
head(dat2)  # examine prediction dataframe



library(raster)  # raster for stacking predictors
pred.dom <- stack(elev_crop, slp, perc.sand, lf.veg, t.max, nlcd)  # build raster stack
pred.dom  # examine stack
names(pred.dom)

# evaluate model (an x-fold process: see help(evaluate))

mod.pred <- predict(mod.MAX.PresAbs1, dat1[5:n.col])  # maxent prediction
modl <- "mod.MAX.PresAbs1"  # var placeholder
dat2 <- cbind(modl, dat1[2], mod.pred)  # build dataframe w/mod1 predictions
head(dat2)  # examine prediction dataframe


mod.val <- evaluate(mod.MAX.PresAbs1, p = dat1[dat1$presence== 1, c(3:4)], 
                     a = dat1[dat1$presence == 0, c(3:4)], x = pred.dom)  # x-fold cross-val
mod.val  # examine
threshold(mod.val)  # view maxent thresholds 

# generate confusion matrix; see help(theshold) for maxent threshold options
#  here use spec_sens: highest sum of sens & spec
mod.cut <- threshold(mod.val)
mod.cut  # view maxent thresholds
mod.cfmat <- table(dat2[[2]], factor(as.numeric(dat2$mod.pred >= mod.cut$spec_sens)))
mod.cfmat

# calculate model accuracies with standard deviation=F
#library(PresenceAbsence)  # PresenceAbsence for accuracy metrics
mod.acc <- presence.absence.accuracy(dat2, threshold = mod.cut[[2]], st.dev = F)
tss <- mod.acc$sensitivity + mod.acc$specificity - 1  # code TSS metric
mod.acc <- cbind(mod.acc[1:7], tss)  # bind all metrics
mod.acc[c(1, 4:5, 7:8)]  # examine accuracies

# plotting AUC
auc.roc.plot(dat2, color = T)


##### PREDICTION MAPPING #####
mod.MAXprob <- predict(mod.MAX.PresAbs1, pred.dom)  
plot(mod.MAXprob, main = "Probabiliy Map of DKM Presence")
plot(states, add = T, lwd = 1.0) 

mod.MAXclas <- reclassify(mod.MAXprob, c(0, mod.cut[[2]], 0, mod.cut[[2]], 1, 1))
plot(mod.MAXclas, main = "Classification Map")
plot(states, add = T, lwd = 1.0) 

