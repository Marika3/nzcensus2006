##Making a map of percentage of people who travelled to work by any means but car by total for meshblock
## uses RGdal for shapefiles with the data in SQLite

library("RSQLite")
library("rgdal")
library("Hmisc")

setwd("~/Documents/census2006")

#read in the shapefiles
meshbase <- readOGR("NZ_L2_2006_NZTM_ArcShp", "MB06_LV2")


#Make any initial database queries
drv <- dbDriver("SQLite")
con <- dbConnect(drv, "nzcensus.sqlite")

#get all the relevant questions from the database
#dataFromDB = dbGetQuery(con, "SELECT tblCountsMeshBlock.MeshBlockID, tblCountsMeshBlock.QuestionID, tblCountsMeshBlock.Count FROM tblCountsMeshBlock, tblGeogMeshBlock  WHERE (tblCountsMeshBlock.QuestionID = 2 OR tblCountsMeshBlock.QuestionID = 691 OR tblCountsMeshBlock.QuestionID = 1 OR tblCountsMeshBlock.QuestionID = 686 OR tblCountsMeshBlock.QuestionID = 3 OR tblCountsMeshBlock.QuestionID = 696) AND tblCountsMeshBlock.MeshBlockID = tblGeogMeshBlock.MeshBlockID AND tblGeogMeshBlock.TerritorialAuthorityID > 2 AND tblGeogMeshBlock.TerritorialAuthorityID < 10 ORDER BY tblCountsMeshBlock.MeshBlockID")
dataFromDB = dbGetQuery(con, "SELECT tblCountsMeshBlock.MeshBlockID, tblCountsMeshBlock.QuestionID, tblCountsMeshBlock.Count FROM tblCountsMeshBlock, tblGeogMeshBlock  WHERE (tblCountsMeshBlock.QuestionID = 2 OR tblCountsMeshBlock.QuestionID = 691 OR tblCountsMeshBlock.QuestionID = 1 OR tblCountsMeshBlock.QuestionID = 686 OR tblCountsMeshBlock.QuestionID = 3 OR tblCountsMeshBlock.QuestionID = 696) AND tblCountsMeshBlock.MeshBlockID = tblGeogMeshBlock.MeshBlockID AND tblGeogMeshBlock.TerritorialAuthorityID > 4 AND tblGeogMeshBlock.TerritorialAuthorityID < 10 ORDER BY tblCountsMeshBlock.MeshBlockID")
#3=usual residents 2006, 696=total dwellings 2006, 2=usual residents 2001, 691=total dwellings 2001, 1=usual residents 1996, 686=total dwellings 1996
#Auckland area is 3 through 9


#close the database connection
dbDisconnect(con)

#convert data from long to wide format
#followed tutorial at http://www.seananderson.ca/2013/10/19/reshape.html
library(reshape2)
mbdwide <- dcast(dataFromDB, MeshBlockID ~ QuestionID, value.var="Count" )

#fix names
names(mbdwide)[2:7] <- paste("q",names(mbdwide)[2:7],sep="")
#clear out any meshblocks with unavailable data or total of 0 dwellings or people
mbdwide <- mbdwide[complete.cases(mbdwide),]
mbdwide <- mbdwide[mbdwide$q686 != 0,]
mbdwide <- mbdwide[mbdwide$q691 != 0,]
mbdwide <- mbdwide[mbdwide$q696 != 0,]
mbdwide <- mbdwide[order(mbdwide$MeshBlockID),]

#convert entries with -1 to 1.5 on the theory that by preventing
#individual identification, the entries are either 1 or 2
mbdwide[mbdwide == -1] <- 1.5

#add leading zeros for match to meshdata
mbdwide$MeshBlockID <- formatC(mbdwide$MeshBlockID, width = 7, format = "d", flag = "0") 

#pick out the ones that match the SQL query
mapdata <- meshbase[meshbase$MB06 %in% mbdwide$MeshBlockID,]

#work out people to dwellings ratios
dens1996 <- mbdwide$q1/mbdwide$q686
dens2001 <- mbdwide$q2/mbdwide$q691
dens2006 <- mbdwide$q3/mbdwide$q696
dens1996[dens1996 > 5] <- 5
dens2001[dens2001 > 5] <- 5
dens2006[dens2006 > 5] <- 5

#be sure both are in meshblock order
mapdata <- mapdata[order(mapdata$MB06),]

#colourscheme
cutpoints = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
colourset=heat.colors(length(cutpoints))
palette(colourset)

#1996map
png("density1996.png", width = 2000, height = 2000)
dens1996grouped <- cut2(dens1996, cuts=cutpoints)
regioncolour = dens1996grouped
legendtext <- levels(dens1996grouped)
legendtext[length(legendtext)] <- paste(legendtext[length(legendtext)],"+",sep="")
plot(mapdata, axes=TRUE, border=gray(.5), col=regioncolour, ylim=c(5890000,5940000), xlim=c(1730000,1780000))
#legend and title
labels <- legendtext
position <- 'topleft'
colors <- colourset
inset <- c(0.02, 0)
legend(position, labels, fill=colors, inset=inset, cex=3.0)
title(main="Usual residents / Private Dwellings 1996")
dev.off()

#2001map
png("density2001.png", width = 2000, height = 2000)
dens2001grouped <- cut2(dens2001, cuts=cutpoints)
regioncolour = dens2001grouped
legendtext <- levels(dens2001grouped)
legendtext[length(legendtext)] <- paste(legendtext[length(legendtext)],"+",sep="")
plot(mapdata, axes=TRUE, border=gray(.5), col=regioncolour, ylim=c(5890000,5940000), xlim=c(1730000,1780000))
#legend and title
labels <- legendtext
position <- 'topleft'
colors <- colourset
inset <- c(0.02, 0)
legend(position, labels, fill=colors, inset=inset, cex=3.0)
title(main="Usual residents / Private Dwellings 2001")
dev.off()

#2006map
png("density2006.png", width = 2000, height = 2000)
dens2006grouped <- cut2(dens2006, cuts=cutpoints)
regioncolour = dens2006grouped
legendtext <- levels(dens2006grouped)
legendtext[length(legendtext)] <- paste(legendtext[length(legendtext)],"+",sep="")
plot(mapdata, axes=TRUE, border=gray(.5), col=regioncolour, ylim=c(5890000,5940000), xlim=c(1730000,1780000))
#legend and title
labels <- legendtext
position <- 'topleft'
colors <- colourset
inset <- c(0.02, 0)
legend(position, labels, fill=colors, inset=inset, cex=3.0)
title(main="Usual residents / Private Dwellings 2006")
dev.off()

