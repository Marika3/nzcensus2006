##Making a map of percentage of people who travelled to work by any means but car by total for meshblock
## uses RGdal for shapefiles with the data in SQLite

library("RSQLite")
library("rgdal")

setwd("~/Documents/census2006")

#read in the shapefiles
meshbase <- readOGR("NZ_L2_2006_NZTM_ArcShp", "MB06_LV2")


#Make any initial database queries
drv <- dbDriver("SQLite")
con <- dbConnect(drv, "nzcensus.sqlite")
#groupqueryresult = dbGetQuery(con, "SELECT tblCountsMeshBlock.MeshBlockID, tblCountsMeshBlock.Count FROM tblCountsMeshBlock, tblGeogMeshBlock  WHERE (tblCountsMeshBlock.QuestionID = 662 OR tblCountsMeshBlock.QuestionID = 663 OR tblCountsMeshBlock.QuestionID = 664 )AND tblCountsMeshBlock.MeshBlockID = tblGeogMeshBlock.MeshBlockID AND tblGeogMeshBlock.TerritorialAuthorityID = '71' ORDER BY tblCountsMeshBlock.MeshBlockID")
#question 662 is the drove private car to work today question for 2006, 663 company car, 664 passenger 71 is dunedin City

# the census suppresses individually identifiable information at the meshblock level, but we can assume a -1 code is 1 or 2 people, as these weem to be the level they are cleaning out since we have meshblock entries for 0 and 3.

totalqueryresult <- dbGetQuery(con, "SELECT tblCountsMeshBlock.MeshBlockID, tblCountsMeshBlock.Count FROM tblCountsMeshBlock, tblGeogMeshBlock  WHERE tblCountsMeshBlock.QuestionID = 672 AND tblCountsMeshBlock.MeshBlockID = tblGeogMeshBlock.MeshBlockID AND tblGeogMeshBlock.TerritorialAuthorityID = '71' ORDER BY tblCountsMeshBlock.MeshBlockID")
# question 672 is the total number of people in the meshblock answering the travel to work question

#close the database connection
dbDisconnect(con)

#get rid of the matching entries where no-one answered the question in that census block
totalqueryresult <- totalqueryresult[totalqueryresult$Count > 0,]
aggdata <- aggdata[aggdata$Count > 0,]
names(totalqueryresult)[2] <- "total"
summarydata <- merge (aggdata, totalqueryresult, by.x ="Group.1", by.y = "MeshBlockID" )
summarydata <- summarydata[order(summarydata$Group.1),]

#pick out the ones that match the SQL query
mapdata <- meshbase[meshbase$MB06 %in% summarydata$Group.1,]

#express as percent
percentcycling <- 100 - ((summarydata$Count)/summarydata$total * 100)
percentcycling[percentcycling <1] <- 1

#be sure it is in the same order as the SQL query
mapdata <- mapdata[order(mapdata$MB06),]

#There is probably an easier way of doing the colour steps than this, but...
sizeOfSteps <- 5
topofscale <- ceiling(max(percentcycling)/sizeOfSteps)
bottomofscale <- floor(min(percentcycling)/sizeOfSteps)+1
rangeofscale <- topofscale - bottomofscale +1
colourset=heat.colors(rangeofscale)
palette(colourset)
regioncolour = ceiling(percentcycling/sizeOfSteps)

#a quick bit of legend text setup
basegroup = bottomofscale:topofscale
leftside = (basegroup-1)*sizeOfSteps
rightside = (basegroup) * sizeOfSteps
legendtext = paste(as.character(leftside),"-",as.character(rightside))

#plot the points
plot(mapdata, axes=FALSE, border=gray(.5), col=regioncolour, ylim=c(4910000,4920000), xlim=c(1402500,1415000))

#legend and title
labels <- legendtext
position <- 'bottomright'
colors <- colourset
inset <- c(0.02, 0)
legend(position, labels, fill=colors, inset=inset)
title(main="Approx. % of people who didn't travel by car to work, census day 2006", sub="includes entries like 'did not go to work today'")
