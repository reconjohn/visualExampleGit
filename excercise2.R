wikiLink='https://en.wikipedia.org/wiki/Democracy_Index'
wikiPath='//*[@id="mw-content-text"]/div/table[2]'

##
library(htmltab)
demoData=htmltab(doc = wikiLink,which = wikiPath)
names(demoData)
NewNames=c("Rank","Country","Score","Electoral","Functioning","Participation","Culture","Liberties","Regimetype","Continent")
names(demoData)=NewNames
str(demoData)

#this is wrong: as.numeric(demoData[,c(1,3:8)])
#
#this is good:
demoData[,c(1,3:8)]=lapply(demoData[,c(1,3:8)],as.numeric)
table(demoData$Regimetype)

goodLevels=c("Authoritarian","Flawed democracy","Hybrid regime","Full democracy")
demoData$Regimetype=ordered(demoData$Regimetype,levels=goodLevels)
str(demoData)

demoData$indexMD=apply(demoData[,c(4:8)],
                       MARGIN = 1, # by row / 2 by column
                       median)
threshold=median(demoData$indexMD,na.rm = T)
#
# expect positive and negative values:
demoData$indexGAP=demoData$indexMD-threshold
# a discrete value: True or False
demoData$indexFLAG=demoData$indexMD>=threshold
# a discrete value: True or False
demoData$LibBetterPart=demoData$Liberties>demoData$Participation
summary(demoData[,c("Score","indexMD","indexGAP","indexFLAG","LibBetterPart")])

library(cluster)

set.seed(2019) # very important

# computing distance among points
distanceMatrix = daisy(demoData[,c(4:8)])

# computing FOUR clusters with: 
result<- pam(distanceMatrix, k = 4)
# View(result)

# View(demoData)
silResult=silhouette(result$cluster, distanceMatrix)

demoData$cluster=silResult[,"cluster"]
demoData$quality=silResult[,"sil_width"]

table(demoData$cluster)
demoData[demoData$quality<0,'Country']

library(matlab)
top=10*as.vector(ones(1,5))
bottom=as.vector(zeros(1,5))
# those become two rows of a data frame
limitCases=as.data.frame(rbind(bottom,top))
limitCases

subDemo=demoData[,c(4:8)]

# FIRST, we need both DFs share same column names
names(limitCases)=names(subDemo)
# appending:
subDemo=rbind(subDemo,limitCases)

library(lavaan)

modelNUM='
demox=~Electoral + Functioning + Participation + Culture + Liberties
'

fitNUM<-cfa(modelNUM, data = subDemo)
indexCFA=lavPredict(fitNUM)

library(BBmisc)
indexCFANorm=normalize(indexCFA, 
                       method = "range", 
                       margin=2, # by column
                       range = c(0, 10))

tail(indexCFANorm)
demoData$indexCFA=head(indexCFANorm,-2)

plot(demoData$Score,demoData$indexCFA)


library(mirt)

tempDemo2=floor(demoData[,c(4:8)]) # keeping the integer
tempDemo2=rbind(tempDemo2,limitCases)

modelORD ='
democracy = Electoral, Functioning, Participation, Culture,Liberties
' 
fitORD <- mirt(data=tempDemo2, 
               model=modelORD, 
               itemtype="graded",verbose=F)
indexPIRT <- fscores(fitORD,response.pattern = tempDemo2)[,'F1']

# rescale
indexPIRTNorm=normalize(indexPIRT, method = "range", margin=2,range = c(0, 10))
#
# keep values needed
demoData$indexPIRT=head(indexPIRTNorm,-2)

plot(demoData$indexPIRT,demoData$indexCFA)

plot(demoData[,c("Score","indexPIRT","indexCFA")])


## GGPLOT
inventedDF <- data.frame(schoolType=c("Public", "Private", "Other"),
                         Count=c(200, 20, 80))
inventedDF

library(ggplot2)
base=ggplot(data=inventedDF, # name of data frame
            aes(x=schoolType,# variables to use in each object
                y=Count))
base + geom_bar(stat = 'identity') # object BAR plots "y", as is.base + geom_bar(stat = 'identity') # object BAR plots "y", as is.

# all text for titles
Titles=list(Ti="Nice title",STi="sub is here",XTi="Type of school",YTi="Amount of schools",Sou="Source: imaginary")
bar1= base + geom_bar(stat = 'identity')
# using texts
bar1 + labs(title = Titles$Ti,subtitle = Titles$STi,
            xlab=Titles$XTi,ylab=Titles$YTi,
            caption = Titles$Sou)

#texts for annotation
annots=list(forBar="HELLO")

bar1=bar1 + labs(title = Titles$Ti,subtitle = Titles$STi,
                 xlab=Titles$XTi,ylab=Titles$YTi,
                 caption = Titles$Sou)
#annotating
bar1 + annotate(geom = 'text',x = 2,y=30,label=annots$forBar)

# background
base= base + theme_classic()
#
bar1= base + geom_bar(stat = 'identity')
bar1= bar1 + labs(title = Titles$Ti,subtitle = Titles$STi,
                  xlab=Titles$XTi,ylab=Titles$YTi,
                  caption = Titles$Sou)
bar1 + annotate(geom = 'text',x = 2,y=30,label=annots$forBar)


# label position on horizontal
NEWorder=c('Private','Other','Public')

bar1= bar1 + annotate(geom = 'text',x = 1,y=30,label=annots$forBar)
bar1= bar1 + scale_x_discrete(limits=NEWorder)
# realignment of texts in titles
bar1 + theme(plot.title= element_text(hjust= 0.5),
             plot.subtitle= element_text(hjust= 0.5),
             plot.caption= element_text(hjust= 0))

# Adding variable for coloring
bar1= base + geom_bar(stat = 'identity', aes(fill=schoolType)) 
#
bar1= bar1 + labs(title = Titles$Ti,subtitle = Titles$STi,
                  xlab=Titles$XTi,ylab=Titles$YTi,
                  caption = Titles$Sou)
bar1= bar1 + annotate(geom = 'text',x = 1,y=30,label=annots$forBar)
bar1= bar1 + scale_x_discrete(limits=NEWorder)
bar1= bar1 + theme(plot.title= element_text(hjust= 0.5),
                   plot.subtitle= element_text(hjust= 0.5),
                   plot.caption= element_text(hjust= 0))
#
# color and legend changes
bar1 + scale_fill_brewer(palette = "Accent") + guides(fill = "legend")
