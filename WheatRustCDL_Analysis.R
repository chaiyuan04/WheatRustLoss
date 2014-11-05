#Clear memory
rm(list=ls())

#Set directory
mainDir <- "C:/Users/chaix026/Dropbox/0000 HavestChoice Project/Rust Losses/Rust Loss Calculations/20141015 Using R Analysis"
setwd(mainDir)

#Read data
WheatRustCDL <- read.table("WheatRustCDL20141015.csv", header = TRUE, sep = ",")
head(WheatRustCDL)
tail(WheatRustCDL)

#Keep Leaf/Stripe/Stem rust
WheatLeafRust   <- subset(WheatRustCDL, select = -c(StemPer, StripePer))
WheatStripeRust <- subset(WheatRustCDL, select = -c(StemPer, LeafPer))
WheatStemRust   <- subset(WheatRustCDL, select = -c(StripePer, LeafPer))

##Choose which disease
##Needs user input
rust <- readline(prompt="Which rust do you want to run? Stem, Stripe, or Leaf? ")

#Need the above line has been given an user input
if (rust=="Stem") {
  ###Stem rust
  WheatRust <- WheatStemRust
  names(WheatRust)[names(WheatRust)=="StemPer"] <- "LossPer"
} else if(rust=="Stripe") {
  ###Stripe rust
  WheatRust <- WheatStripeRust
  names(WheatRust)[names(WheatRust)=="StripePer"] <- "LossPer"
} else if(rust=="Leaf"){
  ###Leaf rust
  WheatRust <- WheatLeafRust
  names(WheatRust)[names(WheatRust)=="LeafPer"] <- "LossPer"
} else {
  cat("unrecognized rust")
  
}

#Setting subfolder
subDir <- rust
if (!file.exists(subDir)){
  dir.create(file.path(mainDir, subDir))
} 
setwd(file.path(mainDir, subDir))


#Calculations
WheatRust$RustFree1000Bu <- WheatRust$Prod1000Bu /(100-WheatRust$LossPer)*100       #Rust Free Production
WheatRust$Loss1000Bu <- WheatRust$RustFree1000Bu * WheatRust$LossPer/100            #Rust Loss in 1000 Bushels


#Aggregate Losses by Year by State
#install.packages("sqldf")
library(sqldf)
RustAggYrSt <- sqldf("select Year, State,
                     sum(Prod1000Bu) Prod1000Bu,
                     sum(Loss1000Bu) Loss1000Bu,
                     sum(RustFree1000Bu) RustFree1000Bu
                     from WheatRust
                     group by Year, State")
RustAggYrSt$LossPer <- RustAggYrSt$Loss1000Bu / RustAggYrSt$RustFree1000Bu * 100     #Rust Percent Losses in each State

summary(RustAggYrSt)  #check percentages to check innormality
RustAggYrSt[which.max(RustAggYrSt$LossPer), ] #Largest Loss

#Save data
write.csv(RustAggYrSt, file=paste(rust, "Rust.bYear.bState.csv", sep=""), row.names = FALSE)

#Aggregate Loss by Year in the US
RustAggYr <- sqldf("select Year,
                   sum(Prod1000Bu) Prod1000Bu,
                   sum(Loss1000Bu) Loss1000Bu,
                   sum(RustFree1000Bu) RustFree1000Bu
                   from WheatRust 
                   group by Year")
RustAggYr$LossPer <-  RustAggYr$Loss1000Bu / RustAggYr$RustFree1000Bu * 100
summary(RustAggYr)
RustAggYr[which.max(RustAggYr$LossPer), ]

png(file=paste(rust, " Rust Losses in the US, 1918-2013.png", sep=""), width=1600, height=800)
plot(LossPer~Year, data=RustAggYr, type="h", col = "black", lwd=8, xaxt = "n")
axis(side = 1, at = c(1918, seq(1920, 2013, by=1)))
dev.off()

#Save data
write.csv(RustAggYr, file=paste(rust, "Rust.bYear.US.csv", sep=""), row.names = FALSE)

#Generate maps
if(require(mapproj)) {
  pdf(file=paste(rust, "RustYearUS.pdf", sep=""))  #open a pdf "RustYearUS.pdf" for figures
  
  for(i in 1918:2013){
    # Use data "RustAggYrStFull"
    MapData <- RustAggYrSt[RustAggYrSt$Year==i, ]
    MapData$State <- sapply(MapData$State, tolower)
    
    # define color buckets
    colors = c("#ffffff", "#feedde", "#fdbe85", "#fd8d3c", "#e6550d", "#a63603")
    MapData$colorBuckets <- as.numeric(cut(MapData$LossPer, breaks=c(-1, 0, 1, 5, 10, 20, 100), right=TRUE))
    leg.txt <- c("0", "<=1%", "<=5%", "<=10%", "<=20%", ">20%")
    
    # align data with map definitions by matching state
    colorsmatched <- MapData$colorBuckets [match(map("state", plot=FALSE)$names, MapData$State)]
    
    # draw map
    par(mar=c(5, 4, 4, 8)+0.1, xpd=TRUE)  #set margins to allow legend on the right
    map("state", col = colors[colorsmatched], fill = TRUE, resolution = 0, lty = 1, projection="polyconic")
    title(paste(rust, " Rust Losses in the U.S., Year", toString(i), sep=" ") )
    legend('bottomright', inset=c(-0.2, 0.2), leg.txt, horiz = F, fill = colors, cex=0.8)
  }
  
  dev.off()  #close graphics
}



