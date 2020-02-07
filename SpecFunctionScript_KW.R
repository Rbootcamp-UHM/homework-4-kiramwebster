# Script to analyze spectrophotometer files
## By Kira Webster 

#setwd("C:/Users/kiram/Documents/Rclass/homework-4-kiramwebster")	#will be different for everyone
 
myfiles <- list.files(path = "Data", pattern=".txt") 	#pulling only the text files out of the data folder
nfiles <- length(myfiles)	
df <- data.frame(matrix(NA, nrow = nfiles, ncol = 3))	#creating empty dataframe the length of the number of files
col_headings <- c("file","max.intensity","lambda")
names(df) <- col_headings 	#assigning the headers of the dataframe the names file, max.intensity, and lambda

#reading my files
read.spec <- function ( myfiles )  {  
	dat <- read.table(file=myfiles, skip=17, comment.char=">")
	names(dat) <- c("lambda", "intensity")
	dat <- dat[ dat$lambda >= 300 & dat$lambda <= 700,   ]
	return (dat)
}

#plotting myfiles
plot.spec <- function( X ) {
  plot(X, type="l")
  max <- max(X$intensity, na.rm = TRUE)
  l <- X[X$intensity == max, "lambda"]
  points(x=l,y=max,type="p",col="red",pch=19,cex=2)
} 


#creating pdf wrapper on the forloop. For every loop the graph it creates gets put into the pdf on a new page, not a new file. Every loop also generates a line that is put into the dataframe previously created.
pdf(file=paste("Spectrophotometer Graphs", ".pdf", sep=""), onefile=T) 
for(i in 1:nfiles){
  dat <- read.spec(paste0("Data/", myfiles[i]))
  plot.spec(dat)
  max <- max(dat$intensity, na.rm = TRUE)
  l <- dat[dat$intensity == max, "lambda"]
  df[i,] <- c(myfiles[i], max, l)
  }
  dev.off()

write.csv(df,"Data\\homework4.csv", row.names = F) 	#writting a csv from the dataframe created
