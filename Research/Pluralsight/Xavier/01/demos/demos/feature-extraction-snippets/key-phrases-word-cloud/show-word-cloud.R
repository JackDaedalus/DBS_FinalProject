library(tm)
library(wordcloud)
dataset <- maml.mapInputPort(1)

# Create new plot with desired size and remove default rViz file
graphics.off()
png("myplot.png",width=1024,height=640,res=300) 
file.remove(Sys.glob("*rViz*png"))

# Create term-document matrix of key phrases
commatokenizer = function(x) unlist(strsplit(as.character(x),","))
textcp <- Corpus(DataframeSource(data.frame(dataset[,1])))
tdm = TermDocumentMatrix(textcp,control=list(tokenize=commatokenizer))

# Compute frequency of each key phrase
f <- rowSums(as.matrix(tdm))

#Show word clould
wordcloud(names(f),f,min.freq = 10)
