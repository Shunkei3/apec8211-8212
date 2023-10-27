library(data.table)

load("Data/glewwe_cleaned.rdata")
setDT(basicinfo)

basicinfo[,.N, by=countycode]

i8.subset<- subset(basicinfo, ittsamp8==TRUE)
i8.subset.badvision <- subset(i8.subset, badvision==TRUE)
i9.subset <- subset(basicinfo, ittsamp9==TRUE)
i9.subset.badvision <- subset(i9.subset, badvision==TRUE)

nrow(i8.subset)
sum(i8.subset$badvision, na.rm=TRUE)
sum(!i8.subset.badvision$glsbefor)
sum(i8.subset.badvision$glsbefor)

library(dplyr)
library(rio)
data <- 
	import("data/endowment_data.dta") %>%
	data.table()

