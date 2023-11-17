library(data.table)
library(sandwich)
library(dplyr)

# === Load data === #
load("data/glewwe_cleaned.Rdata")

data <- data.table(basicinfo)


# ittsamp8: intention to treat (ITT) sample
# ittsamp9: ll township pairs, even if randomization was compromised 

unique(data[,glsbefor])

#/*--------------------------------*/
#' ## Table 1 (25 compliant townships)
#/*--------------------------------*/
# Number of children (okay)
data[ittsamp8==1, .N, by=countycode]

# Children with vision problem (okay)
data[ittsamp8==1&badvision==1, .N, by=countycode]

# Of which (okay)
data[ittsamp8==1&badvision==1, .N, by=.(glsbefor, countycode)] %>%
	.[order(countycode),]

# Mean test scores by vision problem (okay)
data[ittsamp8==1,.(
	avg_chinease = round(mean(chinese04s2, na.rm=TRUE),1),
	avg_math = round(mean(math04s2, na.rm=TRUE),1),
	avg_science = round(mean(science04s2, na.rm=TRUE),1)
	), by=.(badvision, countycode)] %>%
	.[order(badvision),]


#/*--------------------------------*/
#' ## Table 3 
#/*--------------------------------*/