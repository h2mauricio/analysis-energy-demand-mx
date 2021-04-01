###############################################################################
## This script merges two datasets: a dataset with data of the ids of the 
## households surveyed in ENCEVI (household id, municipality id, municipality 
## location) with a dataset that has the electricity tariffs per municipality.
## The results are stored in folder "/input", as agem_tariff_byfolio.csv
## Note: The info about the tariffs that are applied to each municipality  
## was provided by Alejandro Lopez from CIDE (lopeza@terpmail.umd.edu)
## h2.mauricio@gmail.com - May 2019
###############################################################################

##############################
# Load libraries
##############################
library('psych')

##############################
# Source files
##############################
#importing data of tariffs per municipality 
df.tariffs.munic <- read.table("./input/tariffs_municipality.csv", sep=',', 
                               header = TRUE, as.is=T)

# Dataset with municipality codes of ENCEVI survey
df.munic.ids <- read.csv("./input/ENCEVI_municipalities.csv")

##############################
# Code
##############################
df.tariffs.munic <- subset(df.tariffs.munic, select=c('agem', 'region.tariff', 
                                                      'region.tariff.f',
                                                      'tariff'))
df.munic.ids <- subset(df.munic.ids, select=c(-X,-entidad,-municipio))

# Cleaning and merging dataframes
df.agem.tariff <- merge(df.munic.ids, df.tariffs.munic, by="agem", all.x = TRUE)
table(df.agem.tariff$tariff, exclude = NULL)

#show table with missing values
write.csv(df.agem.tariff, "./input/agem_tariff_byfolio.csv", 
          row.names=TRUE, na="")