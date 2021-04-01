###############################################################################
## This script merges two datasets: a dataset with data of the ids of the 
## households surveyed in ENCEVI and municipality ids with a dataset that has
## the municipality ids and the features of each municipality in Mexico. 
## The results are stored as "input/ENCEVI_municipalities.csv"
## Note: The dataset ENCEVI_CLVE_ENT_MUN.dbf was requested to INEGI.
## The dataset AGEEML_20195281547102.dbf was downloaded from INEGI website at
## https://www.inegi.org.mx/app/ageeml/#. Both files can be download from:
## http://mauricioh2.com/files/
## h2.mauricio@gmail.com - May 2019
###############################################################################

# Get municipality codes ordered and cleaned, results are stored in 
# folder "/input", as INEGI_agem_short.csv, these data was provided 
# by INEGI and it is related to ENCEVI 2018 survey

library('dplyr') 
library('tidyverse') 
library('foreign') 

##############################
# Source files
##############################
# Municipalities from ENCEVI 2018
df.munic <- read.dbf("./input/ENCEVI_CLVE_ENT_MUN.dbf", FALSE)
# Municipalities codes from INEGI
df.agem.codes <- read.dbf("./input/AGEEML_INEGI_2019.dbf", FALSE)

##############################
# Code
##############################
names(df.munic)<-tolower(colnames(df.munic))
names(df.agem.codes)<-tolower(colnames(df.agem.codes))

#Create a new column with the state and municipality codes
df.munic <- unite(df.munic, "agem", c("entidad","municipio"), 
                  sep = "", remove = FALSE)

df.agem.codes <- unite(df.agem.codes, "agem", 
                       c("cve_ent","cve_mun"), 
                       sep = "", remove = FALSE)

#Keep only the data that is relevant
df.agem.codes <- subset(df.agem.codes, select=c(agem, nom_mun, 
                                                lat_decimal,
                                                lon_decimal, 
                                                altitud))

# The values in the file are read as factors. 
# Here, the values are converted to numeric  
df.agem.codes$lat_decimal <- as.numeric(as.character(df.agem.codes$lat_decimal))
df.agem.codes$lon_decimal <- as.numeric(as.character(df.agem.codes$lon_decimal))
df.agem.codes$altitud <- as.numeric(as.character(df.agem.codes$altitud))

# Getting the average location of each municipality, 
# based on the coordinates of all their neighborhoods
df.agem.codes <- df.agem.codes %>%
  group_by(agem, nom_mun) %>%
  summarise_at(vars(lat_decimal, lon_decimal, altitud), 
               funs(mean(., na.rm=TRUE)))

df.munic <- merge(df.munic, df.agem.codes, by="agem", all.x = TRUE)

#Reorder columns to place the "folio" column in the first column
df.munic <- df.munic[c("folio", "agem", "entidad", "municipio", "nom_mun",
                       "lat_decimal", "lon_decimal", "altitud")]

#Save results in input folder
write.csv(df.munic, file = "input/ENCEVI_municipalities.csv", 
          row.names=TRUE, na="")