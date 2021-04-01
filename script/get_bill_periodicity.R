###############################################################################
## This script includes the functions needed to compute the electricity 
## consumption (in KWh) for each recoreded bill of ENCEVI,s survey
###############################################################################
GetBillPeriodicity <- function (df.bill.date){
  # Computes the power consumption of each household based on the information
  # collected from ENCEVI's survey, related to the electricity bill 
  # (type of tariff, periodicity of bill, and summer month tariff ) 
  #
  # Args:
  #   df.bill: dataframe with the info of all the households surveyed (only 
  #            data related to bill)   
  #   df.tariff.sch: dataframe with schema of the Mexican electricity tariffs 
  #   periodicity.bill: string used to filter "monthly" or "bi-monthly" bills
  #   tariff.type: string used to filter the bills by type of tariff
  #   is.summer: string ("Yes"/"No") used to filter summer or no summer tariffs
  # 
  # Returns:
  #   df.bill.sub: dataframe with the same data of "df.bill" plus the columns 
  #               "cons.kwh" (computed power consumption estimation) and 
  #               "tariff.block" (tariff block based on the Mexican tariff schema)

df.bill.date$bill.ini.date <- NA
df.bill.date$bill.end.date <- NA

# The data was obtained from January 2018 to June 2018.
# So the bills could be from 2017 or 2018. As the survey does not report 
# the year of the electricity bill, it is calculated here
df.bill.date$year.ini <- "2018"
df.bill.date$year.end <- "2018"

# Case 1. If the initial period of the bill is from june to december, 
# it is assumed that the year of the initial period is 2017
df.bill.date$year.ini[df.bill.date$mes_inic >= 6] <- 2017

# Case 2. If the final period of the bill is from july to december, 
# it is assumed that the year of the final period is 2017
df.bill.date$year.end[df.bill.date$mes_final >= 7] <- 2017

# Case 3. If the initia month period of the bill is higher than the final month, 
# it is assumed that the year of the initial period is 2017
df.bill.date$year.ini[df.bill.date$mes_inic > df.bill.date$mes_final] <- 2017

df.bill.date$bill.ini.date <-  str_replace_all(paste(df.bill.date$mes_inic, "-", 
                                                      df.bill.date$inicia, "-", 
                                                      df.bill.date$year.ini), 
                                                pattern=" ", repl="")

df.bill.date$bill.end.date <-  str_replace_all(paste(df.bill.date$mes_final, "-", 
                                                      df.bill.date$final, "-", 
                                                      df.bill.date$year.end), 
                                                pattern=" ", repl="")

df.bill.date$bill.ini.date <- as.Date(as.character(df.bill.date$bill.ini.date), 
                                       format="%m-%d-%Y")

df.bill.date$bill.end.date <- as.Date(as.character(df.bill.date$bill.end.date), 
                                       format="%m-%d-%Y")

df.bill.date$bill.days <- as.integer((df.bill.date$bill.end.date - df.bill.date$bill.ini.date))

# After obtaining the number of days of the period in the electricity bill. 
# There are still some special cases that are corrected here.
# Case 4. If the period in the electricity bill is longer than 1 year, the 
# initial year is assumed to be 2018
df.bill.date$year.ini[df.bill.date$bill.days >= 365 & 
                         (df.bill.date$mes_inic <= df.bill.date$mes_final)] <- 2018

# Case 5. If the period in the electricity bill is negative, the initial 
# year is assumed to be 2017
df.bill.date$year.ini[df.bill.date$bill.days < 0] <- 2017

df.bill.date$bill.end.date <-  str_replace_all(paste(df.bill.date$mes_final, "-", 
                                                      df.bill.date$final, "-", 
                                                      df.bill.date$year.end), 
                                                pattern=" ", repl="")

df.bill.date$bill.ini.date <-  str_replace_all(paste(df.bill.date$mes_inic, "-", 
                                                      df.bill.date$inicia, "-", 
                                                      df.bill.date$year.ini), 
                                                pattern=" ", repl="")

df.bill.date$bill.end.date <-  str_replace_all(paste(df.bill.date$mes_final, "-", 
                                                      df.bill.date$final, "-", 
                                                      df.bill.date$year.end), 
                                                pattern=" ", repl="")

df.bill.date$bill.ini.date <- as.Date(as.character(df.bill.date$bill.ini.date), 
                                       format="%m-%d-%Y")

df.bill.date$bill.end.date <- as.Date(as.character(df.bill.date$bill.end.date), 
                                       format="%m-%d-%Y")

df.bill.date$bill.days <- as.integer((df.bill.date$bill.end.date - 
                                        df.bill.date$bill.ini.date))


df.bill.date$bill.days.valid <- df.bill.date$bill.days

#Keeping only the bi-monthly (56-64 days) and monthly bills (27-33 days)
df.bill.date$bill.days.valid[df.bill.date$bill.days.valid < 25 ] <- NA
df.bill.date$bill.days.valid[df.bill.date$bill.days.valid > 70 ] <- NA
df.bill.date$bill.days.valid[df.bill.date$bill.days.valid >= 40 & 
                                df.bill.date$bill.days.valid <= 50  ] <- NA

df.bill.date <- subset(df.bill.date,
                        select=-c(mes_inic, inicia, mes_final, final))

return (df.bill.date)
}