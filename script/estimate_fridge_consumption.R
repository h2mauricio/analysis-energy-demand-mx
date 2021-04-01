###############################################################################
## This script includes the functions needed to estimate the electricity 
## consumption (in KWh) for each fridge recorded by ENCEVI
###############################################################################

EstimateFridgeAnnualCons <- function(df.fridge, df.fridge.specs){
    # Computes the power consumption of each fridgebased on the information
    # collected in ENCEVI (fridge age, dimension, # of doors, etc) 
    #
    # Args:
    #   df.fridge: dataframe with the info of all the households surveyed (only 
    #            data related to fridges)   
    #   df.fridge.specs: dataframe with the typical annual power consumption of
    #                    different fridges based on the information obtained 
    #                    from the website "Flip Your Fridge calculator". REf:
    #                   https://www.energystar.gov/index.cfm?fuseaction=refrig.calculator
    # Returns:
    #   df.fridge.sub: dataframe with the same data of "df.fridge" plus the columns 
    #               "annual.cons.kwh" (computed power consumption estimation) and 
    #               "fridge.year.adj" (tariff block based on the Mexican tariff schema)

  # To relate the features of each fridge with its power consumption, we use 10-character
  # identifier called "id.appl" that has the following form: fridg_WXYZ
  #
  # W. Year: 1. Before 1980, 2. 1980-1989, 3. 1990-1992, 4. 1993-2000, 
  #         5. 2001-2010, 6. 2011-2015, 7. 2016-2018
  # X. Capacity: 1. <6 ft3", 2."6-10 ft3", 3. "11-15 ft3", 4. "16-20 ft3", 
  #              5. ">20 ft3", 9. "Don't know"
  # Y. Freezer location: 1. Up, 2. Bottom, 3. Side, 4. No freezer
  # Z. Number doors: 1 to 4 doors
  # For example: if the id is fridge_6524, means that  the fridge was 
  # adquired between 2011 and 2015 (6), its capacity is above 20 feet (5), its
  # freezer is located at the bottom of the fridge (2), and it has 4 doors (4)
  
    range <- data.frame("start.date" = c(1900, 1979, 1989, 1992, 2000, 
                                         2010, 2015, 2018))
    
    # Adjusting year of fridge if it was adquired as "used"
    df.fridge$fridge.year.adj <- df.fridge$adqui_ref
    
    df.fridge$random <-floor(runif(nrow(df.fridge), min=1, max=10))
    df.fridge$fridge.year.adj <- df.fridge$adqui_ref
    
    df.fridge$fridge.year.adj[df.fridge$fridge.year.adj ==1] <- 0
    df.fridge$fridge.year.adj[df.fridge$fridge.year.adj ==2] <- 1
    
    df.fridge$fridge.year.adj <-  df.fridge$fridge.year - 
      (df.fridge$random * df.fridge$fridge.year.adj)
    
    # Obtaining year.id, based on the ranges in which the power consumption
    # values of the fridge can be calculated: 1. Before 1980, 2. 1980-1989, 
    # 3. 1990-1992, 4. 1993-2000, 5. 2001-2010, 6. 2011-2015, 7. 2016-2018
    df.fridge$year.id <- 0
    df.fridge <- df.fridge[order(df.fridge$fridge.year.adj),]
    
    cut.vector <- c(range$start.date, max(df.fridge$fridge.year.adj))
    
    df.fridge$year.id <- 
      cut(df.fridge$fridge.year.adj, cut.vector, 
          include.lowest = TRUE, labels = FALSE)
    
    ## Calculating appliance id "id.appl"
    df.fridge$id.appl <- paste0("fridg_", df.fridge$year.id, df.fridge$cap_refri,
                                df.fridge$ubica_cong, df.fridge$ptas_refri)
    
    SubstrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    # Manual replacement of special cases, where data is not available,
    # but models are similar to these models.
    
    #a. Top Freezer with 3 doors, equivalent to French type
    df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "13"] <- 
      paste0(substr(df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "13"],
                    0, 8), "23")
    
    df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "33"] <-
      paste0(substr(df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "33"],
                    0, 8), "23")
    #b.Side or Bottom Freezer w/1 door, equivalent to Upright freezer w/1 door 
    df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "31"] <- 
      paste0(substr(df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "31"], 
                    0, 8), "11")
    df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "21"] <- 
      paste0(substr(df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "21"], 
                    0, 8), "11")
    #c. Top/side Freezer w/4 doors, equivalent to bottom freezer w/4 doors
    df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "14"] <-
      paste0(substr(df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "14"], 
                    0, 8), "24")
    
    df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "34"] <-
      paste0(substr(df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "34"], 
                    0, 8), "24")
    # No Freezer w/2 doors, equivalent to no freezer w/1 door
    df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "42"] <- 
      paste0(substr(df.fridge$id.appl[SubstrRight(df.fridge$id.appl, 2) == "42"], 
                    0, 8), "41")
    # If the capacity of the fridge is unknown, the ids are deleted
    df.fridge$id.appl[df.fridge$cap_refri==9] <- NA
    df.fridge$id.appl[is.na(df.fridge$cap_refri)] <- NA
    
    #Merging datasets
    df.fridge <- df.fridge[order(df.fridge$folio),]
    
    df.fridge.sub <- merge(x = df.fridge, y = df.fridge.specs, 
                           by = "id.appl", all.x=TRUE)
    
    #df.fridge <- merge(x = df.fridge, y = df.fridge.specs, 
    #                   by = "id.appl", all.x=TRUE)[ ,c("folio", "fridge.year.adj",
    #                                                   "annual.cons.kwh")]
    #df.fridge <- df.fridge[order(df.fridge$folio),]
    
    return(df.fridge.sub)
  }