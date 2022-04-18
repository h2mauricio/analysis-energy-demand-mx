###############################################################################
## This script includes the functions needed to compute the electricity 
## consumption (in KWh) for each bill recorded in ENCEVI
###############################################################################

EstimateBillConsumptionKWh <- function (df.bill, df.tariff.sch, periodicity.bill, 
                                        tariff.type, is.summer){
  # Computes the power consumption of each household based on the information
  # collected in ENCEVI, related to the electricity bill 
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
  
  is.low.avg.high <- "low"
  
  df.tarif.sch.sub <- subset(df.tariff.cost, df.tariff.cost$summer == is.summer)
  df.tarif.sch.sub <- subset(df.tarif.sch.sub, df.tarif.sch.sub$tariff == tariff.type)
  
  df.bill.sub <- subset(df.bill, df.bill$is.summer1 == is.summer)
  df.bill.sub <- subset(df.bill.sub, df.bill.sub$bill.period1 == periodicity.bill)
  df.bill.sub <- subset(df.bill.sub, df.bill.sub$tipo_tarif.f == tariff.type & 
                          df.bill.sub$bill.amount1 >= 0)
  
  if(periodicity.bill == "bi-monthly"){
    df.bill.sub$bill.notax.tmp <- df.bill.sub$bill.notax / 2.0
  } else{
    df.bill.sub$bill.notax.tmp <- df.bill.sub$bill.notax
  }
  
  df.bill.sub <- transform(df.bill.sub,
                           tariff.block=df.tarif.sch.sub$tariff.block[findInterval(bill.notax.tmp,
                                                                                   df.tarif.sch.sub$cost.range.min)])
  
  df.bill.sub$tariff.block <- sapply(df.bill.sub$tariff.block, function(x) as.character(x))
  

  for (row in 1:nrow(df.bill.sub)){

    # Block 1
    if(!is.na(df.bill.sub$tariff.block[row]) & df.bill.sub$tariff.block[row] >= 1 ){

      if(df.bill.sub$bill.notax.tmp[row] >= df.tarif.sch.sub$cost.range[df.tarif.sch.sub$tariff.block == 1]){
        #subtracting the bill amount
        df.bill.sub$bill.notax.tmp[row] = df.bill.sub$bill.notax.tmp[row] - 
          df.tarif.sch.sub$cost.range[df.tarif.sch.sub$tariff.block == 1] 
        
        df.bill.sub$cons.kwh[row] = df.tarif.sch.sub$cost.range[df.tarif.sch.sub$tariff.block == 1] / 
          df.tarif.sch.sub$cost.kwh[df.tarif.sch.sub$tariff.block == 1]
      }
      else{
        df.bill.sub$cons.kwh[row] = df.bill.sub$bill.notax.tmp[row] / 
          df.tarif.sch.sub$cost.kwh[df.tarif.sch.sub$tariff.block == 1]
      }
    }
    # Block 2

    if(!is.na(df.bill.sub$tariff.block[row]) & df.bill.sub$tariff.block[row] >= 2 ){
      if(df.bill.sub$bill.notax.tmp[row] >= df.tarif.sch.sub$cost.range[df.tarif.sch.sub$tariff.block == 2]){
        #subtracting the bill amount
        df.bill.sub$bill.notax.tmp[row] <- df.bill.sub$bill.notax.tmp[row] - 
          df.tarif.sch.sub$cost.range[df.tarif.sch.sub$tariff.block == 2] 
        
        df.bill.sub$cons.kwh[row] <- df.bill.sub$cons.kwh[row] + 
          df.tarif.sch.sub$cost.range[df.tarif.sch.sub$tariff.block == 2] / 
          df.tarif.sch.sub$cost.kwh[df.tarif.sch.sub$tariff.block == 2]
      }
      
      else{
        df.bill.sub$cons.kwh[row] = df.bill.sub$cons.kwh[row] + 
          df.bill.sub$bill.notax.tmp[row] / 
          df.tarif.sch.sub$cost.kwh[df.tarif.sch.sub$tariff.block == 2]
      }
    }
    
    # Block 3  
    if(!is.na(df.bill.sub$tariff.block[row]) & df.bill.sub$tariff.block[row] >= 3 ){
      if(df.bill.sub$bill.notax.tmp[row] >= df.tarif.sch.sub$cost.range[df.tarif.sch.sub$tariff.block == 3]){
        df.bill.sub$cons.kwh[row] = df.bill.sub$cons.kwh[row] + 
          df.bill.sub$bill.notax.tmp[row] / 
          df.tarif.sch.sub$cost.kwh[ df.tarif.sch.sub$tariff.block == 3]
      }
      
      else{
        df.bill.sub$cons.kwh[row] = df.bill.sub$cons.kwh[row] + 
          df.bill.sub$bill.notax.tmp[row] / 
          df.tarif.sch.sub$cost.kwh[ df.tarif.sch.sub$tariff.block == 3]
      }
    }
    
    #Block 4
    if(!is.na(df.bill.sub$tariff.block[row]) & df.bill.sub$tariff.block[row] >= 4 ){
      # As this is the last level
      if(df.bill.sub$bill.notax.tmp[row] >= df.tarif.sch.sub$cost.range[df.tarif.sch.sub$tariff.block == 4]){
        df.bill.sub$cons.kwh[row] = df.bill.sub$cons.kwh[row] + 
          df.bill.sub$bill.notax.tmp[row] / 
          df.tarif.sch.sub$cost.kwh[ df.tarif.sch.sub$tariff.block == 4]
      }
      
      else{
        df.bill.sub$cons.kwh[row] = df.bill.sub$cons.kwh[row] + 
          df.bill.sub$bill.notax.tmp[row] / 
          df.tarif.sch.sub$cost.kwh[ df.tarif.sch.sub$tariff.block == 4]
      }
    }  
  }
  
  if(periodicity.bill == "bi-monthly"){
    df.bill.sub$cons.kwh <- df.bill.sub$cons.kwh * 2
  }
  
  return(df.bill.sub) 
}