setwd("/shared/nas/bu/co/persist/rzgutie/Training/latam")


DF2Hive <- function(data="",db="",path="",user="",format="orc",overwrite=FALSE){
  if(overwrite){
    system(paste0("hive -e 'drop table ",db,".",user,"_",deparse(match.call()[["data"]]),";'"))
  }
  
  namesClasses <- cbind(colnames(data),unname(sapply(data,class)))
  namesClasses[,2] <- gsub("integer","int",namesClasses[,2])
  namesClasses[,2] <- gsub("numeric","double",namesClasses[,2])
  namesClasses[,2] <- gsub("character","string",namesClasses[,2])
  namesClasses[,2] <- gsub("factor","string",namesClasses[,2])
  namesClasses[,2] <- gsub("Date","date",namesClasses[,2])
  namesClasses[,2] <- gsub("logical","boolean",namesClasses[,2])
  ## add similar lines here to improve the variable class mapping from R to Hive (e.g. dates and characters)
  
  dataName <- deparse(match.call()[["data"]])
  
  
  
  
  # Step 1: save data frame to tier 2 in csv format
  print("before")
  write.table(data,paste0("/shared/shape_tier2/",db,"/",user,"/",dataName,".tsv"),sep="\t",quote = FALSE,col.names=FALSE,row.names=F)
  print("after")
  
  # Steps 2-5: create system call
  #   Step 2: Convert csv to Hive table stored as text
  #   Step 3: Create empty Hive table formatted as ORC
  #   Step 4: Load data from text table to ORC table
  #   Step 5: Drop text table
  hiveCall <- paste0(
    "DROP TABLE IF EXISTS ",db,".",user,"_",deparse(match.call()[["data"]]),"_temp; "
    ,"CREATE TABLE ",db,".",user,"_",deparse(match.call()[["data"]]),"_temp ("
    ,paste0(apply(namesClasses, 1, paste, collapse=" "),collapse=", ")
    ,")  row format delimited fields terminated by '\t' stored as textfile;
    LOAD DATA INPATH '/",path,"/",user,"/",deparse(match.call()[["data"]]),".tsv' OVERWRITE INTO TABLE "
    ,db,".",user,"_",dataName,"_temp; SET hive.default.fileformat=",format
    ,"; CREATE TABLE ",db,".",user,"_",deparse(match.call()[["data"]])
    ," AS SELECT * FROM ",db,".",user,"_",deparse(match.call()[["data"]]),"_temp;"
    ," DROP TABLE ",db,".",user,"_",deparse(match.call()[["data"]]),"_temp; "
  )
  
  # Step 6: Send system call 
  print(paste0("hive -e \"",hiveCall,"\""))
  system(paste0("hive -e \"",hiveCall,"\""))
}

# usage:
co_base_trades <- data.table::fread(file.choose())
DF2Hive(data=co_base_trades,db="bu_lam",path="bu/lam",user="erosale",format="orc")









# please load 

# i think it's fine now.  admins tweaked some path stuff and it broke this function.
# all better now.Good i think it works with copy_to funcion?

#F2Hive(data=claro_ingreso,db="bu_co",user="rzgutie",format="orc")
