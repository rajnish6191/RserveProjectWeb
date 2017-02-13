# State of Execution
print(Sys.time())
memory.limit(8000)

#Load Libraries

library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(splitstackshape)
library(tcltk)
library(Rserve)
Rserve()


get_scriptpath <- function() {
  # location of script can depend on how it was invoked:
  # source() and knit() put it in sys.calls()
  path <- NULL
  
  if(!is.null(sys.calls())) {
    path <- as.character(sys.call(1))[2] 
    # make sure we got a file that ends in .R, .Rmd or .Rnw
    if (grepl("..+\\.[R|Rmd|Rnw]", path, perl=TRUE, ignore.case = TRUE) )  {
      return(path)
    } else { 
      message("Obtained value for path does not end with .R, .Rmd or .Rnw: ", path)
    }
  } else{
    # Rscript and R -f put it in commandArgs
    args <- commandArgs(trailingOnly = FALSE)
  }
  return(path)
}


mypath <- get_scriptpath()

# output_name <- paste("SBI", format(Sys.time(),"_%Y.%m.%d_%H.%M"),".csv", sep = "" )
output_name <- paste("Inquiry_Attributes_Script",".csv", sep = "" )

DIR <- dirname(mypath)
setwd(DIR)
batch_path <- read.table("batch_name.txt")
unlink("batch_name.txt")
setwd("..")
# setwd("D://data//Project//Input")
# setwd("C://Users//RajnishKumar//data//Project//Input")
# setwd("/ubuntu//data//Project//Input")
setwd(as.character(batch_path[1,1]))
setwd("Input")

#Input Data
# d<-read.table("D:/Script/Sbi_Card/Account_Sbi_Card.csv",sep="|",header=T,colClasses=c("CANDIDATE...ID"="character","LOS.APP.ID"="character"),fill=T) Original

for(i in 1:length(list.files())){
  if(tools::file_ext(list.files()[i])== "csv"){
    file <- read.csv(list.files()[i], sep="|",header=T,colClasses=c("CANDIDATE...ID"="character","LOS.APP.ID"="character"), nrows = 1)
    if((ncol(file) == 42 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID") | 
       (ncol(file) == 41 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID") | 
       (ncol(file) == 43 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID") |
       (ncol(file) == 40 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID") |
       (ncol(file) == 44 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID")){
      
      dz <- read.csv(list.files()[i], sep="|",header=T,colClasses=c("NULL", NA,"NULL","NULL","NULL","NULL","NULL","NULL","NULL",
                                                                    "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL",
                                                                    "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL",
                                                                    "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL",
                                                                    "NULL","NULL","NULL","NULL","NULL"), row.names = NULL)
      dy <- read.csv(list.files()[i], sep="|",header=T,colClasses=
                       c("CANDIDATE...ID"="character","LOS.APP.ID"="character"),
                     row.names = NULL, nrows = 1)
      rm(file)
      break
    } else if (i == length(list.files())){
      print("No Account file found!!")
    }
  }
}
# d<-d[nchar(as.character(d$REPORTED.DATE...HIST))>0,]
# d$X<-NULL



# df<-read.table("D:/Script/Sbi_Card/Inquiry_Sbi_Card.csv",sep="|",header=T,fill=T) /////Original
for(i in 1:length(list.files())){
  if(tools::file_ext(list.files()[i])== "csv"){
    file <- read.csv(list.files()[i], sep="|",header=T, nrows = 1,colClasses=c("LOS.APP.ID"="character"))
    if((ncol(file) == 32 & colnames(file)[2] == "LOS.APP.ID" & colnames(file)[3] == "BRANCH.ID") | 
       (ncol(file) == 33 & colnames(file)[2] == "LOS.APP.ID" & colnames(file)[3] == "BRANCH.ID") |
       (ncol(file) == 31 & colnames(file)[2] == "LOS.APP.ID" & colnames(file)[3] == "BRANCH.ID")){
      
      df <- read.csv(list.files()[i], sep="|",header=T,colClasses=c("LOS-APP-ID"="character"))
      rm(file)
      break
    } else if (i == length(list.files())){
      print("No Inquiry file found!!")
    }
  }
}
df$X<-NULL



run_date<-substr(df$CREDT.RPT.ID,5,10)
run_date<-unique(run_date)
run_date<-max(run_date)
run_date<-as.Date(as.character(run_date),"%y %m %d")


## Retro PR Date from Inquiry
retro_date <- unique(df$RETRO.PR.DT)
retro_date <- as.Date(paste(substr(retro_date,1,2),'-',substr(retro_date,4,5),'-',substr(retro_date,7,10), sep = ''),  format="%d-%m-%Y")

##Retro OR As-on-Run Date
date<-ifelse(is.na(retro_date),as.character(run_date),as.character(retro_date))
date<-as.Date(date)

#df1<-read.table("D:/Script/Sbi_Card/Summary_Sbi_Card.csv",sep="|",header=T,colClasses=c("LOS-APP-ID"="character"),check.names=FALSE,fill=T)  /////Original
for(i in 1:length(list.files())){
  if(tools::file_ext(list.files()[i])== "csv"){
    file <- read.csv(list.files()[i], sep="|",header=T, nrows = 1,colClasses=c("LOS.APP.ID"="character"))
    if((ncol(file) == 28 & colnames(file)[2] == "CUSTOMER.ID.MBR.ID" & colnames(file)[3] == "LOS.APP.ID")|
       (ncol(file) == 27 & colnames(file)[2] == "CUSTOMER.ID.MBR.ID" & colnames(file)[3] == "LOS.APP.ID")|
       (ncol(file) == 26 & colnames(file)[2] == "CUSTOMER.ID.MBR.ID" & colnames(file)[3] == "LOS.APP.ID")){
      
      df1 <- read.csv(list.files()[i], sep="|",header=T,colClasses=c("LOS-APP-ID"="character"),check.names=FALSE,fill=T)
      rm(file)
      break
    } else if (i == length(list.files())){
      print("No Summary file found!!")
    }
  }
}
df1$X<-NULL

# df2<-read.table("D:/Script/Sbi_Card/Summary_Sbi_Card.csv",sep="|",header=T,fill=T) /////Original
for(i in 1:length(list.files())){
  if(tools::file_ext(list.files()[i])== "csv"){
    file <- read.csv(list.files()[i], sep="|",header=T, nrows = 1,colClasses=c("LOS.APP.ID"="character"))
    if((ncol(file) == 10 & colnames(file)[2] == "CUSTOMER.ID.MBR.ID" & colnames(file)[3] == "LOS.APP.ID")|
       (ncol(file) == 9 & colnames(file)[2] == "CUSTOMER.ID.MBR.ID" & colnames(file)[3] == "LOS.APP.ID")|
       (ncol(file) == 11 & colnames(file)[2] == "CUSTOMER.ID.MBR.ID" & colnames(file)[3] == "LOS.APP.ID")){
      
      df2 <- read.csv(list.files()[i], sep="|",header=T,colClasses=c("LOS.APP.ID"="character"),fill=T)
      rm(file)
      break
    } else if (i == length(list.files())){
      print("No Ioi file found!!")
    }
  }
}
df2$X<-NULL


rownumber <- nrow(dz)
column.names <- colnames(dy)

rm(dz)
rm(dy)
gc()

l<-function(d,c1,c2)
{
  c<-ifelse((c1>c2),c1,c2)
  c<-as.numeric(c)
  return(c)
}

k<-function(d)
{
  
  all_trades<-function(data.frame,ACCOUNT.STATUS)
  {
    act<-aggregate(d$ACCOUNT.STATUS,list(crd=d$CREDT.RPT.ID),table)
    colnames(act) <- c("CREDT.RPT.ID", "ALL_TYPES_TRADES")
    return(act)
  }
  ALL_TRADES<-all_trades(d,ACCOUNT.STATUS)
  
  
  all_trades_types<-function(data.frame)
  {
    act<-aggregate(d$ACCT.TYPE,list(crd=d$CREDT.RPT.ID),table)
    colnames(act) <- c("CREDT.RPT.ID", "TRADES")
    return(act)
  }
  PDT_TYPE<-all_trades_types(d)
  
  
  pcblgtthreethousand<-function(d)
  {
    pclgt3k<-function(d)
    {
      
      
      
      
      
      clgt3k<-function(x,y)
      {
        c<-ifelse(x>30&y>3000,1,0) 
        return(c)
      }
      c1<-clgt3k(d$DPD...HIST_01,d$CUR.BAL...HIST_01)
      c2<-clgt3k(d$DPD...HIST_02,d$CUR.BAL...HIST_02)
      c3<-clgt3k(d$DPD...HIST_03,d$CUR.BAL...HIST_03)
      c4<-clgt3k(d$DPD...HIST_04,d$CUR.BAL...HIST_04)
      c5<-clgt3k(d$DPD...HIST_05,d$CUR.BAL...HIST_05)
      c6<-clgt3k(d$DPD...HIST_06,d$CUR.BAL...HIST_06)
      c7<-clgt3k(d$DPD...HIST_07,d$CUR.BAL...HIST_07)
      c8<-clgt3k(d$DPD...HIST_08,d$CUR.BAL...HIST_08)
      c9<-clgt3k(d$DPD...HIST_09,d$CUR.BAL...HIST_09)
      c10<-clgt3k(d$DPD...HIST_10,d$CUR.BAL...HIST_10)
      c11<-clgt3k(d$DPD...HIST_11,d$CUR.BAL...HIST_11)
      c12<-clgt3k(d$DPD...HIST_12,d$CUR.BAL...HIST_12)
      c1[is.na(c1)]<-0
      c2[is.na(c2)]<-0
      c3[is.na(c3)]<-0
      c4[is.na(c4)]<-0
      c5[is.na(c5)]<-0
      c6[is.na(c6)]<-0
      c7[is.na(c7)]<-0
      c8[is.na(c8)]<-0
      c9[is.na(c9)]<-0
      c10[is.na(c10)]<-0
      c11[is.na(c11)]<-0 
      c12[is.na(c12)]<-0
      
      q<-c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12
      return(q)
    }
    
    d$gtthree<-pclgt3k(d)
    act<-aggregate(d$gtthree,list(crd=d$CREDT.RPT.ID),sum) 
    colnames(act) <- c("CREDT.RPT.ID", "DEL_GRTR_THIRTY_BALANCE_GRT_THREE_THOUSAND")
    return(act)
  }
  DPDGRTR30_CURRBAL_GRTR3000<-pcblgtthreethousand(d)
  
  
  PAYDEL_12MNT<-function(d)
  {
    bad<-function(d)
    {
      l<-function(d,c1,c2)
      {
        count<-ifelse(c1>30&(c1>c2),1,0)
        return(count)
      }
      c1<-l(d,d$DPD...HIST_01,d$DPD...HIST_02)
      c2<-l(d,d$DPD...HIST_02,d$DPD...HIST_03)
      c3<-l(d,d$DPD...HIST_03,d$DPD...HIST_04)
      c4<-l(d,d$DPD...HIST_04,d$DPD...HIST_05)
      c5<-l(d,d$DPD...HIST_05,d$DPD...HIST_06)
      c6<-l(d,d$DPD...HIST_06,d$DPD...HIST_07)
      c7<-l(d,d$DPD...HIST_07,d$DPD...HIST_08)
      c8<-l(d,d$DPD...HIST_08,d$DPD...HIST_09)
      c9<-l(d,d$DPD...HIST_09,d$DPD...HIST_10)
      c10<-l(d,d$DPD...HIST_10,d$DPD...HIST_11)
      c11<-l(d,d$DPD...HIST_11,d$DPD...HIST_12)
      c12<-l(d,d$DPD...HIST_12,d$DPD...HIST_13)
      c1[is.na(c1)]<-0
      c2[is.na(c2)]<-0
      c3[is.na(c3)]<-0
      c4[is.na(c4)]<-0
      c5[is.na(c5)]<-0
      c6[is.na(c6)]<-0
      c7[is.na(c7)]<-0
      c8[is.na(c8)]<-0
      c9[is.na(c9)]<-0
      c10[is.na(c10)]<-0
      c11[is.na(c11)]<-0
      c12[is.na(c12)]<-0
      
      
      q<-c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12
      return(q)
      
    }
    d$bb<-bad(d)
    act<-aggregate(d$bb,list(crd=d$CREDT.RPT.ID),sum)
    colnames(act) <- c("CREDT.RPT.ID", "DEL_INC_12_MNTH")
    return(act)
  }
  INCREASE_12MNTH_DEL<-PAYDEL_12MNT(d)
  
  
  P2_VARIABLES <-function(d)
  {
    #d[is.na(d)] <- 0 
    
    d$PR_NUM <- (ifelse(is.na(d$AMT.PAID...HIST_01),0,d$AMT.PAID...HIST_01) + ifelse(is.na(d$AMT.PAID...HIST_02),0,d$AMT.PAID...HIST_02) + ifelse(is.na(d$AMT.PAID...HIST_03),0,d$AMT.PAID...HIST_03))
    d$PR_DEN <- (ifelse(is.na(d$CUR.BAL...HIST_02),0,d$CUR.BAL...HIST_02) + ifelse(is.na(d$CUR.BAL...HIST_03),0,d$CUR.BAL...HIST_03) + ifelse(is.na(d$CUR.BAL...HIST_04),0,d$CUR.BAL...HIST_04))
    
    act_num<-aggregate(d$PR_NUM,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(act_num) <- c("CREDT.RPT.ID", "PAY_RATIO_NUM")
    
    act_den<-aggregate(d$PR_DEN,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(act_den) <- c("CREDT.RPT.ID", "PAY_RATIO_DEN")
    
    l<-function(d,c1,c2)
    {
      c<-ifelse((c1>c2),1,0)
      c<-as.character(c)
      return(c)
    }
    
    c1<-l(d,as.numeric(d$CUR.BAL...HIST_01),as.numeric(d$CUR.BAL...HIST_02))
    c2<-l(d,as.numeric(d$CUR.BAL...HIST_02),as.numeric(d$CUR.BAL...HIST_03))
    c3<-l(d,as.numeric(d$CUR.BAL...HIST_03),as.numeric(d$CUR.BAL...HIST_04))
    c4<-l(d,as.numeric(d$CUR.BAL...HIST_04),as.numeric(d$CUR.BAL...HIST_05))
    c5<-l(d,as.numeric(d$CUR.BAL...HIST_05),as.numeric(d$CUR.BAL...HIST_06))
    c6<-l(d,as.numeric(d$CUR.BAL...HIST_06),as.numeric(d$CUR.BAL...HIST_07))
    c7<-l(d,as.numeric(d$CUR.BAL...HIST_07),as.numeric(d$CUR.BAL...HIST_08))
    c8<-l(d,as.numeric(d$CUR.BAL...HIST_08),as.numeric(d$CUR.BAL...HIST_09))
    c9<-l(d,as.numeric(d$CUR.BAL...HIST_09),as.numeric(d$CUR.BAL...HIST_010))
    c10<-l(d,as.numeric(d$CUR.BAL...HIST_10),as.numeric(d$CUR.BAL...HIST_11))
    c11<-l(d,as.numeric(d$CUR.BAL...HIST_11),as.numeric(d$CUR.BAL...HIST_12))
    c12<-l(d,as.numeric(d$CUR.BAL...HIST_12),as.numeric(d$CUR.BAL...HIST_13))
    
    d$BAL_BIN<-paste(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12, sep = "")
    
    d1<-l(d,as.numeric(d$DPD...HIST_01),as.numeric(d$DPD...HIST_02))
    d2<-l(d,as.numeric(d$DPD...HIST_02),as.numeric(d$DPD...HIST_03))
    d3<-l(d,as.numeric(d$DPD...HIST_03),as.numeric(d$DPD...HIST_04))
    d4<-l(d,as.numeric(d$DPD...HIST_04),as.numeric(d$DPD...HIST_05))
    d5<-l(d,as.numeric(d$DPD...HIST_05),as.numeric(d$DPD...HIST_06))
    d6<-l(d,as.numeric(d$DPD...HIST_06),as.numeric(d$DPD...HIST_07))
    d7<-l(d,as.numeric(d$DPD...HIST_07),as.numeric(d$DPD...HIST_08))
    d8<-l(d,as.numeric(d$DPD...HIST_08),as.numeric(d$DPD...HIST_09))
    d9<-l(d,as.numeric(d$DPD...HIST_09),as.numeric(d$DPD...HIST_010))
    d10<-l(d,as.numeric(d$DPD...HIST_10),as.numeric(d$DPD...HIST_11))
    d11<-l(d,as.numeric(d$DPD...HIST_11),as.numeric(d$DPD...HIST_12))
    d12<-l(d,as.numeric(d$DPD...HIST_12),as.numeric(d$DPD...HIST_13))
    
    d$DPD_BIN<-paste(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12, sep = "")
    
    d$DPD_COUNT<-ifelse((grepl("1",d$DPD_BIN)==TRUE),1,0)
    d$DPD_COUNT<-ifelse((grepl("11",d$DPD_BIN)==TRUE),2,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("111",d$DPD_BIN)==TRUE),3,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("1111",d$DPD_BIN)==TRUE),4,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("11111",d$DPD_BIN)==TRUE),5,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("111111",d$DPD_BIN)==TRUE),6,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("1111111",d$DPD_BIN)==TRUE),7,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("11111111",d$DPD_BIN)==TRUE),8,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("111111111",d$DPD_BIN)==TRUE),9,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("1111111111",d$DPD_BIN)==TRUE),10,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("11111111111",d$DPD_BIN)==TRUE),11,d$DPD_COUNT)
    d$DPD_COUNT<-ifelse((grepl("111111111111",d$DPD_BIN)==TRUE),12,d$DPD_COUNT)
    
    d$BAL_COUNT<-ifelse((grepl("1",d$BAL_BIN)==TRUE),1,0)
    d$BAL_COUNT<-ifelse((grepl("11",d$BAL_BIN)==TRUE),2,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("111",d$BAL_BIN)==TRUE),3,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("1111",d$BAL_BIN)==TRUE),4,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("11111",d$BAL_BIN)==TRUE),5,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("111111",d$BAL_BIN)==TRUE),6,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("1111111",d$BAL_BIN)==TRUE),7,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("11111111",d$BAL_BIN)==TRUE),8,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("111111111",d$BAL_BIN)==TRUE),9,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("1111111111",d$BAL_BIN)==TRUE),10,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("11111111111",d$BAL_BIN)==TRUE),11,d$BAL_COUNT)
    d$BAL_COUNT<-ifelse((grepl("111111111111",d$BAL_BIN)==TRUE),12,d$BAL_COUNT)
    
    d<-data.frame(d$CREDT.RPT.ID,d$ACCT.TYPE,d$DPD_COUNT,d$BAL_COUNT,d$PR_NUM,d$PR_DEN)
    names(d)<-c("CREDT.RPT.ID","ACCT.TYPE","DPD_COUNT","BAL_COUNT","PR_NUM","PR_DEN")
    
    d$DPD_COUNT_CC<-ifelse((d$ACCT.TYPE=="Credit Card"),d$DPD_COUNT,NA)
    DPD_COUNT_CC<-aggregate(d$DPD_COUNT_CC,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(DPD_COUNT_CC) <- c("CREDT.RPT.ID", "DPD_COUNT_CC")
    
    d$DPD_COUNT_PL<-ifelse((d$ACCT.TYPE=="Personal Loan"),d$DPD_COUNT,NA)
    DPD_COUNT_PL<-aggregate(d$DPD_COUNT_PL,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(DPD_COUNT_PL) <- c("CREDT.RPT.ID", "DPD_COUNT_PL")
    
    d$DPD_COUNT_HL<-ifelse((d$ACCT.TYPE=="Housing Loan"),d$DPD_COUNT,NA)
    DPD_COUNT_HL<-aggregate(d$DPD_COUNT_HL,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(DPD_COUNT_HL) <- c("CREDT.RPT.ID", "DPD_COUNT_HL")
    
    d$DPD_COUNT_AL<-ifelse((d$ACCT.TYPE=="Auto Loan (Personal)"),d$DPD_COUNT,NA)
    DPD_COUNT_AL<-aggregate(d$DPD_COUNT_AL,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(DPD_COUNT_AL) <- c("CREDT.RPT.ID", "DPD_COUNT_AL")
    
    d$BAL_COUNT_CC<-ifelse((d$ACCT.TYPE=="Credit Card"),d$BAL_COUNT,NA)
    BAL_COUNT_CC<-aggregate(d$BAL_COUNT_CC,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(BAL_COUNT_CC) <- c("CREDT.RPT.ID", "BAL_COUNT_CC")
    
    d$BAL_COUNT_PL<-ifelse((d$ACCT.TYPE=="Personal Loan"),d$BAL_COUNT,NA)
    BAL_COUNT_PL<-aggregate(d$BAL_COUNT_PL,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(BAL_COUNT_PL) <- c("CREDT.RPT.ID", "BAL_COUNT_PL")
    
    d$BAL_COUNT_HL<-ifelse((d$ACCT.TYPE=="Housing Loan"),d$BAL_COUNT,NA)
    BAL_COUNT_HL<-aggregate(d$BAL_COUNT_HL,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(BAL_COUNT_HL) <- c("CREDT.RPT.ID", "BAL_COUNT_HL")
    
    d$BAL_COUNT_AL<-ifelse((d$ACCT.TYPE=="Auto Loan (Personal)"),d$BAL_COUNT,NA)
    BAL_COUNT_AL<-aggregate(d$BAL_COUNT_AL,list(crd=d$CREDT.RPT.ID),sum,na.rm=TRUE)
    colnames(BAL_COUNT_AL) <- c("CREDT.RPT.ID", "BAL_COUNT_AL")
    
    act<-Reduce(function(x, y) merge(x, y,by=c("CREDT.RPT.ID"), all=TRUE), list(act_num,act_den,DPD_COUNT_CC,DPD_COUNT_PL,DPD_COUNT_HL,DPD_COUNT_AL,BAL_COUNT_CC,BAL_COUNT_PL,BAL_COUNT_HL,BAL_COUNT_AL))
    act<-mutate(act, PAY_RATIO = PAY_RATIO_NUM / PAY_RATIO_DEN)
    
    act$PAY_RATIO_NUM<-NULL
    act$PAY_RATIO_DEN<-NULL
    
    #act[act==-10] <- NA
    
    return(act)
  }
  P2_VARIABLES<-P2_VARIABLES(d)
  
  # Merge results of all functions by credit report id
  aggregate<-Reduce(function(x, y) merge(x, y, all=TRUE),list(ALL_TRADES,PDT_TYPE,DPDGRTR30_CURRBAL_GRTR3000,INCREASE_12MNTH_DEL,P2_VARIABLES))
  
  #aggregate<-Reduce(function(x, y) merge(x, y,by=c("CREDT.RPT.ID"), all=TRUE), list(df1,aggregate))
  
  return(aggregate)
  # end of execution
  print(Sys.time())
}


counter = 1
row_counter = 0
print("data reading over")

while(counter <= ceiling(rownumber/50000)){
  print(counter)
  for(i in 1:length(list.files())){
    if(tools::file_ext(list.files()[i])== "csv"){
      file <- read.csv(list.files()[i], sep="|",header=T,colClasses=c("CANDIDATE...ID"="character","LOS.APP.ID"="character",
                                                                      "DATE.REPORTED"="character"), nrows = 1)
      if((ncol(file) == 42 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID") | 
         (ncol(file) == 41 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID") | 
         (ncol(file) == 43 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID") |
         (ncol(file) == 40 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID") |
         (ncol(file) == 44 & colnames(file)[2] == "CANDIDATE...ID" & colnames(file)[3] == "LOS.APP.ID")){
        
        d <- read.csv(list.files()[i], sep="|",header=F,colClasses=c("character"),
                      row.names = NULL, skip = 1 + row_counter, nrows = 50000
        )
        rm(file)
        break
      } else if (i == length(list.files())){
        print("No Account file found!!")
      }
    }
  }
  colnames(d) <- column.names
  d<-d[nchar(as.character(d$REPORTED.DATE...HIST))>0,]
  d$X<-NULL
  
  
#eliminate tradelines which do not have an update in the bureau since the last 3 years
d$DATE.REPORTED<-as.Date(d$DATE.REPORTED,"%d-%m-%Y")
d$date_diff<-difftime(date,d$DATE.REPORTED,unit="days")
d<-d[d$date_diff<=1080,]
#d$as.on.date<-NULL
d$date_diff<-NULL


  
  # df$RETRO.PR.DT<-run_date
  # df2$INQUIRY.DATE<-as.Date(df2$INQUIRY.DATE,"%d-%m-%Y")
  # date<-unique(df$RETRO.PR.DT)
  # df2$Inquiries_6mnth<-ifelse(date-df2$INQUIRY.DATE>=0 & date-df2$INQUIRY.DATE<=180,1,0)
  # df3<-aggregate(df2$Inquiries_6mnth,list(crd=df2$CREDT.RPT.ID),sum)
  # colnames(df3)<-c("CREDT.RPT.ID","Inquiries_6mnth")
  # 
  # 
  # Retro PR Date
  #date<-unique(df$RETRO.PR.DT)
  #date<-as.Date(paste(substr(date,1,2),'-',substr(date,4,5),'-',substr(date,7,10), sep = ''),  format="%d-%m-%Y")
  #df2$INQUIRY.DATE<-as.Date(df2$INQUIRY.DATE,"%d-%m-%Y")
  
  
  # Data Preparation
  print("data prep begin")
  d$DPD...HIST<-gsub("XXX", "000", d$DPD...HIST)
  d$DPD...HIST<-gsub("DDD", "000", d$DPD...HIST)
  
  d$ASSET.CLASS...HIST<-gsub("XXX", "000", d$ASSET.CLASS...HIST)
  d$ASSET.CLASS...HIST<-gsub("DDD", "000", d$ASSET.CLASS...HIST)
  d$ASSET.CLASS...HIST<-gsub("L01", "000", d$ASSET.CLASS...HIST)
  d$ASSET.CLASS...HIST<-gsub("L02", "091", d$ASSET.CLASS...HIST)
  d$ASSET.CLASS...HIST<-gsub("L03", "361", d$ASSET.CLASS...HIST)
  d$ASSET.CLASS...HIST<-gsub("L04", "361", d$ASSET.CLASS...HIST)
  d$ASSET.CLASS...HIST<-gsub("L05", "001", d$ASSET.CLASS...HIST)
  
  d$DAS...HIST<-gsub("(.{3})", "\\1 ", d$DAS...HIST)
  d<-concat.split.multiple(d, split.col="DAS...HIST", sep=" ")
  
  #d$DPD_HIST_ORIGINAL<-d$DPD...HIST
  d$DPD...HIST<-gsub("(.{3})", "\\1 ", d$DPD...HIST)
  d<-concat.split.multiple(d, split.col="DPD...HIST", sep=" ")
  
  d$ASSET.CLASS...HIST<-gsub("(.{3})", "\\1 ", d$ASSET.CLASS...HIST)
  d<-concat.split.multiple(d, split.col="ASSET.CLASS...HIST", sep=" ")
  
  d<-concat.split.multiple(d, split.col="CUR.BAL...HIST", sep=",")  
  
  d<-concat.split.multiple(d, split.col="AMT.PAID...HIST", sep=",")
  
  
  print("function l begin")
  d$DPD...HIST_01<-l(d,d$DPD...HIST_01,d$ASSET.CLASS...HIST_01)
  d$DPD...HIST_02<-l(d,d$DPD...HIST_02,d$ASSET.CLASS...HIST_02)
  d$DPD...HIST_03<-l(d,d$DPD...HIST_03,d$ASSET.CLASS...HIST_03)
  d$DPD...HIST_04<-l(d,d$DPD...HIST_04,d$ASSET.CLASS...HIST_04)
  d$DPD...HIST_05<-l(d,d$DPD...HIST_05,d$ASSET.CLASS...HIST_05)
  d$DPD...HIST_06<-l(d,d$DPD...HIST_06,d$ASSET.CLASS...HIST_06)
  d$DPD...HIST_07<-l(d,d$DPD...HIST_07,d$ASSET.CLASS...HIST_07)
  d$DPD...HIST_08<-l(d,d$DPD...HIST_08,d$ASSET.CLASS...HIST_08)
  d$DPD...HIST_09<-l(d,d$DPD...HIST_09,d$ASSET.CLASS...HIST_09)
  d$DPD...HIST_10<-l(d,d$DPD...HIST_10,d$ASSET.CLASS...HIST_10)
  d$DPD...HIST_11<-l(d,d$DPD...HIST_11,d$ASSET.CLASS...HIST_11)
  d$DPD...HIST_12<-l(d,d$DPD...HIST_12,d$ASSET.CLASS...HIST_12)
  d$DPD...HIST_13<-l(d,d$DPD...HIST_13,d$ASSET.CLASS...HIST_13)
  
  d<-as.data.frame(d)
  
  
  print("function k begin")
  aggregate <- k(d)
  aggregate<-as.data.frame(aggregate)
  colnames(aggregate)[1]<-"CREDT-RPT-ID"
  
  aggregate<- merge(df1,aggregate,by="CREDT-RPT-ID", all.X = T)
  
  if (!file.exists("Setting")){
    dir.create("Setting")
  }
  setwd(".//Setting")
  
  #write a file with counter name
  write.table(aggregate,paste(counter,".csv", sep = ""),sep="|",row.names = FALSE)
  
  setwd("..")
  
  counter = counter + 1
  
  row_counter = row_counter + 50000
  
}

setwd(".//Setting")
settingfolder <- getwd()
final <- matrix(data = NA, nrow = 1, ncol = 81)


colnames(final) = c('CREDT.RPT.ID','CUSTOMER.ID.MBR.ID','LOS.APP.ID','STATUS','ERROR','PERFORM_CNS.SCORE','PERFORM_CNS.SCORE.DESCRIPTION','PRI.NO.OF.ACCTS',
                    'PRI.ACTIVE.ACCTS','PRI.OVERDUE.ACCTS','PRI.CURRENT.BALANCE','PRI.SANCTIONED.AMOUNT','PRI.DISBURSED.AMOUNT','SEC.NO.OF.ACCTS','SEC.ACTIVE.ACCTS','SEC.OVERDUE.ACCTS',
                    'SEC.CURRENT.BALANCE','SEC.SANCTIONED.AMOUNT','SEC.DISBURSED.AMOUNT','PRIMARY.INSTAL.AMT','SEC.INSTAL.AMT','NEW.ACCTS.IN.LAST.SIX.MONTHS',
                    'DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS','AVERAGE.ACCT.AGE','CREDIT.HISTORY.LENGTH','NO.OF_INQUIRIES','ALL_TYPES_TRADES.Active','ALL_TYPES_TRADES.Closed',
                    'ALL_TYPES_TRADES.Delinquent','ALL_TYPES_TRADES.Restructured','ALL_TYPES_TRADES.Settled','ALL_TYPES_TRADES.Suit.Filed','ALL_TYPES_TRADES.Written.Off',
                    'TRADES.Auto.Loan..Personal.','TRADES.Business.Loan.Against.Bank.Deposits','TRADES.Business.Loan.General','TRADES.Business.Loan.Priority.Sector..Agriculture',
                    'TRADES.Business.Loan.Priority.Sector..Others','TRADES.Business.Loan.Priority.Sector..Small.Business',
                    'TRADES.Business.Non.Funded.Credit.Facility.Priority.Sector..Small.Business','TRADES.Business.Non.Funded.Credit.Facility.General',
                    'TRADES.Commercial.Vehicle.Loan','TRADES.Construction.Equipment.Loan','TRADES.Consumer.Loan','TRADES.Corporate.Credit.Card','TRADES.Credit.Card',
                    'TRADES.Education.Loan','TRADES.Fleet.Card','TRADES.Gold.Loan','TRADES.Housing.Loan','TRADES.Individual','TRADES.JLG.Individual','TRADES.Kisan.Credit.Card',
                    'TRADES.Leasing','TRADES.Loan.Against.Bank.Deposits','TRADES.Loan.against.Card','TRADES.Loan.Against.Shares...Securities','TRADES.Loan.to.Professional',
                    'TRADES.Microfinance.Business.Loan','TRADES.Microfinance.Personal.Loan','TRADES.Non.Funded.Credit.Facility','TRADES.Other','TRADES.Overdraft','TRADES.Personal.Loan',
                    'TRADES.Property.Loan','TRADES.Secured.Credit.Card','TRADES.Staff.Loan','TRADES.Two.Wheeler.Loan','TRADES.Used.Car.Loan','TRADES.Used.Tractor.Loan',
                    'DEL_GRTR_THIRTY_BALANCE_GRT_THREE_THOUSAND','DEL_INC_12_MNTH','DPD_COUNT_CC','DPD_COUNT_PL','DPD_COUNT_HL','DPD_COUNT_AL','BAL_COUNT_CC','BAL_COUNT_PL','BAL_COUNT_HL',
                    'BAL_COUNT_AL','PAY_RATIO')
final <- as.data.frame(final)
for(file in 1:length(list.files())){
  newfile <- read.csv(list.files()[file], sep = "|", header = TRUE,
                      stringsAsFactors = FALSE,colClasses=c("LOS.APP.ID"=
                                                              "character"))
  
  final <- plyr::rbind.fill(final, newfile)
}

colnames(final) = c('CREDT-RPT-ID','CUSTOMER ID/MBR ID','LOS-APP-ID','STATUS',
                    'ERROR','PERFORM_CNS SCORE','PERFORM_CNS SCORE-DESCRIPTION',
                    'PRI-NO-OF-ACCTS','PRI-ACTIVE-ACCTS','PRI-OVERDUE-ACCTS',
                    'PRI-CURRENT-BALANCE','PRI-SANCTIONED-AMOUNT',
                    'PRI-DISBURSED-AMOUNT','SEC-NO-OF-ACCTS','SEC-ACTIVE-ACCTS',
                    'SEC-OVERDUE-ACCTS','SEC-CURRENT-BALANCE',
                    'SEC-SANCTIONED-AMOUNT','SEC-DISBURSED-AMOUNT','PRIMARY-INSTAL-AMT',
                    'SEC-INSTAL-AMT','NEW-ACCTS-IN-LAST-SIX-MONTHS','DELINQUENT-ACCTS-IN-LAST-SIX-MONTHS',
                    'AVERAGE-ACCT-AGE','CREDIT-HISTORY-LENGTH','NO-OF_INQUIRIES','ALL_TYPES_TRADES.Active',
                    'ALL_TYPES_TRADES.Closed','ALL_TYPES_TRADES.Delinquent','ALL_TYPES_TRADES.Restructured',
                    'ALL_TYPES_TRADES.Settled','ALL_TYPES_TRADES.Suit Filed','ALL_TYPES_TRADES.Written Off',
                    'TRADES.Auto Loan (Personal)','TRADES.Business Loan Against Bank Deposits',
                    'TRADES.Business Loan General','TRADES.Business Loan Priority Sector  Agriculture',
                    'TRADES.Business Loan Priority Sector  Others','TRADES.Business Loan Priority Sector  Small Business',
                    'TRADES.Business Non-Funded Credit Facility-Priority Sector- Small Business',
                    'TRADES.Business Non-Funded Credit Facility General','TRADES.Commercial Vehicle Loan',
                    'TRADES.Construction Equipment Loan','TRADES.Consumer Loan','TRADES.Corporate Credit Card',
                    'TRADES.Credit Card','TRADES.Education Loan','TRADES.Fleet Card','TRADES.Gold Loan','TRADES.Housing Loan',
                    'TRADES.Individual','TRADES.JLG Individual','TRADES.Kisan Credit Card','TRADES.Leasing',
                    'TRADES.Loan Against Bank Deposits','TRADES.Loan against Card','TRADES.Loan Against Shares / Securities',
                    'TRADES.Loan to Professional','TRADES.Microfinance Business Loan','TRADES.Microfinance Personal Loan',
                    'TRADES.Non-Funded Credit Facility','TRADES.Other','TRADES.Overdraft','TRADES.Personal Loan',
                    'TRADES.Property Loan','TRADES.Secured Credit Card','TRADES.Staff Loan','TRADES.Two-Wheeler Loan',
                    'TRADES.Used Car Loan','TRADES.Used Tractor Loan','DEL_GRTR_THIRTY_BALANCE_GRT_THREE_THOUSAND',
                    'DEL_INC_12_MNTH','DPD_COUNT_CC','DPD_COUNT_PL','DPD_COUNT_HL','DPD_COUNT_AL','BAL_COUNT_CC',
                    'BAL_COUNT_PL','BAL_COUNT_HL','BAL_COUNT_AL','PAY_RATIO'
)

#  deletes the auxiallary folder.
# setwd("..")
# unlink("Setting")

# sets the wd to output location we defined and save with the chosen name.
setwd("..")
setwd("..")

if (!file.exists("Output")){
  dir.create("Output")
}
setwd(".//Output")

final <- final[!duplicated(final$`CREDT-RPT-ID`),]
final <- final[-1,]

# we sometimes have NA in calculated fields. 
# turn NA to zero values. keep pay ratio column with NA's
final_values <- final[,26:80]
final_PRat <- final[,81]
final <- final[,1:25]
final_values[is.na(final_values)] <- 0
final <- cbind(final, final_values, final_PRat)
final <- arrange(final, `CREDT-RPT-ID`)

write.table(final,file = output_name,sep="|",row.names = FALSE)
print(Sys.time())
setwd(settingfolder)
rm(list=ls(all=TRUE))
gc()
setwd("..")
unlink("Setting", recursive = TRUE)
