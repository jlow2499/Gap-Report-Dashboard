library(shiny)
library(dplyr)
library(stringr)

m<- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/m.csv")
n <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/n.csv")




df<-rbind(m,n)

rm(m); rm(n)

emp <-c("24985","105986","103297","21747","47254","45885","131978","190830","119359",
        "167905","CN2009","CN4450","CN2371","CN3679","CN3653","CN3857","CN3907","CN4199",
        "76924","115866","175947","163618","158836","22094","121681","93150","45845",
        "168041","148992","170717","167902","173045","95513","174646","179647","47252",
        "176843","170715","93288","43738","202198","201274","162776","199709","164007",
        "189722","166008","162723","173092","143377","172790","161094","181043","149549",
        "201071","180225","194484","195197","180239","180229","161696","15857","101241",
        "195504","202195","162827","162718","169500","185554","192285","185291","198018",
        "25607","157831","193339","189377","193344","164774","180055","113172","185292",
        "29962","99661","0","1")
df$EMPNUM <- as.character(df$EMPNUM)
df <- df[!df$EMPNUM %in% emp,]
rm(emp)

df$T <- gsub("^.*? ","",df$TIME)
df$DTE <- gsub(" 0:00:00","",df$ACT_DATE)
time <- as.data.frame(str_split_fixed(df$T,":",n=3))
time <- dplyr::rename(time, Hour = V1, Minute = V2, Second = V3)

time$Hour <- as.numeric(as.character(time$Hour))
time$Minute <- as.numeric(as.character(time$Minute))
time$Second <- as.numeric(as.character(time$Second))

df <- cbind(df,time)
rm(time)
df$DTE <- as.Date(df$DTE,"%m/%d/%Y")

df <- df %>%
  arrange(Second) %>% 
  arrange(Minute) %>%
  arrange(Hour) %>%
  arrange(DTE) %>%
  arrange(EMPNUM) %>%
  mutate(Minute = Minute + Second/60) %>%
  mutate(Hour.Min = Hour + Minute/60) %>%
  group_by(EMPNUM,DTE) %>%
  mutate(Diff = Hour.Min - lag(Hour.Min,default=Hour.Min[1])) %>%
  mutate(Diff_Minutes = Diff *60)

ARMASTER <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",
                     stringsAsFactors=FALSE)

df <- plyr::join(df,ARMASTER,by="EMPNUM",match="first")
rm(ARMASTER)
#df <- df[df$off != "ALL", ]
df$DEPT <- as.factor(df$DEPT)
df$off <- as.factor(df$off)

df$off <- plyr::revalue(df$off,c("K"="Knoxville","C"="Columbus","B"="Columbus2","W"="Westlake","A"="Atlanta"))
#df <- df[!df$DEPT %in% c("ADM","CLR","FAC"), ]
df <- df[df$CODE_2 !="CM",]
df$Month <- format(df$DTE,format="%B %Y")

EMP_GAP_Day <- df %>%
  group_by(A.R,manager,DTE,DEPT,off) %>%
  summarise(More_Than_5_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=5)>3,sum(Diff_Minutes>=5),0)),
            More_Than_15 = as.numeric(ifelse(sum(Diff_Minutes>=15)>3,sum(Diff_Minutes>=15),0)))
EMP_GAP_Day <- EMP_GAP_Day[EMP_GAP_Day$manager != "",]

EMP_GAP_Day <- dplyr::rename(EMP_GAP_Day,Manager=manager,AR = A.R, Department = DEPT, Office = off,Date = DTE)

EMP_GAP_MONTH<- df %>%
  group_by(A.R,manager,Month,DEPT,off) %>%
  summarise(More_Than_5_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=5)>3,sum(Diff_Minutes>=5),0)),
            More_Than_15_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=15)>3,sum(Diff_Minutes>=15),0)))
EMP_GAP_MONTH <- EMP_GAP_MONTH[EMP_GAP_MONTH$manager != "",]
EMP_GAP_MONTH <- EMP_GAP_MONTH[complete.cases(EMP_GAP_MONTH),]

EMP_GAP_MONTH <- dplyr::rename(EMP_GAP_MONTH,Manager=manager,AR = A.R, Department = DEPT, Office = off)

MGR_GAP_Day <- df %>%
  group_by(manager,DTE,DEPT,off) %>%
  summarise(More_Than_5_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=5)>3,sum(Diff_Minutes>=5),0)),
            More_Than_15_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=15)>3,sum(Diff_Minutes>=15),0)))
MGR_GAP_Day <- MGR_GAP_Day[MGR_GAP_Day$manager != "",]
MGR_GAP_Day <- MGR_GAP_Day[complete.cases(MGR_GAP_Day),]

MGR_GAP_Day <- dplyr::rename(MGR_GAP_Day,Manager=manager, Department = DEPT, Office = off,Date = DTE)

MGR_GAP_MONTH<- df %>%
  group_by(manager,Month,DEPT,off) %>%
  summarise(More_Than_5_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=5)>3,sum(Diff_Minutes>=5),0)),
            More_Than_15_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=15)>3,sum(Diff_Minutes>=15),0)))
MGR_GAP_MONTH <- MGR_GAP_MONTH[MGR_GAP_MONTH$manager != "",]
MGR_GAP_MONTH <- MGR_GAP_MONTH[complete.cases(MGR_GAP_MONTH),]

MGR_GAP_MONTH <- dplyr::rename(MGR_GAP_MONTH,Manager=manager, Department = DEPT, Office = off)

order<- c("July 2015", "August 2015", "September 2015","October 2015","November 2015","December 2015","January 2016","February 2016","March 2016","April 2016","May 2016")

MGR_GAP_MONTH$Month <- as.factor(MGR_GAP_MONTH$Month)
EMP_GAP_MONTH$Month <- as.factor(EMP_GAP_MONTH$Month)
EMP_GAP_MONTH$Month <- factor(EMP_GAP_MONTH$Month,levels=order)
EMP_GAP_Day$AR <- as.factor(EMP_GAP_Day$AR)
EMP_GAP_Day$Manager <- as.factor(EMP_GAP_Day$Manager)
EMP_GAP_MONTH$AR <- as.factor(EMP_GAP_MONTH$AR)
EMP_GAP_MONTH$Manager <- as.factor(EMP_GAP_MONTH$Manager)
MGR_GAP_Day$Manager <- as.factor(MGR_GAP_Day$Manager)
MGR_GAP_MONTH$Manager <- as.factor(MGR_GAP_MONTH$Manager)

df <- df %>%
  select(EMPNUM,ARNUM,TFILE,CODE_1,CODE_2,CODE_3,T,DTE,Diff_Minutes,A.R,desk,manager,mgr.,DEPT,off,SUB,Month)


setwd("//Knx3fs01/ED_BA_GROUP/Lowhorn/GAP Report/Application")
runApp(host="0.0.0.0",port=5050)
