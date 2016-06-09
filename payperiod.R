library(dplyr)



TRACKERFILE <- read.delim("//knx3it/AWG Management/Lowhorn/MTD Database/TRACKERFILE.txt", stringsAsFactors=FALSE)
MasterTalx_EX <- read.csv("//KNX3IT/AWG Management/Lowhorn/Talx POE Master/MasterTalx_EX.csv", stringsAsFactors=FALSE)
MasterTalx <- select(MasterTalx_EX,Debtor, EMPLOYER_NAME)


TRACKERFILE <- TRACKERFILE[TRACKERFILE$FundsType %in% c("RGWG","RVWG"),]

TRACKERFILE <- left_join(TRACKERFILE,MasterTalx,by="Debtor")


Payments <- TRACKERFILE %>%
  group_by(Debtor) %>%
  summarize(Payments = sum(TransAmt))
Periods <- TRACKERFILE %>%
  group_by(Debtor) %>%
  summarize(Periods = n())

TRACKERFILE$PostTms <- as.Date(TRACKERFILE$PostTms,"%Y-%m-%d")
TRACKERFILE <- TRACKERFILE %>%
  arrange(desc(PostTms))

Recent <- TRACKERFILE[!duplicated(TRACKERFILE$Debtor),]
TRACKERFILE <- TRACKERFILE %>%
  arrange(PostTms)

Oldest <- TRACKERFILE[!duplicated(TRACKERFILE$Debtor),]

Recent <- select(Recent,Debtor, PostTms)
Oldest <- select(Oldest,Debtor, PostTms) 



Recent <- rename(Recent,Recent_Date = PostTms)
Oldest <- rename(Oldest,Oldest_Date = PostTms)

Master <- left_join(Payments,Periods,by="Debtor")
Master <- left_join(Master,Recent,by="Debtor")
Master <- left_join(Master,Oldest,by="Debtor")

Master <- Master %>%
  mutate(TotalDays = Recent_Date - Oldest_Date) %>%
  mutate(Pay_Period = TotalDays/Periods)


Master <- arrange(Master,desc(Pay_Period))

Master <- left_join(Master,MasterTalx,by="Debtor")


