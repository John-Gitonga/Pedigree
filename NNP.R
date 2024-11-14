#input: files stored on the Google drive.
#outputs: a list called "kifaru" with rhinos, mortalities, sightings, patrols
library(tidyverse)
library("googlesheets4")
library(dplyr)
library(stringr)

#Animal IDs- combine all of the files here, or bring in the cleaned file
r1<-read_sheet("https://docs.google.com/spreadsheets/d/1cfK3EvdeDRYFZ_PUZFR2XNLVXnQ2Dl73OvK1AIUXJZw/edit?usp=sharing", col_types = "c")
rhinos.1<-data.frame("ID.No."= r1$ID.No.,
                     "Name"= toupper(r1$Name), 
                     "Sex"= r1$Sex,
                     "Date.of.Birth"= as.Date(r1$Date.of.Birth, format = "%Y-%m-%d"),
                     "Mother"=toupper(r1$Mother),
                     "Father"=toupper(r1$Father))
#Additional rhinos obtained from the translocation history believed to have been deleted from the database after translocation
#Rhino obtained from Ol Jogi Tranlocation In history data except for ALICE
r2<-read_sheet("https://docs.google.com/spreadsheets/d/1cbtXD0ocQHoSNGvquaDw6u1EQ94Aey3m5WsNqcJgInA/edit?usp=sharing", col_types = "c")
rhinos.2<-data.frame("ID.No."= r2$ID.No.,
                     "Name"= toupper(r2$Name), 
                     "Sex"= r2$Sex,
                     "Date.of.Birth"= as.Date(r2$Date.of.Birth, format = "%m/%d/%Y"),
                     "Mother"=toupper(r2$Mother),
                     "Father"=toupper(r2$Father))
#Additional rhinos from MNP introductions
r3<-read_sheet("https://docs.google.com/spreadsheets/d/1vyO4mbcYZoDBn938rmYURAHQGBMPKmHiyAMVuLB0d80/edit?usp=sharing")
rhinos.3<- data.frame("ID.No."=r3$ID.No.,
                      "Name"= toupper(r3$Name),
                      "Sex"=r3$Sex,
                      "Date.of.Birth"= as.Date(r3$Date.of.Birth, format= "%Y-%m-%d"),
                      "Mother"= toupper(r3$Mother),
                      "Father"= toupper(r3$Father))

rhinos<-unique(rbind(rhinos.1, rhinos.2, rhinos.3))
rhinos%>%distinct(Name,Sex,Date.of.Birth,.keep_all = TRUE)
#Rename calves not assigned any unique name to their year of births
#Rename JUDY CALF of ID 219 born in  2017 to JUDY CALF 2017
i<-which(rhinos$ID.No. == 219)
print(rhinos$name[i])
rhinos$Name[i]<-"JUDY CALF 2017"
#Rename KULECHO CALF of ID 203 born in 2014 to KULECHO CALF 2014
i<-which(rhinos$ID.No. == 203)
rhinos$Name[i]<-"KULECHO CALF 2014"
#Rename KYELA'S CALF of ID 225 born in 2018 to KYELA CALF 2018
i<-which(rhinos$ID.No. == 225)
rhinos$Name[i]<-"KYELA CALF 2018"
#Rename MILDRED CALF of ID 236 born 2020 to MILDRED CALF 2020
i<-which(rhinos$ID.No. == 236)
rhinos$Name[i]<-"MILDRED CALF 2020"
#Rename WAHITO'S CALF of ID 238 born in 2020 to WAHITO CALF 2020
i<-which(rhinos$ID.No. == 238)
rhinos$Name[i]<-"WAHITO CALF 2020"
#Assign ALIce date of Birth and ID No.
i<-which(rhinos$Name == "ALICE")
rhinos$ID.No.[i]<-93
rhinos$Date.of.Birth[i]<- as.Date("1996/06/01", format = "%Y/%m/%d")
rhinos$Sex[i]<-"Female"
#Correct the ID.No. and Date of Birth for Trufena whose ID.No. are conflicting with Gitonga
i<-which(rhinos$Name == "TRUFENA")
rhinos$ID.No.[i]<- 170
rhinos$Date.of.Birth[i]<- as.Date("2005/02/01", format = "%Y/%m/%d")
#NNP Mortalities - Imports
m1<-read_sheet("https://docs.google.com/spreadsheets/d/1hhIXedK1K0pnfPHJoll8KHZmMpkVKGKvQJ9TOdjTq50/edit?usp=sharing", col_types = "c")

mortalities<-data.frame("Species"="rhino",
                        "ID.No."=m1$ID.No.,
                        "Name"=toupper(m1$Animal.Name),
                        "Sex"=m1$Sex,
                        "Date.of.Birth"=as.Date(m1$Date.of.Birth, format = "%m/%d/%Y"),
                        "Mortality.Date"=as.Date(m1$Mortality.Date, format="%m/%d/%Y"),
                        "Apparent.Cause.of.Death"=m1$Apparent.Cause.of.Death,
                        "Carcass.State"=NA)
#Add ID No. to Grace and Adam
i<-which(mortalities$Name == "GRACE")
mortalities$ID.No.[i]<-220
i<-which(mortalities$Name == "ADAM")
mortalities$ID.No.[i]<- 69
#Assign details to ALICE 
i<-which(mortalities$Name == "ALICE")
mortalities$ID.No.[i]<-93
mortalities$Date.of.Birth[i]<- as.Date("1996/06/01", format = "%Y/%m/%d")
mortalities$Sex[i]<-"Female"
#Nairobi NP Translocations
t1<-read_sheet("https://docs.google.com/spreadsheets/d/1OTCoWCkEVpouXq2lVc3c6tDQYZZesy-4pKMT8ME-gn8/edit?usp=sharing", col_types = "c")
trans.1<-data.frame(ID.No.= t1$ID.No.,
                    Name= toupper(t1$Animal.Name),
                    Sex= t1$Sex,
                    Date.of.Birth= as.Date(t1$Date.of.Birth, format = "%m/%d/%Y"),
                    Date.Moved.Out= as.Date(t1$Date.Moved.Out, format = "%m/%d/%Y"),
                    Destination=t1$Destination)
#Correct the ID.No. and Date of Birth for TRUFENA to 156 born on 1st Feb 2005
#i<-which(trans.out$ID.No. == 143)
#trans.out$ID.No.[i]<- 170
#trans.out$Date.of.Birth[i]<- as.Date("2005/02/01", format = "%Y/%m/%d")

#Add trans.out from MNP introductions
#Translocation out date was extracted from the introduction date in  MNP
t2<-read_sheet("https://docs.google.com/spreadsheets/d/1R5oiNwrMIDtFDNolHo9l-8b1sbKg6TSv9Lhx6tB87-g/edit?usp=sharing", col_types = "c")
trans.2<-data.frame("ID.No."= t2$ID.No.,
                    "Name"= toupper(t2$Name),
                    "Sex"=t2$Sex,
                    "Date.of Birth"=as.Date(t2$Date.of.Birth, format = "%m/%d/%Y"),
                    "Date.Moved.Out"=as.Date(t2$Date.Moved.Out, format = "%m/%d/%Y"),
                    "Destination"= t2$Destination)
trans.2$Name[trans.2$Name=="DOREEN"]<- "DORENE"
trans.out<- unique(rbind(trans.1, trans.2))
trans.in<-data.frame( species = NA,
                      ID.No.= NA,
                      Name= NA,
                      Sex= NA,
                      Date.of.Birth=  NA,
                      Date.Moved.In= NA,
                      Source=NA)

#Correct the ID.No. and Date of Birth for TRUFENA to 156 born on 1st Feb 2005
i<-which(trans.out$ID.No. == 143)
trans.out$ID.No.[i]<- 170
trans.out$Date.of.Birth[i]<- as.Date("2005/02/01", format = "%Y/%m/%d")

final.data<-list(site="Nairobi National Park",
                 rhinos=rhinos,
                 mortalities=mortalities,
                 introductions=trans.in,
                 removals=trans.out,
                 date.gen=Sys.Date(), 
                 creator=Sys.info()["user"]
)
#save to local PC
# Find the maximum number of rows
#max_rows <- max(sapply(final.data, length))
# Fill columns with fewer rows with NA
#final.data <- lapply(final.data, function(column) {
  #length(column) <- max_rows
 # return(column)
#})

# Convert the list back to a data frame
#final.data <- as.data.frame(final.data)

#file_path <- "C:/Users/user/OneDrive/Documents/Strathmore/MSc Data Science Analytics/Year 1/Module III/Dissertation Seminar I/Masters Project"
#write.csv(rhinos, file = file_path, row.names = FALSE)

###Generate a Cow Calf relationshi
rhinos_2 <- rhinos
rhinos_2<-data.frame(mother=rhinos$Mother,
               child=rhinos$Name) 
               
rhinos_2$Mother[is.na(rhinos_2$mother)]<-"UNKNOWN"

#if mother in snot in child columne, add 
'%!in%' <- function(x,y)!('%in%'(x,y))

to.add<-which(unique(rhinos_2$Mother)%!in%rhinos_2$Name==TRUE)
to.add<-unique(rhinos_2$Mother)[to.add]
to.add.df<-data.frame(
  mother="UNKNOWN",
  child=to.add,
  status="in population"
)

i.na<-which(is.na(rhinos_2$child)==TRUE)
if(length(i.na)>0){
  df<-df[-i.na,]} 
i.na<-which(is.na(to.add.df$child)==TRUE)
if(length(i.na)>0){
  to.add.df<-to.add.df[-i.na,]} 

df<-rbind(df,to.add.df)

df<-rbind(c(NA,"UNKNOWN",	"in population"),df)

df$color <- df$status
levels(df$color) <- c("red","green","yellow","black")



collapsibleTreeNetwork(
  df,
  attribute = "status",
  fill = "color",
  collapsed = T)


