#Brandon Sandusky
# ST 542
#PCR Positive Ca

#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("xlsx")
#install.packages("rROC")
#install.packages("caret")
library(xlsx)
require(readxl)
require(tidyverse)
require(xlsx)
cat_rawdata <- read_excel("~/Desktop/ST 542/cat_data.xlsx", sheet = "Through 12.2017")

# Working to clean up data here
#Make Column names more readable/simplistic
#Recode Yes/No, pos/neg to 0s and 1s
#Fix age to a be a single unit(months)
#Fix Breed misspellings

names(cat_rawdata)<- c("Patient","Num","Age","Breed","Sex","Housing","Collection","Smear","Flagellated",
                       "History","Hist","Treatment","Tylosin","systemic anti","Steroids","antihelminthic",
                       "AlbonorPonazuril","probiotic","Metronidazole","Ronidazole","Notes","Date","Results","Clinic")
cats<- cat_rawdata[,-c(1,2,10,21,22)]
#Fix categorical Variables
cats <- cats[grep("Pos|Neg", cats$Results, ignore.case = TRUE),]
cats <- cats %>% mutate(Results = ifelse(grepl("^Pos", Results, ignore.case = TRUE), 1, Results)) %>% mutate(Results = ifelse(grepl("Neg", Results, ignore.case = TRUE), 0, Results))
cats <- cats %>% mutate(Housing = ifelse(grepl("s.", Housing, ignore.case = TRUE), "Single", Housing))
cats <- cats %>% mutate(Sex = ifelse(grepl("CM", Sex, ignore.case = TRUE), "MC", Sex))

##############################################################################################
#Fixing the age format in this section
#Try to calculate age in months
#If an age does not contain a an m, y, w to indicate the units, make it NA
#Not throwing away these observations because they may have other features that can be included in a model
Left = function(text,numchar){
  substr(trimws(text),1,numchar)
}

Right = function(text,numchar,endchar){
  substr(trimws(text),numchar, endchar)
}

require(stringr)
testmat<- as.matrix(cats[,1])  

GetCharString <- gsub("[^a-zA-Z]","", testmat[,1])
cats$DateString<- GetCharString
cats$Date_Val <- as.numeric(gsub("[^0-9\\.\\-]","", testmat[,1]))
Uniqstringvals<- unique(cats$DateString)
print(Uniqstringvals)
yrs<- c("Y","y","yr","yrs")
mon<- c("M","m")
wk<- c("W","w","wk","wks")
yrmo<- c("YM","ym")
#invalidchar <- c()
cats$Age_Months<-0

for (i in 1:nrow(cats)){
  if(cats[i,20] %in% yrs){
    cats[i,22] <- cats[i,21]*12
  } else if (cats[i,20] %in% mon){
    cats[i,22] <- cats[i,21]
  }else if (cats[i,20] %in% wk){
    cats[i,22] <- cats[i,21] /4
  } else if (cats[i,20] %in% yrmo){ #these values have years and months
    YPosition<- gregexpr("y",gsub(" ","",cats[i,1]),ignore.case = TRUE)
    XPosition<- gregexpr("m",gsub(" ","",cats[i,1]),ignore.case = TRUE)
    ypos<- YPosition[[1]][1]
    xpos<- XPosition[[1]][1]
    Y <- as.numeric(Left(gsub(" ","",cats[i,1]),ypos-1))*12
    X<- as.numeric(Right(gsub(" ","",cats[i,1]),ypos+1,xpos-1))
    cats[i,22] <- X+Y
  } else {
    cats[i,22]<- NA
  }
}

############################################################################################

#Recode all of the yes/no columns to 0s and 1s
#The original treatment data was labeled as no treatment, so to make this more understandable I recoded it to treatment
# Values of yes in that column indicate no treatment, so I want them to be coded 0
for (i in 6:17){
  if (i != 9){
    cats[,i]<- ifelse(cats[,i] == "Yes",1,0)
  } else  { 
    cats[,i]<- ifelse(cats[,i] == "Yes",0,1)
  }
}

#If there is a history and Treatment is NA, recode the NAs for the treatments to 1
# This is due to re-labelling done above
for (i in 1:nrow(cats)){
  if((!is.na(cats[i,8])) & (is.na(cats[i,9]))){
    if(cats[i,8]==1){
      cats[i,9] <- 1
    }
    else {
    }
  } else {
  }
}

cats$Remove<- NA
# If there was a missing value for treatment and another treatment variable contains yes, delete those records due to inconsistencies
#There are only 2 of these in this dataset
for (i in 1:nrow(cats)){
  for(j in 10:17){
    if(is.na(cats[i,8]) & !is.na(cats[i,j])){
      if(cats[i,j]==1){
        cats[i,23] = "Remove"
      }
      else {
        cats[i,23] = "Keep"
      }
    } else {
      cats[i,23] = "Keep"
    }
  }
}
# Delete rows where one of th treatments was flagged yes, but no treatment provided
cats<- subset(cats,Remove != "Remove")

#If there is a treatment history(1), and no treatment(0), recode the NAs for the treatment variables to 0

for (i in 1:nrow(cats)){
  if((!is.na(cats[i,8])) & (!is.na(cats[i,9]))){
    for (j in 10:17){
      if(is.na(cats[i,j]) & (cats[i,8] == 1) & (cats[i,9] == 0)){
        # cats[is.na(cats)] <- 0
        cats[i, j][is.na(cats[i,j])] <- 0
      } else if(is.na(cats[i,j]) & (cats[i,8] == 1) & (cats[i,9] == 1)){
        cats[i, j][is.na(cats[i,j])] <- 0
      }
    }
  } else {
  }
}
cats$Treat <- names(cats[10:17])[max.col(cats[10:17])]

#Group cats into 2 categories based on age: old and young
#according to  https://www.petmd.com/cat/care/defining-senior-age-cats, a cat is considered senior between ages 7 and 10
# For this analysis we will consider 7 years or 84 months as senior
#0 is young, 1 is old

cats$AgeGroup<- ifelse(cats$Age_Months<84,"Young","Old")

###################################################################################
#Group cats as mixed breed or not mixed
# 0 is domestic or mixed, 1 is a breed
# Grouping cats like this because domestic and mixed brreds are not actually recognized as breeds, as they are from a mized ancestry
mixed<- c("Domestic","Mix")
cats$Breed_Group<-ifelse(gsub( " .*$", "", cats$Breed)%in%mixed,"Mixed","Purebred")
###################################################################################




