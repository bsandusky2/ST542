source("MyLibs.R", local = TRUE)
source("CleanCatData.R", local = TRUE)

#Get Data Ready for R Shiny App
cats<- cats[,-c(1,2,8, 10:17, 20, 21, 23, 24)]
cats[,4:6] <- unlist(cats[,4:6])
cats$Results<- as.numeric(cats$Results)
cats$AgeGroup<- as.factor(cats$AgeGroup)
cats$Breed_Group<- as.factor(cats$Breed_Group)
cats$Sex<- as.factor(cats$Sex)
cats$Housing<- as.factor(cats$Housing)
cats$Collection<- as.factor(cats$Collection)
