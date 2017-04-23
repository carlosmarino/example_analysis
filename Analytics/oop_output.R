#############################################################
###############  CARLOS ANTONIO MARINO  #####################
#############################################################
##########Part 2: Longitudinal Data Class and Methods########
#############################################################

## Read in the data
library(readr)
library(magrittr)
source("oop_code.R")


data <- read_csv("MIE.csv")

################################################################

setClass("LongitudinalData", slots = c(DF = "data.frame"))
setClass("Subject", slots= c(id = "numeric",value = "numeric",
                             timepoint = "numeric"), contains = "LongitudinalData") 
setClass("Visit", slots = c(visit = "numeric"), contains = "subject")
setClass("Room", slots = c(room= "character"),contains = "subject")

################################################################

setGeneric("summary")
setGeneric("make_LD",function(x,...)
{standardGeneric("make_LD")
}
)
setGeneric("SetDF", function(x,...)
{standardGeneric("SetDF")
}
)
setGeneric("subject",function(s_ID,...)
{standardGeneric("subject")
}
)
setGeneric("visit",function(v_ID, ...)
{standardGeneric("visit")
}
)
setGeneric("room",function(r_name, ...)
{standardGeneric("room")
}
)

################################################################

NW<-new("LongitudinalData")
NW_Subject <-new("Subject", id = data$id, value =data$value, timepoint = data$timepoint)
NW_Visit <-new("Visit", visit = data$visit)
NW_Room <-new("Room", room = as.character(data$room))


################################################################

setMethod(f="SetDataFile","data.frame",function(x)
{NW@DF<<-x
}
)
setMethod(f="make_LD","data.frame",function(x)
{structure(list(data=x)) 
class<-"LongitudinalData"
}
)
setMethod("summary", signature(x="numeric", y="character"), function(x,y)
{dat<<-subset(NW@DF, NW@DF[[y]]==x)
class(dat)=="data.frame"
f<-(with(dat, tapply(value, list(visit, room), FUN=mean)))
return(print(f))
}
)

setMethod("subject","numeric", function(s_ID)
{if(!(s_ID %in% LD_Subject@id))
{cat("Subject ID does not exist")}
  else
  { paste("Subject ID:",NW_Subject[s_ID])
  }
}
)

setMethod("visit", "numeric", function(v_ID){
  class(x) <- "data.frame"
  y <- x %>% filter(visit == n)
  structure(y, class = "visit")
}
)
setMethod("room", "character",function(r_name){
  class(x) <- "data.frame"
  y <- x %>% filter(room == r_name)
  structure(y, class = "room")
}
)




