#############################################
#######  CARLOS ANTONIO MARINO  #############
#############################################
#Part 2: Longitudinal Data Class and Methods#
#############################################

## Read in the data
library(readr)
library(magrittr)
source("oop_code.R")
## Load any other packages that you may need to execute your code

data <- read_csv("MIE.csv")

################################################################

setClass("LongitudinalData", slots = c(DF = "data.frame"))
setClass("Subject", slots= c(id = "numeric",value = "numeric",
                             timepoint = "numeric"), contains = "LongitudinalData") 
setClass("Visit", slots = c(visit = "numeric"), contains = "Subject")
setClass("Room", slots = c(room= "character"),contains = "Subject")

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

setMethod(f="SetDataFile","data.frame",function(x)
{LD@DF<<-x
}
)
setMethod(f="make_LD","data.frame",function(x)
{structure(list(data=x))
  class<-"LongitudinalData"
  SetDataFile(data)
  return(data)
}
)
setMethod("[",  signature(x="LongitudinalData",i="ANY"), definition=function(x, i)
{f<-x@id[which(x@id==i)]
return(d<-f[1])
}
)
setMethod("summary", signature(x="numeric", y="character"), function(x,y)
{dat<<-subset(LD@DF, LD@DF[[y]]==x)
class(dat)=="data.frame"
f<-(with(dat, tapply(value, list(visit, room), FUN=mean)))
return(print(f))
}
)

setMethod("subject","numeric", function(s_ID)
{if(!(s_ID %in% LD_Subject@id))
{cat("Subject ID does not exist")}
  else
  { paste("Subject ID:",LD_Subject[s_ID])
  }
}
)

################################################################

LD_Subject <-new("Subject", id = data$id, value =data$value, timepoint = data$timepoint)
LD_Visit <-new("Visit", visit = data$visit)
LD_Room <-new("Room", room = as.character(data$room))

data <- read_csv("MIE.csv")
x <- make_LD(data)
print(class(x))
print(x)
# 
# ## Subject 10 doesn't exist
out <- subject(10)
print(out)

out <- subject(14)
print(out)

out <- subject(54) %>% summary
print(out)


out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)