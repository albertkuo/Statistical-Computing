## HW 3
## Statistical Computing
## Albert Kuo
## 10/19/2017

library(readr)
library(dplyr)
library(tidyr)
library(methods)
library(roxygen2)

# mie <- read_csv("~/Documents/Grad School/Computing/MIE.zip", col_types = "cicdi")

#' class LongitudinalData
#' 
#' A class for LongitudinalData.
#' 
#' @slot id a character representing subject ID
#' @slot visit an integer corresponding to the visit number (0, 1, 2)
#' @slot room a character representing the room name
#' @slot value a numeric representing the pollution measured in micrograms per cubic meter
#' @slot timepoint an integer representing the time the measurement was taken
#' @slot selectiontags a character
#' @export
#' 
setClass("LongitudinalData",
         slots = c(id = "character",
                   visit = "integer",
                   room = "character",
                   value = "numeric",
                   timepoint = "integer",
                   selectiontags = "character"))

#' class subject
#' 
#' A class for subject, inherits from LongitudinalData
#' 
#' @slot id a character representing subject ID
#' @slot visit an integer corresponding to the visit number (0, 1, 2)
#' @slot room a character representing the room name
#' @slot value a numeric representing the pollution measured in micrograms per cubic meter
#' @slot timepoint an integer representing the time the measurement was taken
#' @slot selectiontags a character
#' @export
#' 
setClass("subject", contains="LongitudinalData")

#' class visit
#' 
#' A class for visits, inherits from LongitudinalData
#' 
#' @slot id a character representing subject ID
#' @slot visit an integer corresponding to the visit number (0, 1, 2)
#' @slot room a character representing the room name
#' @slot value a numeric representing the pollution measured in micrograms per cubic meter
#' @slot timepoint an integer representing the time the measurement was taken
#' @slot selectiontags a character
#' @export
#' 
setClass("visit", contains="LongitudinalData")

#' class room
#' 
#' A class for room, inherits from LongitudinalData
#' 
#' @slot id a character representing subject ID
#' @slot visit an integer corresponding to the visit number (0, 1, 2)
#' @slot room a character representing the room name
#' @slot value a numeric representing the pollution measured in micrograms per cubic meter
#' @slot timepoint an integer representing the time the measurement was taken
#' @slot selectiontags a character
#' @export
#' 
setClass("room", contains="LongitudinalData")

#' LongitudinalData Constructor
#'
#' This function constructs a new LongitudinalData object given a data frame.
#' 
#' @param DF a data frame
#' @return LD a LongitudinalData object
#' @export
#' @importFrom methods new
#' 
make_LD = function(DF){
  new("LongitudinalData",
      id=DF$id,
      visit=DF$visit,
      room=DF$room,
      value=DF$value,
      timepoint=DF$timepoint,
      selectiontags=c(""))
}

#' subject
#'
#' A generic function for extracting subject-specific information.
#' 
#' @param LD a LongitudinalData object
#' @param subject_char a character representing subject ID
#' @return subject a LongitudinalData object
#' @export
#' @importFrom methods new
#' 
setGeneric(name="subject", def=function(LD, subject_char){
  ind = which(LD@id == subject_char)
  subject_new = new("subject",
                    id = LD@id[ind],
                    visit=LD@visit[ind],
                    room=LD@room[ind],
                    value=LD@value[ind],
                    timepoint=LD@timepoint[ind],
                    selectiontags=append(LD@selectiontags, "subject"))
  return(subject_new)
}
)

#' visit
#'
#' A generic function for extracting visit-specific information.
#' 
#' @param LD a LongitudinalData object
#' @param visit_int an integer corresponding to the visit number (0, 1, 2)
#' @return visit a LongitudinalData object
#' @export
#' @importFrom methods new
#' 
setGeneric(name="visit",def=function(LD, visit_int){
  ind = which(LD@visit == visit_int)
  visit_new = new("visit",
                  id = LD@id[ind],
                  visit=LD@visit[ind],
                  room=LD@room[ind],
                  value=LD@value[ind],
                  timepoint=LD@timepoint[ind],
                  selectiontags=append(LD@selectiontags, "visit"))
  return(visit_new)
}
)

#' room
#'
#' A generic function for extracting room-specific information.
#' 
#' @param LD a LongitudinalData object
#' @param room_char a character representing the room name
#' @return room a LongitudinalData object
#' @export
#' @importFrom methods new
#' 
setGeneric(name="room", def=function(LD, room_char){
  ind = which(LD@room == room_char)
  room_new = new("room",
                 id = LD@id[ind],
                 visit=LD@visit[ind],
                 room=LD@room[ind],
                 value=LD@value[ind],
                 timepoint=LD@timepoint[ind],
                 selectiontags=append(LD@selectiontags, "room"))
  return(room_new)
}
)

#' print
#'
#' This function prints a description of a LongitudinalData object.
#' 
#' @param x a LongitudinalData object
#' @return None
#' @export
#' @import dplyr tidyr
#' 
setMethod(f="print", signature="LongitudinalData", definition=function(x){
  num_subjects = length(unique(x@id))
  if(length(x@selectiontags)==1)
    print(paste("Longitudinal dataset with", num_subjects, "subjects"))
  else{
    if("subject" %in% x@selectiontags){
      if(is.na(x@id[1]))
        print(NULL)
      else
        print(paste("Subject ID:", unique(x@id)))
    }
    if("visit" %in% x@selectiontags){
      if(is.na(x@visit[1]))
        print(NULL)
      else
        print(paste("Visit:", unique(x@visit)))
    }
    if("room" %in% x@selectiontags){
      if(is.na(x@room[1]))
        print(NULL)
      else
        print(paste("Room:", unique(x@room)))
    }
  }
})

#' summary
#'
#' This function returns mean summary statistics for a LongitudinalData object.
#' 
#' @param object a LongitudinalData object
#' @return list containing Subject ID string and summary object
#' @export
#' 
setMethod(f="summary", signature="LongitudinalData", definition=function(object){
  facets = setdiff(c("subject", "visit", "room"),
                   object@selectiontags)
  
  DF = data.frame(id = object@id, 
                  visit = object@visit, 
                  room = object@room,
                  value = object@value, 
                  timepoint = object@timepoint)
  
  summary_DF = NULL
  
  if(length(facets)==0){
    summary_DF = summary(DF$value)
    
  } else{
    summary_DF = DF %>% 
      group_by_at(vars(one_of(facets))) %>%
      summarize(mean=mean(value)) %>%
      spread(room, mean) # Note that this will only work for certain facet combinations
    summary_DF = data.frame(summary_DF)
  }
  return(list(paste("ID:",unique(object@id)),summary_DF))
})
