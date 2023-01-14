rootDir <- "docDB"


#' Configure the "database" in the form of a directory.  If the database already
#' exists, then no additional actions are taken. 
#' 
#' @param root the name of the root directory for the database.
#' @param path the path to create the root directory.  Defaults to the current
#'             working directory if one is not provided.
#'             
configDB <- function(root = rootDir, path = getwd()) {
  
  # Set the working directory 
  setwd(path)
  
  if(!dir.exists(root)) {
    dir.create(root)
    print("Created database")
  } else {
    print("Existing database")
  }
  
}


#' Generate an objects path given the root directory and the object's tag.
#' 
#' @param root the name of the root directory for the database. 
#' @param tag  the tag of the object to that will be used to create the path. 
#' @return the path of the object, as a String. 
#' 
genObjPath <- function(root = rootDir, tag) {
  
  if(substring(tag, 1, 1) == "#") {
    tag <- substring(tag, 2)
  } 
  
  return(paste(root, tag, sep = "/"))
}


#' Get the tags from a file given the file name
#' 
#' @param fileName the name of the file to be parsed. 
#'
getTags <- function(fileName) {
  
  # Split the file name into tokens using the space between the characters.
  tokens <- strsplit(fileName, " ")[[1]]
  
  # Isolate only the elements of the vector starting with a '#' character.
  tags <- c() 
  
  for (i in 1:length(tokens)) {
    if(substring(tokens[i], 1, 1) == "#") {
      tags[i] <- tokens[i]
    }
  }
  
  # remove the NA tags from the list. 
  tags <- tags[!is.na(tags)]
  
  # Determine if the tags contain any file extensions and remove them. 
  for (i in 1:length(tags)) {
    tagChar <- strsplit(tags[i], "")[[1]]
    if(any(tagChar == ".")) {
      stopChar <- which(tagChar == ".") - 1
      tags[i] <- substring(tags[i], 1, stopChar)
    }
  }
  
  return(tags)
  
}


#'
#'
getFileName <- function(fileName) {
  
  # Ensure there is at least one space in front of a file extension
  fileName <- sub(".", " .", fileName)
  
  # Split the file name into tokens using the space between the characters.
  tokens <- strsplit(fileName, " ")[[1]]
  
  # Isolate only the elements of the vector not starting with a '#' character.
  pathPieces <- c() 
  
  for (i in 1:length(tokens)) {
    if(substring(tokens[i], 1, 1) != "#") {
      pathPieces[i] <- tokens[i]
    } else {
      pathPieces[i] <- NA
    }
  }
  
  # remove the NA pieces from the vector. 
  pathPieces <- pathPieces[!is.na(pathPieces)]
  
  return (paste(pathPieces))
  
}

storeObjs <- function(folder, root) {
  
}

clearDB <- function(root = rootDir) {
  
}

#' The main entry point into the script. 
#' 
main <- function() {
  
  # Test config Database
  configDB(rootDir)
  
  # Test genObjPath
  print(genObjPath(rootDir, "#hashTest"))
  print(genObjPath(rootDir, "noHashTest"))
  
  # Test getTags
  print(getTags("CampusAtNight #Northeastern #ISEC.jpg"))
  print(getTags("CampusAtNight.jpg #Northeastern #ISEC"))
  
  # Get fileName 
  print(getFileName("CampusAtNight #Northeastern #ISEC.jpg"))
  print(getFileName("CampusAtNight.jpg #Northeastern #ISEC"))
}

#########################################################

main()


