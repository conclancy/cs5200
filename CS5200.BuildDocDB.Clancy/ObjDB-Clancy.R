#' This program creates a document store that uses folders as a means to 
#' organize data. The "records" that are stored are images. Images can be 
#' tagged. Each folder represents a tag. 
#' 
#' @author:     Connor Clancy
#' @email:      clancy.co@northeastern.edu
#' @published:  21 JAN 2023
#' @assignment: Homework 1 - CS5200 - Spring 2023
#' 


# Create global variables.
rootDir <- "docDB"


#' Helper method to get the number of directories and files located in the 
#' 'database' directory passed to the function. 
#' 
#' @param root path for the directory of the database 
#' @return the number of items in the database, as an integer. 
#' 
databaseSize <- function(root = rootDir) {
  return(length(list.files(path = root)) + length(list.dirs(path = root))) 
}


#' Configure the "database" in the form of a directory.  If the database already
#' exists, then no additional actions are taken. 
#' 
#' @param root the name of the root directory for the database.
#' @param path the path to create the root directory.  Defaults to the current
#'             working directory if one is not provided.
#' @return void
#'             
configDB <- function(root = rootDir, path = getwd()) {
  
  # Set the working directory 
  setwd(path)
  
  if(!dir.exists(root)) {
    dir.create(root)
  } 
}


#' Generate an objects path given the root directory and the object's tag.
#' 
#' @param root the name of the root directory for the database. 
#' @param tag  the tag of the object to that will be used to create the path. 
#' @return the path of the object, as a Character Vector.  
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
#' @returns void
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


#' Get the file name of an image that may or may not contain tags. 
#'
#'@param fileName the name of the file to be isolated
#'@returns the name of the file, as characters, 
#'
getFileName <- function(fileName) {
  
  # Ensure there is at least one space in front of a file extension
  fileName <- sub("\\.", " .", fileName)
  
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
  
  return (paste(pathPieces, sep = " ", collapse = ""))
}


#' Copies all files in the specified folder to their correct folders underneath 
#' root.
#' 
#' @param folder the folder containing the files to be copied. This function 
#'               assumes that the folder is in the current working directory. 
#' @param root   the root directory where files should be placed. 
#' @returns void 
#' 
storeObjs <- function(folder, root = rootDir) {
  
  # Create path variables 
  cwd <- getwd()
  folderForCopy <- paste(cwd, folder, sep = "/")
  
  # Logic to handle folder existence 
  if(dir.exists(folderForCopy)) {
    files <- list.files(folderForCopy)
    
    # create folders and copy files 
    for (f in files) {
      
      # create folders if they do not already exist 
      tags <- getTags(f)
      
      for(tag in tags) {
        folder <- genObjPath(tag = tag)
        
        if(!dir.exists(folder)) {
          dir.create(folder)
        }
        
        fileToCopy <- paste(folderForCopy, f, sep="/")
        fileToPaste <- paste(folder, getFileName(f), sep ="/")
        
        file.copy(fileToCopy, fileToPaste)
      }
    }
  } 
}

#' Removes all folders and files in the folder passed as `root`. This function
#' does not remove the root itself. 
#' 
#' @param root the directory to be cleared of files and folders 
#' @return the number of items in the database 
#'
clearDB <- function(root = rootDir) {
  files <- list.files(path = root)
  
  setwd(root)
  
  for (f in files) {
    unlink(f, recursive = TRUE)
  }
  
  return(databaseSize(root = root)) 
}


#' The main entry point into the script. 
#' 
main <- function() {
  
  errors = 0
  
  #######################################
  #                Test 1               #
  #     Test Database Configuration     #
  #######################################
  
  # Delete the root directory to ensure a clean start to the test. 
  if (dir.exists(rootDir))
  {
    unlink(rootDir)
  }
  
  # Create database 
  configDB(rootDir)
  
  # Increment if errors 
  if (databaseSize() != 1) {
    errors <- errors + 1
    print("Error: (Test 1.0) The database size does not equal 0.")
  }
  
  
  #######################################
  #                Test 2               #
  #     Test Object Path Generation     #
  #######################################
  
  # test if the path has a tag
  if(!genObjPath(rootDir, "#hashTest") == "docDB/hashTest") {
    errors <- errors + 1
    print("Error: (Test 2.0) Tag test has failed.")
  }
  
  # test if the path has a tag
  if(!genObjPath("test", "#hashTest") == "test/hashTest") {
    errors <- errors + 1
    print("Error: (Test 2.1) Tag test has failed.")
  }
  
  # test if the object does not have a tag
  if(!genObjPath(rootDir, "noHashTest") == "docDB/noHashTest") {
    errors <- errors + 1
    print("Error: (Test 2.2) Tag test has failed.")
  }
  

  #######################################
  #                Test 3               #
  #         Test Tag Generation         #
  #######################################  
  
  test_tags = c("#Northeastern", "#ISEC")
  
  if(!all(test_tags == getTags("CampusAtNight #Northeastern #ISEC.jpg"))) {
    errors <- errors + 1
    print("Error: (Test 3.0): Tag generation has failed.")
  }
  
  if(!all(test_tags == getTags("CampusAtNight.jpg #Northeastern #ISEC"))) {
    errors <- errors + 1
    print("Error: (Test 3.1): Tag generation has failed.")
  }
  
  
  #######################################
  #                Test 4               #
  #      Test File Name Generation      #
  #######################################  
  
  file_name = "CampusAtNight.jpg"
  
  if(getFileName("CampusAtNight #Northeastern #ISEC.jpg") != file_name) {
    errors <- errors + 1
    print("Error: (Test 4.0): File name generation has failed.")
  }
  
  if(getFileName("CampusAtNight.jpg #Northeastern #ISEC") != file_name) {
    errors <- errors + 1
    print("Error: (Test 4.1): File name generation has failed.")
  }

  
  #######################################
  #                Test 5               #
  #         Test Object Creation        #
  ####################################### 
  
  # Database should be empty at this point
  if(databaseSize(root = rootDir) != 1) {
    errors <- errors + 1
    print("Error: (Test 5.0): The database size is not 0.")
  }
  
  # Test to ensure the object storage method is working 
  storeObjs("images")
  storeObjs("test")
  
  # There should now be 11 objects in the database 
  if(databaseSize() != 11) {
    errors <- errors + 1
    print("Error: (Test 5.1): The database size is not 11.")
  }
 
  
  #######################################
  #                Test 6               #
  #         Test Object Creation        #
  ####################################### 
  
  # Clear the current database.
  clearDB()
  
  # Test to ensure the clearDB() method removed the objects. 
  if(databaseSize(root = rootDir) != 0) {
    errors <- errors + 1
    print("Error: (Test 5.0): The database size is not 0.")
  }
  
  
  #######################################
  #            Test Output              #
  #         Print Final Results         #
  ####################################### 
  
  if(errors == 0) {
    print("#######################################")
    print("All tests Passed!")
    print("#######################################")
  } else {
    print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    print(paste(errors, "tests failed!  See messages above!", sep = " "))
    print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  }
}

#########################################################

main()


