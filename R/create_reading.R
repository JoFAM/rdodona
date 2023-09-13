#' Create reading material
#'
#' With this function you can create a directory set up for a reading material.
#'
#' @param name the name of the directory with the reading activity.
#' @param path the place to put this directory
#' @param lang the languages ("en" and or "nl") for english and dutch respectively
#' @param descnames a named list with optional descriptions for in the config.json file.
#' @param labels an optional vector with labels
#'
#' @return TRUE if success, FALSE otherwise
#' @importFrom jsonlite toJSON
create_reading <- function(name,
                           path = ".",
                           lang = c("en","nl"),
                           descnames = list(),
                           labels = character(0)){
  lang <- match.arg(lang,
                    choices = c("en","nl"),
                    several.ok = TRUE)


  # create config
  js <- list(type = "content")
  if(length(descnames) == 0){
    descnames[lang] <- rep(name,length(lang))
  }

  .check_desc(descnames)

  js$description$names <- descnames

  if(length(labels)){
    js$labels <- labels
  }

  # Create dirs
  maindir <- file.path(path,name)
  if(!dir.exists(maindir))
    dir.create(maindir)
  else
    stop("Directory exists already.")

  dir.create(file.path(maindir,"description"))

  # Create files
  writeLines(toJSON(js, pretty = TRUE),
             con = file.path(maindir,
                             "config.json")
  )
  if("en" %in% lang){
    file.create(file.path(maindir,
                          "description",
                          "description.en.md"))
  }
  if("nl" %in% lang){
    file.create(file.path(maindir,
                          "description",
                          "description.nl.md"))
  }

  return(TRUE)
}


## Check labels
.check_desc <- function(x){
  out <- is.list(x) &&
    !is.null(names(x)) &&
    all(names(x) %in% c("en","nl")) &&
    length(x) <= 2 &&
    all(sapply(x,is.character))
  if(!out)
    stop("Description names look off.")
}
