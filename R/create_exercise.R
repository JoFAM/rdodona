#' Create an exercise
#'
#' This function creates the blueprint of an exercise,
#' including some boilerplate if needed. The files can then be
#' adapted manually later on. It will create a config.json file
#' specifically for R.
#'
#' @param name the name of the directory with the reading activity.
#' @param path the place to put this directory
#' @param lang the languages ("en" and or "nl") for english and dutch respectively
#' @param descnames a named list with optional descriptions for in the config.json file.
#' @param labels an optional vector with labels
#' @param boilerplate a logical value indicating whether you want a boilerplate, or a character value with the boilerplate code
#'
#' @return If the function finishes, the value \code{TRUE}.
#' @export
create_exercise <- function(name,
                           path = ".",
                           lang = c("en","nl"),
                           descnames = list(),
                           labels = character(0),
                           boilerplate = FALSE){

  # Processing arguments
  lang <- match.arg(lang,
                    choices = c("en","nl"),
                    several.ok = TRUE)
  if(is.character(boilerplate)){
    add_boil <- TRUE
    boiler_content <- boilerplate
  } else if(is.logical(boilerplate) ){
    add_boil <- boilerplate
    boiler_content <- character(1)
  }

  # create config
  js <- list(evaluation = list(handler = "R"),
             programming_language = "R")
  if(length(descnames) == 0){
    descnames[lang] <- rep(name,length(lang))
  }

  .check_desc(descnames)

  js$description$names <- descnames

  if(length(labels)){
    js$labels <- labels
  }

  js <- js[c("description","labels","evaluation",
             "programming_language")]

  # Create dirs
  maindir <- file.path(path,name)
  if(!dir.exists(maindir))
    dir.create(maindir)
  else
    stop("Directory exists already.")

  dir.create(file.path(maindir,"description"))
  dir.create(file.path(maindir,"evaluation"))

  if(add_boil){
    dir.create(file.path(maindir,
                         "description",
                         "boilerplate"))
  }

  # Create files
  writeLines(toJSON(js, pretty = TRUE, auto_unbox = TRUE),
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

  file.create(file.path(maindir,
                        "evaluation",
                        "test.R"))

  if(add_boil){
    writeLines(boiler_content,
               file.path(maindir,
                          "description",
                          "boilerplate",
                         "boilerplate")) # this is the extensionless file!
  }

  return(TRUE)
}
