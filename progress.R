


# -------------------------------------------------------------------------
# FUNCTIONS
# -------------------------------------------------------------------------


#'progress.initialize
#'
#'initializes progress bar
#'@param runner object that the loop is performed on
#'
progress.initialize = function(runner, track_time=F){
  
  # create new environment
  progress.environment <<- new.env()
  
  # save params to new environment
  local(runner <- runner, env=progress.environment) # running steps
  local(width <- round((2/3)*getOption("width")) +
          round((2/3)*getOption("width"))%%2,
        env=progress.environment) # width of prgress bar (even number forced)
  local(scaling <- width/length(runner), env=progress.environment) # scalar for each iteration
  local(index_to_indicate_progress <- 1, env=progress.environment) # start index
  local(startTime <- Sys.time(), env=progress.environment) # track time?
  local(track_time <- track_time, env=progress.environment) # track time?
  
  invisible(TRUE)
}

# -------------------------------------------------------------------------

#'progress.indicate
#'
#'print progress of a loop, sapply oder lapply function
#'
progress.indicate = function(){
  
  # load params from environment
  runner = local(runner, env=progress.environment)
  width = local(width, env=progress.environment)
  scaling = local(scaling, env=progress.environment)
  index_to_indicate_progress = local(index_to_indicate_progress, env=progress.environment)
  track_time = local(track_time, env=progress.environment)
  
  # calculate progress
  progress = paste(floor(100*index_to_indicate_progress/length(runner)), "%  ")
  
  # track time if desired
  if(track_time && index_to_indicate_progress == 1){
    diff = difftime(Sys.time(), local(startTime, env=progress.environment))*length(runner)
    cat("Approx. Time until finish:", round(difftime(Sys.time()+diff, startTime),2), attr(difftime(Sys.time()+diff, startTime), "units"),"\n")
  }
  
  # print progress bar
  cat("\r",
      strrep("=", round(index_to_indicate_progress * scaling)),
      strrep(" ", round(width - index_to_indicate_progress*scaling)),
      " | ",
      progress, sep="")
  
  # update running parameter
  local(index_to_indicate_progress <- index_to_indicate_progress+1, env=progress.environment)
  
  # if done: reset running variable
  if(index_to_indicate_progress == length(runner)+1){
    local(index_to_indicate_progress <- 1, env=progress.environment)
    cat("\n")
  }
  
  # return invisibly
  invisible(TRUE)
}

# -------------------------------------------------------------------------

#'progress.undo
#'
#'delete environment
#'
progress.undo = function(){
  rm(progress.environment, envir = globalenv())
}


# -------------------------------------------------------------------------
# EXAMPLES
# -------------------------------------------------------------------------


object = 1:1000

progress.initialize(object)
for(index in object){
  index%%2
  progress.indicate()
}

# -------------------------------------------------------------------------

object = 1:100

progress.initialize(object)
result = lapply(object, function(x){
  Sys.sleep(0.05)
  progress.indicate()
  x
})

# -------------------------------------------------------------------------


object = 1:10

progress.initialize(object, T)
result = lapply(object, function(x){
  Sys.sleep(0.5)
  progress.indicate()
  x
})
