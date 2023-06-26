


# -------------------------------------------------------------------------
# FUNCTIONS
# -------------------------------------------------------------------------


#'progress.initialize
#'
#'initializes progress bar
#'@param runner object that the loop is performed on
#'@param track_time if T, progress.indicate() returns approx. duration of the loop. Note: Only works well with small iterations. To work properly, progress.indicate() has to be the last or second to last function call in the loop if track_time == T
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
  local(track_time <- track_time, env=progress.environment) # track time?
  if(track_time) local(startTime <- Sys.time(), env=progress.environment)  
  
  invisible(TRUE)
}

# -------------------------------------------------------------------------

progress.indicate = function(){
  
  # load params from environment
  runner = local(runner, env=progress.environment)
  width = local(width, env=progress.environment)
  scaling = local(scaling, env=progress.environment)
  index_to_indicate_progress = local(index_to_indicate_progress, env=progress.environment)
  track_time = local(track_time, env=progress.environment)
  
  # track time if desired
  if(track_time && index_to_indicate_progress == 1){
    time_diff = difftime(Sys.time(), local(startTime, env=progress.environment), units = "secs")*(length(runner)-1)
    if (time_diff >= 86400) {  # 86400 secs == 1 day
      time_diff <- time_diff/86400
      attr(time_diff, "units") <- "days"
    } else if (time_diff >= 3600) {  # 3600 secs == 1 h
      time_diff <- time_diff/3600
      attr(time_diff, "units") <- "hours"
    } else if (time_diff >= 60) {  # 60 secs == 1 min
      time_diff <- time_diff/60
      attr(time_diff, "units") <- "mins"
    } else {
      attr(time_diff, "units") <- "secs"
    }
    cat("Approx. Time until finish:", round(time_diff,2), attr(time_diff, "units"),"\n")
  }
  
  # calculate progress
  progress = paste(floor(100*index_to_indicate_progress/length(runner)), "%  ")
  
  # print progress bar
  cat("\r",
      strrep("=", round(index_to_indicate_progress * scaling)),
      strrep(" ", round(width - index_to_indicate_progress*scaling)),
      " | ",
      progress, sep="")
  
  # update running parameter
  local(index_to_indicate_progress <- index_to_indicate_progress+1, env=progress.environment)
  
  # if done: reset running variable
  if(local(index_to_indicate_progress, env=progress.environment) == length(runner)+1){
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
