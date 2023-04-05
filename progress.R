


# -------------------------------------------------------------------------
# FUNCTIONS
# -------------------------------------------------------------------------


#'progress.initialize
#'
#'initializes progress bar
#'@param runner object that the loop is performed on
#'
progress.initialize = function(runner){
  
  # create new environment
  progress.environment <<- new.env()
  
  # save params to new environment
  local(runner <- runner, env=progress.environment) # running steps
  local(width <- round((2/3)*getOption("width")) +
          round((2/3)*getOption("width"))%%2,
        env=progress.environment) # width of prgress bar (even number forced)
  local(scaling <- width/length(runner), env=progress.environment) # scalar for each iteration
  local(index_to_indicate_progess <- 1, env=progress.environment) # start index
  
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
  index_to_indicate_progess = local(index_to_indicate_progess, env=progress.environment)
  width = local(width, env=progress.environment)
  scaling = local(scaling, env=progress.environment)
  
  # calculate progress
  progress = paste(floor(100*index_to_indicate_progess/length(runner)), "%  ")
  
  # print progress bar
  cat("\r",
      strrep("=", round(index_to_indicate_progess * scaling)),
      strrep(" ", round(width - index_to_indicate_progess*scaling)),
      " | ",
      progress, sep="")
  
  # update running parameter
  local(index_to_indicate_progess <- index_to_indicate_progess+1, env=progress.environment)
  
  # if done: reset running variable
  if(local(index_to_indicate_progess, env=progress.environment) == length(runner)+1){
    local(index_to_indicate_progess <- 1, env=progress.environment)
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

