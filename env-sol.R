
library(rlang)
library(pryr)
library(testthat)

# a) ----------------------------------------------------------------------

# Difference between an environment and a list:
# 1. Every object in an environment must have a name, not in a list
# 2. Unlike a list, names in an environment are not ordered
# 3. An environment has a parent
# 4. Environments are not copied when modified



# b) ----------------------------------------------------------------------

# ls() and rm() are searching in the current environment, 
# if no environment is specified


# c) ----------------------------------------------------------------------

#  <-: creates a new binding in the current environment
# <<-: modifies an existing binding found in a parent environment


# d)  ---------------------------------------------------------------------


# input: name of variable, environment
# output: list of all environments
anywhere <- function(name, env = parent.frame()) {
  
  stopifnot(is.character(name), length(name) == 1)
  
  env <- pryr:::to_env(env)
  
  env_list <- list()
  
  if (identical(env, emptyenv())) {
    return(env_list)
  }
  if (exists(name, env, inherits = FALSE)) {
    env_list <- c(env_list, env)
  }
  
  env_list <- c(env_list, anywhere(name, parent.env(env)))
  
}


source("test-env-anywhere.R")
