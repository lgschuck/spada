if(interactive()){

  repo <- 'https://packagemanager.posit.co/cran/2024-12-10'
  names(repo) <- 'posit_10122024'
  options(repos = repo)
  rm(repo)

  .libPaths('../.spada_libs')

  library(shiny)
  library(bslib)
  library(DT)
  library(bsicons)
  library(data.table)
  library(dplyr)
  library(conflicted)

  conflicted::conflict_prefer('filter', 'dplyr')

}
