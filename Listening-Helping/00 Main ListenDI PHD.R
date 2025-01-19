# Author:  Sarit Pery, The Hebrew university of Jerusalem
# Date:    Sep-2023

# Project Code Files description
# -------------------------------------------------------------------------
  # Relevant code files for the project are:
  # 00 Main ListenDI.R          Current file. Project variables initiation
  # 99_Utility functions.R      utility functions shared across all my projects

  # Each File creates an .RImage file to be used by the next code.
  # Since all .RImage files are ready, each file can be executed as stand-alone.
  

# This File Process:
# -------------------------------------------------------------------------
  # Set parameters for the run (mainly control for which code to run)
  
# House cleaning ---------------------------------------------------------------
  rm(list = ls())                              #Clean the Global Environment
  cat ("\014")                                 #Clean the R console
  if (is.null(dev.list()) == FALSE) dev.off()  #Clean Plots
  options(scipen = 10)                         #Avoid scientific notation for up to 10 digits

# Project and Utility Parameters, functions, and packages ----------------------
  source("99_utility Functions.R")

  studyName <-        "ListenDI"
  percTargetTerminology <- TRUE
  
  save.image("main.RImage")
  
################################################################################
##                       MAIN PROGRAM                                        ###
################################################################################
    