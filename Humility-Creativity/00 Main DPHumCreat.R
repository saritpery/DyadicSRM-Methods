# Author: Sarit Pery, The Hebrew university of Jerusalem
# Date:    Originaly created                  Jan-2024
#          Wrapped for publication            Jan-2025


# ==============================================================================
# ========== DP - Dyadic Processes, Dissertation Illustrations  ================
# ==========      Using Humility-Creativity DataSet             ================
# ==============================================================================

# Project description - Code wise ==============================================
#   Files concept:
#     File numbers: The code files are numbered according to run order (00-99), 
#                   though each file is stand-alone.
#     Each File creates an .RImage file to be used by the next code.
#     Since all .RImage files are ready, each code file can be executed as stand-alone.
#   Special Files:
#     00 Main DPHumCreat.R 
#                   Current file.
#                   Sets project parameters.
#                   Generates main.RImage to be loaded by each code file.
#     99_utility Functions.R 
#                   Includes utility functions, used across all my projects. 
#                   Is loaded into main.RImage, and hence available for all 
#                   code files.

# Project description - Theory wise ============================================
#   This project is based on Study 3 of our Humility-performance JAP (2022) paper.
#   The purpose is the exploration of my phd thesis on the Humility-Creativity Data.
#   Topics explored:
#   1. Basic SRM analysis of the data (using tripleR) - for data acquaintance only
#   2. SRM scores extraction 
#   3. Pairwise format creation
#   4. Addition of dummy codes to Pairwise format
#   4. Implementation of the dissertation's Illustrations
#       

# House cleaning ---------------------------------------------------------------
  rm(list = ls())                              #Clean the Global Environment
  cat ("\014")                                 #Clean the R console
  if (is.null(dev.list()) == FALSE) dev.off()  #Clean Plots
  options(scipen = 10)                         #Avoid scientific notation for up to 10 digits

# Project and Utility Parameters, functions, and packages ----------------------
  source("99_utility Functions.R")

  studyName             <- "DPHumCreat"   # DP - Dyadic Process, on Humility and Creativity
  RRstyle               <- "perception"   # either "perception" or "behavior"
  x <- read_rds(fname(".LongData", fileExtention = "RDS", includeTime = FALSE))
  save.image("main.RImage")
  

  