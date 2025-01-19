# Author: Sarit Pery, The Hebrew university of Jerusalem
# Date:    Originaly created                  Dec-2018
#          Wrapped for publication            March-2022
#          Converted for phd thesis analyses  2022


# Project description - Code wise ==============================================
#   Files concept:
#     File numbers: The code files are numbered according to run order (01-09), 
#                   and then grouped around explored topics 
#                   (see next documentation section)
#     Each File creates an .RImage file to be used by the next code.
#     Since all .RImage files are ready, fileS can be executed as stand-alone.
#   Special Files:
#     00 Main phdHumPer.R 
#                   Sets project parameters.
#                   Generates main.RImage to be loaded by each code file.
#     99_utility Functions.R 
#                   Includes utility functions, used across all my projects. 
#                   Is loaded into main.RImage, and hence available for all 
#                   code files.

# Project description - Theory goals wise ======================================
#   This project is based on Study 3 of our Humility-performance JAP (2022) paper.
#   The purpose is the exploration of my phd thesis on the HumPerf Data.
#   Topics explored:
#   1. files 01-10  Original analyses of the published research,
#   2. Files 11-19  Play with Attenuation & reliabilities,
#                   Test the affects of individual level on dyadic levels. Where 
#                   my intuition proved wrong, and Avi's voice of reasoning won.
#                   This still requires writing and processing on my behalf.
#   3. Files 20-29  2L = 2levels integrated analysis - 
#                   a. Using nlme with Kenny's dummy codes
#                   b. Using lme4 with crossed effects.
#   4. Files 30-39  SRM Implementation by SRM PACKAGE
#   5. Files 40-49  SRM Implementation by CROSS-EFFECTS lmer package.
#                   note that 3R output APIM cross-effects analysis is under 2L files.
#   6. Files 50-59  Dyadic indices - mainly humility power couples.
#   7. Files 85-87  dsAPIM from Kenny's 2018 paper
#   7. Files 90-95  initial code generation of utility functions 
#       

# House cleaning ---------------------------------------------------------------
  rm(list = ls())                              #Clean the Global Environment
  cat ("\014")                                 #Clean the R console
  if (is.null(dev.list()) == FALSE) dev.off()  #Clean Plots
  options(scipen = 10)                         #Avoid scientific notation for up to 10 digits

# Project and Utility Parameters, functions, and packages ----------------------
  source("99_utility Functions.R")

  studyName             <- "phdHumPerf"
  RRstyle               <- "perception"   # either "perception" or "behavior"

  save.image("main.RImage")


  