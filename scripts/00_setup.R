# sabah_lichens
# Setup

rm(list = ls())  # clear R's memory

orig_wd <- getwd()  # store working directory
Sys.setenv(TZ = "UTC")  # set timezone (avoid `as.POSIX*` warnings)
set.seed(123456)  # set seed for random number generation




# Packages ----------------------------------------------------------

# load required packages:
library(tidyverse)
library(vegan)




# Functions ---------------------------------------------------------

# load functions:
source('./scripts/functions.R')