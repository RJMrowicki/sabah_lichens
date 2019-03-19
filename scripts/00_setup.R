# sabah_lichens
# Setup

rm(list = ls())  # clear R's memory
orig_wd <- getwd()  # store working directory

# set environmental variables and constants:

Sys.setenv(TZ = "UTC")  # set timezone (avoid `as.POSIX*` warnings)
set.seed(123456)  # set seed for random number generation

n_perm <- 9999  # specify no. permutations for multivariate analyses




# Packages ----------------------------------------------------------

# load required packages:
library(tidyverse)
library(vegan)




# Functions ---------------------------------------------------------

# load functions:
source('./scripts/functions.R')
