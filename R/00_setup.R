library(easyR)
library(sf)
library(raster)
library(tidyverse)
library(dplyr)

g_path <- get_google_drive_path("juanmayorga@ucsb.edu")

emlab_data_dir <- file.path(g_path,"/Shared drives/emlab/data")

gocp_project_dir <- file.path(g_path,"Shared drives/emlab/projects/current-projects/ocean-conservation-priorities")

project_dir <- file.path(g_path,"/Shared drives/emlab/projects/current-projects/gocpR")
