library(googlesheets)
library(stringr)

gsheet_title = "Survey: Technological Modifications of the Human  (Responses)"
gsheet = gs_title(gsheet_title)
ws_title = gs_ws_ls(gsheet)[1]
dat = gs_read(ss=gsheet, ws=ws_title)
dat = as.data.frame(dat)

# DATA CLEANING
# remove first and last column
dat = dat[c(-1, -ncol(dat))]
# extract questions and rename columns
questions = colnames(dat)[-c(17,18)]
qcols = c(paste('A', seq(1,4), sep=""),
          paste('B', seq(1,4), sep=""),
          paste('C', seq(1,4), sep=""),
          paste('D', seq(1,4), sep=""))

colnames(dat) = c(qcols, colnames(dat)[17], colnames(dat)[18])
# extract numerical graduating year
dat$Year = as.numeric(str_extract_all(dat$Year, "[0-9]+"))
# extract numerical value for support/oppose/neutral and make categorical
dat[seq(1,16)] = str_extract_all(dat[seq(1,16)], "[0-9]+")
# TODO: extract majors

dataset <- dat


setwd('/Users/AnnieTang/Documents/DUKE/17-18/GSF 366/presentation')
library(shiny)
runApp()
