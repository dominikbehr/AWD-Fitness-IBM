###################################################################
# LOAD PACKAGES

library(lattice)
library(ggplot2)
library(standardize)
library(psych)
library(lubridate)
library(chron)
library(lme4)
library(openxlsx)
library(fitdistrplus)
library(nimble)
library(mcmcplots)
library(runjags)
library(gridExtra)


###################################################################
# LOAD FUNCTIONS

source(file = "02 - functions.R")


###################################################################
# LOAD EMPIRICAL DATA

dir <- "Data/"
files <- list.files(path = dir)
files <- files[grepl(pattern = ".RData", x = files)]
for(file in files){
  temp <- readRDS(file = paste0(dir,file))
  assign(value = temp, x = gsub(pattern = ".RData", replacement = "", x = file))
  remove(temp)
}
remove(dir, files, file)


###################################################################
# SETTINGS FOR PLOTTING:

# define background color of plot (black or white)
colBG <- "white"

font <- "Helvetica"
fontSize <- 20
pointsize <- 3 # define size of points in the plot
pointsize2 <- 2 # define size of points in the plot
lineSize <- 3 # define line size
lineSize2 <- 1.5 # define line size
alphaRibbon <- 0.1
alphaLine <- 0.6

# define colors for males and females for black background:
colM <- "#0571b0"
colF <- "#ca0020"
colPlot <- "black"

if(colBG=="black"){
  colM <- "#00BFC4"
  colF <- "#F8766D"
  colPlot <- "white"
  alphaRibbon <- 0.2
}


# manually add hex codes of transparent colors:
colM0.6 <- "#82aed3"
colF0.6 <- "#e07e89"
colM0.5 <- "#9bbdda"
colF0.5 <- "#e5979e"
colM0.3 <- "#c5d8ea"
colF0.3 <- "#f0c2c7"

if(colBG=="black"){
  #transparent equivalents:
  colM0.6 <- "#85dadd"
  colF0.6 <- "#fbb2ad"
  colM0.5 <- "#006062"
  colF0.5 <- "#7c3b37"
  colM0.3 <- "#003a3b"
  colF0.3 <- "#4b2421"
}


# define general theme:
theme_general <- function(){ 
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(axis.text = element_text(colour = colPlot, size = fontSize, family = font),
          axis.title.x = element_text(colour = colPlot, size = fontSize, family = font, margin = margin(t = 15, r = 0, b = 0, l = 0)), # top, right, bottom, left
          axis.title.y = element_text(colour = colPlot, size = fontSize, family = font, margin = margin(t = 0, r = 15, b = 0, l = 0), angle = 90),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = colPlot),
          axis.ticks = element_line(colour = colPlot),
          plot.background = element_rect(fill=colBG, color = colBG, size = 0),
          plot.title = element_text(colour = colPlot, size = fontSize, family = font, hjust = 0, margin = margin(t = 0, r = 0, b = 15, l = 0)),
          legend.position = c(0.9, 0.9), #legend.position can be also a numeric vector c(x,y). In this case it is possible to position the legend inside the plotting area. x and y are the coordinates of the legend box. Their values should be between 0 and 1. c(0,0) corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
          legend.justification = "right", # justification defines which side of the legend that the legend.position coordinates refer to --> can use left, right, centre or numeric value (0 ≤ x ≤ 1)
          legend.box.margin=margin(t = 0, r = 0, b = 0, l = 0),
          legend.text = element_text(colour = colPlot, size = fontSize, family = font),
          legend.title = element_text(colour = colPlot, size = fontSize, family = font),
          legend.key.width = unit(35,"pt"),
          
          
    ) 
}
