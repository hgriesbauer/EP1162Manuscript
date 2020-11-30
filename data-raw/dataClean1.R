# Script to clean and format EP1162 raw data

library(tidyverse)
library(here)

# Read Excel file
X<-readxl::read_excel(here("data-raw","4.0 - FINAL DATASET FOR ANALYSIS - EP 1162 -  1992-2019 FIELD DATA (Feb 4th 2019) MJ.xlsx"),
                      skip=10)


# Start formatting
# Break dataset into five different subsets, and then concatenate at the end
# Five subsets:
# 1992, 1994, 1997, 2009 and 2019

##############
# FUNCTION to extract data and put into a dataframe

yearExtract<-function(year,colList) { # colList has four values:

  # Need to identify four columns for year of interest:
  # Corrected DBH
  # Dead DBH
  # Height
  # Comments

# New names to be assigned
newColNames=c("Plot","TreeID","TreeID.NEW","Species","Sector", # admin
              "DBH","Dead.DBH","Height","Comments", # tree measurements
              "Conk","BlindConk","Scar","Fork.Crook","Frost.Crack","Mistletoe",
              "R.Branch","DBT","Crown.Class","L.Crown")

  X %>% 
  dplyr::select(`Plot #`:Sector,colList,Conk:`L. Crown % (1=10% etc.)`) %>% 
  setNames(newColNames) %>% 
  mutate(Year=year) %>% # Add in year of measurement
    return()
 
  
}
  

# Line up columns with their year
cols.1992=names(X)[c(14,19,25,29)] # identify which columns you are interested in
cols.1994=names(X)[c(15,20,26,30)]
cols.1997=names(X)[c(16,21,27,31)]
cols.2009=names(X)[c(17,22,24,32)] # note the 24 column identifier - this is because heights were not recorded in 2009, and there is no corresponding column in the spreadsheet.  Just picked a dummy column to contain 'NA' values
cols.2019=names(X)[c(18,23,28,33)]


data.Sub1<-rbind(yearExtract(year=1992,colList=cols.1992),
                yearExtract(year=1994,colList=cols.1994),
                yearExtract(year=1997,colList=cols.1997),
                yearExtract(year=2009,colList=cols.2009),
                yearExtract(year=2019,colList=cols.2019)) %>% 

# Data cleaning and formatting on entire dataset

  # Concatenate tree tags
  # We will use whatever is more recent, the 2019 tag or earlier.
  # Replace missing values with NA
  mutate(TreeID.NEW=replace(TreeID.NEW,TreeID.NEW=="--",NA)) %>% 
  mutate(TreeID.NEW=replace(TreeID.NEW,TreeID.NEW=="N/A",NA)) %>% 
  mutate(TreeID=coalesce(TreeID.NEW,TreeID)) %>% 
  dplyr::select(-TreeID.NEW) %>% # drop this column, no longer needed
  
  # Live/Dead trees
  mutate(Status="Live") %>% # initialize a new column
  mutate(Status=replace(Status,!is.na(Dead.DBH),"Dead")) %>%  # if tree has dead DBH recorded, then change its status to dead
  mutate(DBH=as.numeric(DBH)) %>% 
  mutate(DBH=coalesce(DBH,Dead.DBH)) %>%  # if tree has dead DBH recorded, then record its DBH in DBH column
  dplyr::select(-Dead.DBH) %>%  # remove DBH for dead trees

  # formatting
  # some of the original Excel data were based on formulae, and so numbers are too precise
  mutate(Height=as.numeric(Height)) %>% # NAs will be introduced
  
  # most columns need to be factors
  mutate_at(c("Plot","TreeID","Species","Sector","Year","Status"),factor) %>% 
  
  # drop rows that contain NA
  drop_na(Plot,Species) %>% 
  
  # Reorganize columns
  dplyr::select(Plot,TreeID,Species,Sector,Year,Status,everything())

dat<-data.Sub1
save(dat,file="data/ep1162_Data.RData")

###########################
# Screening

# Look at entries where tree number is duplicated within plot
dat %>% 
  add_count(Plot,TreeID,Year) %>% # get a number by this grouping
  filter(n>1) %>% 
  View()

# Look for DBH outliers
boxplot(dat$DBH) # all good there, nothing seems out of the ordinary
boxplot(dat$Height) # all good there, nothing seems out of the ordinary


# Look at entries where recorded DBH is less from one measurement to the other
DBH_decrease<-
  dat %>% 
  filter(TreeID!="New") %>% 
  filter(TreeID!=631) %>% # this is a duplicated tree
  pivot_wider(id_cols=c(Plot,TreeID),names_from="Year",values_from="DBH") %>% 
  mutate(diff.2019=`2019`-`2009`) %>% 
  mutate(diff.2009=`2009`-`1997`) %>% 
  mutate(diff.1997=`1997`-`1994`) %>% 
  mutate(diff.1994=`1994`-`1992`) %>% 
  filter(diff.2019<0 | diff.2009<0 | diff.1997<0 | diff.1994<0) %>% 
  dplyr::select(Plot,TreeID) %>% # select these trees with decreasing DBH measurements
  left_join(dat,by=c("Plot","TreeID")) %>%  # left join to filter for trees with decreasing DBH
  mutate_at(vars(contains("DBH")),round,1)

# Export this table
write.csv(DBH_decrease,file="data-raw/EP1162_trees_DBHDecreasing.csv",row.names = FALSE)

  

