## Master

source('r/00_setup_filepaths.R')
source('r/Zdm000_setupdata.R')

## Tables

# Participants characteristics
source("r/an_tab1.R")

## Contact overall by country
source("r/an_tab2.R")

## Contacts by characteristics
source("r/an_tab3.R")

## Contact for other settings by characteristics
source('r/an_tab3_supp.R')

## Figures

# Bar chart by duration
source("r/an_fig1_freq.R")
# Bar chart by Frequency
source("r/an_fig2_time.R")

## Contact matrices

## Creating the contact matrices

##source("r/dm_cms.R") ## This will take a while to run because of bootstraps
source("r/dm_polymod_cms.R")
source("r/an_fig3_cms.R")
