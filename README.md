
# Code for the analyses of the last CoMix data.
Results from this analysis are presented in the manuscript "Social contact patterns following the COVID-19 pandemic: a snapshot of post-pandemic behaviour from the CoMix study"  [available on MedrXiv](https://www.medrxiv.org/content/10.1101/2023.08.29.23294767v1).

# Repository organization
The repository is organized in the following folders:
- data: Folder that contains the datasets. 
- r: Folder with the main code.
- stan: Folder with Stan related functions.
- output: Folder where the output of the analysis (tables and figures of the manuscript) is saved.

# Data
The data analysed in the manuscript is available in the folder `data/zenodo`, together with the corresponding dictionary (`CoMix_last_wave_dictionary.xlsx`). The dataset is also available independently on Zenodo [at the following link](https://doi.org/10.5281/zenodo.11154066).
The data is formatted according to the standard of the [socialcontactdata.org initiative](https://socialcontactdata.org/data/) and is therefore interoperable with the [socialmixr R package](https://cran.r-project.org/web/packages/socialmixr/index.html).
The participant information is reported (one participant per line) in the files `participants_common` and `participants_extra`, while the contacts information are reported (one contact per line) in the `contacts_common` and `contacts_extra` files.

 

# Analysis
To perform the analysis:
- Update the "r/00_setup_filepaths.R" file to include the working path of the local machine.
- Run the "r/master.R" file.
The code relies on specific implementa


# Output

The code generates as output the three tables of the main paper:
- Table 1 ("tab1_characteristics.docx"): Participants characteristics. The code generates a table of the participants features, listing both absolute numbers and percentages. To compute correctly the percentages the custom funcion `get_perc` is used.
- Table 2 ("tab2_contacts.docx"): Mean daily contacts per participant by country and setting, together with bootstrap confidence intervals.
- Table 3 ("tab3_contacts.docx"): Mean daily contacts per participant by characteristics, together with bootstrap confidence intervals.

And figure 1,2 and 3 of the main paper. In particular, the code to plot the contact matrices can be found in `an_fig3_cms.R` and could be adapted to explore the data.

We refer to the manuscript for the interpretation of the results.


# Required packages:
- data.table
- lubridate
- flextable
- magrittr
- dplyr
- socialmixr
- ggplot2
- patchwork
- forcats
- gtsummary
- boot
- wpp2022
- viridis
- cowplot
- tidyverse
- stringr
- readxl
- reshape2

