
# Code for the analyses of the last CoMix data.
Results from this analysis are presented in the manuscript "Social contact patterns following the COVID-19 pandemic: a snapshot of post-pandemic behaviour from the CoMix study"  [available on MedrXiv](https://www.medrxiv.org/content/10.1101/2023.08.29.23294767v1).
# Repository organization
The repository is organized in the following folders:
- data: Folder that contains the datasets.
- r: Folder with the R code, including a "functions" subfolder that contains the necessary functions.
- stan: Folder with Stan related functions.
- output: Folder where the output of the analysis (tables and figures of the manuscript) is saved.

# Analysis
To perform the analysis:
- Update the "r/00_setup_filepaths.R" file to include the working path of the local machine.
- Run the "r/master.R" file.

Required packages:
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

# Output

The code generates as output the three tables of the main paper:
- Table 1 ("tab1_characteristics.docx"): Participants characteristics.
- Table 2 ("tab2_contacts.docx"): Mean daily contacts per participant by country and setting.
- Table 3 ("tab3_contacts.docx"): Mean daily contacts per participant by characteristics.
And figure 1,2 and 3 of the main paper.

We refer to the manuscript for a more detailed interpretation of the results.
