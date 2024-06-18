<!-- badges: start -->
<!-- badges: end -->

## Seafood Watch Forage Fish Identification

This repository includes data and analysis scripts for the white paper:

**Roos, Marjoleine, M.H. (2024). Forage species: Who is key? Monterey Bay Aquarium Seafood Watch.** [ADD URL ONCE AVAILABLE]

---
## Collaborators:
*Marjoleine M.H. Roos, Quantitative Marine Ecologist (LEAD AUTHOR)*

*Santi Roberts, Monterey Bay Aquarium*

*Sam Wilding, Monterey Bay Aquarium*

*Andre Boustany, Monterey Bay Aquarium*

*Rachel Zuercher, Monterey Bay Aquarium*


---
## Description:
This code allows for the identification of key forage species concerning their trophic
linkages within an ecosystem’s food web. This is done by calculating three published indices, including the connectance index and the SUpportive Role to Fishery ecosystems (SURF) index calculated from mass-balanced models, and the SURF index calculated from energy-balanced models. All use food web data from existing static ecosystem food web models.
--- 
## In the repository:
The data/raw folder of .xlsx files is needed to replicate these analyses.

The folder contains two types of .xlsx files:
`diet.xlsx` -- diet matrix data file with all species used in the project

`groupdata.xlsx` -- data file containing all species groups used in the project (rows) 
and the following information for each: B, QB, ED, ED source, group name and group code (columns)

Two .R scripts are needed to replicate these analyses. 
`index_calculations.R` -- R code to calculate connectance, SURF mass and SURF energy indices
`group.aggregating.fxn.R` -- R code to create function to aggregate groups in diet matrices to calculate keyness at a coarser taxonomic resolution

Instructions:
1. Run the `group.aggregating.fxn.R` script to define a necessary function
2. Run the `index_calculations.R` to perform all calculations and output data files.

This will perform calculations for every geography represented by .xlsx data files 
in the data/raw folder. If you are interested in performing calculations for only
one geography, the code can be edited by defining your geography of interest as a
numerical value (geog=# instead of code line 35) and running the code without the
outermost for loop. 

