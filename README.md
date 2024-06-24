<!-- badges: start -->
<!-- badges: end -->

## Seafood Watch Forage Fish Identification

This repository includes data and analysis scripts for the white paper:

**Roos, Marjoleine, M.H. (2024). Forage species: Who is key? Monterey Bay Aquarium Seafood Watch.** [ADD URL ONCE AVAILABLE]

---
## Code authors:
*Marjoleine M.H. Roos, Quantitative Marine Ecologist (LEAD AUTHOR, marjoleineroos@gmail.com)*\
*Tim E. Essington, University of Washington*

### Collaborators:
*Santi Roberts, Monterey Bay Aquarium*\
*Sam Wilding, Monterey Bay Aquarium*\
*Andre Boustany, Monterey Bay Aquarium*\
*Rachel Zuercher, Monterey Bay Aquarium*


---
## Description:
This code allows for the identification of key forage species concerning their trophic linkages within an ecosystem’s food web. This is done by calculating three published indices, including the connectance index and the SUpportive Role to Fishery ecosystems (SURF) index calculated from mass-balanced models, and the SURF index calculated from energy-balanced models. All use food web data from existing static ecosystem food web models.

**Connectance index calculations based on:** *Smith, A. D., C. J. Brown, C. M. Bulman, E. A. Fulton, P. Johnson, I. C. Kaplan, H. Lozano-Montes, S. Mackinson, M. Marzloff, and L. J. Shannon. 2011. Impacts of fishing low–trophic level species on marine ecosystems. Science 333(6046):1147-1150*

**Mass SURF index calculations based on:** *Plagányi, É. E., and T. E. Essington. 2014. When the SURFs up, forage fish are key. Fisheries Research 159:68-74.*

**Energy SURF index calculations based on:** *Surma, S., E. A. Pakhomov, and T. J. Pitcher. 2022. Pacific herring (Clupea pallasii) as a key forage fish in the southeastern Gulf of Alaska. Deep Sea Research Part II: Topical Studies in Oceanography 196:105001.*

## In the repository:
The data/raw folder of .xlsx files is needed to replicate these analyses. It contains two types of .xlsx files:

`diet.xlsx` -- diet matrix data files for each geography with all species included in the project

`groupdata.xlsx` -- data files for each geography containing all species groups used in the project (rows) 
and the following information for each: B, QB, ED, ED source, group name and group code (columns)

One .R script is needed to replicate these analyses. 
`index_calculations.R` -- R code to calculate connectance, SURF mass and SURF energy indices

## Instructions:
1. Run the `index_calculations.R` to perform all calculations and output data files.

This will perform calculations for every geography represented by .xlsx data files  in the data/raw folder. If you are interested in performing calculations for only one geography, the code can be edited by defining your geography of interest as a numerical value (geog=# instead of code line 34) and running the code without the outermost for loop. 

