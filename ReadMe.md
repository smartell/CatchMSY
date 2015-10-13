## Installing the catchMSY package from github.

First you need to install the devtools package

__Mac and Linux:__

    install.packages("devtools")
    
__Window:__

    library(devtools)
    build_github_devtools()
    
    #### Restart R before continuing ####
    install.packages("devtools.zip", repos = NULL, type = "source")
    
    # Remove the package after installation
    unlink("devtools.zip")
    
Once devtools is installed you can install the catchMSY package directly from github:

    devtools::install_github("smartell/catchMSY",build_vignettes=TRUE)



## Vignettes
You can build all vignettes from the console with devtools::build() to create a package bundle with the vignettes included. RStudio’s “Build & reload” does not build vignettes to save time. Similarly, devtools::install_github() (and friends) will not build vignettes by default because they’re time consuming and may require additional packages. You can force building with devtools::install_github(build_vignettes = TRUE). This will also install all suggested packages.

You can also build the vignettes using "devtools::build_vignettes()" in R.
---
## Workflow for building the catchMSY package

### Using the makefile
From the terminal navigate to the directory containg the catchMSY pacakge.

	make clean
	make 
	make install
	



Using the devtools package for building the catchMSY package.

	devtools::load_all()

----
### To do
- [ ] fix non-linear search routine
- [ ] Create S3 ploting routines for sID class
    - [ ] biomass plot
    - [ ] fishing mortality rate plot
    - [ ] depletion
    - [ ] pairs plot for posterior samples
- [ ] Summary function for sid class.
    - [ ] non-linear search summary.
    - [ ] importance sample summary.
- [ ] Add nll for average weight.
- [ ] add nll for change in mean length.
- [ ] add the importance function for statistical SIR
- [ ] life-hisotry: specifically sequential hermaphrodites (protogynous Scarids–Parrot fish)

----

