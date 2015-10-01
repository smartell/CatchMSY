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
    
One devtools is installed you can install the catchMSY package directly from github:

    devtools::install_github("smartell/catchMSY",build_vignettes=TRUE)



## Vignettes
You can build all vignettes from the console with devtools::build() to create a package bundle with the vignettes included. RStudio’s “Build & reload” does not build vignettes to save time. Similarly, devtools::install_github() (and friends) will not build vignettes by default because they’re time consuming and may require additional packages. You can force building with devtools::install_github(build_vignettes = TRUE). This will also install all suggested packages.
