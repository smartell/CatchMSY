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

    devtools::install_github("smartell/catchMSY")