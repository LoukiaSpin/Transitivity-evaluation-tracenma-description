# A database to initiate methodological advances in the evaluation of transitivity assumption in network meta-analysis: qualitative features and limitations of tracenma R package

## Description of the repository

The repository offers a folder for R scripts to replicate the main and supplementary Figures and a folder to save the generated Figures.
* The _R_ folder includes 9 scripts to replicate the main and supplementary Figures. 
* The _Figure_ folder aims to contain the Figures generated from the R scripts.

The data supporting the findings can be found in the [tracenma R package](https://CRAN.R-project.org/package=tracenma). 

After downloading/cloning the repo, the user can use the .Rproj file to source all code.

## Output 

Prerequisite R packages: [dplyr](https://CRAN.R-project.org/package=dplyr), 
[ggplot2]( https://CRAN.R-project.org/package=ggplot2),
[ggpubr](https://cran.r-project.org/web/packages/ggpubr/),
[plyr](https://CRAN.R-project.org/package=plyr),
[tracenma](https://CRAN.R-project.org/package=tracenma)

## Important note

There have been corrections to the characteristic types and subtypes of few datasets since the first submission of the package. Therefore, you should use the development version of [tracenma](https://CRAN.R-project.org/package=tracenma) to reproduce the results of the submitted article:

    install.packages("devtools")
    devtools::install_github("LoukiaSpin/tracenma")
    library(tracenma)
