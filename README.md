Optimistic Semi-supervised Least Squares Classification
===============

Authors: Jesse H. Krijthe and Marco Loog

This repository contains the code to generate/reproduce the paper "Optimistic Semi-supervised Least Squares Classification".

# Abstract

The goal of semi-supervised learning is to improve supervised classifiers by using additional unlabeled training examples. In this work we study a simple self-learning approach to semi-supervised learning applied to the least squares classifier. We show that a soft-label and a hard-label variant of self-learning can be derived by applying block coordinate descent to two related but slightly different objective functions. The resulting soft-label approach is related to an idea about dealing with missing data that dates back to the 1930s. We show that the soft-label variant typically outperforms the hard-label variant on benchmark datasets and partially explain this behaviour by studying the relative difficulty of finding good local minima for the corresponding objective functions.

# How to use this code

## Installing the dependencies
Aside from R, some dependencies require a correctly configured C++ compiler to be set up. To generate the paper, a working Latex installation is required. On Windows, you may be prompted to install Rtools. On macOS, you may need to additionally install the Fortran compiler from http://r.research.att.com/tools/. On Ubuntu, you may need:
```{bash}
sudo apt-get -y install libssh2-1-dev libssl-dev libcurl4-gnutls-dev texlive-latex-base texlive-fonts-recommended texlive-latex-extra texlive-publishers
```

----
After installing R, install the required packages using:
```{r}
install.packages(c("devtools","ggthemes","magrittr","dplyr",
"tidyr","cowplot","randomForest","knitr","R.matlab"))

devtools::install_github("jkrijthe/createdatasets",
              ref="55929afcc0f966f4e49bcf03685e04803d19353b")
devtools::install_github("jkrijthe/RSSL",
              ref="415c91565f310da30bb8a10b178efacd2c26473e")
```

## Running the experiments (Optional)
To rerun the experiments, from the command line, run the following commands:
```{bash}
Rscript R/attraction1d.R
Rscript R/localoptima.R
Rscript R/learningcurves-enough.R
```
Note that the last two, but especially the last one will take a long time to run.

There are some additional experiments that you could run that were not in the original paper:
```{bash}
Rscript R/attraction.R
Rscript R/initializations.R
Rscript R/learningcurves-fraction.R
```

## Generating the paper
The easiest way to generate the paper is by using knitr in RStudio to rebuild the optimisticssl.Rnw file.

Without Rstudio, you can regenerate the .tex file and figures from the command line using:
```{bash}
Rscript -e 'knitr::knit("optimisticssl.Rnw")'
```
And build the PDF using:
```{bash}
pdflatex optimisticssl
bibtex optimisticssl
pdflatex optimisticssl
pdflatex optimisticssl
```

To reproduce the reproducibility report, change optimisticssl to reproducing-optimisticssl.