# vat
###Visualizing Atlantis Tool [![Build Status](https://travis-ci.org/cddesja/vat.svg)](https://travis-ci.org/cddesja/vat)

`vat` is an interactive R package used with the [Atlantis](http://atlantis.cmar.csiro.au/) ecosystem model. 

An older example of `vat` using data from the Iceland Atlantis model can be seen [here](http://130.208.71.121:3838/vat)

To install: 

```R
library(devtools)
install_github(repo = "cddesja/vat")
```

To run the interactive [Shiny](http://shiny.rstudio.com/) application for `vat`, named `vat`. You need to running the following sequence of functions.

1. `create_vat()`. This creates an object of class `vat` containing all the data needed for `vat()`.

2. `vat_animate()`. This function creates an animated plot necessary for `vat()`.

3. `vat()`. This is the function that launches the actual shiny application.

Please see each functions respective manpage for details on its usage. 

At present `vat` is a moving target. I welcome all feedback on it's UI as well as any additional features, you would like to see added. Please report issues and feature requests. Once I am happy with `vat`'s UI, I will freeze this branch and take all new development to a different branch. 

Atlantis users may also be interested in my [rlantis](http://github.com/cddesja/rlantis) package. This package contains helper files for Atlantis as well as creating static graphs.
