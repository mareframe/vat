# vat
###Visualizing Atlantis Tool [![Build Status](https://travis-ci.org/cddesja/vat.svg)](https://travis-ci.org/cddesja/vat)

`vat` is an interactive R package used with the [Atlantis](http://atlantis.cmar.csiro.au/) ecosystem model. 

An example of `vat` from the Icelandic Atlantis model can be seen [here](http://130.208.71.121:3838/vat). Note, that the model in that example is a moving Atlantis target.

To install: 

```R
library(devtools)
install_github(repo = "mareframe/vat")
```

To run the interactive [Shiny](http://shiny.rstudio.com/) application for `vat`, named `vat`. You need to running the following sequence of functions.

1. `create_vat()`. This creates an object of class `vat` containing all the data needed for `vat()`.

2. `vat_animate()`. This function creates an animated plot necessary for `vat()`.

3. `vat()`. This is the function that launches the actual shiny application.

Please see each functions respective manpage for details on its usage. 

At present `vat` is a moving target. I welcome all feedback on it's UI as well as any additional features, you would like to see added. Please report issues and feature requests. Once I am happy with `vat`'s UI, I will freeze this branch and take all new development to a different branch. 

Please note if you if you are interested in running `vat` in the manner shown in the example (i.e. on a locally hosted web server), you must install [shiny-server](https://www.rstudio.com/products/shiny/shiny-server/) and you need to modify `vat`. If there is interest in this from other Atlantis modelers, please open an issue and I will start maintaining a seperate repository for that code.

Atlantis users may also be interested in the [rlantis](http://github.com/mareframe/rlantis) package. This package contains helper files for Atlantis as well as creating static graphs. 
