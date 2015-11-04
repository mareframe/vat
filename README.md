# vat
###Visualizing Atlantis Tool [![Build Status](https://travis-ci.org/mareframe/vat.svg)](https://travis-ci.org/mareframe/vat)

`vat` is an interactive R package used with the [Atlantis](http://atlantis.cmar.csiro.au/) ecosystem model. 

An example of `vat` from the Icelandic Atlantis model can be seen [here](http://130.208.71.121:3838/vat). Note, that the Atlantis model in that example is a work-in-progress.

To install: 

```R
library(devtools)
install_github(repo = "mareframe/vat")
```

##### Windows users that are getting errors installing `ncdf4`
Please read the [wiki](https://github.com/mareframe/vat/wiki/Instructions-for-Windows-Users) to install the `vat` R scripts seperately. If you can install the `R ncdf4` package from source (i.e. because you can have installed Visual Studio) then the simple commands shown above should work for you.


#### How to run `vat`
To run the interactive [Shiny](http://shiny.rstudio.com/) application for `vat`, named `vadt` for visualizing Atlantis diagnostic tool. You need to running the following sequence of functions.

1. `create_vadt()`. This creates an object of class `vadt` containing all the data needed for `vadt()`.

2. `animate_vadt()`. This function creates optional animated plots for `vadt()`.

3. `vadt()`. This is the function that launches the actual Shiny application.

Please see each functions respective manpage for details on its usage. For example, `?create_vadt` and `?vadt` in `R`.

At present `vat` is a moving target. We welcome all feedback on it's UI as well as any additional features, you would like to see added. Please report issues and feature requests. Once the `vadt` UI is frozen and when work in the Icelandic Atlantis model moves onto data presentation for stakeholders, then a new function(s) will be added to this package for data presentation.

Please note if you if you are interested in running `vat` in the manner shown in the example (i.e. on a locally hosted web server), you must install [shiny-server](https://www.rstudio.com/products/shiny/shiny-server/) and you need to modify `vat`. If there is interest in this from other Atlantis modelers, please open an issue and I will start maintaining a seperate repository for that code.

Atlantis users may also be interested in the [rlantis](http://github.com/mareframe/rlantis) package. This package contains helper files for Atlantis as well as creating static graphs. 
