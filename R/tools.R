# functions
#' Loads packages and installs those that are not in the library
#' @param  vector of package names
#' @export

using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))
    need<-libs[req==FALSE]
    if(length(need)>0){ 
        install.packages(need)
        lapply(need,require,character.only=TRUE)
    }
}


  packages = c('anytime','arm','data.table', 'effects', 'foreach', 'ggplot2', 'ggthemes', 'glue',  'grid', 'here', 'htmlTable', 'lattice', 'lubridate', 'magrittr', 'multcomp', 'plyr','raster','stringr','zoo')
  sapply(packages, function(x) suppressPackageStartupMessages(using(x)) )

