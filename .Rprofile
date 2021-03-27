# .Rprofile -- commands to execute at the beginning of each R session
#
# You can use this file to load packages, set options, etc.
#
# NOTE: changes in this file won't be reflected until after you quit
# and start a new session
#
.First <- function() {
    .libPaths('~/R/library')
    options(blogdown.ext = ".Rmd")
    knitr::opts_chunk$set(fig.retina = 2, # Control using dpi
                          fig.width = 6,  # generated images
                          fig.pos = "t",  # pdf mode
                          fig.align = "center",
                          dpi = if (knitr::is_latex_output()) 72 else 300,
                          out.width = "100%")
}
