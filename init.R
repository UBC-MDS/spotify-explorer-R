# R script to run author supplied code, typically used to install additional R packages
# contains placeholders which are inserted by the compile script
# NOTE: this script is executed in the chroot context; check paths!

r <- getOption('repos')
r['CRAN'] <- 'http://cloud.r-project.org'
options(repos=r)

# ======================================================================

# packages go here
install.packages(c('dash', 'plotly', 'readr', 'here', 'ggthemes', 'purrr', 'dplyr', 'remotes', 'dashBootstrapComponents', 'dashHtmlComponents', 'dashCoreComponents', 'ggplot2'))
remotes::install_github('facultyai/dash-bootstrap-components@r-release')
