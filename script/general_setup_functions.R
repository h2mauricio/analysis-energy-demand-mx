UsePackage <- function(p) {
  # Installs the package if it hasn't already been installed.
  # Includes the library into the session, if the package has been installed.
  #
  # Args:
  #   p: package name
  #   installed.packages(): The other vector. x and y must have the same length, greater 
  #                         than one, with no missing values.
  # 
  # Returns:
  #   N/A
  if (!is.element(p, installed.packages()[,1])){
    install.packages(p, dep = TRUE)
  }
  require(p, character.only = TRUE)
}

##Load and attach add-on packages
UsePackage('stringr')
UsePackage('knitr')
UsePackage('readr')
UsePackage('RColorBrewer')
UsePackage('plotly')
UsePackage('broman')       # rounding values, kbroman.org/knitr_knutshell/pages/Rmarkdown.html
UsePackage('survey')       # survey design
UsePackage("devtools")
UsePackage('kableExtra')   # tables 
UsePackage('stringr')      # twrappers over stringi package, replace matched patterns 
UsePackage('DT')           # create HTML table widgets using the DataTables library
UsePackage('summarytools') # DF summaries, cross-tabulations, weight-enabled freq. tables
UsePackage('data.table')
UsePackage('captioner')    # Handling figures, tables, and equations numbering
UsePackage('reticulate')   # Set of tools for interoperability between Python and R
UsePackage("magick")
UsePackage("webshot")
UsePackage("srvyr")
UsePackage("foreign")      #read INEGI tables with different formats
UsePackage("doBy")         #to order tables based on different variables
UsePackage("splitTools")
UsePackage("ranger")

#webshot::install_phantomjs()


Sys.setenv("plotly_username" = "mauricioh2")
Sys.setenv("plotly_api_key" = "CzXpBQKn6zRQj1ExEd5O")

table_captions <- captioner::captioner(prefix="Table")
figure_captions <- captioner::captioner(prefix="Fig.")

t.ref <- function(label){
  stringr::str_extract(table_captions(label), "[^:]*")
}

f.ref <- function(label){
  stringr::str_extract(figure_captions(label), "[^:]*")
}