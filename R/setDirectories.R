
# get path directory get_prefix(), e.g. /Users/Chaehan or /home/chaehan
get_prefix <- function()
{
  prefix <- gsub("^(/.*/.*?)/.*", "\\1", getwd())
  return(prefix)
}

# set directories
credentialsDir <- paste0(get_prefix(), "/Dropbox/06 Machine Learning/06 ML Scripts/_credentials")
inputML <- paste0(get_prefix(), "/Dropbox/06 Machine Learning/06 ML Scripts/_data/")
outputML <- paste0(get_prefix(), "/Dropbox/06 Machine Learning/06 ML Scripts/_output/")
inputDV <- paste0(get_prefix(), "/Google Drive/01 Lectures/WS2017 IDAS Lectures/WS17-02 Data Visualization/Datavis Exercises/_input")
outputDV <- paste0(get_prefix(), "/Google Drive/01 Lectures/WS2017 IDAS Lectures/WS17-02 Data Visualization/Datavis Exercises/_output")
inputTP <- paste0(get_prefix(), "/Google Drive/03 Publishing/21 Time Pressure/_input")
outputTP <- paste0(get_prefix(), "/Google Drive/03 Publishing/21 Time Pressure/_output")


dirs <- list(credentialsDir,
             inputML, outputML,
             inputDV, outputDV,
             inputTP, outputTP)

lapply(dirs, function(dir) assign(toString(dir), dir, envir = .GlobalEnv))




