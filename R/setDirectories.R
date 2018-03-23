
# get path directory path.base, e.g. /Users/Chaehan or /home/chaehan
get_path_base <- function()
{
  prefix <- gsub("^(/.*/.*?)/.*", "\\1", getwd())
  if (prefix == "/Users/Chaehan" | prefix == "/private/var")
  {
    output <- "/Users/Chaehan"
  } else {
    output <- "/home/chaehan"
  }
  return(output)
}

path.base <- get_path_base()

# set directories
credentialsDir <- paste0(path.base, "/Google Drive/06 Data Analysis/06 Machine Learning/06 ML Scripts/_credentials")
inputML <- paste0(path.base, "/Google Drive/06 Data Analysis/06 Machine Learning/06 ML Scripts/_data/")
outputML <- paste0(path.base, "/Google Drive/06 Data Analysis/06 Machine Learning/06 ML Scripts/_output/")
inputDV <- paste0(path.base, "/Users/Chaehan/Google Drive/06 Data Analysis/06 Data Visualization/_input")
outputDV <- paste0(path.base, "/Users/Chaehan/Google Drive/06 Data Analysis/06 Data Visualization/_output")

dirs <- list(credentialsDir,
             inputML, outputML,
             inputDV, outputDV,
             inputTP, outputTP)

lapply(dirs, function(dir) assign(toString(dir), dir, envir = .GlobalEnv))

