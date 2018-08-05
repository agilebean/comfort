
# get path directory path.base, e.g. /Users/Chaehan or /home/chaehan
get_path_base <- function()
{
  prefix <- gsub("^(/.*/.*/.*?)/.*", "\\1", getwd())

  if (!(prefix == "/home/rstudio"))
  {
      prefix <- "/Users/Chaehan/Google Drive"
  }

  return(prefix)
}

path.base <- get_path_base()

# set directories
credentialsDir <- paste0(path.base, "/03 Data Analysis/03 ML Scripts/_credentials/")
inputML <- paste0(path.base, "/03 Data Analysis/03 ML Scripts/_input/")
outputML <- paste0(path.base, "/03 Data Analysis/03 ML Scripts/_output/")

inputEEG <- paste0(path.base, "/03 Data Analysis/03 EEG Scripts/_input/")
outputEEG <- paste0(path.base, "/03 Data Analysis/03 EEG Scripts/_output/")

inputDV <- paste0(path.base, "/03 Data Analysis/03 Datavis Scripts/_input/")
outputDV <- paste0(path.base, "/03 Data Analysis/03 Datavis Scripts/_output/")

inputTimex <- paste0(path.base, "/04 Publishing/21 Timex/_input/")
outputTimex <- paste0(path.base, "/04 Publishing/21 Timex/_output/")

inputSF <- paste0(path.base, "/04 Publishing/23 Smartphone Flow/_input/")
outputSF <- paste0(path.base, "/04 Publishing/23 Smartphone Flow/_output/")

dirs <- list(credentialsDir,
             inputML, outputML,
             inputEEG, outputEEG,
             inputDV, outputDV,
             inputTimex, outputTimex,
             inputSF, outputSF
             )

lapply(dirs, function(dir) assign(toString(dir), dir, envir = .GlobalEnv))
