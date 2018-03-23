
# get path directory path.base, e.g. /Users/Chaehan or /home/chaehan
get_path_base <- function()
{
  prefix <- gsub("^(/.*/.*/.*?)/.*", "\\1", getwd())

  if (!(prefix == "/home/rstudio"))
  {
    if (prefix == "/private/var/folders")
    {
      prefix <- "/Users/Chaehan/Google Drive"

    } else {

      prefix <- "/home/rstudio"
    }
  }

  prefix <- paste0(prefix, "/03 Data Analysis")
  return(prefix)
}

path.base <- get_path_base()

# set directories
credentialsDir <- paste0(path.base, "/03 ML Scripts/_credentials/")
inputML <- paste0(path.base, "/03 ML Scripts/_input/")
outputML <- paste0(path.base, "/03 ML Scripts/_output/")

inputEEG <- paste0(path.base, "/03 EEG Scripts/_input/")
outputEEG <- paste0(path.base, "/03 EEG Scripts/_output/")

inputDV <- paste0(path.base, "/03 Datavis Scripts/_input/")
outputDV <- paste0(path.base, "/03 Datavis Scripts/_output/")


dirs <- list(credentialsDir,
             inputML, outputML,
             inputEEG, outputEEG,
             inputDV, outputDV
             )

lapply(dirs, function(dir) assign(toString(dir), dir, envir = .GlobalEnv))

