
# get path directory path.base, e.g. /Users/Chaehan or /home/chaehan
get_path_base <- function()
{
  prefix <- gsub("^(/.*/.*?)/.*", "\\1", getwd())
  if (prefix == "/Users/Chaehan" | prefix == "/private/var")
  {
    output <- "/Users/Chaehan/06 Data Analysis"
  } else {
    output <- prefix
  }
  return(output)
}

path.base <- get_path_base()

# set directories
credentialsDir <- paste0(path.base, "/03 ML Scripts/_credentials")
inputML <- paste0(path.base, "/03 ML Scripts/_data/")
outputML <- paste0(path.base, "/03 ML Scripts/_output/")
inputDV <- paste0(path.base, "/03 Datavis Scripts/_input")
outputDV <- paste0(path.base, "/03 Datavis Scripts/_output")

dirs <- list(credentialsDir,
             inputML, outputML,
             inputDV, outputDV,
             inputTP, outputTP)

lapply(dirs, function(dir) assign(toString(dir), dir, envir = .GlobalEnv))

