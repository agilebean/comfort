################################################################################
#
# Survey Functions
#
################################################################################

######################################################################
# Function convert_numeric()
# IN:   userdata (dataframe) containing factors
# OUT:  output (dataframe) factors transformed into numerical
######################################################################
# convert dataframe with factors into dataframe with numeric values
convert_numeric <- function(userdata)
{
  numeric_matrix <- lapply(userdata, function(x) if(is.factor(x)) { # lapply not sapply (returns 1 col)!!!
    as.numeric(as.character(x)) } else if (is.character(x))
    { as.numeric(x) }
    else {x})
  numeric_dataframe <- data.frame(numeric_matrix)
  return(numeric_dataframe)
}

######################################################################
# Function determine_factor_extraction_no()
# IN:   items_df (dataframe)
# OUT:  output (text)
#
######################################################################
determine_factor_extraction_no <- function(items_df)
{
  # set max.#cores by parallel::detectCores()
  options(mc.cores=4)
  # getOption("mc.cores")
  # Parallel analysis
  parallel <- fa.parallel(items_df)
  # Wayne Velicer's VSS+MAP criterion for number of factor extraction
  vss <- VSS(items_df)
  summary(vss)
}

######################################################################
# Function do_factor_analysis()
# IN:   items_input (dataframe), n_factors(integer), factor_method(string),
#       corr_type(string), cut_off(dbl): below factors loadings suppressed
# OUT:  output (dataframe)
######################################################################
do_factor_analysis <- function(items_input, n_factors, factor_method,
                               corr_type, cut_off=NULL)
{
  items <- na.omit(items_input)
  items <- data.matrix(items)
  # PCA from psych package - works with small sample size!
  if (factor_method=="pca")
  {
    corr.matrix <- cor(items,items)
    # psych::principal for Principal Components Analysis
    output <- principal(corr.matrix,rotate="oblimin", nfactors=n_factors)
  }
  else # psych:fa for Factor Analysis
  {
    output <- fa(items, rotate="oblimin", use="pairwise",
                 fm=factor_method, nfactors=n_factors, cor=corr_type)
  }
  # print (blank) for values within cutoff range

  cutoff <- ifelse(cut_off, cut_off, 0)
  output$weights %<>% # updates input after feeding into pipe
    replace(., .>-cutoff & .<cutoff, NA) %>%
    round(., digits = 2) %>% print(., na.print="")

  return(output)
}

######################################################################
# Function iterate_factor_analysis()
# IN:   items (dataframe), max_factors(integer), factor_methods(string)
# OUT:  result (dataframe)
######################################################################
iterate_factor_analysis <- function(items, max_factors, factor_methods)
{
  result <- NULL

  for (no_factors in 1:max_factors)
  {
    for (method in factor_methods)
    {
      # rotate on correlation matrix
      fa <- do_factor_analysis(items, no_factors, method, "cor")
      fa.cor <- c(no_factors, method, fa$rms, fa$fit.off, "cor")
      # rotate on covariance matrix
      fa <- do_factor_analysis(items, no_factors, method, "cov")
      fa.cov <- c(no_factors, method, fa$rms, fa$fit.off, "cov")
      result <- rbind(result, fa.cov, fa.cor)
    }
  }
  # dataframe requires non-duplicate row
  row.names(result) <- NULL
  result <- as.data.frame(result)
  # round() needs numeric input!
  result[,3:4] <- round(convert_numeric(result[,3:4]), digits = 2)
  colnames(result) <- c("factor method", "no_factors", "RMSR", "Fit.off", "matrix")
  return(result)
}


######################################################################
# Function get_alpha()
# IN:   data (dataframe)
# OUT:  cronbach alpha & r.drop corrected by smc (list)
######################################################################
get_alpha <- function(data)
{
  # cronbach alpha short form
  data <- data.matrix(data)
  # alpha function was overriden, must be called from package!
  alpha_list <- psych::alpha(data, na.rm=TRUE)
  cronbachAlpha <- alpha_list$total$std.alpha
  # extract alpha stats: r.drop
  trennsch.1 <- alpha_list$item.stats["r.drop"]
  trennsch.2 <- alpha_list$item.stats["r.cor"]
  output <- list(cronbachAlpha = cronbachAlpha, r.cor = trennsch.2)
  # Remember: assign to output variable!
  output <- lapply(output, round, digits=2)
  return(output)
}

################################################################################
#
# Scale Labels
# Todo: extend by new labels
#
################################################################################
scale.effective.labels <- c("Not effective at all","Slightly effective","Moderately effective","Highly effective","Extremely effective")
# scale.extent.labels <- c("Not at all","Slightly","Moderately","Highly","Extremely")
scale.extent.labels <- c("not at all","a little","moderately","a lot","extremely")
scale.agreement.labels <- c("Disagree strongly", "Disagree a little", "Neither disagree nor agree", "Agree a little", "Agree strongly")
scale.likeability.labels <- c("Not at all","Not very much ","Neutral","Very much","Extremely")
scale.lastwatched.labels <- c("More than 2 years ago","Between 1 - 2 years", "Between 6 months - 1 year", "In the last 6 months")
scale.numberwatched.labels <- c("0","1-2","3-4","5-6","More than 6")

quartzFonts(gillsans = c("Gill Sans Light", "Gill Sans Light", "Gill Sans Italic", "Gill Sans Bold Italic"))

################################################################################


######################################################################
# Function encode_scale_labels()
# replace scale responses containing label names with their scale codes
#
# IN:   matrix (matrix), label_names(vector(char))
# OUT:  cronbach alpha & r.drop corrected by smc (list)
#
######################################################################
encode_scale_labels <- function(matrix, label_names)
{
  label_no <- length(label_names)
  scale.codes <- 1:label_no
  matrix[matrix==""] <- NA
  for (index in scale.codes)
  {
    matrix[matrix==label_names[index]] <- scale.codes[index]
  }
  return(matrix)
}

# create mean scores of a Latent Variable
get_mean_score <- function(data)
{
  return(rowMeans(data.matrix(data), na.rm = TRUE))
}


######################################################################
# Function remove_duplicates()
# IN:   file (google sheet)
# OUT:  non_duplicates (dataframe)
######################################################################
remove_duplicates <- function(file)
{
  require(dplyr)
  # remove timestamp (first column) & duplicates
  non_duplicates <- file[-1] %>% file[!duplicated(file),]
  return(non_duplicates)
}

######################################################################
# Function import_file_google()
# IN:   file_name (google sheet)
# OUT:  file (dataframe)
######################################################################
import_file_google <- function(file_name)
{
  # gs_file.dd2() %>% gs_copy(to = filename.dd2)
  file.identifier <- gs_title(file_name)
  ws.identifier <- gs_ws_ls(file.identifier)
  # file.dd2 <- file.identifier %>% gs_read(ws = ws.identifier)
  file <- file.identifier %>% gs_read_csv(ws = ws.identifier)
  # remove duplicates
  output <- remove_duplicates(file)
  return(output)
}

today <- paste(format(Sys.time(), "%Y-%m-%d"))

######################################################################
# Function create_email_list()
# IN:   email_df (dataframe)
# OUT:  result (dataframe)
######################################################################
create_email_list <- function(email_df)
{
  result <- NULL
  for (item in email_df)
  {
    # print(as.character(item))
    result <-paste0(result, item, sep="; ")
  }
  result <- gsub("(.*)(, )", "\\1", result)
  return(result)
}

