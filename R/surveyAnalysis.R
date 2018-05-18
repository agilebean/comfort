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
  require(dplyr)
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
scale.likeability.labels <- c("Not at all","Slightly","Moderately","Highly","Extremely")
scale.animationlike.labels <- c("Not at all","Not very much","Neutral","Very much","Extremely")
scale.lastwatched.labels <- c("More than 2 years ago",
                              "Between 1 to 2 years",
                              "Between 6 months to 1 year",
                              "In the last 6 months")
scale.numberwatched.labels <- c("0","1 to 2","3 to 4","5 to 6","More than 6")

quartzFonts(gillsans = c("Gill Sans Light", "Gill Sans Light", "Gill Sans Italic", "Gill Sans Bold Italic"))

################################################################################


######################################################################
# Function encode_scale_labels()
# replace scale responses containing label names with their scale codes
#
# IN:   scale_data_frame (dataframe), scale_labels(vector(char))
# OUT:  encoded_df (dataframe)
#
######################################################################
encode_scale_labels <- function (scale_data_frame, scale_labels)
{
  matrix <- as.matrix(scale_data_frame)
  scale.codes <- 1:length(scale_labels)
  matrix[matrix == ""] <- NA

  for (index in scale.codes)
  {
    matrix[matrix == scale_labels[index]] <- scale.codes[index]
  }

  # retain the colum names in list
  encoded_df <- as.data.frame(matrix)
  return(encoded_df)
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
  require(dplyr)
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

######################################################################
# Function remove_descriptive_columns()
# IN:   descriptive_columns_matrix (matrix)
#       survey_raw (dataframe)
# OUT:  out (dataframe)
######################################################################
remove_descriptive_columns <- function(descriptive_columns_matrix, survey_raw)
{
  # set label and name for descriptive.columns
  descriptive.columns <- descriptive_columns_matrix %>%
    as.data.frame %>%
    setNames(c("label","name"))

  # extract descriptive columns (by label) from survey data
  survey.descriptive <- survey_raw[, names(survey_raw) %in% descriptive.columns$label]

  # set item names (by name) for descriptive survey
  survey.descriptive %<>% setNames(descriptive.columns$name)

  # create output object otherwise
  output <- NULL

  output$descriptive <- survey.descriptive

  survey.data  <- survey_raw[, -which(names(survey_raw) %in% descriptive.columns$label)]

  output$survey <- survey.data

  return(output)
}

######################################################################
# Function encode_survey_and_scales()
# IN:   survey_data (dataframe)
#       LV_labels (list)
#       LV_scale_list (list)
# OUT:  out (dataframe)
######################################################################
encode_survey_and_scales <- function(survey_data, LV_labels, LV_scale_list)
{
  ## Create list of item names for each LV
  # 1. LV.item.list: assign item names to latent variable labels
  LV.item.list <- lapply(LV_labels, function(name) assign(name, eval(parse(text = name))))
  names(LV.item.list) <- LV_labels

  # 2. survey.data: set column names from LV.item.list
  colnames(survey_data) <- LV.item.list %>% unlist %>% as.vector()

  # 3. LV.data.list: extract survey.data columns by item names
  LV.data.list <- lapply(LV.item.list, function(items) survey_data[items] )

  # 4. Encoding: apply scale for each LV in LV.data.list
  survey.factor.list <- mapply(encode_scale_labels, LV.data.list, LV.scale.list)

  survey.numeric.list <- lapply(survey.factor.list, convert_numeric)

  survey.numeric <- survey.numeric.list %>%
    # remove the scale name from item name
    setNames(NULL) %>%  as.data.frame

  output <- NULL
  output$survey <- survey.numeric

  # create scales
  scales <- lapply(survey.numeric.list, rowMeans) %>% as.data.frame()

  output$scales <- scales

  # scales <- scales %>% dplyr::select(-Attention1, -Attention2)

  return(output)
}
