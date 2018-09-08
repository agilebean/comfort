################################################################################
#
# PLOT Functions
#
################################################################################

#
# in DD3 Analysis v1.3 ggplot.boxplots.R
#
create_boxplot_mean_gg <- function(data, xlabel, ylabel=NULL, title)
{
  # if data became matrix (e.g. after transpose), then names don't exist
  if (is.null(names(data)))
  {
    data <- as.data.frame(data)
  }
  mean <- mean(colMeans(data))
  colnames(data) <- gsub("\\.", "\\\n", colnames(data)) # replace dot in "pleasantly.surprised"
  ######
  # keeping the column order in data by levels instead of aes(x=ind, y=values)
  # g   <- ggplot(stack(data), aes(x = factor(ind, levels = names(data)), y = values))
  ###
  # Much better for displaying boxplots for all columns in a dataframe:
  # reshape2::melt() transforms dataframe to ggplot input
  # SUPER!
  g   <-  ggplot(data = melt(data), aes(x=variable, y=value)) + theme_bw()
  gg  <-  g + geom_boxplot(aes(fill=variable)) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust=.5, size=32, face="bold", family="gillsans")) +
    xlab(xlabel) +
    theme(axis.title = element_text(size=20, face="bold", family="gillsans"),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y = element_blank()) +
    theme(axis.text = element_text(size = 20, family="gillsans", margin = 40)) +
    theme(legend.position="none") +
    ## color outside aes!!!
    ## size denotes line width
    geom_hline(aes(yintercept=mean), color="khaki3", linetype="dashed", size=1.4) +
    background_grid(major = "xy", minor = "none") +
    # fix scale range 1 to 5
    scale_y_continuous(limits=c(1,5))

  if (!is.null(ylabel)) { gg <- gg + ylab(ylabel)}
  if (nchar(toString(colnames(data))) > 50)
  {
    gg <- gg + theme(axis.text.x = element_text(size=14))
  }

  return(gg)
}


find_jpg_from_id <- function(id, path_list)
{
  require("magick")
  id.index <- unlist(lapply(path_list, function(x) grepl(id, x)))
  id.path <-  unlist(path_list)[id.index]

  par(oma=c(0,0,0,0))
  par(mar=c(0,0,0,0))

  jpg <-  image_read(id.path) # read the file
  return(jpg) # jpg is plot()able
}

plot_raster = function(raster_image)
{
  require("jpeg")
  res <-  dim(raster_image) # get the resolution
  par(oma=c(0,0,0,0))
  # rasterImage requires a basic plot be set up
  plot(0,0,xlim=c(0,res[2]),ylim=c(0,res[1]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  output <- rasterImage(raster_image,0,0,res[2],res[1], interpolate = FALSE)
  print(output) # in for loop, must explicitly print image object, return doesn't work
}

plot_hist_by <- function(id, data, hist_var, by_var)
{
  # Extract the hist_var column from data and calculate mean
  hist_var.col.name   <- paste0(id, ".", hist_var) # e.g. <id>.NPS
  hist_var.col.index  <- grepl(hist_var.col.name, names(data))
  hist_var.col        <- data[, hist_var.col.index, drop=FALSE]
  hist_var.mean       <- colMeans(hist_var.col)

  # get the factor variable that explains hist_var
  fill.factor <- paste0(id, ".", by_var)
  # print(paste(hist_var.col.name, "explained by ", fill.factor))

  g <- ggplot(data, aes(x = factor(get(hist_var.col.name)),
                        fill=factor(get(fill.factor)))) +
    ggtitle(hist_var) +
    theme_bw() +
    theme(plot.title = element_text(hjust=.5, size=32, face="bold", family="gillsans")) +
    geom_bar(color="black") +
    xlab(id) +
    guides(fill=guide_legend(title=by_var)) +
    theme(axis.title = element_text(size=20, face="bold", family="gillsans"),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
          axis.title.y = element_blank()) +
    theme(axis.text = element_text(size = 20, family="gillsans", margin = 40)) +
    theme(legend.position = c(1, 0.5)) +
    scale_x_discrete(breaks = 1:10, limits = 1:10) + # for factor: discrete. limits: EACH value!
    scale_y_continuous(limits = c(0,5)) +
    background_grid(major = "xy", minor = "none") +
    geom_vline(aes(xintercept=hist_var.mean), color="khaki3", linetype="dashed", size=1.4)

  # in a for loop, ggplot must explicitly be printed insted of returned!!!
  return(g)
}

#####
# dendogram
plot_dendrogram <- function(data, mode)
{
  require(dplyr)
  require(dendextend)
  dend <- data %>% dist %>% hclust %>% as.dendrogram %>%
    set("branches_k_color", k=3) %>% set("branches_lwd", c(5,2,1.5)) %>%
    set("branches_lty", c(1,1,3,1,1,2)) %>%
    set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
    set("nodes_pch", 19) %>% set("nodes_col", c("orange", "black", "plum", NA))

  if(mode == "vertical1")
  {
    par("mar" = c(6,0,0,0))
    plot(dend, cex.lab = 2)
  } else if (mode == "horizontal1") {
    par("mar" = c(6,0,0,0))
    require(lattice)
    require(grid)
    grid.newpage()
    viewport(angle = 90) %>% pushViewport()
    print(plot(dend), newpage=FALSE)
    upViewport()
  } else if (mode == "vertical2") {
    par("mar" = c(6,0,0,5))
    dend %>%  as.ggdend %>%  ggplot(horiz = FALSE, theme = NULL)
    # scale_y_reverse(expand = c(0.1, -0.1))
  } else if (mode == "horizontal2") {
    par("mar" = c(6,0,0,5))
    dend %>%  as.ggdend %>%  ggplot(horiz = TRUE, theme = NULL) +
      coord_flip()
    # scale_y_reverse(expand = c(0.1, -0.1))
  } else if (mode == "circular") {
    dend %>%  as.ggdend %>%  ggplot(horiz = TRUE, theme = NULL) +
      coord_polar(theta="x")
  } else if (mode == "circlize") {
    par(mar = c(1,3,1,3))
    circlize_dendrogram(dend)
  }
}

show_word_cloud <- function(text, palette)
{
  require(wordcloud)
  wordcloud(file.descriptive$comments, scale=c(5,0.5), max.words=100, random.order=FALSE,
            rot.per=0.25, use.r.layout=FALSE, colors=brewer.pal(8, palette))
}


################################################################################
####
# PRINT Functions
####
################################################################################


# calling export::graph2ppt with the same parameters
put_plot2ppt <- function(file_name_ppt, margin, aspect_ratio)
{
  # par(mar=c(4,3,3,1)+0.1)
  graph2ppt(file=file_name_ppt, margins=margin, aspectr=aspect_ratio, append=TRUE, upscale=TRUE)
}

put_table2ppt <- function(title, data)
{
  cat(title, data, file="output.txt", sep="\n", append = TRUE)
}


print_fa2pdf <- function(fa_list, file_name) {
  plot.list <- lapply(fa_list, function(fa.items) {
    fa.diagram(fa.items)
    recordPlot()
  })
  # create pdf with all plots
  pdf(file_name)
  lapply(plot.list, replayPlot)
  dev.off()
}

# ALT Determine number of clusters
print_cluster_no <- function(item_list, file_name)
{
  require(NbClust)
  plot.list <- lapply(item_list, function(items) {
    NbClust(items, min.nc=2, max.nc=15, method="kmeans") %$%
      .$Best.nc[1,] %>% table  %>%
      barplot(xlab="Number of Clusters", ylab="Number of Criteria",
              main="Number of Clusters Chosen by 26 Criteria") %>%
      recordPlot
  })

  pdf(file_name)
  lapply(plot.list, replayPlot)
  dev.off()
}

print_cluster_analyis <- function(item_list, file_name, dist, hclust, iterations)
{
  require(dplyr)
  require(pvclust)
  require(parallel)
  plot.list <- lapply(item_list, function(items) {
    pvclust(items, method.dist = dist, method.hclust = hclust, nboot = iterations) %T>%
      plot %>%
      # pvrect(., alpha = 0.95) %>%
      recordPlot
  })

  pdf(file_name)
  lapply(plot.list, replayPlot)
  dev.off()
}
