#Script: treemix.bootstrap
#License: GPLv3 or later
#Written by: Marco Milanesi, Elia Vajana
#Contact: marco.milanesi.mm@gmail.com, vajana.elia@gmail.com
#Description: Plot treemix tree with bootstrap values on branches
#Modification date: 2017-06-09

treemix.bootstrap = function(
  in.file,
  out.file="tmp",
  phylip.file, 
  pop.color.file=NULL,
  nboot,
  cex = 1,
  disp = 0.003,
  plus = 0.01,
  flip = vector(),
  arrow = 0.05,
  ybar = 0.1,
  xbar = 0, 
  xmin = 0,
  lwd = 1,
  font = 1, 
  scale = T,
  mbar = T,
  plotmig = T,
  plotnames = T,
  plotboot=T,
  boot.cex = 1, 
  boot.legend.location = NULL, 
  boot.legend.cex = 1, 
  ...
){
  ##### The function is based on "treemix" functions. The annotation is the same of the original functions. #####
  ### The user can check the treemix manual to refer to the original funtions. For any dubt, please contact the maintainer ###
  
  ## Preparation file (COSA INTENDI PER PREPARATION FILES?)
  d = paste(in.file, ".vertices.gz", sep = "")
  e = paste(in.file, ".edges.gz", sep = "")
  d = read.table(gzfile(d), as.is = T, comment.char = "", quote = "")
  e = read.table(gzfile(e), as.is  = T, comment.char = "", quote = "")
  e[,3] = e[,3]*e[,4]
  e[,3] = e[,3]*e[,4]
  
  pops <- sort(d$V2[!is.na(d$V2)])
  if (length(pop.color.file) == 0){
    o <- as.data.frame(cbind(pops,rep("black",length(pops))), stringsAsFactors = F)
    colnames(o) <- c("V1","V2")
  }else{
    if (!file.exists(pop.color.file)){
      cat("File with population order and colors not found. Please check! \n")
      stop("Exit",call. = F)
    }else{
      o = read.table(pop.color.file, as.is = T, comment.char = "", quote = "")
      if (!setequal(x = pops, y = o[,1])){
        cat("File with population order and colors doesn't match with the population in the dataset. Please check! \n")
        stop("Exit",call. = F)
      }
    }
  }
  
  se = paste(in.file, ".covse.gz", sep = "")
  se = read.table(gzfile(se), as.is = T, comment.char = "", quote = "")
  m1 = apply(se, 1, mean)
  mse = mean(m1)
  for(i in 1:length(flip)){
    d = flip_node(d, flip[i])
  }
  d$x = "NA"
  d$y = "NA"
  d$ymin = "NA"
  d$ymax = "NA"
  options(warn=-1)
  d$x = as.numeric(d$x)
  d$y = as.numeric(d$y)
  d$ymin = as.numeric(d$ymin)
  d$ymax = as.numeric(d$ymax)
  options(warn=0)
  d = set_y_coords(d)
  d = set_x_coords(d, e)
  tmplog <- capture.output({
    d = set_mig_coords(d, e)
  })
  
  # Vertices from TREEMIX consensus_tree results 
  vertices_treemix.df <- as.data.frame(matrix(NA, ncol=1, nrow = nrow(d)))
  for (pos1 in 1:nrow(d)){
    tmp1 <- unlist(strsplit(d[pos1,11], split = ","))
    tmp3 <- NULL
    for (pos2 in 1:length(tmp1)){
      tmp2 <- gsub(pattern = "\\(", replacement = "", x = unlist(strsplit(tmp1[pos2], ":"))[1])
      tmp3 <- c(tmp3,tmp2)
      rm(tmp2)
    }
    vertices_treemix.df[pos1,1] <- paste(tmp3, collapse = " ")
    rm(tmp3)
  }
  vertices_treemix.df[vertices_treemix.df == "NA"] <- NA
  
  # Vertices from PHYLIP consensus_tree results 
  vertices_boot.df <- newick.split(phylip.file)
  
  # Create a duplicate d file. 
  d2 <- cbind(d,vertices_treemix.df)
  colnames(d2)[ncol(d2)] <- "treemix_vert"
  d2$Boot <- NA
  
  # Match between TREEMIX and bootstrap vertices
  # NB: not all the vertices in the bootstrap tree will match with TREEMIX tree because TREEMIX may collapses some nodes
  rstvert <- NULL # vector with bootstrap vertices not assigned
  for (b in 1:nrow(vertices_boot.df)){
    # Vertices and information extraction
    vertsplit <- unlist(strsplit(vertices_boot.df[b,2], split = " "))
    vertval <- vertices_boot.df[b,1]
    vertlength <- length(vertsplit)
    pp <- 0
    
    # Searching the boot.vert in treemix.vert
    for (a in vertsplit){
      if (which(vertsplit == a ) == 1){
        tmpgrep <- d2$treemix_vert
      }
      tmpgrep2 <- tmpgrep[grep(pattern = a, x = tmpgrep)]
      tmpgrep <- tmpgrep2
      rm(tmpgrep2)
    }
    
    # Select the right one comparing the length # ESPLICITEREI COSA INTENDI PER "RIGHT ONE"
    for (a in 1:length(tmpgrep)){
      tmplng <- length(unlist(strsplit(tmpgrep[a], split = " ")))
      if (tmplng == vertlength){
        # Boot value in d2
        d2$Boot[which(d2$treemix_vert == tmpgrep[a])] <- vertval
        pp <- 1
      }
    }
    
    if (pp == 0){
      rstvert <- c(rstvert, vertices_boot.df[b,2])
    }
    rm(tmpgrep)
  }
  
  # Report vertices without matching - bootstrap
  write.table(x = rstvert, file = paste(out.file, "_NoMatch_bootstrap.txt", sep=""), quote = F, row.names = F, col.names = F)
  
  # Convert bootstrap values in percentage
  if (!exists("nboot") & !is.numeric(nboot)){
    cat("Number of bootstrap is unknown or is not a number. Please check! \n")
    stop("Exit",call. = F)
  }
  d2$Boot <- round(d2$Boot/nboot*100,2) # instead the value we put a percentage
  
  ### Colour and point ###
  d2$color <- NA
  d2$color[which(d2$Boot <=100)] <- adjustcolor("green4", 0.97)
  d2$color[which(d2$Boot <90)] <- adjustcolor("green2", 1)
  d2$color[which(d2$Boot <75)] <- adjustcolor("olivedrab2", 1) 
  d2$color[which(d2$Boot <50)] <- adjustcolor("#7F9380", 1)
  d2$point <- NA
  d2$point[which(d2$Boot <=100)] <- 19
  d2$point[which(d2$Boot <90)] <- 15
  d2$point[which(d2$Boot <75)] <- 17
  d2$point[which(d2$Boot <50)] <- 4
  d2$point <- as.numeric(d2$point)
  
  # Report vertices without matching - Treemix
  write.table(x = d2[which(d2[,3] != "ROOT" & d2[,4] != "MIG" & d2[,5] != "TIP" & is.na(d2$Boot)),16], 
              file = paste(out.file, "_NoMatch_treemix.txt", sep=""), quote = F, row.names = F, col.names = F)
  
  #### PLOTTING TREE ## plot_tree_internal_boot
  plot(d$x, d$y, axes = F, ylab = "", xlab = "Drift parameter", 
       xlim = c(xmin, max(d$x)+plus), pch = "")
  axis(1)
  
  # Plot bootstrap points
  if (plotboot){
    for (i in 1:nrow(d2)){
      if (!(is.na(d2$Boot[i]))){
        # if (d2$Boot[i] >= 50){
        points(x=d2$x[i], y=d2$y[i], 
               col=d2$color[i], cex=boot.cex, pch=as.numeric(d2$point[i])) #(cex*1.3)
        # }
      }
    }
  }
  
  # Plot the tree with migration edges
  mw = max(e[e[,5]=="MIG",4])
  mcols = rev(heat.colors(150))
  
  for(i in 1:nrow(e)){
    col = "gray35"
    
    if (e[i,5] == "MIG"){
      w = floor(e[i,4]*200)+50
      if (mw > 0.5){
        w = floor(e[i,4]*100)+50
      }
      col = mcols[w]
      if (is.na(col)){
        col = "blue"
      }
    }
    v1 = d[d[,1] == e[i,1],]
    v2 = d[d[,1] == e[i,2],]
    
    if (e[i,5] == "MIG"){
      if (plotmig){
        arrows( v1[1,]$x, v1[1,]$y, v2[1,]$x, v2[1,]$y, col = col, length = arrow)
      }
    }else{
      lines( c(v1[1,]$x, v2[1,]$x), c(v1[1,]$y, v2[1,]$y), col = col, lwd = lwd)
    }
  }
  
  # Plot the breeds' names
  tmp = d[d[,5] == "TIP",]
  for(i in 1:nrow(tmp)){
    tcol = o[o[,1] == tmp[i,2],2]
    if(plotnames){
      #print(tmp[i,2])
      text(tmp[i,]$x+disp, tmp[i,]$y, labels = tmp[i,2], adj = 0, cex = cex, col  = tcol, font = font)
    }
  }
  
  # Plot the scale 
  if (scale){
    # print (paste("mse", mse))
    lines(c(xbar, xbar+(mse*10)), c(ybar, ybar))
    text( xbar, ybar - 0.04, lab = "10 s.e.", adj = 0, cex  = 0.8)
    lines( c(xbar, xbar), c( ybar - 0.01, ybar+0.01))
    lines( c(xbar+(mse*10), xbar+(mse*10)), c(ybar- 0.01, ybar+ 0.01))
  }
  
  # Plot the migration bar
  if (mbar){
    mcols = rev( heat.colors(150) )
    mcols = mcols[50:length(mcols)]
    ymi = ybar+0.15
    yma = ybar+0.35
    l = 0.2
    w = l/100
    xmi = xbar
    xma = xbar+(max(d$x/20))
    rect( rep(xmi, 100), ymi+(0:99)*w, rep(xma, 100), ymi+(1:100)*w, col = mcols, border = mcols)
    #text(xma+disp, ymi, lab = "0", adj = 0, cex = 0.7)
    text(xma+disp, ymi, lab = "0", adj = 0, cex = 1)
    if ( mw >0.5){ text(xma+disp, yma, lab = "1", adj = 0, cex = 0.7)}
    else{
      #text(xma+disp, yma, lab = "0.5", adj = 0, cex =0.7)
      text(xma+disp, yma, lab = "0.5", adj = 0, cex = 1)
    }
    text(xmi, yma+0.06, lab = "Migration weight", adj = 0 , cex = 1)
    #text(xmi, yma+0.03, lab = "weight", adj = 0 , cex = 0.6)
  }	
  
  # Plot the bootstrap bar
  if(plotboot){
    if (is.null(boot.legend.location)){
      legend(max(d$x)*0.95,max(d$y), 
             legend = c("[90-100]%", "[75-90[%", "[50-75[%", "]50%"), 
             pch = c(19,15,17, 4), 
             col=c(adjustcolor("green4", 0.97), adjustcolor("green2", 1),adjustcolor("olivedrab2", 1), adjustcolor("#7F9380", 1)), 
             cex = boot.legend.cex, title = "Bootstrap values", border = F, box.lwd = 0) 
    }else{
      legend(boot.legend.location, 
             legend = c("[90-100]%", "[75-90[%", "[50-75[%", "]50%"), 
             pch = c(19,15,17, 4), 
             col=c(adjustcolor("green4", 0.97), adjustcolor("green2", 1),adjustcolor("olivedrab2", 1), adjustcolor("#7F9380", 1)), 
             cex = boot.legend.cex, title = "Bootstrap values", border = F, box.lwd = 0) 
    }
  }
  
}
