# Source this module to define acquisition and plotting functions only.
# The distributed clips retain each external product's native grid and values.
prepare_external_comparison <- function(root = getwd(), bonnaventure, obu) {
  if (!requireNamespace("terra",quietly=TRUE)) stop("The terra package is required.")
  dest <- file.path(root,"data/permafrost/external")
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  training <- terra::vect(file.path(root,
    "data/permafrost/training/MineSitePermafrost_TetraTech_20170224.shp"))
  training <- terra::project(training, "EPSG:3579")
  extent <- terra::ext(training) + 5000
  footprint <- terra::as.polygons(extent,crs=terra::crs(training))
  for(name in c("bonnaventure_2012","obu_2018")) {
    source <- if(name=="bonnaventure_2012") bonnaventure else obu
    r <- terra::rast(source)
    clip <- terra::crop(r,terra::project(footprint,r),snap="out")
    terra::writeRaster(clip,file.path(dest,paste0(name,"_probability_coffee.tif")),
      overwrite=FALSE,wopt=list(datatype="FLT4S",gdal=c("COMPRESS=DEFLATE","PREDICTOR=3")))
  }
  write.csv(data.frame(xmin=terra::xmin(extent),xmax=terra::xmax(extent),
    ymin=terra::ymin(extent),ymax=terra::ymax(extent),crs="EPSG:3579",buffer_m=5000),
    file.path(dest,"comparison_extent.csv"),row.names=FALSE)
  invisible(dest)
}

plot_external_comparison <- function(cfg) {
  extent <- read.csv(file.path(cfg$root,"data/permafrost/external/comparison_extent.csv"))
  target <- terra::ext(unlist(extent[1,c("xmin","xmax","ymin","ymax")]))
  model <- terra::crop(terra::rast(raster_file(cfg,"prediction_w_snow_hypso.tif")),target)
  mess <- terra::crop(terra::rast(raster_file(cfg,"MESS_snow_hypso.tif")),model)
  model <- mask_mess(model,mess)
  training <- terra::project(terra::vect(cfg$training),model)
  water <- terra::crop(terra::project(terra::vect(cfg$water),model),target)
  streams <- terra::crop(terra::project(terra::vect(cfg$streams),model),target)
  model <- terra::mask(model,water,inverse=TRUE)
  external <- lapply(cfg$external_probability,function(path) terra::rast(path))
  # Nearest-neighbour display reprojection retains the coarse Obu cell pattern.
  # Neither product is resampled to the 16 m model grid or used as validation truth.
  external <- Map(function(r,resolution) {
    template <- terra::rast(target,resolution=resolution,crs=terra::crs(model))
    terra::project(r,template,method="near")
  },external,c(30,1000))
  point <- terra::project(terra::vect(data.frame(lon=-139.34044,lat=62.8893),
    geom=c("lon","lat"),crs="EPSG:4326"),model)
  probability_cols <- grDevices::hcl.colors(100,"YlGnBu",rev=FALSE)
  class_cols <- c("#BCE7FA","#00BFFF","#FAFAF5","#084B9B")
  type <- as.character(training$Type)
  raw_order <- c("Ice-poor","Ice-rich","Unfrozen","Very Ice-rich")
  polygon_cols <- class_cols[match(type,raw_order)]
  stopifnot(!anyNA(polygon_cols))
  draw <- function() {
    previous <- graphics::par(no.readonly=TRUE);on.exit(graphics::par(previous))
    graphics::layout(matrix(1:6,nrow=3,byrow=TRUE),heights=c(1,1,0.20))
    graphics::par(mar=c(1,1,3,1),family="sans")
    panel <- function(title,r=NULL,probability=FALSE,field=FALSE) {
      plot(NA,xlim=as.vector(target)[1:2],ylim=as.vector(target)[3:4],asp=1,
           axes=FALSE,xlab="",ylab="",main=title,cex.main=1)
      if(!is.null(r)) terra::plot(r,add=TRUE,legend=FALSE,axes=FALSE,
        col=if(probability) probability_cols else class_cols,
        zlim=if(probability)c(0,1) else NULL,maxcell=1000000,colNA="white")
      if(field) terra::plot(training,add=TRUE,col=polygon_cols,border=NA)
      terra::plot(water,add=TRUE,col="grey85",border=NA)
      terra::lines(streams,col="#5C93AD",lwd=0.35)
      terra::lines(training,col="grey30",lwd=0.5)
      terra::points(point,pch=8,col="#7B3294",cex=1.1)
      u<-par("usr");x<-u[1]+0.05*diff(u[1:2]);y<-u[3]+0.05*diff(u[3:4])
      segments(x,y,x+5000,y,lwd=2);segments(c(x,x+5000),y-120,c(x,x+5000),y+120)
      text(x+2500,y+350,"5 km",cex=0.7);box()
    }
    panel("A  Bonnaventure et al. (2012)\nPermafrost probability, 30 m",external[[1]],TRUE)
    panel("B  Obu et al. (2018 dataset)\nPermafrost probability, approximately 1 km",external[[2]],TRUE)
    panel("C  Coffee field map\nGround ice classes",field=TRUE)
    panel("D  Random forest prediction\nGround ice classes, MESS-filtered",model)
    par(mar=c(1,2,0,2));plot.new();plot.window(xlim=c(0,1),ylim=c(0,1))
    for(i in seq_along(probability_cols))rect((i-1)/100,0.55,i/100,0.82,
      col=probability_cols[i],border=NA)
    text(c(0,0.5,1),0.28,c("0","0.5","1"),cex=0.8)
    text(0.5,1,"Permafrost probability (panels A and B)",cex=0.9)
    plot.new();legend("center",legend=c("Permafrost-free","Ice-poor","Ice-rich","Very ice-rich"),
      fill=class_cols[c(3,1,2,4)],ncol=2,bty="n",cex=0.85,
      title="Ground ice classes (panels C and D)")
  }
  png <- output_file(cfg,"Figure7_external_comparison.png")
  grDevices::png(png,width=3600,height=3000,res=300)
  tryCatch(draw(),finally=grDevices::dev.off())
  grDevices::pdf(output_file(cfg,"Figure7_external_comparison.pdf"),width=12,height=10)
  tryCatch(draw(),finally=grDevices::dev.off())
  invisible(png)
}
