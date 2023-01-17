f.plot.lines <- function(res, obj.name, ylim = NULL, opacity = 0.25,
                         mf.r = 2, mf.c = 5, l.width = 8, l.height = 5,
                         method.ind, method.names){
  
  pdf(file = paste0("Graphics/lines/lines-",obj.name,".pdf"), width = l.width, height=l.height)
  par(mfrow=c(mf.r,mf.c))
  i <- 0
  for(method.number in method.ind){
    i <- i+1
    f.lines(res = res, method.number = method.number, 
            plot.title = method.names[i],
            ylim = ylim, opacity = opacity)  
  }
  dev.off()
  
}
