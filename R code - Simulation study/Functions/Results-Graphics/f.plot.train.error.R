f.plot.train.error.each <- function(obj.name, runs, ylim = NULL,
                                    l.width = 15, l.height = 8,
                                    methods, method.names,
                                    cols, x.legend, y.legend, scenario){
  
  pdf(file = paste0("Graphics/pop_error_lines/pop_error_lines-",obj.name,".pdf"), width = length(runs)*3, height=length(methods)*4)
  par(mfrow=c(length(methods),length(runs)))
  for(method in methods){
    for(run in runs){
      f.pop.error(res = get(obj.name), method = method, title = paste0(scenario, " - ", method.names[which(methods %in% method)],"\n\nr = ", run),
                  run = run, col = cols[which(methods %in% method)], ylim = ylim, x.legend = x.legend, y.legend = y.legend)  
    }
  }
  dev.off()
 
}  


f.plot.train.error.all <- function(obj.name, runs, ylim = NULL,
                                    l.width = 15, l.height = 8,
                                    methods, method.names,
                                    cols, x.legend, y.legend, scenario){
  
  pdf(file = paste0("Graphics/pop_error_lines/pop_error_lines-allmethods-",obj.name,".pdf"), width = l.width, height=l.height)
  par(mfrow=c(2,length(runs)))
  for(run in runs){
    f.train.error(res = get(obj.name), run = run, methods = methods, method.names = method.names,
                  ylim = ylim, plot.title = paste0(scenario, " - Training model --> population\n\nr = ", run),
                  cols = cols, plot.type = "train.to.pop", x.legend = x.legend, y.legend = y.legend)  
  }
  for(run in runs){
    f.train.error(res = get(obj.name), run = run, methods = methods, method.names = method.names,
                  ylim = ylim, plot.title = paste0(scenario, " - Training model --> test set\n\nr = ", run),
                  cols = cols, plot.type = "train.to.test", x.legend = x.legend, y.legend = y.legend)
  }
  dev.off()
  
  
}
