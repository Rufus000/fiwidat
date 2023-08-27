


pplot=function (model, xvar, by = NULL, breaks = NULL, partial = FALSE, 
          type = c("conditional", "contrast"), scale = c("linear", 
                                                         "response"), leg.name = NULL, leg.labs = NULL, cols = c("red", 
                                                                                                                 "blue", "green", "purple"), linesize = 1.5, transp = 0.5, 
          rsize = 2, rcolor = "red", rshade = FALSE, rshapes = TRUE) 
{
  require(visreg)
  require(ggplot2)
  theme_set(theme_rufus(base_size = 20))
  if (is.null(by)) {
    p = visreg(model, xvar, plot = FALSE, type = type)
  }
  else {
    p = visreg(model, xvar, by, breaks, plot = FALSE, type = type)
  }
  ix = which(names(p$fit) == p$meta$x)
  ib = which(names(p$fit) == p$meta$by)
  if (is.null(leg.name)) 
    leg.name = p$meta$by
  if (is.null(leg.labs)) 
    leg.labs = waiver()
  if (!is.null(p$meta$by)) {
    plt = ggplot(p$fit, aes(x = p$fit[, ix], y = visregFit, 
                            fill = as.factor(p$fit[, ib]))) + geom_ribbon(aes(ymin = visregLwr, 
                                                                              ymax = visregUpr), alpha = transp) + scale_fill_manual(values = cols, 
                                                                                                                                     name = leg.name, labels = leg.labs) + geom_line(size = linesize) + 
      xlab(p$meta$x) + ylab(p$meta$y)
    if (partial == T) {
      ix = which(names(p$res) == p$meta$x)
      ib = which(names(p$res) == p$meta$by)
      grp = as.factor(p$res[, ib])
      if (rshade == T) 
        cl = grp
      else cl = as.factor(1)
      if ((rshapes == T)) 
        sh = grp
      else sh = as.factor(1)
      plt = plt + geom_point(data = p$res, aes(x = p$res[, 
                                                         ix], y = visregRes, fill = grp, shape = sh, color = cl), 
                             show.legend = FALSE, size = rsize) + scale_color_manual(values = c("gray35", 
                                                                                                "gray50", "gray65", "gray80"))
    }
  }
  else {
    if (class(p$fit[, 1]) == "factor") {
      n = dim(p$fit)[1]
      plt = ggplot(p$fit, aes(x = p$fit[, 1], y = p$fit$visregFit)) + 
        geom_point(size = 3, color = "blue") + geom_errorbar(aes(ymin = p$fit$visregLwr, 
                                                                 ymax = p$fit$visregUpr), width = 0.15, color = "blue")
      plt = plt + xlab("")
      plt = plt + ylab(p$meta$y)
    }
    else {
      plt = ggplot(p$fit, aes(x = p$fit[, ix], y = visregFit)) + 
        geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), 
                    fill = cols[1], alpha = transp) + geom_line(size = linesize) + 
        xlab(p$meta$x) + ylab(p$meta$y)
      
    }
    if (partial == T) {
      plt = plt + geom_point(data = p$res, aes(x = p$res[, 
                                                         ix], y = visregRes), colour = rcolor, size = rsize, 
                             shape = "circle")
      #plt = plt+geom_line(data=p$fit, aes(x = p$fit[, ix], y = p$fit$visregFit))+geom_line(linewidth=1.5)
    }
  }
  plt
}

