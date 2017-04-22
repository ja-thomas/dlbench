makeCorrelationImageProblem = function(n, resolution = c(32, 32), points.per.image = 100, pointsize = 2, create.task = TRUE) {

  require(mvtnorm)
  require(readbitmap)
  require(BBmisc)
  require(mlr)

  corr = runif(n, -1, 1)

  res = vapply(corr, function(x) {
    d = rmvnorm(n = points.per.image, mean = c(0,0), sigma = matrix(c(1, x, x, 1), ncol = 2))
    d = normalize(d, method = "range", margin = 2L)
    tmp = tempfile()
    bmp(tmp, width = resolution[1], height = resolution[2], pointsize = pointsize)
    plot(d, pch = 20, axes=FALSE, xlab = "", ylab = "")
    dev.off()
    c(x, as.numeric(read.bitmap(tmp)))
  }, numeric(prod(resolution) + 1))

  res = data.frame(t(res))
  colnames(res)[1] = "target"

  if (create.task)
    res = makeRegrTask(id = "CorrelationImageProblem", data = res, target = "target")

  return(res)

}
