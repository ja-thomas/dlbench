#' @title Correlation Image Problem
#'
#' @description
#' This benchmark is motivated by \url{http://guessthecorrelation.com/}. The task is to
#' assess the correlation between two variables visually by a scatterplot of sampled points.
#' Each observation is generated as follows:
#' \itemize{
#'  \item{A random correlation value is drawn uniformly between -1 and 1.}
#'  \item{\code{points.per.image} points are sampled from a bivariate normal distribution with zero mean, sampled correlation and a marginal variance of 1.}
#'  \item{The observations are scaled between 0 and 1.}
#'  \item{An image of the data is generated and saved in a given resolution and pointsize.}
#'  \item{The values of the pixels are returned as a vector with the correlation as target.}
#' }
#'
#'
#' @param n [\code{integer(1)}]\cr
#'   Number of observations to create.
#'   Default is \code{5000L}.
#' @param resolution [\code{integer(2)}]\cr
#'   Resolution of the image. This defines the number of features.
#'   Default is \code{c(32, 32)}, which creates \code{1024} features.
#' @param points.per.image [\code{integer(1)}]\cr
#'   How man points should be generated per image.
#'   Default is \code{100}.
#' @param pointsize [\code{integer(1)}]\cr
#'   Pointsize of intepreted big points (1/72 inch). See \code{\link{bmp}}.
#'   Default is \code{2}.
#' @param create.task [\code{logical(1)}]\cr
#'   Should a task be created? If \code{TRUE} a \pkg{mlr} regression task (see \code{\link[mlr]{makeRegrTask}}
#'   is returned. Otherwise a \code{data.frame} is returned.
#' @return \code{RegrTask} | \code{data.frame}.
#' @examples
#'  data = makeCorrelationImageProblem(n = 20, create.task = FALSE)
#'  image(matrix(as.numeric(data[1, -1]), ncol = 32))
#' @export
makeCorrelationImageProblem = function(n = 5000L, resolution = c(32, 32), points.per.image = 100,
  pointsize = 2, create.task = TRUE) {

  assertIntegerish(n, lower = 1, len = 1)
  assertIntegerish(resolution, lower = 1, len = 2)
  assertIntegerish(points.per.image, lower = 1, len = 1)
  assertIntegerish(pointsize, lower = 1, len = 1)
  assertFlag(create.task)

  corr = runif(n, -1, 1)

  res = vapply(corr, function(x) {
    d = mvtnorm::rmvnorm(n = points.per.image, mean = c(0,0), sigma = matrix(c(1, x, x, 1), ncol = 2))
    d = normalize(d, method = "range", margin = 2L)
    tmp = tempfile()
    bmp(tmp, width = resolution[1], height = resolution[2], pointsize = pointsize)
    plot(d, pch = 20, axes=FALSE, xlab = "", ylab = "")
    dev.off()
    c(x, readbitmap::read.bitmap(tmp))
  }, numeric(prod(resolution) + 1))

  res = data.frame(t(res))
  colnames(res)[1] = "target"

  if (create.task)
    res = makeRegrTask(id = "CorrelationImageProblem", data = res, target = "target")

  return(res)

}
