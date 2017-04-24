#' @title MNIST like problem.
#'
#' @description
#' Generate images of letters or numbers in various ways. Randomized size, position, rotations
#' and fonts are available to set the difficulty of the problem.
#' @param n [\code{integer(1)}]\cr
#'   Number of observations to create.
#'   Default is \code{5000L}.
#' @param sample.font [\code{logical(1)}]\cr
#'   Should every image be sampled from a different font. Fonts are sampled from \code{\link{Hershey}}.
#'   Default is \code{TRUE}.
#' @param random.rotation [\code{numeric(1)}]\cr
#'   Should the rotation of the letters/numbers be randomized. Value is the maximum degree of rotation.
#'   Default is 0, which is no rotation at all.
#' @param position.range [\code{numeric(2}]\cr
#'   Randomize position of the letter/number. First number is the lower bound for x/y value,
#'   second number the upper bound. Minimal/maximal values of the canvas are 0 and 1.
#'   Note that it is possible that some element of the letters/number might be out of the bound,
#'   depending on the range and size.
#'   Default is \code{c(0.3, 0.7)}
#' @param size.range [\code{numeric(2)}]\cr
#'   Size of the letters/numbers, is sampled between first and second value. This is the
#'   \code{cex} value of the plot.
#'   Default is \code{c(15, 50)}.
#' @param letters [\code{numeric() | character()}]\cr
#'   Letters/Numbers to sample from. Can be anything that can be passed to \code{text()}.
#'   Default is \code{LETTERS}.
#' @param resolution [\code{integer(2)}]\cr
#'   Resolution of the image. This defines the number of features.
#'   Default is \code{c(32, 32)}, which creates \code{1024} features.
#' @param pointsize [\code{integer(1)}]\cr
#'   Pointsize of intepreted big points (1/72 inch). See \code{\link{bmp}}.
#'   Default is \code{2}.
#' @param create.task [\code{logical(1)}]\cr
#'   Should a task be created? If \code{TRUE} a \pkg{mlr} classification task (see \code{\link[mlr]{makeClassifTask}}
#'   is returned. Otherwise a \code{data.frame} is returned.
#' @return \code{ClassifTask} | \code{data.frame}.
#' @export
makeNotNotMnistProblem = function(n = 5^4, sample.font = TRUE, random.rotation = 0, position.range = c(0.3, 0.7),
  size.range = c(15, 50), letters = LETTERS, resolution = c(32, 32), pointsize = 2,  create.task = TRUE) {


  letter = sample(letters, n, replace = TRUE)

  x = vapply(letter, function(l) {

    rotation = runif(1, 0, random.rotation)
    size = runif(1, size.range[1], size.range[2])
    pos = runif(2, position.range[1], position.range[2])
    tmp = tempfile()
    bmp(tmp, width = resolution[1], height = resolution[2], pointsize = pointsize)
    plot(c(), xlim = c(0,1), ylim = c(0,1), axes=FALSE, xlab = "", ylab = "")

    if (sample.font) {
      vf = Hershey$allowed[sample.int(nrow(Hershey$allowed), 1), ]
      vfont = c(Hershey$typeface[vf[1]], Hershey$fontindex[vf[2]])
      text(pos[1], pos[2], l, cex = size, srt = rotation, vfont = vfont)
    } else {
      text(pos[1], pos[2], l, cex = size, srt = rotation)
    }
    dev.off()
    as.numeric(readbitmap::read.bitmap(tmp))
  },  numeric(prod(resolution)))

  res = data.frame(letter = as.character(letter), t(x))

  if (create.task)
    res = makeClassifTask(id = "notNotMnsitProblem", data = res, target = "letter")

  res

}
