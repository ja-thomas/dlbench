makeNotNotMnistProblem = function(n, sample.font = TRUE, random.rotation = 0, position.range = c(0.3, 0.7),
  size.range = c(15, 20), letters = LETTERS, resolution = c(32, 32), pointsize = 2,  create.task = TRUE) {


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
