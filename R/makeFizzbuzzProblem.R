#' @title Fizzbuzz Classification Problem
#'
#' @description
#' This problem is inspired by this blog post \url{http://joelgrus.com/2016/05/23/fizz-buzz-in-tensorflow/}.
#' Obviously this is not a serious benchmarking problem, but it isn't totally trivial to solve.
#' Each observation is one number encoded in 32-bit binary. The target if
#' \itemize{
#'  \item{\code{"fizz"} if the number is dividable by 3.}
#'  \item{\code{"buzz"} if the number is dividable by 5.}
#'  \item{\code{"fizzbuzz"} if the number is dividable by 15.}
#'  \item{\code{"number"} otherwise.}
#' }
#' @param n [\code{integer(1)}]\cr
#'   Number of observations to create.
#'   Default is \code{5000L}.
#' @param create.task [\code{logical(1)}]\cr
#'   Should a task be created? If \code{TRUE} a \pkg{mlr} classification task (see \code{\link[mlr]{makeClassifTask}}
#'   is returned. Otherwise a \code{data.frame} is returned.
#' @return \code{ClassifTask} | \code{data.frame}.
#' @export
makeFizzbuzzProblem = function(n = 10^4, create.task = TRUE) {

  n = seq_len(n)
  target = ifelse(n %% 15 == 0, "fizzbuzz", ifelse(n %% 5 == 0, "buzz", ifelse(n %% 3 == 0, "fizz", "number")))
  x = vapply(n, function(x) as.integer(intToBits(x)), FUN.VALUE = numeric(32))

  res = data.frame(target, t(x))

  res = res[sample(n), ]

  if (create.task)
    res = makeClassifTask(id = "FizzbuzzProblem", data = res, target = "target")

  return(res)

}
