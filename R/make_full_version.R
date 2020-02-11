#' @title Make Full Package Version
#' @description Makes a package version to have all the same length.
#' This is helpful when using \code{\link{compareVersion}}.
#'
#' @param x Character vector of package versions
#'
#' @return Character vector of versions, each with the same length.
#' @export
#'
#' @examples
#' x = c("1.6", "1.6.0")
#' compareVersion(x[1], x[2])
#' x2 = make_full_version(x)
#' compareVersion(x2[1], x2[2])
#' x = c("1.6", "1.6.0")
#' compareVersion(x2[1], x2[2])
make_full_version = function(x) {
  nx = names(x)
  x = as.character(x)
  r <- lapply(strsplit(x, "[.-]"), as.integer)
  lx = sapply(r, length)
  mlx = max(lx)
  r <- lapply(r, function(ver) {
    c(ver, rep(0, length = mlx - length(ver)))
  })
  x = sapply(r, paste, collapse = ".")
  names(x) = nx
  return(x)
}



