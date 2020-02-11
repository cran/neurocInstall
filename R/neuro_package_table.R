#' @title Neuroconductor Package Table
#' @description Returns the table of Neuroconductor packages
#' @return \code{data.frame} of packages with commit IDs
#' @param path Path to the table of package
#' @param long Should the data be "long" (with respect to stable/current)
#' @param deployment indicator if this is a release, not standard flag.
#' @export
#'
#' @note Package information is obtained from
#' \url{https://neuroconductor.org/neurocPackages}
#'
#' @importFrom stats reshape
#' @examples
#' neuro_package_table()
neuro_package_table = function(
  path = "https://neuroconductor.org/neurocPackages",
  long = FALSE,
  deployment = FALSE
) {
  #############################
  ## grab list of current neuroc packages
  #############################
  args = list(file = path,
              stringsAsFactors = FALSE, header = TRUE,
              na.strings = ifelse(deployment, "NA", ""))
  suppressWarnings({
    tab = try( {
      do.call("read.csv", args)
    } , silent = TRUE)
  })
  if (inherits(tab, "try-error")) {
    args$file = gsub("^https", "http", args$file)
    suppressWarnings({
      tab = try( {
        do.call("read.csv", args)
      } , silent = TRUE)
    })
    if (inherits(tab, "try-error")) {
      if (requireNamespace("httr", quietly = TRUE)) {
        destfile = tempfile()
        httr::GET(args$file,
                  httr::write_disk(path = destfile),
                  config = httr::config(ssl_verifypeer = FALSE))
        args$file = destfile
        tab = do.call("read.csv", args)
      }
    }
  }

  if (nrow(tab) == 0) {
    return(NULL)
  }
  xcn = colnames(tab) = c("repo",
                          "version.stable",
                          "neuroc_version.stable",
                          "commit_id.stable",
                          "version.current",
                          "neuroc_version.current",
                          "commit_id.current")
  bad_version = is.na(tab$version.stable) | tab$version.stable %in% ""
  tab$v = "0.0.0"
  tab$v[!bad_version] = package_version(tab$version.stable[!bad_version])
  if (nrow(tab) == 0 & !long) {
    return(tab)
  }
  ss = split(tab, tab$repo)
  ss = lapply(ss, function(x) {
    x = x[ order(x$v, decreasing = TRUE), ]
    x = x[1,,drop = FALSE]
    x$v = NULL
    x
  })
  tab = do.call("rbind", ss)
  tab = as.data.frame(tab, stringsAsFactors = FALSE)

  rownames(tab) = NULL
  if (long) {
    cn = colnames(tab)
    varying = cn[ cn != "repo"]
    if (nrow(tab) == 0) {
      cn = sapply(strsplit(xcn, "[.]"), function(x) x[1])
      cn = unique(cn)
      tab = matrix(NA, nrow = 0, ncol = length(cn))
      tab = data.frame(tab)
      colnames(tab) = cn
    } else {
      tab = reshape(
        data = tab, direction = "long",
        idvar = "repo", varying = varying,
        times = c("current", "stable"), timevar = "release")
    }
    rownames(tab) = NULL
  }
  return(tab)
}



#' @title Neuroconductor Packages
#' @description Returns the vector of Neuroconductor packages
#' @return \code{vector} of packages available on Neuroconductor
#' @param ... Arguments passed to \code{\link{neuro_package_table}}
#'
#' @export
#'
#' @examples
#' neuro_packages()
neuro_packages = function(...) {
  tab = neuro_package_table(...)
  tab = tab$repo
  tab = unique(tab)
  return(tab)
}
