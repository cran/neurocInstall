#' Latest Neuroconductor Release Location
#'
#' @param secure Should https vs. http be used
#' @param release Stable or current (development) versions
#' @return URL of release page
#' @export
#'
#' @examples
#' make_release_version("2018/02/", check = FALSE)
#' \donttest{
#' latest_neuroc_release()
#' }
latest_neuroc_release = function(secure = TRUE) {
  make_release_version(
    release_path = NULL,
    secure = secure)
}

#' @rdname latest_neuroc_release
#' @export
binary_release_repo = function(
  release = c("stable", "current"),
  secure = TRUE) {
  release = match.arg(release)
  release_version = paste0("latest/", release, "/")
  release_version = make_release_version(release_version, secure = secure)
  return(release_version)
}

#' @rdname latest_neuroc_release
#' @param release_path path to the release on
#' \url{https://neuroconductor.org/releases/}
#' @param check should the `release_path` need to be checked against
#' all the releases?
#' @export
make_release_version = function(release_path = NULL, secure = TRUE,
                                check = TRUE) {
  if (is.null(release_path)) {
    check = TRUE
  }
  if (check) {
    df = release_versions()

    if (is.null(release_path)) {
      release_path = df$release[1]
    }
    if (!all(release_path %in% df$release)) {
      warning(paste0("Release path created, but not in the ",
                     "Neuroconductor set of releases"))
    }
  }
  release_path = paste0(
    "http", ifelse(secure, "s", ""), "://neuroconductor.org/releases/",
    release_path)
  release_path
}



#' @rdname latest_neuroc_release
#' @importFrom utils download.file
#' @export
release_versions = function(secure = TRUE) {
  # read from the page Adi Makes
  # currently fail
  url = paste0("http", ifelse(secure, "s", ""),
               "://neuroconductor.org/api/releases/")
  destfile = tempfile(fileext = ".txt")
  x = try({
    download.file(url = url, destfile = destfile, quiet = TRUE)
  }, silent = TRUE)
  if (inherits(x, "try-error") || x != 0) {
    warning(paste0(
      "Releases did not download, may be error with downloading ",
      url))
    if (requireNamespace("httr", quietly = TRUE)) {
      url = sub("https", "http", url)
      res = httr::GET(url,
                      httr::write_disk(path = destfile, overwrite = TRUE),
                      config = httr::config(ssl_verifypeer = FALSE))
      httr::warn_for_status(res)
    }
  }
  releases = readLines(destfile, warn = FALSE)
  releases = trimws(releases)
  releases = gsub('"', "", releases)
  releases = releases[grepl("releases/", releases)]
  releases = gsub('"', "", releases)
  releases = trimws(releases)
  releases = sub(',$', "", releases)

  releases = sub("^releases/", "", releases)
  ss = t(sapply(strsplit(releases, "/"), rbind))
  colnames(ss) = c("year", "month")
  df = data.frame(release = releases, stringsAsFactors = FALSE)
  df = cbind(df, ss, stringsAsFactors = FALSE)
  df = df[ df$year != "latest", , drop = FALSE]
  df$year = as.numeric(df$year)
  df$date = paste0(df$year, "-", df$month, "-01")
  df$date = as.Date(x = df$date, format = "%Y-%m-%d")
  df = df[ order(df$date, decreasing = TRUE), , drop = FALSE]
  return(df)
}
