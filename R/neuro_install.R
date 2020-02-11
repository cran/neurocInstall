#' @title Neuroconductor Installer
#' @description Install function for neuroconductor packages
#' @param repo Package name in neuroconductor
#' @param release Stable or current (development) versions/branches
#' @param release_repo Repository for release repository, passed to
#' \code{\link{install.packages}}.  If \code{release_repo = "github"},
#' then it will install using GitHub.  If you set this using
#' \code{\link{make_release_version}} or specify the URL directly,
#' it will override \code{release} option.
#'
#' @param type character, indicating the type of package to download and
#' install, passed to \code{\link{install.packages}}.
#'
#' @param upgrade_dependencies Should dependencies be updated?
#' passed to \code{\link[devtools]{install}} if using
#' \code{release_repo = "github"}
#' @param ... additional arguments passed to
#' \code{\link{install.packages}}
#' or \code{\link[devtools]{install_github}} if
#' \code{release_repo = "github"}
#'
#' @return Result from  \code{\link{install.packages}} or
#' \code{\link[devtools]{install_github}}
#'
#' @export
#' @importFrom devtools install_github
#' @importFrom utils read.csv
#' @importFrom utils compareVersion install.packages installed.packages
#'
#' @examples
#' \donttest{
#'    tlib = tempfile()
#'    dir.create(tlib, showWarnings = FALSE)
#'    system.time({
#'    install.packages("oro.asl",
#'    lib = tlib,
#'    repos = "https://neuroconductor.org/releases/2019/12/")
#'    })
#'    repos = getOption("repos")
#'    print(repos)
#'    #if (repos["CRAN"] == "@CRAN@") {
#'    #    repos["CRAN"] = "https://cloud.r-project.org"
#'    #    options(repos = repos)
#'    #}
#'    options(repos = NULL)
#'    print(getOption("repos"))
#'    neuro_install("oro.asl", lib = tlib,
#'    release_repo = "https://neuroconductor.org/releases/2019/12")
#'    options(repos = repos)
#'  }
#' \donttest{
#'    neuro_install("cifti", type = "source", lib = tlib)
#'    neuro_install("cifti",
#'    release_repo = latest_neuroc_release("stable"),
#'    lib = tlib)
#'
#'    neuro_install("cifti", release_repo = "github")
#' }
#'
neuro_install = function(
  repo,
  release = c("stable", "current"),
  release_repo = latest_neuroc_release(),
  upgrade_dependencies = FALSE,
  type = getOption("pkgType"),
  ...){

  #############################
  # Create a data.frame for merging
  #############################
  release = match.arg(release)

  l_repo = trimws(tolower(release_repo))

  if (!l_repo %in% "github") {
    args = list(...)
    repos = args$repos
    repos = c(Neuroconductor = release_repo,
              getOption("repos"),
              repos)
    args$repos = repos
    contriburl = utils::contrib.url(repos, type)
    repos = c(Neuroconductor = release_repo,
              getOption("repos"),
              repos)
    repos = repos[ repos != "@CRAN@" ]
    if ("contriburl" %in% names(args)) {
      args$contriburl = c(contriburl, args$contriburl)
    }
    repos = repos[!duplicated(repos)]
    args$contriburl = unique(args$contriburl)
    args$pkgs = repo
    # args$repos = repos
    args$type = type
    x = do.call(utils::install.packages, args = args)
    # x = install.packages(pkgs = repo,
    #                      repos = c(Neuroconductor = release_repo,
    #                                getOption("repos")),
    #                      type = type,
    #
    #                      ...)
    lib.loc = NULL
    if (!is.null(args$lib)) {
      lib.loc = args$lib
    }
    not_installed = repo[!repo %in% installed.packages(
      lib.loc = lib.loc
    )[, "Package"]]
    if (length(not_installed) > 0) {
      msg = paste0("Package(s): ", paste(not_installed, sep = ", "),
                   " released binaries/sources were not installed,",
                   " please try to install with release_repo = \"github\"")
      warning(msg)
    }
    return(invisible(x))
  }

  df = data.frame(repo = repo, stringsAsFactors = FALSE)

  tab = neuro_package_table(long = TRUE)
  tab = tab[ tab$release %in% release, ]

  ## import list of packages
  # error if pkg not in list of packages
  check_install = df$repo %in% tab$repo
  if (!all(check_install)) {
    bad_pkgs = df$repo[!check_install]
    bad_pkgs = paste(bad_pkgs, collapse = ", ")
    message(paste0("Available Packages on neuroconductor are ",
                   paste(unique(tab$repo), collapse = ", ")))
    stop(paste0("Package(s) ", bad_pkgs,
                " are not in neuroconductor"))
  }
  tab = merge(df, tab, by = "repo", all.x = TRUE)
  tab$version = numeric_version(tab$version)

  # pkg = tab$pkg
  # tab$commit_id = tab[, "commit_id"]
  tab = split(tab, tab$repo)
  tab = lapply(tab, function(x) {
    x$version = x[, "version"]
    max_version = max(x$version)
    x = x[ x$version %in% max_version,, drop = FALSE]
    return(x)
  })
  tab = do.call("rbind", tab)
  tab = data.frame(tab, stringsAsFactors = FALSE)
  tab$repo = paste0("neuroconductor/", tab$repo, "@", tab$commit_id)

  args = list(tab$repo)
  gh_func = devtools::install_github
  if ("upgrade" %in% methods::formalArgs(gh_func)) {
    args$upgrade = upgrade_dependencies
  } else {
    args$upgrade_dependencies = upgrade_dependencies
  }

  if (!upgrade_dependencies) {
    res = try({
      results = do.call(gh_func, args = args)
    })
    if (is.logical(results)) {
      check = any(!results)
    } else {
      check = FALSE
    }
    if (inherits(res, "try-error") || check) {
      stop("Installation failed, please try with upgrade_dependencies = TRUE")
    }
  } else {
    results = do.call(gh_func, args = args)
  }
}

#' @rdname neuro_install
#' @aliases neuroc_install
#' @aliases neuro_install
#' @export
neuroc_install = function(...) {
  neuro_install(...)
}

#' @rdname neuro_install
#' @aliases neurocLite
#' @export
neurocLite = function(...) {
  neuro_install(...)
}

