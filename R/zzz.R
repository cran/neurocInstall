#' @importFrom utils citation
.onAttach <- function(...) {
  if (!interactive()) return()

  x = citation("neurocInstall")
  x = format(x, "text")
  ack <- paste(
    'Cite the Neuroconductor Project using:\n',
    x)

  packageStartupMessage(paste(strwrap(ack), collapse = "\n"))
}