#' curlProgress
#'
#' Download a binary file from remote to local, with progress monitor
#'
#' @import RCurl
#'
#' @param url A remote URL
#' @param fname The local destination, including the file name and extension
#' @param width_fx (Optional) The console output message width; defaults to \code{0.8}
#'
#' @details
#' This is unabashedly taken from https://stackoverflow.com/a/21961094, and all credit is due.
#'
#' @seealso \code{\link{ftp_getBinPars}} for creating FTP parameters to pass to this function.
#'
#' @return The requested asset
#' @export
#'
#' @examples
#' \dontrun{
#' # ftp example
#' curlProgress(
#'   url = "ftp://user:pwd@.xyz.com/dir/myfile.zip",
#'   fname = "./local_dir/myfile.zip"
#' )
#' }
curlProgress=function(url, fname, width_fx = 0.8){
  f = CFILE(fname, mode="wb")
  width= getOption("width") * width_fx   # you can make here your line shorter/longer
  pcur=0
  ret=curlPerform(url=url, writedata=f@ref,  noprogress=FALSE,
                  progressfunction=function(down,up) pcur<<-.progressDown(down, up, pcur, width),
                  followlocation=TRUE)
  close(f)
  cat('\n Download', names(ret), '- Ret', ret, '\n') # is success?
}


NULL
### Callback function for curlPerform
.progressDown=function(down, up, pcur, width){
  total=as.numeric(down[1]) # Total size as passed from curlPerform
  cur=as.numeric(down[2])   # Current size as passed from curlPerform
  x=cur/total
  px= round(100 * x)
  ## if(!is.nan(x) &&  px>60) return(pcur) # Just to debug at 60%
  if(!is.nan(x) && px!=pcur){
    x= round(width * x)
    sc=rev(which(total> c(1024^0, 1024^1, 1024^2, 1024^3)))[1]-1
    lb=c('B', 'KB', 'MB', 'GB')[sc+1]
    cat(paste(c(
      "\r  |", rep.int(".", x), rep.int(" ", width - x),
      sprintf("| %g%s of %g%s %3d%%",round(cur/1024^sc, 2), lb, round(total/1024^sc, 2), lb, px)),
      collapse = ""))
    utils::flush.console() # if the outptut is buffered, it will go immediately to console
    return(px)
  }
  return(pcur)
}
