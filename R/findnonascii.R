findNonASCII <- function(string) {
  # find non ASCII characters that are not part of the automatic conversion routine
  # make sure to sync with conv.R
  nonASCII <- gregexpr("[^\\x00-\\x7F|äöüÄÖÜß°“”„…—–]", string, perl = TRUE)[[1]]
  if (nonASCII[1]==-1) return(NULL)

  return(nonASCII)
}
