library(gutenbergr)
library(dplyr)

mirrorList <- c(
  'http://www.gutenberg.org/dirs/',
  'http://mirrors.xmission.com/gutenberg/',
  'http://gutenberg.readingroo.ms/',
  'http://aleph.gutenberg.org/',
  'https://gutenberg.pglaf.org/',
  'http://mirror.csclub.uwaterloo.ca/gutenberg/', 
  'http://eremita.di.uminho.pt/gutenberg/',
  'http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/',
  'ftp://mirrors.xmission.com/gutenberg/',
  'ftp://ftp.ibiblio.org/pub/docs/books/gutenberg/',
  'rsync://rsync.mirrorservice.org/gutenberg/',
  'ftp://aleph.gutenberg.org/', 
  'ftp://gutenberg.readingroo.ms/gutenberg/', 
  'ftp://gutenberg.pglaf.org', 
  'ftp://eremita.di.uminho.pt/pub/gutenberg/', 
  'rsync://gutenberg.pglaf.org/gutenberg', 
  'rsync://aleph.gutenberg.org/gutenberg/',
  'ftp://ftp.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/',
  'gopher://gutenberg.pglaf.org:70')

findMirror <- function() {
  
  for (i in mirrorList) {

    out <- tryCatch(
      {
        tempOutput <- gutenberg_download(c(15809), mirror = i) # Downloads a 21kb ebook
        rm(tempOutput)
        return(i)
      },
      error=function(e) {
      },
      warning=function(e){
      }
    )  
  }
}