\name{NeoRun}
\alias{NeoRun}
\title{NeoRun backup data conversion}
\usage{
.res <- NeoRun_data(fname,Memo)
}
\description{
set fname as backup file
Run NeoRun_data and get the results.
.res$base :: basic information of the event
.res$Lap :: Get Lap data table
.res$Graph :: Graph plot related table
.res$GPS :: on the check. might have bug.
<<<<<<< HEAD
.res$Tag :: part of csv file as measured data
.res$Memo :: any memo
}
\examples{
library(NeoRun)
fname <- "inst/extdata/20170801-20170801.csv"
memo <- "Weekend Run"
.res <- NeoRun_data(fname,memo)
.res$base
.res$Lap
.res$Graph
.res$Tag
.res$Memo
}
\examples{
fname <- "20170801-20170801.csv"
.res <- NeoRun_data(fname)
.res$base
.res$Lap
.res$Graph
>>>>>>> 3c96888c859b22235d47b2e66ba410e1396f8d3b
}
