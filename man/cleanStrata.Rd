% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{cleanStrata}
\alias{cleanStrata}
\title{cleanStrata}
\usage{
cleanStrata(x)
}
\arguments{
\item{x}{default is \code{NULL}.  This is a field in a data frame}
}
\description{
This function takes the strata field, and ensures only the first 3 characters are 
returned
}
\seealso{
Other internal: 
\code{\link{charToBinary}()},
\code{\link{cleanEse}()},
\code{\link{cleanfields}()},
\code{\link{getEseTables}()},
\code{\link{get_value}()},
\code{\link{keepFieldsBaskets}()},
\code{\link{keepFieldsCatches}()},
\code{\link{keepFieldsMissions}()},
\code{\link{keepFieldsSets}()},
\code{\link{keepFieldsSpecimens}()},
\code{\link{meters2Fathoms}()},
\code{\link{populate_DATA_DESC}()},
\code{\link{reFormatSpecimen}()},
\code{\link{redistributeMixedCatch2}()},
\code{\link{transmogrifyBaskets}()},
\code{\link{transmogrifyCatches}()},
\code{\link{transmogrifyLV1_OBS}()},
\code{\link{transmogrifyMissions}()},
\code{\link{transmogrifySets}()},
\code{\link{transmogrifySpecimens}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{internal}
