% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transmogrify.R
\name{transmogrifyMissions}
\alias{transmogrifyMissions}
\title{transmogrifyMissions}
\usage{
transmogrifyMissions(df = NULL)
}
\arguments{
\item{df}{default is \code{NULL}.  This is the data frame containing the usable fields from the 
cruise_data}
}
\description{
This function takes the fields from the andes cruise_data, and does the various 
processing necessary to make them equivalent to the formats used in the ESE_MISSIONS table
}
\seealso{
Other internal: 
\code{\link{charToBinary}()},
\code{\link{cleanEse}()},
\code{\link{cleanStrata}()},
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
\code{\link{transmogrifySets}()},
\code{\link{transmogrifySpecimens}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{internal}
