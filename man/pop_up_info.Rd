% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pop_up_info.R
\name{pop_up_info}
\alias{pop_up_info}
\title{Pop up text when clicking to the gauge on leaflet map}
\usage{
pop_up_info(gauge_name, gauge_id, NSE, are_skm)
}
\arguments{
\item{gauge_name}{character, name of the gauge}

\item{gauge_id}{character, gauge id named after the CAMELS-DE data}

\item{NSE}{numeric, the performance of the LSTM for the test period for
this gauge}

\item{are_skm}{numeric, drainage area of this gauge in square kilometer}
}
\value{
information about the gauged, organized in a nice way
}
\description{
Pop up text when clicking to the gauge on leaflet map
}
\examples{

pop_up_info(gauge_name = "Achstetten", gauge_id = "DE110040",
            NSE = 0.882, are_skm = 273.0)

}
