% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Zlokarnik.R
\name{K.Zlokarnik}
\alias{K.Zlokarnik}
\title{Compute K and m from Zlokarnik-Korrelation}
\usage{
K.Zlokarnik(
  kLa,
  power,
  vol.rea,
  vel.gas,
  rho = 998,
  gforce = 9.81,
  eta = 0.724
)
}
\arguments{
\item{kLa}{kLa}

\item{power}{power input in Watt}

\item{vol.rea}{volume of the reactor vessel in m^3}

\item{vel.gas}{gas velocity in l/min}

\item{rho}{the density of the fermentation broth}

\item{gforce}{the g-force}

\item{eta}{the viscosity}
}
\value{
K and m
}
\description{
Compute K and m from Zlokarnik-Korrelation
}
