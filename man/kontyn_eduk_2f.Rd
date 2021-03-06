% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wskazniki_poziom_zagregowany_2rm_2f.R
\name{kontyn_eduk_2f}
\alias{kontyn_eduk_2f}
\title{Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym}
\usage{
kontyn_eduk_2f(x)
}
\arguments{
\item{x}{ramka danych ze wskaźnikami na poziomie indywidualnym}
}
\value{
lista
}
\description{
Funkcja obliczająca odsetki wybierania ścieżek dalszej nauki:
\itemize{
\item{"dowolna_forma"}{Odsetek absolwentów, którzy rozpoczęli naukę w
dowolnej formie (KN1_1 = 1 lub KN1_2 = 1 lub KN1_5 = 1)}
\item{"kkz"}{Odsetek, którzy rozpoczęli naukę na kwalifikacyjnym kursie
zawodowym (KN1_5 = 1)}
\item{"bs2"}{Odsetek absolwentów, którzy rozpoczęli naukę w szkole branżowej
drugiego stopnia (KN1_1 = 1)}
\item{"lic_dor"}{Odsetek absolwentów, którzy rozpoczęli naukę w liceum dla
dorosłych (KN1_2 = 1)}
\item{"studia"}{Odsetek absolwentów, którzy rozpoczęli naukę w szkole
wyższej (na studiach) (KN1_4 = 1)}
\item{"spolic"}{Odsetek absolwentów, którzy rozpoczęli naukę w szkole
policealnej (KN1_3 = 1)}
}
}
