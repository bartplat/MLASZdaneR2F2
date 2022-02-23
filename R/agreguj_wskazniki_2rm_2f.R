#' @title Obliczanie wskaznikow z 2. fali 2. rundy monitoringu na poziomie
#' zagregowanym
#' @description Funkcja oblicza wartości wskaźników na poziomie zagregowanym
#' na podstawie ramki danych z wynikami ankiety CAWI z drugiej fali drugiej
#' rundy monitoringu (absolwenci).
#' @param wskazniki ramka danych z wynikami 2 rundy monitoringu
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{dane_szkoly_2f}},}
#'  \item{\code{\link{liczba_ucz_2f}},}
#'  \item{\code{\link{liczba_kobiet_2f}},}
#'  \item{\code{\link{liczba_kobiet_2f_waga}},}
#'  \item{\code{\link{liczba_abs_2f}},}
#'  \item{\code{\link{liczba_abs_2f_waga}},}
#'  \item{\code{\link{firma_badawcza_2f}},}
#'  \item{\code{\link{formy_2f}},}
#'  \item{\code{\link{zawod_liczebnosc_2f}},}
#'  \item{\code{\link{zawod_liczebnosc_2f_waga}},}
#'  \item{\code{\link{praca_zarobkowa_2f}},}
#'  \item{\code{\link{praca_zarobkowa_2f_waga}},}
#'  \item{\code{\link{praca_zarobkowa_mies_2f}},}
#'  \item{\code{\link{praca_zarobkowa_mies_2f_waga}},}
#'  \item{\code{\link{bezrobocie_2f}},}
#'  \item{\code{\link{bezrobocie_2f_waga}},}
#'  \item{\code{\link{bezrobocie_mies_2f}},}
#'  \item{\code{\link{bezrobocie_mies_2f_waga}},}
#'  \item{\code{\link{neet_2f}},}
#'  \item{\code{\link{neet_2f_waga}},}
#'  \item{\code{\link{neet_mies_2f}},}
#'  \item{\code{\link{neet_mies_2f_waga}},}
#'  \item{\code{\link{status_2020_10}},}
#'  \item{\code{\link{status_2020_10_waga}},}
#'  \item{\code{\link{kontyn_eduk_2f}},}
#'  \item{\code{\link{kontyn_eduk_2f_waga}},}
#'  \item{\code{\link{praca_zarob_2f}},}
#'  \item{\code{\link{praca_zarob_2f_waga}},}
#'  \item{\code{\link{praca_zarob_zawod_2f}},}
#'  \item{\code{\link{praca_zarob_zawod_2f_waga}},}
#'  \item{\code{\link{praca_zarob_branza_2f}},}
#'  \item{\code{\link{praca_zarob_branza_2f_waga}},}
#'  \item{\code{\link{praca_zad_ogolnie_2f}},}
#'  \item{\code{\link{praca_zad_ogolnie_2f_waga}},}
#'  \item{\code{\link{praca_zad_rozwoj_2f}},}
#'  \item{\code{\link{praca_zad_rozwoj_2f_waga}},}
#'  \item{\code{\link{praca_zad_zycie_2f}},}
#'  \item{\code{\link{praca_zad_zycie_2f_waga}},}
#'  \item{\code{\link{praca_zad_zarobki_2f}},}
#'  \item{\code{\link{praca_zad_zarobki_2f_waga}},}
#'  \item{\code{\link{praca_zad_pewnosc_2f}},}
#'  \item{\code{\link{praca_zad_pewnosc_2f_waga}},}
#'  \item{\code{\link{czas_przed_ogolne_2f}},}
#'  \item{\code{\link{czas_przed_ogolne_2f_waga}},}
#'  \item{\code{\link{czas_przedzaw_teo_2f}},}
#'  \item{\code{\link{czas_przedzaw_teo_2f_waga}},}
#'  \item{\code{\link{czas_przedzaw_prak_2f}},}
#'  \item{\code{\link{czas_przedzaw_prak_2f_waga}},}
#'  \item{\code{\link{war_internet_2f}},}
#'  \item{\code{\link{war_internet_2f_waga}},}
#'  \item{\code{\link{war_komp_2f}},}
#'  \item{\code{\link{war_komp_2f_waga}},}
#'  \item{\code{\link{war_miejsce_2f}},}
#'  \item{\code{\link{war_miejsce_2f_waga}},}
#'  \item{\code{\link{war_pomoc_2f}},}
#'  \item{\code{\link{war_pomoc_2f_waga}}}
#' }
#' @export
#' @importFrom dplyr .data
agreguj_cawi_ucz_2rm_2f = function(wskazniki, grupy) {
  stopifnot(is.data.frame(wskazniki),
            is.data.frame(grupy))
  # nazwy = c("B0", "szk_adres", "PLC", "Wykonawca", "B1", "wojewodztwo", "B2",
  #           "PR1_2_1", "PR1_2_2", "PR1_2_3", "PR1_5_1", "PR1_5_2", "PR1_5_3",
  #           "PR1_1_1", "PR1_2_1", "PR1_3_1", "PR1_4_1", "PR1_1_2", "PR1_2_2",
  #           "PR1_3_2", "PR1_4_2", "PR1_1_3", "PR1_2_3", "PR1_3_3", "PR1_4_3",
  #           "KN1_1", "KN1_2", "KN1_3", "KN1_4", "KN1_5", "PR0", "PR4_1",
  #           "PR4_2", "PR7_1", "PR7_2", "PR7_3", "PR7_4", "PR7_5", "E5_1",
  #           "E5_2", "E5_3", "E6_1", "E6_2", "E6_3", "E6_4", "n_ucz")
  # sprawdz_nazwy(names(wskazniki), nazwy)

  wskazniki = agreguj_wskazniki(
    wskazniki, grupy,
    dane_szkoly_2f = dane_szkoly_2f(.data),
    liczba_ucz_2f = liczba_ucz_2f(.data),
    liczba_kobiet_2f = liczba_kobiet_2f(.data),
    liczba_kobiet_2f_waga = liczba_kobiet_2f_waga(.data),
    liczba_abs_2f = liczba_abs_2f(.data),
    liczba_abs_2f_waga = liczba_abs_2f_waga(.data),
    firma_badawcza_2f = firma_badawcza_2f(.data),
    formy_2f = formy_2f(.data),
    zawod_liczebnosc_2f = zawod_liczebnosc_2f(.data),
    zawod_liczebnosc_2f_waga = zawod_liczebnosc_2f_waga(.data),
    praca_zarobkowa_2f = praca_zarobkowa_2f(.data),
    praca_zarobkowa_2f_waga = praca_zarobkowa_2f_waga(.data),
    praca_zarobkowa_mies_2f = praca_zarobkowa_mies_2f(.data),
    praca_zarobkowa_mies_2f_waga = praca_zarobkowa_mies_2f_waga(.data),
    bezrobocie_2f = bezrobocie_2f(.data),
    bezrobocie_2f_waga = bezrobocie_2f_waga(.data),
    bezrobocie_mies_2f = bezrobocie_mies_2f(.data),
    bezrobocie_mies_2f_waga = bezrobocie_mies_2f_waga(.data),
    neet_2f = neet_2f(.data),
    neet_2f_waga = neet_2f_waga(.data),
    neet_mies_2f = neet_mies_2f(.data),
    neet_mies_2f_waga = neet_mies_2f_waga(.data),
    status_2020_10 = status_2020_10(.data),
    status_2020_10_waga = status_2020_10_waga(.data),
    kontyn_eduk_2f = kontyn_eduk_2f(.data),
    kontyn_eduk_2f_waga = kontyn_eduk_2f_waga(.data),
    praca_zarob_2f = praca_zarob_2f(.data),
    praca_zarob_2f_waga = praca_zarob_2f_waga(.data),
    praca_zarob_zawod_2f = praca_zarob_zawod_2f(.data),
    praca_zarob_zawod_2f_waga = praca_zarob_zawod_2f_waga(.data),
    praca_zarob_branza_2f = praca_zarob_branza_2f(.data),
    praca_zarob_branza_2f_waga = praca_zarob_branza_2f_waga(.data),
    praca_zad_ogolnie_2f = praca_zad_ogolnie_2f(.data),
    praca_zad_ogolnie_2f_waga = praca_zad_ogolnie_2f_waga(.data),
    praca_zad_rozwoj_2f = praca_zad_rozwoj_2f(.data),
    praca_zad_rozwoj_2f_waga = praca_zad_rozwoj_2f_waga(.data),
    praca_zad_zycie_2f = praca_zad_zycie_2f(.data),
    praca_zad_zycie_2f_waga = praca_zad_zycie_2f_waga(.data),
    praca_zad_zarobki_2f = praca_zad_zarobki_2f(.data),
    praca_zad_zarobki_2f_waga = praca_zad_zarobki_2f_waga(.data),
    praca_zad_pewnosc_2f = praca_zad_pewnosc_2f(.data),
    praca_zad_pewnosc_2f_waga = praca_zad_pewnosc_2f_waga(.data),
    czas_przed_ogolne_2f = czas_przed_ogolne_2f(.data),
    czas_przed_ogolne_2f_waga = czas_przed_ogolne_2f_waga(.data),
    czas_przedzaw_teo_2f = czas_przedzaw_teo_2f(.data),
    czas_przedzaw_teo_2f_waga = czas_przedzaw_teo_2f_waga(.data),
    czas_przedzaw_prak_2f = czas_przedzaw_prak_2f(.data),
    czas_przedzaw_prak_2f_waga = czas_przedzaw_prak_2f_waga(.data),
    war_internet_2f = war_internet_2f(.data),
    war_internet_2f_waga = war_internet_2f_waga(.data),
    war_komp_2f = war_komp_2f(.data),
    war_komp_2f_waga = war_komp_2f_waga(.data),
    war_miejsce_2f = war_miejsce_2f(.data),
    war_miejsce_2f_waga = war_miejsce_2f_waga(.data),
    war_pomoc_2f = war_pomoc_2f(.data),
    war_pomoc_2f_waga = war_pomoc_2f_waga(.data)
  )
  return(wskazniki)
}
