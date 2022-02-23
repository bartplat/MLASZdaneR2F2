#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja przechowująca nazwę i adres szkoły jako listę.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
dane_szkoly_2f = function(x) {
  list(
    `nazwa szkoły` = unique(trimws(x$B0)),
    `adres szkoły` = unique(x$szk_adres)
  ) %>% return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę uczniów, do których
#' zostało wysłane zaproszenie do wypełnienia ankiety, czyli de facto liczbę
#' uczniów ostatnich klas badanych szkół w roku szkolnym 2019/2020.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' @importFrom dplyr summarise pull
liczba_ucz_2f = function(x) {
  x %>%
    summarise(liczba_ucz = unique(n_ucz)) %>%
    pull(liczba_ucz) %>%
    sum(na.rm = TRUE) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych absolwentów
#' w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
liczba_abs_2f = function(x) {
  return(sum(x$PLC %in% c(1, 2), na.rm = TRUE))
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{liczba_abs_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia
#' (np. województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' @importFrom dplyr %>% count pull
liczba_abs_2f_waga = function(x) {
  x %>%
    filter(PLC %in% c(1, 2)) %>%
    count(PLC, wt = waga_tuk_2f) %>%
    pull(n) %>%
    sum(na.rm = TRUE) %>%
    round() %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych kobiet w
#' grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
liczba_kobiet_2f = function(x) {
  return(sum(x$PLC %in% 1, na.rm = TRUE))
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{liczba_kobiet_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia
#' (np. województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' #' @importFrom dplyr %>% filter count pull
liczba_kobiet_2f_waga = function(x) {
  x %>%
    filter(PLC %in% 1) %>%
    count(PLC, wt = waga_tuk_2f) %>%
    pull(n) %>%
    sum(na.rm = TRUE) %>%
    round() %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja przechowująca informację o nazwie firmy realizującej
#' badanie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return ciąg znaków
#' @importFrom dplyr %>% .data filter case_when summarise
firma_badawcza_2f = function(x) {
  x = x %>%
    filter(!(Wykonawca %in% 0)) %>%
    summarise(Firma_nazwa = case_when(all(.data$Wykonawca %in% 1) ~ "PBS sp. z o.o.",
                                      all(.data$Wykonawca %in% 2) ~ "Danae sp. z o.o."))
  return(x$Firma_nazwa)
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja przechowująca listę form gramatycznych różnych słów lub
#' wyrażeń, które pojawiają się w raporcie w zależności od typu szkoły lub
#' liczby obserwacji.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% .data case_when distinct mutate select
formy_2f = function(x) {
  x = x %>%
    select("B1", "wojewodztwo") %>%
    distinct() %>%
    mutate(szkola_mian = case_when(.data$B1 %in% 1 ~ "szkoła branżowa pierwszego stopnia",
                                   .data$B1 %in% 2 ~ "technikum",
                                   .data$B1 %in% 3 ~ "szkoła policealna"),
           szkola_dop = case_when(.data$B1 %in% 1 ~ "szkoły branżowej pierwszego stopnia",
                                  .data$B1 %in% 2 ~ "technikum",
                                  .data$B1 %in% 3 ~ "szkoły policealnej"),
           szkola_lmn = case_when(.data$B1 %in% 1 ~ "szkół branżowych pierwszego stopnia",
                                  .data$B1 %in% 2 ~ "techników",
                                  .data$B1 %in% 3 ~ "szkół policealnych"),
           wypelnil_o = case_when(nrow(x) %in% 1 ~ "wypełnił",
                                  TRUE ~ "wypełniło"),
           aboslwent_ow = case_when(nrow(x) %in% 1 ~ "absolwent",
                                    TRUE ~ "absolwentów"),
           woj_nazwa_dop = case_when(length(unique(.data$wojewodztwo)) %in% 1 ~ paste0(.data$wojewodztwo, "go"),
                                     TRUE ~ "NA")
    ) %>%
    select(-c("B1")) %>%
    distinct() %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca liczbę absolwentów danego zawodu. Ponadto,
#' dodana jest informacja o branży, do której należy zawód.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% select distinct left_join rename count
zawod_liczebnosc_2f = function(x) {
  n = list(n = sum(!is.na(x$B2), na.rm = TRUE))

  mapp = x %>%
    select(B3, B2) %>%
    filter(!(is.na(B3) | is.na(B2))) %>%
    distinct()

  zaw = x %>%
    count(B2) %>%
    rename(zawod = B2, freq = n)

  zaw_tab = mapp %>%
    left_join(zaw, by = c("B2" = "zawod")) %>%
    rename(branza = B3, zawod = B2, n_zaw = freq) %>%
    as.list()

  return(c(n, zaw_tab))
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{zawod_liczebnosc_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia
#' (np. województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% select distinct left_join rename count mutate
zawod_liczebnosc_2f_waga = function(x) {
  n = list(n = sum(as.numeric(!is.na(x$B2)) * x$waga_tuk_2f, na.rm = TRUE))

  mapp = x %>%
    select(B3, B2) %>%
    filter(!(is.na(B3) | is.na(B2))) %>%
    distinct()

  zaw = x %>%
    count(B2, wt = waga_tuk_2f) %>%
    rename(zawod = B2, freq = n) %>%
    mutate(freq = ifelse(round(freq) %in% 0, 1, round(freq)))

  zaw_tab = mapp %>%
    left_join(zaw, by = c("B2" = "zawod")) %>%
    rename(branza = B3, zawod = B2, n_zaw = freq) %>%
    as.list()

  return(c(n, zaw_tab))
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów pracujących zarobkowo w
#' okresie \strong{lipiec-wrzesień 2020} na podstawie pytania \emph{PR1. Proszę
#' spojrzeć na poniższy kalendarz i zaznaczyć miesiące, w których w sumie przez
#' co najmniej dwa tygodnie (bez względu na liczbę godzin w tygodniu) -
#' pracował(a) Pan(i) zarobkowo} - bez podziału na miesiące
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarobkowa_2f = function(x) {
  list(
    etykieta = "odsetek pracujących zarobkowo na podstawie PR1 - lipiec-wrzesień",
    n = sum(x$PR1_2_1 %in% c(1, 2) |
              x$PR1_2_2 %in% c(1, 2) |
              x$PR1_2_3 %in% c(1, 2), na.rm = TRUE),
    ods_prac_zar_ever = sum(x$PR1_2_1 %in% 1 |
                              x$PR1_2_2 %in% 1 |
                              x$PR1_2_3 %in% 1, na.rm = TRUE) /
      sum(x$PR1_2_1 %in% c(1, 2) |
            x$PR1_2_2 %in% c(1, 2) |
            x$PR1_2_3 %in% c(1, 2), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zarobkowa_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia
#' (np. województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarobkowa_2f_waga = function(x) {
  list(
    etykieta = "odsetek pracujących zarobkowo na podstawie PR1 - lipiec-wrzesień - WAŻONE",
    n = sum(as.numeric(
      x$PR1_2_1 %in% c(1, 2) |
        x$PR1_2_2 %in% c(1, 2) |
        x$PR1_2_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    ods_prac_zar_ever = sum(as.numeric(
      x$PR1_2_1 %in% 1 |
        x$PR1_2_2 %in% 1 |
        x$PR1_2_3 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_2_1 %in% c(1, 2) |
          x$PR1_2_2 %in% c(1, 2) |
          x$PR1_2_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów pracujących zarobkowo w
#' okresie \strong{lipiec-wrzesień 2020} na podstawie pytania \emph{PR1. Proszę
#' spojrzeć na poniższy kalendarz i zaznaczyć miesiące, w których w sumie przez
#' co najmniej dwa tygodnie (bez względu na liczbę godzin w tygodniu) -
#' pracował(a) Pan(i) zarobkowo} - w podziale na miesiące.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarobkowa_mies_2f = function(x) {
  list(
    etykieta = "odsetek pracujących zarobkowo na podstawie PR1 - podział na miesiące",
    n = sum(x$PR1_2_1 %in% c(1, 2) |
              x$PR1_2_2 %in% c(1, 2) |
              x$PR1_2_3 %in% c(1, 2), na.rm = TRUE),
    ods_prac_zar_07 = sum(x$PR1_2_1 %in% 1, na.rm = TRUE) /
      sum(x$PR1_2_1 %in% c(1, 2), na.rm = TRUE),
    ods_prac_zar_08 = sum(x$PR1_2_2 %in% 1, na.rm = TRUE) /
      sum(x$PR1_2_2 %in% c(1, 2), na.rm = TRUE),
    ods_prac_zar_09 = sum(x$PR1_2_3 %in% 1, na.rm = TRUE) /
      sum(x$PR1_2_3 %in% c(1, 2), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zarobkowa_mies_2f}, ale
#' zwraca ważone wartości na potrzeby prezentacji danych w grupie odniesienia
#' (np. województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarobkowa_mies_2f_waga = function(x) {
  list(
    etykieta = "odsetek pracujących zarobkowo na podstawie PR1 - podział na miesiące - WAŻONE",
    n = sum(as.numeric(x$PR1_2_1 %in% c(1, 2) |
              x$PR1_2_2 %in% c(1, 2) |
              x$PR1_2_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    ods_prac_zar_07 = sum(as.numeric(x$PR1_2_1 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum((x$PR1_2_1 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    ods_prac_zar_08 = sum(as.numeric(x$PR1_2_2 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum((x$PR1_2_2 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    ods_prac_zar_09 = sum(as.numeric(x$PR1_2_3 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum((x$PR1_2_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów zarejestrowanych w
#' urzędzie pracy jako bezrobotni w okresie \strong{lipiec-wrzesień 2020} na
#' podstawie pytania \emph{PR1. Proszę spojrzeć na poniższy kalendarz i
#' zaznaczyć miesiące, w których w sumie przez co najmniej dwa tygodnie (bez
#' względu na liczbę godzin w tygodniu) - był(a) Pan(i) zarejestrowany(a) w
#' urzędzie pracy jako osoba bezrobotna} - bez podziału na miesiące
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
bezrobocie_2f = function(x) {
  list(
    etykieta = "bezrobotni na podstawie PR1 - lipiec-wrzesień",
    n = sum(x$PR1_5_1 %in% c(1, 2) |
              x$PR1_5_2 %in% c(1, 2) |
              x$PR1_5_3 %in% c(1, 2), na.rm = TRUE),
    bezrob_ever = sum(x$PR1_5_1 %in% 1 |
                        x$PR1_5_2 %in% 1 |
                        x$PR1_5_3 %in% 1, na.rm = TRUE) /
      sum(x$PR1_5_1 %in% c(1, 2) |
            x$PR1_5_2 %in% c(1, 2) |
            x$PR1_5_3 %in% c(1, 2), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{bezrobocie_2f}, ale zwraca ważone
#' wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
bezrobocie_2f_waga = function(x) {
  list(
    etykieta = "bezrobotni na podstawie PR1 - lipiec-wrzesień - WAŻONE",
    n = sum(as.numeric(
      x$PR1_5_1 %in% c(1, 2) |
        x$PR1_5_2 %in% c(1, 2) |
        x$PR1_5_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    bezrob_ever = sum(as.numeric(
      x$PR1_5_1 %in% 1 |
        x$PR1_5_2 %in% 1 |
        x$PR1_5_3 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_5_1 %in% c(1, 2) |
          x$PR1_5_2 %in% c(1, 2) |
          x$PR1_5_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów zarejestrowanych w
#' urzędzie pracy jako bezrobotni w okresie \strong{lipiec-wrzesień 2020} na
#' podstawie pytania \emph{PR1. Proszę spojrzeć na poniższy kalendarz i
#' zaznaczyć miesiące, w których w sumie przez co najmniej dwa tygodnie (bez
#' względu na liczbę godzin w tygodniu) - był(a) Pan(i) zarejestrowany(a) w
#' urzędzie pracy jako osoba bezrobotna} - w podziale na miesiące.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
bezrobocie_mies_2f = function(x) {
  list(
    etykieta = "bezrobotni na podstawie PR1 - podział na miesiące",
    n = sum(x$PR1_5_1 %in% c(1, 2) |
              x$PR1_5_2 %in% c(1, 2) |
              x$PR1_5_3 %in% c(1, 2), na.rm = TRUE),
    bezrob_07 = sum(x$PR1_5_1 %in% 1, na.rm = TRUE) /
      sum(x$PR1_5_1 %in% c(1, 2), na.rm = TRUE),
    bezrob_08 = sum(x$PR1_5_2 %in% 1, na.rm = TRUE) /
      sum(x$PR1_5_2 %in% c(1, 2), na.rm = TRUE),
    bezrob_09 = sum(x$PR1_5_3 %in% 1, na.rm = TRUE) /
      sum(x$PR1_5_3 %in% c(1, 2), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{bezrobocie_mies_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia
#' (np. województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
bezrobocie_mies_2f_waga = function(x) {
  list(
    etykieta = "bezrobotni na podstawie PR1 - podział na miesiące - WAŻONE",
    n = sum(as.numeric(
      x$PR1_5_1 %in% c(1, 2) |
        x$PR1_5_2 %in% c(1, 2) |
        x$PR1_5_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    bezrob_07 = sum(as.numeric(
      x$PR1_5_1 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_5_1 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    bezrob_08 = sum(as.numeric(
      x$PR1_5_2 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_5_2 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    bezrob_09 = sum(as.numeric(
      x$PR1_5_3 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_5_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek biernych zawodowo wśród absolwentów.
#' Bierni zawodowo definiowani są jako absolwenci, którzy w  okresie od lipca do
#' września nie mieli żadnego epizodu nauki, pracy, praktyk ani poszukiwania
#' pracy. Obliczone na podstawie pytania \emph{PR1}, a konkretnie podunktów:
#' \emph{uczył(a) się Pan(i) w szkole, na kursie lub na studiach},
#' \emph{pracował(a) Pan(i) zarobkowo}, \emph{odbywała(a) Pan(i) bezpłatne
#' praktyki, staż lub wolontariat} oraz \emph{poszukiwał(a) Pan(i) pracy
#' zarobkowej}
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
neet_2f = function(x) {
  list(
    etykieta = "NEET lipiec - wrzesień",
    n = sum(x$PR1_1_1 %in% c(1, 2) &
              x$PR1_2_1 %in% c(1, 2) &
              x$PR1_3_1 %in% c(1, 2) &
              x$PR1_4_1 %in% c(1, 2), na.rm = TRUE),
    neet_ever = sum(x$PR1_1_1 %in% 2 &
                      x$PR1_2_1 %in% 2 &
                      x$PR1_3_1 %in% 2 &
                      x$PR1_4_1 %in% 2 &
                      x$PR1_1_2 %in% 2 &
                      x$PR1_2_2 %in% 2 &
                      x$PR1_3_2 %in% 2 &
                      x$PR1_4_2 %in% 2 &
                      x$PR1_1_3 %in% 2 &
                      x$PR1_2_3 %in% 2 &
                      x$PR1_3_3 %in% 2 &
                      x$PR1_4_3 %in% 2, na.rm = TRUE) /
      sum(x$PR1_1_1 %in% c(1, 2) &
            x$PR1_2_1 %in% c(1, 2) &
            x$PR1_3_1 %in% c(1, 2) &
            x$PR1_4_1 %in% c(1, 2) &
            x$PR1_1_2 %in% c(1, 2) &
            x$PR1_2_2 %in% c(1, 2) &
            x$PR1_3_2 %in% c(1, 2) &
            x$PR1_4_2 %in% c(1, 2) &
            x$PR1_1_3 %in% c(1, 2) &
            x$PR1_2_3 %in% c(1, 2) &
            x$PR1_3_3 %in% c(1, 2) &
            x$PR1_4_3 %in% c(1, 2), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{neet_2f}, ale zwraca ważone
#' wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
neet_2f_waga = function(x) {
  list(
    etykieta = "NEET lipiec - wrzesień - WAŻONE",
    n = sum(as.numeric(
      x$PR1_1_1 %in% c(1, 2) &
        x$PR1_2_1 %in% c(1, 2) &
        x$PR1_3_1 %in% c(1, 2) &
        x$PR1_4_1 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    neet_ever = sum(as.numeric(
      x$PR1_1_1 %in% 2 &
        x$PR1_2_1 %in% 2 &
        x$PR1_3_1 %in% 2 &
        x$PR1_4_1 %in% 2 &
        x$PR1_1_2 %in% 2 &
        x$PR1_2_2 %in% 2 &
        x$PR1_3_2 %in% 2 &
        x$PR1_4_2 %in% 2 &
        x$PR1_1_3 %in% 2 &
        x$PR1_2_3 %in% 2 &
        x$PR1_3_3 %in% 2 &
        x$PR1_4_3 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_1_1 %in% c(1, 2) &
          x$PR1_2_1 %in% c(1, 2) &
          x$PR1_3_1 %in% c(1, 2) &
          x$PR1_4_1 %in% c(1, 2) &
          x$PR1_1_2 %in% c(1, 2) &
          x$PR1_2_2 %in% c(1, 2) &
          x$PR1_3_2 %in% c(1, 2) &
          x$PR1_4_2 %in% c(1, 2) &
          x$PR1_1_3 %in% c(1, 2) &
          x$PR1_2_3 %in% c(1, 2) &
          x$PR1_3_3 %in% c(1, 2) &
          x$PR1_4_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek biernych zawodowo wśród absolwentów.
#' Bierni zawodowo definiowani są jako absolwenci, którzy w  okresie od lipca do
#' września nie mieli żadnego epizodu nauki, pracy, praktyk ani poszukiwania
#' pracy. Obliczone na podstawie pytania \emph{PR1}, a konkretnie podunktów:
#' \emph{uczył(a) się Pan(i) w szkole, na kursie lub na studiach},
#' \emph{pracował(a) Pan(i) zarobkowo}, \emph{odbywała(a) Pan(i) bezpłatne
#' praktyki, staż lub wolontariat} oraz \emph{poszukiwał(a) Pan(i) pracy
#' zarobkowej} - w podziale na miesiące
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
neet_mies_2f = function(x) {
  list(
    etykieta = "NEET lipiec - wrzesień - w podziale na miesiące",
    n = sum(x$PR1_1_1 %in% c(1, 2) &
              x$PR1_2_1 %in% c(1, 2) &
              x$PR1_3_1 %in% c(1, 2) &
              x$PR1_4_1 %in% c(1, 2), na.rm = TRUE),
    neet_07 = sum(x$PR1_1_1 %in% 2 &
                    x$PR1_2_1 %in% 2 &
                    x$PR1_3_1 %in% 2 &
                    x$PR1_4_1 %in% 2 , na.rm = TRUE) /
      sum(x$PR1_1_1 %in% c(1, 2) &
            x$PR1_2_1 %in% c(1, 2) &
            x$PR1_3_1 %in% c(1, 2) &
            x$PR1_4_1 %in% c(1, 2), na.rm = TRUE),
    neet_08 = sum(x$PR1_1_2 %in% 2 &
                    x$PR1_2_2 %in% 2 &
                    x$PR1_3_2 %in% 2 &
                    x$PR1_4_2 %in% 2 , na.rm = TRUE) /
      sum(x$PR1_1_2 %in% c(1, 2) &
            x$PR1_2_2 %in% c(1, 2) &
            x$PR1_3_2 %in% c(1, 2) &
            x$PR1_4_2 %in% c(1, 2), na.rm = TRUE),
    neet_09 = sum(x$PR1_1_3 %in% 2 &
                    x$PR1_2_3 %in% 2 &
                    x$PR1_3_3 %in% 2 &
                    x$PR1_4_3 %in% 2 , na.rm = TRUE) /
      sum(x$PR1_1_3 %in% c(1, 2) &
            x$PR1_2_3 %in% c(1, 2) &
            x$PR1_3_3 %in% c(1, 2) &
            x$PR1_4_3 %in% c(1, 2), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{neet_mies_2f}, ale zwraca ważone
#' wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
neet_mies_2f_waga = function(x) {
  list(
    etykieta = "NEET lipiec - wrzesień - w podziale na miesiące - WAŻONE",
    n = sum(as.numeric(
      x$PR1_1_1 %in% c(1, 2) &
        x$PR1_2_1 %in% c(1, 2) &
        x$PR1_3_1 %in% c(1, 2) &
        x$PR1_4_1 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    neet_07 = sum(as.numeric(
      x$PR1_1_1 %in% 2 &
        x$PR1_2_1 %in% 2 &
        x$PR1_3_1 %in% 2 &
        x$PR1_4_1 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_1_1 %in% c(1, 2) &
          x$PR1_2_1 %in% c(1, 2) &
          x$PR1_3_1 %in% c(1, 2) &
          x$PR1_4_1 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    neet_08 = sum(as.numeric(
      x$PR1_1_2 %in% 2 &
        x$PR1_2_2 %in% 2 &
        x$PR1_3_2 %in% 2 &
        x$PR1_4_2 %in% 2) * x$waga_tuk_2f , na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_1_2 %in% c(1, 2) &
          x$PR1_2_2 %in% c(1, 2) &
          x$PR1_3_2 %in% c(1, 2) &
          x$PR1_4_2 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    neet_09 = sum(as.numeric(
      x$PR1_1_3 %in% 2 &
        x$PR1_2_3 %in% 2 &
        x$PR1_3_3 %in% 2 &
        x$PR1_4_3 %in% 2) * x$waga_tuk_2f , na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_1_3 %in% c(1, 2) &
          x$PR1_2_3 %in% c(1, 2) &
          x$PR1_3_3 %in% c(1, 2) &
          x$PR1_4_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca informację o sytuacji
#' edukacyjno-zawodowej absolwentów w \strong{październiku} 2020 roku:
#' \itemize{
#'  \item{"tylko pracował"}{PR1_1=1 & PR1_2=2}
#'  \item{"tylko uczył się"}{PR1_1=2 & PR1_2=1}
#'  \item{"uczył się i pracował"}{PR1_1=1 & PR1_2=1}
#'  \item{"nie uczył się ani nie pracował"}{PR1_1=2 & PR1_2=2}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
status_2020_10 = function(x) {
  list(
    etykieta = "STATUS_2020_10",
    n = sum(x$PR1_1_4 %in% c(1, 2) |
              x$PR1_2_4 %in% c(1, 2) |
              x$PR1_3_4 %in% c(1, 2) |
              x$PR1_4_4 %in% c(1, 2), na.rm = TRUE),
    tylko_praca = sum(x$PR1_1_4 %in% 2 & x$PR1_2_4 %in% 1, na.rm = TRUE) / sum(x$PR1_1_4 %in% c(1, 2), na.rm = TRUE),
    tylko_nauka = sum(x$PR1_1_4 %in% 1 & x$PR1_2_4 %in% 2, na.rm = TRUE) / sum(x$PR1_1_4 %in% c(1, 2), na.rm = TRUE),
    nauka_praca = sum(x$PR1_1_4 %in% 1 & x$PR1_2_4 %in% 1, na.rm = TRUE) / sum(x$PR1_1_4 %in% c(1, 2), na.rm = TRUE),
    brak_nauka_brak_praca = sum(x$PR1_1_4 %in% 2 & x$PR1_2_4 %in% 2, na.rm = TRUE) / sum(x$PR1_1_4 %in% c(1, 2), na.rm = TRUE),
    nie_dotyczy_liczba = sum(x$PR1_1_4 %in% 3 & x$PR1_2_4 %in% 3, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{status_2020_10}, ale zwraca ważone
#' wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
status_2020_10_waga = function(x) {
  list(
    etykieta = "STATUS_2020_10 - WAŻONE",
    n = sum(as.numeric(
      x$PR1_1_4 %in% c(1, 2) |
        x$PR1_2_4 %in% c(1, 2) |
        x$PR1_3_4 %in% c(1, 2) |
        x$PR1_4_4 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    tylko_praca = sum(as.numeric(
      x$PR1_1_4 %in% 2 & x$PR1_2_4 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_1_4 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    tylko_nauka = sum(as.numeric(
      x$PR1_1_4 %in% 1 & x$PR1_2_4 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_1_4 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    nauka_praca = sum(as.numeric(
      x$PR1_1_4 %in% 1 & x$PR1_2_4 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_1_4 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    brak_nauka_brak_praca = sum(as.numeric(
      x$PR1_1_4 %in% 2 & x$PR1_2_4 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR1_1_4 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    nie_dotyczy_liczba = sum(as.numeric(
      x$PR1_1_4 %in% 3 & x$PR1_2_4 %in% 3) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetki wybierania ścieżek dalszej nauki:
#' \itemize{
#'  \item{"dowolna_forma"}{Odsetek absolwentów, którzy rozpoczęli naukę w
#'  dowolnej formie (KN1_1 = 1 lub KN1_2 = 1 lub KN1_5 = 1)}
#'  \item{"kkz"}{Odsetek, którzy rozpoczęli naukę na kwalifikacyjnym kursie
#'  zawodowym (KN1_5 = 1)}
#'  \item{"bs2"}{Odsetek absolwentów, którzy rozpoczęli naukę w szkole branżowej
#'  drugiego stopnia (KN1_1 = 1)}
#'  \item{"lic_dor"}{Odsetek absolwentów, którzy rozpoczęli naukę w liceum dla
#'  dorosłych (KN1_2 = 1)}
#'  \item{"studia"}{Odsetek absolwentów, którzy rozpoczęli naukę w szkole
#'  wyższej (na studiach) (KN1_4 = 1)}
#'  \item{"spolic"}{Odsetek absolwentów, którzy rozpoczęli naukę w szkole
#'  policealnej (KN1_3 = 1)}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
kontyn_eduk_2f = function(x) {
  list(
    etykieta = "dalsze ścieżki nauki",

    n_all = sum((x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)), na.rm = TRUE),
    n_bs1 = sum((x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_5 %in% c(1:3)), na.rm = TRUE),
    n_tech_spolic = sum((x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)), na.rm = TRUE),

    dowolna_forma = sum(x$KN1_1 %in% 1 | x$KN1_2 %in% 1 | x$KN1_3 %in% 1 | x$KN1_4 %in% 1 | x$KN1_5 %in% 1, na.rm = TRUE) / sum((x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)), na.rm = TRUE),
    bs2 = sum(x$KN1_1 %in% 1, na.rm = TRUE) / sum((x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_5 %in% c(1:3)), na.rm = TRUE),
    lic_dor = sum(x$KN1_2 %in% 1, na.rm = TRUE) / sum((x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_5 %in% c(1:3)), na.rm = TRUE),
    spolic = sum(x$KN1_3 %in% 1, na.rm = TRUE) / sum((x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)), na.rm = TRUE),
    studia = sum(x$KN1_4 %in% 1, na.rm = TRUE) / sum((x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)), na.rm = TRUE),
    kkz = sum(x$KN1_5 %in% 1, na.rm = TRUE) / sum((x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{kontyn_eduk_2f}, ale zwraca ważone
#' wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
kontyn_eduk_2f_waga = function(x) {
  list(
    etykieta = "dalsze ścieżki nauki - ważone",

    n_all = sum(as.numeric(x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)) * x$waga_tuk_2f, na.rm = TRUE),
    n_bs1 = sum(as.numeric(x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_5 %in% c(1:3)) * x$waga_tuk_2f, na.rm = TRUE),
    n_tech_spolic = sum(as.numeric(x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)) * x$waga_tuk_2f, na.rm = TRUE),

    dowolna_forma = sum(as.numeric(x$KN1_1 %in% 1 | x$KN1_2 %in% 1 | x$KN1_3 %in% 1 | x$KN1_4 %in% 1 | x$KN1_5 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) / sum(as.numeric(x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)) * x$waga_tuk_2f, na.rm = TRUE),
    bs2 = sum(as.numeric(x$KN1_1 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) / sum(as.numeric(x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_5 %in% c(1:3)) * x$waga_tuk_2f, na.rm = TRUE),
    lic_dor = sum(as.numeric(x$KN1_2 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) / sum(as.numeric(x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_5 %in% c(1:3)) * x$waga_tuk_2f, na.rm = TRUE),
    spolic = sum(as.numeric(x$KN1_3 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) / sum(as.numeric(x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)) * x$waga_tuk_2f, na.rm = TRUE),
    studia = sum(as.numeric(x$KN1_4 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) / sum(as.numeric(x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)) * x$waga_tuk_2f, na.rm = TRUE),
    kkz = sum(as.numeric(x$KN1_5 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) / sum(as.numeric(x$KN1_1 %in% c(1:3) | x$KN1_2 %in% c(1:3) | x$KN1_3 %in% c(1:3) | x$KN1_4 %in% c(1:3) | x$KN1_5 %in% c(1:3)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek pracujących zarobkowo (pytanie PR0).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarob_2f = function(x) {
  list(
    etykieta = "obecnie pracujący zarobkowo",
    n = sum(x$PR0 %in% c(1, 2), na.rm = TRUE),
    ods_zarobki = sum(x$PR0 %in% 1, na.rm = TRUE) / sum(x$PR0 %in% c(1, 2), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zarob_2f}, ale zwraca ważone
#' wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarob_2f_waga = function(x) {
  list(
    etykieta = "obecnie pracujący zarobkowo - WAŻONE",
    n = sum(as.numeric(
      x$PR0 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    ods_zarobki = sum(as.numeric(
      x$PR0 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) / sum(as.numeric(
        x$PR0 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów pracujących w wyuczonym
#' zawodzie (pytanie PR4_1 = 1) spośród wszystkich absolwentów, a nie tylko
#' pracujących.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarob_zawod_2f = function(x) {
  list(
    etykieta = "pracujący w wyuczonym zawodzie",
    n = sum(x$PR0 %in% c(1, 2), na.rm = TRUE), # podstawą są wszyscy, a nie tylko pracujący
    ods_zarobki_zawod = sum(x$PR4_1 %in% 1, na.rm = TRUE) /
      sum(x$PR0 %in% c(1, 2), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zarob_zawod_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarob_zawod_2f_waga = function(x) {
  list(
    etykieta = "pracujący w wyuczonym zawodzie - ważone",
    n = sum(as.numeric(x$PR0 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    ods_zarobki_zawod = sum(as.numeric(x$PR4_1 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(x$PR0 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów, któryz nie pracowali w
#' wyuczonym zawodzie, ale pracowali w branży, do której wyuczony zawód należy
#' (PR4_1 = 2 & PR4_2 = 1) spośród wszystkich absolwentów, a nie tylko
#' pracujących.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarob_branza_2f = function(x) {
  list(
    etykieta = "nie pracujący w wyuczonym zawodzie, ale w branży, do której ten zawód należy",
    n = sum(x$PR0 %in% c(1, 2), na.rm = TRUE), # podstawą są wszyscy, a nie tylko pracujący
    ods_zarobki_branza = sum(x$PR4_1 %in% 2 & x$PR4_2 %in% 1, na.rm = TRUE) /
      sum(x$PR0 %in% c(1, 2), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zarob_branza_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zarob_branza_2f_waga = function(x) {
  list(
    etykieta = "nie pracujący w wyuczonym zawodzie, ale w branży, do której ten zawód należy - WAŻONE",
    n = sum(as.numeric(
      x$PR0 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE),
    ods_zarobki_branza = sum(as.numeric(
      x$PR4_1 %in% 2 & x$PR4_2 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR0 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik zadowolenie z obecnej pracy biorąc pod
#' uwagę wszystkie jej aspekty - pytanie \emph{PR7_1: ogólnie, biorąc wszystko pod
#' uwagę}. Podstawą procentowania są tylko pracujący absolwenci (PR0 = 1), czyli
#' wszystkie kody za wyjątkiem kodu 7 = Nie dotyczy. \strong{Uwaga:} w
#' kwestionariuszu kod 7 to "trudno powiedzieć", a w zbiorze ta odowiedź ma kod
#' 8.
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{"Zdecydowanie zadowoleni + Raczej zadowoleni"}{kod 1 + kod2}
#'  \item{"Ani zadowolony(a) ani niezadowolony(a)"}{kod 3}
#'  \item{"Zdecydowanie niezadowoleni + Raczej niezadowoleni"}{kod 4 + kod 5}
#'  \item{"Trudno powiedzieć"}{kod 8}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_ogolnie_2f = function(x) {
  list(
    etykieta = "zadowolenie z pracy: ogólnie",
    n = sum(x$PR7_1 %in% c(1:5, 8), na.rm = TRUE),
    zad_zdecyd_raczej = sum(x$PR7_1 %in% c(1, 2), na.rm = TRUE) / sum(x$PR7_1 %in% c(1:5, 8), na.rm = TRUE),
    ani_zad_ani_nzad = sum(x$PR7_1 %in% 3, na.rm = TRUE) / sum(x$PR7_1 %in% c(1:5, 8), na.rm = TRUE),
    nzad_zdecyd_raczej = sum(x$PR7_1 %in% c(4, 5), na.rm = TRUE) / sum(x$PR7_1 %in% c(1:5, 8), na.rm = TRUE),
    trudno_pow = sum(x$PR7_1 %in% 8, na.rm = TRUE) / sum(x$PR7_1 %in% c(1:5, 8), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zarob_branza_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_ogolnie_2f_waga = function(x) {
  list(
    etykieta = "zadowolenie z pracy: ogólnie - WAŻONE",
    n = sum(as.numeric(
      x$PR7_1 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    zad_zdecyd_raczej = sum(as.numeric(
      x$PR7_1 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_1 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    ani_zad_ani_nzad = sum(as.numeric(
      x$PR7_1 %in% 3) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_1 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    nzad_zdecyd_raczej = sum(as.numeric(
      x$PR7_1 %in% c(4, 5)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_1 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    trudno_pow = sum(as.numeric(
      x$PR7_1 %in% 8) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_1 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik zadowolenie z obecnej pracy biorąc pod
#' uwagę wszystkie jej aspekty - pytanie \emph{PR7_2: pod względem możliwości
#' rozwoju umiejętności zawodowych}. Podstawą procentowania są tylko pracujący
#' absolwenci (PR0 = 1), czyli wszystkie kody za wyjątkiem kodu 7 = Nie dotyczy.
#' \strong{Uwaga:} w kwestionariuszu kod 7 to "trudno powiedzieć", a w zbiorze
#' ta odowiedź ma kod 8.
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{"Zdecydowanie zadowoleni + Raczej zadowoleni"}{kod 1 + kod 2}
#'  \item{"Ani zadowolony(a) ani niezadowolony(a)"}{kod 3}
#'  \item{"Zdecydowanie niezadowoleni + Raczej niezadowoleni"}{kod 4 + kod 5}
#'  \item{"Trudno powiedzieć"}{kod 8}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_rozwoj_2f = function(x) {
  list(
    etykieta = "zadowolenie z pracy: możliwość rozwoju umiejętności zawodowych",
    n = sum(x$PR7_2 %in% c(1:5, 8), na.rm = TRUE),
    zad_zdecyd_raczej = sum(x$PR7_2 %in% c(1, 2), na.rm = TRUE) / sum(x$PR7_2 %in% c(1:5, 8), na.rm = TRUE),
    ani_zad_ani_nzad = sum(x$PR7_2 %in% 3, na.rm = TRUE) / sum(x$PR7_2 %in% c(1:5, 8), na.rm = TRUE),
    nzad_zdecyd_raczej = sum(x$PR7_2 %in% c(4, 5), na.rm = TRUE) / sum(x$PR7_2 %in% c(1:5, 8), na.rm = TRUE),
    trudno_pow = sum(x$PR7_2 %in% 8, na.rm = TRUE) / sum(x$PR7_2 %in% c(1:5, 8), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zad_rozwoj_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_rozwoj_2f_waga = function(x) {
  list(
    etykieta = "zadowolenie z pracy: możliwość rozwoju umiejętności zawodowych - ważone",
    n = sum(as.numeric(
      x$PR7_2 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    zad_zdecyd_raczej = sum(as.numeric(
      x$PR7_2 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_2 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    ani_zad_ani_nzad = sum(as.numeric(
      x$PR7_2 %in% 3) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_2 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    nzad_zdecyd_raczej = sum(as.numeric(
      x$PR7_2 %in% c(4, 5)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_2 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    trudno_pow = sum(as.numeric(
      x$PR7_2 %in% 8) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_2 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik zadowolenie z obecnej pracy biorąc pod
#' uwagę wszystkie jej aspekty - pytanie \emph{PR7_3: pod względem możliwości
#' godzenia pracy z życiem poza pracą}. Podstawą procentowania są tylko
#' pracujący absolwenci (PR0 = 1), czyli wszystkie kody za wyjątkiem kodu 7 =
#' Nie dotyczy. \strong{Uwaga:} w kwestionariuszu kod 7 to "trudno powiedzieć",
#' a w zbiorze ta odowiedź ma kod 8.
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{"Zdecydowanie zadowoleni + Raczej zadowoleni"}{kod 1 + kod 2}
#'  \item{"Ani zadowolony(a) ani niezadowolony(a)"}{kod 3}
#'  \item{"Zdecydowanie niezadowoleni + Raczej niezadowoleni"}{kod 4 + kod 5}
#'  \item{"Trudno powiedzieć"}{kod 8}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_zycie_2f = function(x) {
  list(
    etykieta = "zadowolenie z pracy: możliwość godzenia pracy z życiem poza pracą",
    n = sum(x$PR7_3 %in% c(1:5, 8), na.rm = TRUE),
    zad_zdecyd_raczej = sum(x$PR7_3 %in% c(1, 2), na.rm = TRUE) / sum(x$PR7_3 %in% c(1:5, 8), na.rm = TRUE),
    ani_zad_ani_nzad = sum(x$PR7_3 %in% 3, na.rm = TRUE) / sum(x$PR7_3 %in% c(1:5, 8), na.rm = TRUE),
    nzad_zdecyd_raczej = sum(x$PR7_3 %in% c(4, 5), na.rm = TRUE) / sum(x$PR7_3 %in% c(1:5, 8), na.rm = TRUE),
    trudno_pow = sum(x$PR7_3 %in% 8, na.rm = TRUE) / sum(x$PR7_3 %in% c(1:5, 8), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zad_zycie_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_zycie_2f_waga = function(x) {
  list(
    etykieta = "zadowolenie z pracy: możliwość godzenia pracy z życiem poza pracą - ważone",
    n = sum(as.numeric(
      x$PR7_3 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    zad_zdecyd_raczej = sum(as.numeric(
      x$PR7_3 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_3 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    ani_zad_ani_nzad = sum(as.numeric(
      x$PR7_3 %in% 3) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_3 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    nzad_zdecyd_raczej = sum(as.numeric(
      x$PR7_3 %in% c(4, 5)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_3 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    trudno_pow = sum(as.numeric(
      x$PR7_3 %in% 8) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_3 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik zadowolenie z obecnej pracy biorąc pod
#' uwagę wszystkie jej aspekty - pytanie \emph{PR7_4: pod względem wysokości
#' zarobków}. Podstawą procentowania są tylko pracujący absolwenci (PR0 = 1),
#' czyli wszystkie kody za wyjątkiem kodu 7 = Nie dotyczy. \strong{Uwaga:} w
#' kwestionariuszu kod 7 to "trudno powiedzieć", a w zbiorze ta odowiedź ma kod
#' 8.
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{"Zdecydowanie zadowoleni + Raczej zadowoleni"}{kod 1 + kod 2}
#'  \item{"Ani zadowolony(a) ani niezadowolony(a)"}{kod 3}
#'  \item{"Zdecydowanie niezadowoleni + Raczej niezadowoleni"}{kod 4 + kod 5}
#'  \item{"Trudno powiedzieć"}{kod 8}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_zarobki_2f = function(x) {
  list(
    etykieta = "zadowolenie z pracy: wysokość zarobków",
    n = sum(x$PR7_4 %in% c(1:5, 8), na.rm = TRUE),
    zad_zdecyd_raczej = sum(x$PR7_4 %in% c(1, 2), na.rm = TRUE) / sum(x$PR7_4 %in% c(1:5, 8), na.rm = TRUE),
    ani_zad_ani_nzad = sum(x$PR7_4 %in% 3, na.rm = TRUE) / sum(x$PR7_4 %in% c(1:5, 8), na.rm = TRUE),
    nzad_zdecyd_raczej = sum(x$PR7_4 %in% c(4, 5), na.rm = TRUE) / sum(x$PR7_4 %in% c(1:5, 8), na.rm = TRUE),
    trudno_pow = sum(x$PR7_4 %in% 8, na.rm = TRUE) / sum(x$PR7_4 %in% c(1:5, 8), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zad_zarobki_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_zarobki_2f_waga = function(x) {
  list(
    etykieta = "zadowolenie z pracy: wysokość zarobków - ważone",
    n = sum(as.numeric(
      x$PR7_4 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    zad_zdecyd_raczej = sum(as.numeric(
      x$PR7_4 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_4 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    ani_zad_ani_nzad = sum(as.numeric(
      x$PR7_4 %in% 3) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_4 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    nzad_zdecyd_raczej = sum(as.numeric(
      x$PR7_4 %in% c(4, 5)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_4 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    trudno_pow = sum(as.numeric(
      x$PR7_4 %in% 8) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_4 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik zadowolenie z obecnej pracy biorąc pod
#' uwagę wszystkie jej aspekty - pytanie \emph{PR7_5: pod względem pewności, że
#' ją Pan(i) utrzyma}. Podstawą procentowania są tylko pracujący absolwenci
#' (PR0 = 1), czyli wszystkie kody za wyjątkiem kodu 7 = Nie dotyczy.
#' \strong{Uwaga:} w kwestionariuszu kod 7 to "trudno powiedzieć", a w zbiorze
#' ta odowiedź ma kod 8.
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{"Zdecydowanie zadowoleni + Raczej zadowoleni"}{kod 1 + kod 2}
#'  \item{"Ani zadowolony(a) ani niezadowolony(a)"}{kod 3}
#'  \item{"Zdecydowanie niezadowoleni + Raczej niezadowoleni"}{kod 4 + kod 5}
#'  \item{"Trudno powiedzieć"}{kod 8}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_pewnosc_2f = function(x) {
  list(
    etykieta = "zadowolenie z pracy: pewność utrzymania pracy",
    n = sum(x$PR7_5 %in% c(1:5, 8), na.rm = TRUE),
    zad_zdecyd_raczej = sum(x$PR7_5 %in% c(1, 2), na.rm = TRUE) / sum(x$PR7_5 %in% c(1:5, 8), na.rm = TRUE),
    ani_zad_ani_nzad = sum(x$PR7_5 %in% 3, na.rm = TRUE) / sum(x$PR7_5 %in% c(1:5, 8), na.rm = TRUE),
    nzad_zdecyd_raczej = sum(x$PR7_5 %in% c(4, 5), na.rm = TRUE) / sum(x$PR7_5 %in% c(1:5, 8), na.rm = TRUE),
    trudno_pow = sum(x$PR7_5 %in% 8, na.rm = TRUE) / sum(x$PR7_5 %in% c(1:5, 8), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{praca_zad_pewnosc_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
praca_zad_pewnosc_2f_waga = function(x) {
  list(
    etykieta = "zadowolenie z pracy: pewność utrzymania pracy - ważone",
    n = sum(as.numeric(
      x$PR7_5 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    zad_zdecyd_raczej = sum(as.numeric(
      x$PR7_5 %in% c(1, 2)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_5 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    ani_zad_ani_nzad = sum(as.numeric(
      x$PR7_5 %in% 3) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_5 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    nzad_zdecyd_raczej = sum(as.numeric(
      x$PR7_5 %in% c(4, 5)) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_5 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    trudno_pow = sum(as.numeric(
      x$PR7_5 %in% 8) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$PR7_5 %in% c(1:5, 8)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie \emph{E5.
#' Jak to, że nauka w okresie pandemii odbywała się zdalnie, wpłynęło na czas
#' poświęcany przez Panią/Pana na: 1. naukę przedmiotów ogólnych}.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
czas_przed_ogolne_2f = function(x) {
  list(
    etykieta = "czas poświęcany na: naukę przedmiotów ogólnych",
    n = sum(x$E5_1 %in% c(1:3, 8), na.rm = TRUE),
    mniej = sum(x$E5_1 %in% 1, na.rm = TRUE) / sum(x$E5_1 %in% c(1:3, 8), na.rm = TRUE),
    tyle_samo = sum(x$E5_1 %in% 2, na.rm = TRUE) / sum(x$E5_1 %in% c(1:3, 8), na.rm = TRUE),
    wiecej = sum(x$E5_1 %in% 3, na.rm = TRUE) / sum(x$E5_1 %in% c(1:3, 8), na.rm = TRUE),
    trudno_pow = sum(x$E5_1 %in% 8, na.rm = TRUE) / sum(x$E5_1 %in% c(1:3, 8), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{czas_przed_ogolne_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
czas_przed_ogolne_2f_waga = function(x) {
  list(
    etykieta = "czas poświęcany na: naukę przedmiotów ogólnych - WAŻONE",
    n = sum(as.numeric(
      x$E5_1 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    mniej = sum(as.numeric(
      x$E5_1 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_1 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    tyle_samo = sum(as.numeric(
      x$E5_1 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_1 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    wiecej = sum(as.numeric(
      x$E5_1 %in% 3) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_1 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    trudno_pow = sum(as.numeric(
      x$E5_1 %in% 8) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_1 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie \emph{E5.
#' Jak to, że nauka w okresie pandemii odbywała się zdalnie, wpłynęło na czas
#' poświęcany przez Panią/Pana na: 2. teoretyczną naukę przedmiotów zawodowych}.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
czas_przedzaw_teo_2f = function(x) {
  list(
    etykieta = "czas poświęcany na: teoretyczną naukę przedmiotów zawodowych",
    n = sum(x$E5_2 %in% c(1:3, 8), na.rm = TRUE),
    mniej = sum(x$E5_2 %in% 1, na.rm = TRUE) / sum(x$E5_2 %in% c(1:3, 8), na.rm = TRUE),
    tyle_samo = sum(x$E5_2 %in% 2, na.rm = TRUE) / sum(x$E5_2 %in% c(1:3, 8), na.rm = TRUE),
    wiecej = sum(x$E5_2 %in% 3, na.rm = TRUE) / sum(x$E5_2 %in% c(1:3, 8), na.rm = TRUE),
    trudno_pow = sum(x$E5_2 %in% 8, na.rm = TRUE) / sum(x$E5_2 %in% c(1:3, 8), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{czas_przedzaw_teo_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
czas_przedzaw_teo_2f_waga = function(x) {
  list(
    etykieta = "czas poświęcany na: teoretyczną naukę przedmiotów zawodowych - WAŻONE",
    n = sum(as.numeric(
      x$E5_2 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    mniej = sum(as.numeric(
      x$E5_2 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_2 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    tyle_samo = sum(as.numeric(
      x$E5_2 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_2 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    wiecej = sum(as.numeric(
      x$E5_2 %in% 3) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_2 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    trudno_pow = sum(as.numeric(
      x$E5_2 %in% 8) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_2 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja przechowująca rozkład odpowiedzi na pytanie \emph{E5.
#' Jak to, że nauka w okresie pandemii odbywała się zdalnie, wpłynęło na czas
#' poświęcany przez Panią/Pana na: 3. praktyczną naukę przedmiotów zawodowych}.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
czas_przedzaw_prak_2f = function(x) {
  list(
    etykieta = "czas poświęcany na: praktyczną naukę przedmiotów zawodowych",
    n = sum(x$E5_3 %in% c(1:3, 8), na.rm = TRUE),
    mniej = sum(x$E5_3 %in% 1, na.rm = TRUE) / sum(x$E5_3 %in% c(1:3, 8), na.rm = TRUE),
    tyle_samo = sum(x$E5_3 %in% 2, na.rm = TRUE) / sum(x$E5_3 %in% c(1:3, 8), na.rm = TRUE),
    wiecej = sum(x$E5_3 %in% 3, na.rm = TRUE) / sum(x$E5_3 %in% c(1:3, 8), na.rm = TRUE),
    trudno_pow = sum(x$E5_3 %in% 8, na.rm = TRUE) / sum(x$E5_3 %in% c(1:3, 8), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{czas_przedzaw_prak_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
czas_przedzaw_prak_2f_waga = function(x) {
  list(
    etykieta = "czas poświęcany na: praktyczną naukę przedmiotów zawodowych - WAŻONE",
    n = sum(as.numeric(
      x$E5_3 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    mniej = sum(as.numeric(
      x$E5_3 %in% 1) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_3 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    tyle_samo = sum(as.numeric(
      x$E5_3 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_3 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    wiecej = sum(as.numeric(
      x$E5_3 %in% 3) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_3 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE),
    trudno_pow = sum(as.numeric(
      x$E5_3 %in% 8) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E5_3 %in% c(1:3, 8)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik będący rozkładem ocen warunków do pracy
#' zdalnej na podstawie pytania \emph{E6. Nauka zdalna w okresie epidemii
#' wymagała odpowiednich warunków. Jakie miał(a) Pan(i) wtedy możliwości
#' korzystania z...: 1. dobrego połączenia z internetem}
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{"Niewystarczające"}{W ogóle niewystarczające + Raczej
#'  niewystarczające}
#'  \item{"Wystarczające"}{Całkowicie wystarczające + Raczej wystarczające}
#'  \item{"Niepotrzebne do nauki zdalnej"}{Niepotrzebne do nauki zdalnej}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
war_internet_2f = function(x) {
  list(
    etykieta = "warunki: dobre połączenie z internetem",
    n = sum(x$E6_1 %in% c(1:5), na.rm = TRUE),
    n_wyst = sum(x$E6_1 %in% 3 | x$E6_1 %in% 4, na.rm = TRUE) / sum(x$E6_1 %in% c(1:5), na.rm = TRUE),
    wyst = sum(x$E6_1 %in% 1 | x$E6_1 %in% 2, na.rm = TRUE) / sum(x$E6_1 %in% c(1:5), na.rm = TRUE),
    niepotrz = sum(x$E6_1 %in% 5, na.rm = TRUE) / sum(x$E6_1 %in% c(1:5), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{war_internet_2f}, ale zwraca
#' ważone wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
war_internet_2f_waga = function(x) {
  list(
    etykieta = "warunki: dobre połączenie z internetem - WAŻONE",
    n = sum(as.numeric(
      x$E6_1 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    n_wyst = sum(as.numeric(
      x$E6_1 %in% 3 | x$E6_1 %in% 4) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_1 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    wyst = sum(as.numeric(
      x$E6_1 %in% 1 | x$E6_1 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_1 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    niepotrz = sum(as.numeric(
      x$E6_1 %in% 5) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_1 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik będący rozkładem ocen warunków do pracy
#' zdalnej na podstawie pytania \emph{E6. Nauka zdalna w okresie epidemii
#' wymagała odpowiednich warunków. Jakie miał(a) Pan(i) wtedy możliwości
#' korzystania z...: 2. odpowiedniego komputera, laptopa lub tabletu}
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{"Niewystarczające"}{W ogóle niewystarczające + Raczej
#'  niewystarczające}
#'  \item{"Wystarczające"}{Całkowicie wystarczające + Raczej wystarczające}
#'  \item{"Niepotrzebne do nauki zdalnej"}{Niepotrzebne do nauki zdalnej}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
war_komp_2f = function(x) {
  list(
    etykieta = "warunki: odpowiedni komputer, laptop lub tablet",
    n = sum(x$E6_2 %in% c(1:5), na.rm = TRUE),
    n_wyst = sum(x$E6_2 %in% 3 | x$E6_2 %in% 4, na.rm = TRUE) / sum(x$E6_2 %in% c(1:5), na.rm = TRUE),
    wyst = sum(x$E6_2 %in% 1 | x$E6_2 %in% 2, na.rm = TRUE) / sum(x$E6_2 %in% c(1:5), na.rm = TRUE),
    niepotrz = sum(x$E6_2 %in% 5, na.rm = TRUE) / sum(x$E6_2 %in% c(1:5), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{war_komp_2f}, ale zwraca ważone
#' wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
war_komp_2f_waga = function(x) {
  list(
    etykieta = "warunki: odpowiedni komputer, laptop lub tablet - WAŻONE",
    n = sum(as.numeric(
      x$E6_2 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    n_wyst = sum(as.numeric(
      x$E6_2 %in% 3 | x$E6_2 %in% 4) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_2 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    wyst = sum(as.numeric(
      x$E6_2 %in% 1 | x$E6_2 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_2 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    niepotrz = sum(as.numeric(
      x$E6_2 %in% 5) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_2 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik będący rozkładem ocen warunków do pracy
#' zdalnej na podstawie pytania \emph{E6. Nauka zdalna w okresie epidemii
#' wymagała odpowiednich warunków. Jakie miał(a) Pan(i) wtedy możliwości
#' korzystania z...: 3. spokojnego miejsca do nauki}
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{"Niewystarczające"}{W ogóle niewystarczające + Raczej
#'  niewystarczające}
#'  \item{"Wystarczające"}{Całkowicie wystarczające + Raczej wystarczające}
#'  \item{"Niepotrzebne do nauki zdalnej"}{Niepotrzebne do nauki zdalnej}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
war_miejsce_2f = function(x) {
  list(
    etykieta = "warunki: spokojne miejsce do nauki",
    n = sum(x$E6_3 %in% c(1:5), na.rm = TRUE),
    n_wyst = sum(x$E6_3 %in% 3 | x$E6_3 %in% 4, na.rm = TRUE) / sum(x$E6_3 %in% c(1:5), na.rm = TRUE),
    wyst = sum(x$E6_3 %in% 1 | x$E6_3 %in% 2, na.rm = TRUE) / sum(x$E6_3 %in% c(1:5), na.rm = TRUE),
    niepotrz = sum(x$E6_3 %in% 5, na.rm = TRUE) / sum(x$E6_3 %in% c(1:5), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{war_miejsce_2f}, ale zwraca ważone
#' wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
war_miejsce_2f_waga = function(x) {
  list(
    etykieta = "warunki: spokojne miejsce do nauki - WAŻONE",
    n = sum(as.numeric(
      x$E6_3 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    n_wyst = sum(as.numeric(
      x$E6_3 %in% 3 | x$E6_3 %in% 4) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_3 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    wyst = sum(as.numeric(
      x$E6_3 %in% 1 | x$E6_3 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_3 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    niepotrz = sum(as.numeric(
      x$E6_3 %in% 5) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_3 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja licząca wskaźnik będący rozkładem ocen warunków do pracy
#' zdalnej na podstawie pytania \emph{E6. Nauka zdalna w okresie epidemii
#' wymagała odpowiednich warunków. Jakie miał(a) Pan(i) wtedy możliwości
#' korzystania z...: 4. czyjejś pomocy w nauce w przypadku trudności}
#' Wskaźnik jest rozkładem odpowiedzi:
#' \itemize{
#'  \item{"Niewystarczające"}{W ogóle niewystarczające + Raczej
#'  niewystarczające}
#'  \item{"Wystarczające"}{Całkowicie wystarczające + Raczej wystarczające}
#'  \item{"Niepotrzebne do nauki zdalnej"}{Niepotrzebne do nauki zdalnej}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
war_pomoc_2f = function(x) {
  list(
    etykieta = "warunki: pomoc w nauce w przypadku trudności",
    n = sum(x$E6_4 %in% c(1:5), na.rm = TRUE),
    n_wyst = sum(x$E6_4 %in% 3 | x$E6_4 %in% 4, na.rm = TRUE) / sum(x$E6_4 %in% c(1:5), na.rm = TRUE),
    wyst = sum(x$E6_4 %in% 1 | x$E6_4 %in% 2, na.rm = TRUE) / sum(x$E6_4 %in% c(1:5), na.rm = TRUE),
    niepotrz = sum(x$E6_4 %in% 5, na.rm = TRUE) / sum(x$E6_4 %in% c(1:5), na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla 2. fali badania na poziomie zagregowanym
#' @description Funkcja analogiczna do \code{war_pomoc_2f}, ale zwraca ważone
#' wartości na potrzeby prezentacji danych w grupie odniesienia (np.
#' województwo).
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
war_pomoc_2f_waga = function(x) {
  list(
    etykieta = "warunki: pomoc w nauce w przypadku trudności - WAŻONE",
    n = sum(as.numeric(
      x$E6_4 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    n_wyst = sum(as.numeric(
      x$E6_4 %in% 3 | x$E6_4 %in% 4) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_4 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    wyst = sum(as.numeric(
      x$E6_4 %in% 1 | x$E6_4 %in% 2) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_4 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE),
    niepotrz = sum(as.numeric(
      x$E6_4 %in% 5) * x$waga_tuk_2f, na.rm = TRUE) /
      sum(as.numeric(
        x$E6_4 %in% c(1:5)) * x$waga_tuk_2f, na.rm = TRUE)
  ) %>%
    return()
}
