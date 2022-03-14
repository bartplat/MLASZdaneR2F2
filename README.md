![KL+RP+IBE+EFS](inst/Belka-Losy-absolwentow-Kolor-PL.png)

# MLASZdaneR2F2

Pakiet został opracowany w ramach projektu *Monitorowanie losów edukacyjno-zawodowych absolwentów i młodych dorosłych* (POWR.02.15.00-IP.02-00-004/16) prowadzonego w Instytucie Badań Edukacyjnych w ramach działania 2.15. Kształcenie i szkolenie zawodowe dostosowane do potrzeb zmieniającej się gospodarki II. osi priorytetowej Efektywne polityki publiczne dla rynku pracy, gospodarki i edukacji Programu Operacyjnego Wiedza, Edukacja, Rozwój.

Pakiet `MLASZdaneR2F2` zawiera zbiór funkcji służących do obliczania wskaźników dla 2. fali 2. rundy Monitoringu Losów Absolwentów. Runda ta została zrealizowana za pomocą badania sondażowego metodą CAWI. Przy pomocy tego pakietu oraz drugiego, będącego silnikiem agregacji - [`MLASZdane`](https://github.com/bartplat/MLASZdane), można tworzyć zbiory wskaźników na zadanym poziomie agregacji.

# Instalacja / aktualizacja

Pakiet nie jest dostępny na CRAN, więc trzeba instalować go ze źródeł.

Instalację najprościej przeprowadzić wykorzystując pakiet *devtools*:

```r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('bartplat/MLASZdaneR2F2', build_opts = c("--no-resave-data"))
```

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.

Pakiet `MLASZdaneR2F2` jest zależny od pakietu `MLASZdane`, ale nie ma potrzeby go dodatkowo instalować, ponieważ dzieje się to podczas instalacji pakietu `MLASZdaneR2F2`.

# Użycie

Jako, że pakiet `MLASZdaneR2F2` zawiera zbiór funkcji służących do liczenia wskaźników dla specyficznych danych w specyficzny sposób, poniżej opisano kolejne kroki, które należy przejść, aby otrzymać zbiór wskaźników zagregowanych.

## Definicja grup odniesienia

Do funkcji agregujących, jako jeden z argumentów, należy podać ramkę danych będącą definicją grupy oraz grupy odniesienia, czyli de facto poziomy agregacji dla grup prezentowanych w raportach oraz ich grup odniesienia. Można to osiągnąć na 2 sposoby: albo za pomocą funkcji `utworz_grupowanie_ze_zmiennej()` z pakietu [`MLASZdane`](https://github.com/bartplat/MLASZdane) albo samodzielnie. Istotne jest, aby wynikiem była ramka danych zawierająca kolumny o wymaganych nazwach. Pierwsza opcja ma jednak pewne ograniczenia: grupę odniesienia można definiować tylko za pomocą jednej nazwy zmiennej. W przypadku potrzeby użycia więcej niż jednej zmiennej ramkę danych zawierającą definicje grup i grup odniesienia należy przygotować samodzielnie.

Dodatkowo, ze względu na specyfikę danych, należy za pomocą funkcji `zap_labels()` z pakietu `haven` usunąć etykiety danych.

```r
library(dplyr)
grupy_szkoly = cawi %>%
  select(ID_RSPO, TYP, wojewodztwo) %>%
  # mutate(across(everything(), ~zap_labels(.))) %>% 
  mutate(grupa = paste0("ID_RSPO %in% ", ID_RSPO),
         odniesienie = paste0("!(ID_RSPO %in% ", ID_RSPO, ") & ",
                              "(wojewodztwo %in% \"", wojewodztwo, "\") & ",
                              "(TYP %in% ", TYP, ")")) %>% 
  distinct()
```

Powyższy kod tworzy dwie definicje grup:

1. Prezentowaną grupę - w tym przypadku jest to szkoła (określona numerem RSPO) oraz
2. Grupę odniesienia będącą **pozostałymi** (czyli z wykluczeniem z grupy odniesienia prezentowanej szkoły) szkołami z danego województwa, dla danego typu szkoły

## Obliczanie wskaźników na poziomie zagregowanym

Ostatnim krokiem, mając już ramkę danych z definicja grup odniesienia, jest wygenerowanie wskaźników na danym poziomie agregacji.

```r
library(MLASZdaneR2F2)
wskazniki = agreguj_cawi_ucz_2rm_2f(cawi, grupy_szkoly)

szkola = wskazniki$grupy
wojSzk = wskazniki$grupyOdniesienia
```

Wynikiem działania funkcji `agreguj_cawi_ucz_2rm_2f()` jest lista o dwóch elementach: ramkach danych zagregwanych dla prezentowanej grupy oraz dla jej grupy odniesienia.
