# Försäkringsanalys

## Syfte

Detta projekt analyserar vilka faktorer som påverkar försäkringskostnader.

Målet är att undersöka samband mellan kunders egenskaper, livsstil och historik samt att bygga regressionsmodeller för att förklara kostnaderna.

------------------------------------------------------------------------

## Projektstruktur

Projektet är uppdelat i separata R-skript som tillsammans utgör analysflödet:

-   `scripts/01_load_data.R` – inläsning av data\
-   `scripts/02_prepare_data.R` – datastädning och feature engineering\
-   `scripts/03_eda.R` – explorativ dataanalys och visualiseringar\
-   `scripts/04_model.R` – regressionsmodeller och modellutvärdering\
-   `report.qmd` – slutlig rapport som sammanställer hela analysen

------------------------------------------------------------------------

## Krav

För att köra projektet krävs R samt följande paket:

-   tidyverse

------------------------------------------------------------------------

## Körning

1.  Klona eller ladda ner projektet
2.  Öppna projektet i RStudio
3.  Kör (rendera) `report.qmd`

Rapporten kör hela analysen från dataimport till färdiga resultat.

------------------------------------------------------------------------

## Källor

Projektet är genomfört som en del av en kurs och bygger främst på kursmaterial och videogenomgångar.

Utöver detta har AI använts som stöd för att bolla idéer, förbättra kodstruktur och utveckla resonemang i analysen.
