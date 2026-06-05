# fieldClim-Kurseinheiten

Dieses Paket enthält vier deutsche Quarto-Kurseinheiten zum Anwendungstransfer des `fieldClim`-Pakets. Die Einheiten setzen die zuvor behandelte Theorie zu Geländeklima, Mikroklima und Energiebilanz voraus. Der Schwerpunkt liegt auf der praktischen Übersetzung in R: Stationsdaten laden, prüfen, als `weather_station`-Objekt strukturieren und Wärmeflussmethoden mit nachvollziehbaren Annahmen anwenden.

Die deutschen Rmd-Dateien wurden als sprachliche und didaktische Grundlage genutzt. Die englischen Vignetten und die Formula Reference wurden als fachlich aktuellere Referenzspur behandelt, insbesondere für Notation, Methodenrollen, Messarchitektur und Closure-Semantik.

## Dateien

- `course_units/L01_fieldclim_datenbasis_und_station.qmd`
- `course_units/L02_strahlung_bodenwaermestrom_datenpruefung.qmd`
- `course_units/L03_weather_station_objekt_und_basisworkflow.qmd`
- `course_units/L04_waermeflussmethoden_mit_fieldclim.qmd`
- `assets/geoinfo.bib`
- `assets/apa.csl`

## Arbeitsstil

Der R-Code ist bewusst schlicht gehalten. Die Einheiten verwenden base R für Plots und arbeiten mit `here::here()` für relative Projektpfade. Wenn im Projektordner eine Datei `data/caldern_wiese_2017-06-30.csv` existiert, wird diese genutzt. Sonst greift der Code auf die im Paket `fieldClim` enthaltene Beispieldatei zurück.

## Erwartete Projektstruktur

```text
project/
├── assets/
│   ├── geoinfo.bib
│   └── apa.csl
├── course_units/
│   ├── L01_fieldclim_datenbasis_und_station.qmd
│   ├── L02_strahlung_bodenwaermestrom_datenpruefung.qmd
│   ├── L03_weather_station_objekt_und_basisworkflow.qmd
│   └── L04_waermeflussmethoden_mit_fieldclim.qmd
├── data/
│   └── caldern_wiese_2017-06-30.csv   # optional
└── images/
    ├── splash_L01_1.png
    ├── splash_L02_1.png
    ├── splash_L03_1.png
    └── splash_L04_1.png
```

Die Bannerbilder sind nicht enthalten. Die Header verweisen auf `../images/splash_L0X_1.png`, wie vorgegeben.
