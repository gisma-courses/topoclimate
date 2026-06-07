# ============================================================
# Kommentiertes R-Skript zu: L01_fieldclim_datenbasis_und_station.qmd
# ============================================================
# Dieses Skript wurde aus dem Quarto-Dokument extrahiert.
# Es enthält nur R-Code, keine Fließtexte, keine Aufgabenboxen und keine Quarto-Markup-Elemente.
# Die Kommentare sind absichtlich ausführlich gehalten, damit Anfängerinnen und Anfänger die einzelnen Schritte nachvollziehen können.
# Ausführung: Datei in RStudio öffnen und von oben nach unten ausführen.
# Voraussetzung: Das Paket fieldClim ist installiert; die Kursdaten liegen entweder im Projektordner data/ oder werden aus dem Paketbeispiel geladen.
# ============================================================

# ---- setup ----
# Zweck: Grundeinstellungen für die Ausgabe der Code-Chunks und ihrer Ergebnisse.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

knitr::opts_chunk$set(
  echo = TRUE,
  include = TRUE,
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

# ---- load-packages-and-data ----
# Zweck: Pakete laden, Datendatei finden, CSV einlesen und Zeitspalte in ein echtes Datum-Zeit-Format umwandeln.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# fieldClim enthält die fachlichen Funktionen für Strahlung, Bodenwärmestrom, Stationsobjekte und Wärmeflüsse.
library(fieldClim)
# here erleichtert robuste Projektpfade, damit keine festen lokalen Rechnerpfade im Code stehen.
library(here)

# Kursprojekt: bevorzugt lokale Datei unter data/.
# Zuerst wird der erwartete Kursdatensatz im Projektordner gesucht.
caldern_file <- here::here("data", "caldern_wiese_2017-06-30.csv")

# Fallback: mit fieldClim ausgelieferter Beispieldatensatz.
# Falls die Kursdatei fehlt, wird auf die Beispieldatei aus dem installierten Paket ausgewichen.
if (!file.exists(caldern_file)) {
  caldern_file <- system.file(
    "extdata",
    "caldern_wiese_2017-06-30.csv",
    package = "fieldClim"
  )
}

# Harte Sicherheitsprüfung: Ohne vorhandene Datei soll der Code hier abbrechen.
stopifnot(file.exists(caldern_file))

# Die CSV-Datei wird als data.frame eingelesen. Textwerte wie NULL oder leere Felder werden als NA behandelt.
caldern <- read.csv(
  caldern_file,
  na.strings = c("NULL", "NA", "")
)

# Die Zeitspalte wird in ein echtes Datum-Zeit-Objekt umgewandelt; das ist für Zeitreihenplots und Differenzen nötig.
caldern$datetime <- as.POSIXct(
  caldern$datetime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Europe/Berlin"
)

# ---- first-checks ----
# Zweck: Schnelle Grundprüfung: Zeilenzahl, Zeitraum, Zeitschritt und vorhandene Spalten.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Anzahl der Zeilen prüfen; bei 5-Minuten-Daten entspricht ein vollständiger Tag 288 Zeilen.
nrow(caldern)
# Der Zeitbereich zeigt, ob der erwartete Tag wirklich enthalten ist.
range(caldern$datetime)
# Die Differenzen zwischen Zeitstempeln zeigen, ob der Zeitschritt gleichmäßig ist.
summary(diff(caldern$datetime))
# Spaltennamen prüfen, damit spätere Zugriffe auf Messgrößen nachvollziehbar sind.
names(caldern)

# ---- selected-columns ----
# Zweck: Fachlich zentrale Messspalten aus der Gesamttabelle auswählen und kurz zusammenfassen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Aus der Rohdatentabelle werden nur die Spalten herausgezogen, die für diesen Abschnitt wichtig sind.
selected <- caldern[, c(
  "datetime",
  "Ta_2m", "Ta_10m",
  "Huma_2m", "Huma_10m",
  "Windspeed_2m", "Windspeed_10m",
  "rad_net", "heatflux_soil"
)]

# head() zeigt die ersten Zeilen und ist ein schneller Format- und Plausibilitätscheck.
head(selected)
# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(selected[, -1])

# ---- first-base-plot ----
# Zweck: Erster Sichtcheck der wichtigsten Tagesgänge mit einfachen base-R-Plots.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Die bisherigen Grafikeinstellungen werden gespeichert, damit sie nach dem Mehrfachplot wiederhergestellt werden können.
op <- par(mfrow = c(3, 1), mar = c(3.5, 4, 2, 1))

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(
  caldern$datetime, caldern$Ta_2m,
  type = "l",
  xlab = "Zeit",
  ylab = "°C",
  main = "Lufttemperatur in 2 m"
)

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(
  caldern$datetime, caldern$rad_net,
  type = "l",
  xlab = "Zeit",
  ylab = "W m-2",
  main = "Netto-Strahlung"
)

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(
  caldern$datetime, caldern$heatflux_soil,
  type = "l",
  xlab = "Zeit",
  ylab = "W m-2",
  main = "Bodenwärmestrom"
)

# Die alten Grafikeinstellungen werden wiederhergestellt; dadurch beeinflusst dieser Plot spätere Plots nicht.
par(op)

# ---- build-weather-station ----
# Zweck: Die Rohdaten in ein fieldClim-weather_station-Objekt übersetzen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# build_weather_station() bündelt Messwerte und Standortannahmen in einem einheitlichen Objekt.
ws <- build_weather_station(
# datetime übergibt die Zeitachse; alle Messvektoren müssen zu diesen Zeitpunkten passen.
  datetime = caldern$datetime,
# lon und lat beschreiben die Stationsposition; sie beeinflussen Sonnenstand und Strahlungsmodellierung.
  lon = 8.6832,
# Breite der Station; zusammen mit Zeit und Länge bestimmt sie die astronomische Strahlungssituation.
  lat = 50.8405,
# Höhe über Meer; sie wird unter anderem für Druck- und Dichteabschätzungen verwendet.
  elev = 261,

# temp ist die zentrale Lufttemperaturreihe, hier die Messung in 2 m Höhe.
  temp = caldern$Ta_2m,
# rh ist die relative Luftfeuchte in Prozent; sie ist Grundlage für Feuchte- und Verdunstungsgrößen.
  rh = caldern$Huma_2m,

# t1 und t2 bilden das Temperaturprofil zwischen zwei Messhöhen.
  t1 = caldern$Ta_2m,
# t2 ist die zweite Temperaturhöhe; die Differenz zu t1 steuert Gradientenmethoden.
  t2 = caldern$Ta_10m,
# hum1 und hum2 bilden das Feuchteprofil zwischen zwei Messhöhen.
  hum1 = caldern$Huma_2m,
# hum2 ist die obere Feuchtemessung; kleine Differenzen können Bowen-Rechnungen instabil machen.
  hum2 = caldern$Huma_10m,

# v1 und v2 bilden das Windprofil zwischen zwei Messhöhen.
  v1 = caldern$Windspeed_2m,
# v2 ist die obere Windmessung; Windgradienten sind wichtig für Austausch- und Stabilitätsdiagnosen.
  v2 = caldern$Windspeed_10m,
# z1 ist die untere Messhöhe in Metern.
  z1 = 2,
# z2 ist die obere Messhöhe in Metern.
  z2 = 10,

# rad_bal entspricht in fieldClim der Netto-Strahlung Q_star.
  rad_bal = caldern$rad_net,
# soil_flux entspricht dem Bodenwärmestrom B.
  soil_flux = caldern$heatflux_soil,

# slope beschreibt die Hangneigung; 0 steht hier für eine ebene Fläche.
  slope = 0,
# exposition beschreibt die Hangrichtung; bei ebener Fläche ist der Wert praktisch ohne Wirkung.
  exposition = 0,
# valley markiert, ob eine Tal-/Geländesituation angenommen wird.
  valley = FALSE,
# surface_type steuert Oberflächeneigenschaften wie Albedo- oder Rauigkeitsannahmen.
  surface_type = "field",
# surface_temp ist die Oberflächentemperaturreihe; sie geht in langwellige Bilanzanteile ein.
  surface_temp = caldern$Ts,

# texture ist eine vereinfachte Bodenartangabe für bodenphysikalische Abschätzungen.
  texture = "peat",
# moisture übergibt die Bodenfeuchte, hier aus der Stationsmessung.
  moisture = caldern$water_vol_soil,
# soil_temp1 und soil_temp2 bilden ein Temperaturgefälle für Bodenwärmestromschätzungen.
  soil_temp1 = caldern$Ts,
# soil_temp2 ist die zweite Temperatur für den Boden-/Oberflächen-Gradienten.
  soil_temp2 = caldern$Ta_2m,
# soil_depth1 ist die Tiefe der ersten Bodentemperatur in Metern.
  soil_depth1 = 0.25,
# soil_depth2 ist die Referenztiefe; 0 bedeutet hier Oberfläche.
  soil_depth2 = 0,
# obs_height ist die Standard-Beobachtungshöhe der zentralen Luftmessung.
  obs_height = 2
)

# ---- inspect-basic-object ----
# Zweck: Grundstruktur des weather_station-Objekts prüfen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# class() zeigt, welche Objektklasse R dem Objekt zuweist.
class(ws)
# Spaltennamen prüfen, damit spätere Zugriffe auf Messgrößen nachvollziehbar sind.
names(ws)
# head() zeigt die ersten Zeilen und ist ein schneller Format- und Plausibilitätscheck.
head(as.data.frame(ws))

# ---- default-vs-object ----
# Zweck: Einzelargument-Aufruf und Objektmethode gegenüberstellen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Einzeltest mit expliziten Argumenten.
# pres_p() schätzt den Luftdruck aus Höhe und Temperatur oder liest diese Werte aus dem Stationsobjekt.
pres_p(elev = 261, temp = 20)

# Objektmethode: die Funktion liest die relevanten Felder aus ws.
# pres_p() schätzt den Luftdruck aus Höhe und Temperatur oder liest diese Werte aus dem Stationsobjekt.
pres_p(ws)
