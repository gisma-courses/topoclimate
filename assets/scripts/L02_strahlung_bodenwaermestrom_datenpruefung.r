# ============================================================
# Kommentiertes R-Skript zu: L02_strahlung_bodenwaermestrom_datenpruefung.qmd
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

# ---- shortwave-check ----
# Zweck: Kurzwellige Strahlung, kurzwellige Bilanz und Albedo berechnen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# K_down ist die einfallende kurzwellige Strahlung.
caldern$K_down <- caldern$rad_sw_in
# K_up ist die reflektierte kurzwellige Strahlung.
caldern$K_up <- caldern$rad_sw_out
# K_star ist die kurzwellige Bilanz: einfallend minus reflektiert.
caldern$K_star <- caldern$K_down - caldern$K_up

# Die Albedo-Spalte wird zunächst leer angelegt; berechnet wird sie nur für sinnvolle Strahlungsbedingungen.
caldern$albedo <- NA_real_
# valid_rad markiert Zeitpunkte mit genügend Einstrahlung, damit die Albedo-Berechnung nicht durch Dunkelheit instabil wird.
valid_rad <- is.finite(caldern$K_down) & caldern$K_down > 50
# Für gültige Zeitpunkte wird die Albedo als Verhältnis aus reflektierter und einfallender Strahlung berechnet.
caldern$albedo[valid_rad] <- caldern$K_up[valid_rad] / caldern$K_down[valid_rad]

# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(caldern[, c("K_down", "K_up", "K_star", "albedo")])

# ---- shortwave-plot ----
# Zweck: Kurzwellige Strahlung und Albedo als Tagesgang prüfen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Die bisherigen Grafikeinstellungen werden gespeichert, damit sie nach dem Mehrfachplot wiederhergestellt werden können.
op <- par(mfrow = c(3, 1), mar = c(3.5, 4, 2, 1))

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$K_down, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Einfallende kurzwellige Strahlung")

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$K_up, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Reflektierte kurzwellige Strahlung")

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$albedo, type = "p", pch = 16,
     xlab = "Zeit", ylab = "-", main = "Albedo bei ausreichender Einstrahlung")

# Die alten Grafikeinstellungen werden wiederhergestellt; dadurch beeinflusst dieser Plot spätere Plots nicht.
par(op)

# ---- longwave-and-net ----
# Zweck: Langwellige Bilanz und Netto-Strahlung über verschiedene Spalten kontrollieren.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# L_down ist die atmosphärische langwellige Gegenstrahlung.
caldern$L_down <- caldern$LDnCo
# L_up ist die langwellige Ausstrahlung der Oberfläche.
caldern$L_up <- caldern$LUpCo
# L_star ist die langwellige Bilanz: Gegenstrahlung minus Ausstrahlung.
caldern$L_star <- caldern$L_down - caldern$L_up

# Q_star ist die Arbeitsreihe für Netto-Strahlung.
caldern$Q_star_components <- caldern$K_star + caldern$L_star
# Q_star ist die Arbeitsreihe für Netto-Strahlung.
caldern$Q_star_netcols <- caldern$RsNet + caldern$RlNet
# Q_star ist die Arbeitsreihe für Netto-Strahlung.
caldern$Q_star_measured <- caldern$rad_net

# Diese kleine Prüftabelle vergleicht alternative Berechnungen der Netto-Strahlung.
q_check <- data.frame(
  comparison = c("RsNet + RlNet - rad_net", "K* + L* - rad_net"),
  mean_deviation = c(
    mean(caldern$Q_star_netcols - caldern$Q_star_measured, na.rm = TRUE),
    mean(caldern$Q_star_components - caldern$Q_star_measured, na.rm = TRUE)
  ),
  max_abs_deviation = c(
    max(abs(caldern$Q_star_netcols - caldern$Q_star_measured), na.rm = TRUE),
    max(abs(caldern$Q_star_components - caldern$Q_star_measured), na.rm = TRUE)
  )
)

q_check[, -1] <- round(q_check[, -1], 1)
q_check

# ---- net-radiation-plot ----
# Zweck: Gemessene Netto-Strahlung und Kontrollrechnung grafisch vergleichen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$Q_star_measured, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Netto-Strahlung: Messspalte und Kontrollrechnung")
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$Q_star_netcols, lty = 2)
# Die Legende erklärt, welche Linie zu welcher Mess- oder Modellreihe gehört.
legend("bottomleft", legend = c("rad_net", "RsNet + RlNet"),
       lty = c(1, 2), bty = "n")

# ---- define-qstar ----
# Zweck: Die im weiteren Workflow verwendete Netto-Strahlung eindeutig festlegen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Q_star ist die Arbeitsreihe für Netto-Strahlung.
caldern$Q_star <- caldern$Q_star_measured

# ---- lag-check ----
# Zweck: Einfach prüfen, ob ein zeitlicher Versatz zwischen Komponenten und Messspalte vorliegt.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Mittlere absolute Abweichung für eine bestimmte Zeitverschiebungsvariante berechnen.
diff_0 <- mean(abs(caldern$Q_star_components - caldern$Q_star_measured), na.rm = TRUE)

# Mittlere absolute Abweichung für eine bestimmte Zeitverschiebungsvariante berechnen.
diff_plus <- mean(abs(
# Q_star ist die Arbeitsreihe für Netto-Strahlung.
  caldern$Q_star_components[-1] -
# Q_star ist die Arbeitsreihe für Netto-Strahlung.
    caldern$Q_star_measured[-length(caldern$Q_star_measured)]
), na.rm = TRUE)

# Mittlere absolute Abweichung für eine bestimmte Zeitverschiebungsvariante berechnen.
diff_minus <- mean(abs(
# Q_star ist die Arbeitsreihe für Netto-Strahlung.
  caldern$Q_star_components[-length(caldern$Q_star_components)] -
# Q_star ist die Arbeitsreihe für Netto-Strahlung.
    caldern$Q_star_measured[-1]
), na.rm = TRUE)

# Diese Tabelle prüft, ob ein einfacher 5-Minuten-Zeitversatz die Abweichung reduziert.
lag_check <- data.frame(
  comparison = c("ohne Versatz", "Komponenten +1 Schritt", "Komponenten -1 Schritt"),
  mean_absolute_deviation = round(c(diff_0, diff_plus, diff_minus), 1)
)

lag_check

# ---- soil-and-available-energy ----
# Zweck: Bodenwärmestrom und verfügbare Energie berechnen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# B ist die Arbeitsreihe für Bodenwärmestrom.
caldern$B <- caldern$heatflux_soil
# available_energy ist ebenfalls Q_star minus B; der Name ist für Anfänger direkt lesbar.
caldern$available_energy <- caldern$Q_star - caldern$B

# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(caldern[, c("Q_star", "B", "available_energy")])

# ---- available-energy-plot ----
# Zweck: Netto-Strahlung, Bodenwärmestrom und verfügbare Energie darstellen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Die bisherigen Grafikeinstellungen werden gespeichert, damit sie nach dem Mehrfachplot wiederhergestellt werden können.
op <- par(mfrow = c(2, 1), mar = c(3.5, 4, 2, 1))

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$Q_star, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Q*: Netto-Strahlung")
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$B, lty = 2)
# Die Legende erklärt, welche Linie zu welcher Mess- oder Modellreihe gehört.
legend("bottomleft", legend = c("Q*", "B"), lty = c(1, 2), bty = "n")

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$available_energy, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "A = Q* - B")
# Eine Referenzlinie ergänzt den Plot; h = 0 markiert hier meist den Wechsel des Vorzeichens.
abline(h = 0, lty = 2, col = "grey50")

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

# ---- soil-package-check ----
# Zweck: Modellierten Bodenwärmestrom mit gemessenem Bodenwärmestrom vergleichen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# B ist die Arbeitsreihe für Bodenwärmestrom.
caldern$B_model <- soil_heat_flux(ws)
# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(caldern$B_model - caldern$B)

# ---- soil-package-plot ----
# Zweck: Gemessenen und modellierten Bodenwärmestrom grafisch gegenüberstellen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$B, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Bodenwärmestrom: gemessen und modelliert")
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$B_model, lty = 2)
# Die Legende erklärt, welche Linie zu welcher Mess- oder Modellreihe gehört.
legend("bottomleft", legend = c("gemessen", "aus Bodenparametern"),
       lty = c(1, 2), bty = "n")
