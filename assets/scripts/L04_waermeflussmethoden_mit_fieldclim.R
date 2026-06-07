# ============================================================
# Kommentiertes R-Skript zu: L04_waermeflussmethoden_mit_fieldclim.qmd
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

# ---- prepare-flux-data ----
# Zweck: Grundgrößen für die Wärmeflussmethoden vorbereiten.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Q_star ist die Arbeitsreihe für Netto-Strahlung.
caldern$Q_star <- caldern$rad_net
# B ist die Arbeitsreihe für Bodenwärmestrom.
caldern$B <- caldern$heatflux_soil
# Q_minus_B ist die verfügbare Energie für die Wärmeflussmethoden.
caldern$Q_minus_B <- caldern$Q_star - caldern$B

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

# ---- inspect-before-flux ----
# Zweck: Vor der Methodenrechnung prüfen, welche Pfade mit den vorhandenen Eingaben möglich sind.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# inspection sammelt Diagnosen dazu, ob das Stationsobjekt die nötigen Eingaben enthält.
inspection <- inspect_weather_station_inputs(ws)
inspection$method_readiness[, c("method", "ready", "missing_fields", "partial_fields", "notes")]

# ---- priestley-taylor ----
# Zweck: Priestley-Taylor-Flüsse berechnen und in explizite Spalten schreiben.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Priestley-Taylor wird hier separat berechnet, weil dieser Pfad eine eigene Partition der verfügbaren Energie liefert.
flux_pt <- turb_flux_calc(ws, pt_only = TRUE)

# H_pt ist die fühlbare Wärme aus der Priestley-Taylor-Partition.
caldern$H_pt <- flux_pt$sensible_priestley_taylor
# LE_pt ist die latente Wärme aus der Priestley-Taylor-Partition.
caldern$LE_pt <- flux_pt$latent_priestley_taylor
# Die Summe aus H und LE kontrolliert, ob die Partition zur verfügbaren Energie passt.
caldern$sum_pt <- caldern$H_pt + caldern$LE_pt

# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(caldern[, c("H_pt", "LE_pt", "sum_pt")])

# ---- priestley-plot ----
# Zweck: Priestley-Taylor-Ergebnisse für fühlbare und latente Wärme darstellen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Die bisherigen Grafikeinstellungen werden gespeichert, damit sie nach dem Mehrfachplot wiederhergestellt werden können.
op <- par(mfrow = c(2, 1), mar = c(3.5, 4, 2, 1))

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$H_pt, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Priestley--Taylor: H")
# Eine Referenzlinie ergänzt den Plot; h = 0 markiert hier meist den Wechsel des Vorzeichens.
abline(h = 0, lty = 2, col = "grey50")

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$LE_pt, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Priestley--Taylor: LE")
# Eine Referenzlinie ergänzt den Plot; h = 0 markiert hier meist den Wechsel des Vorzeichens.
abline(h = 0, lty = 2, col = "grey50")

# Die alten Grafikeinstellungen werden wiederhergestellt; dadurch beeinflusst dieser Plot spätere Plots nicht.
par(op)

# ---- bulk-residual ----
# Zweck: Bulk-Residual-Methode mit Richardson-Guard berechnen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Bulk-Residual berechnet H über einen Profilansatz und LE anschließend als Restgröße.
flux_bulk <- turb_flux_bulk_residual(
  ws,
  stability_method = "ri_guard"
)

# H_bulk ist die fühlbare Wärme aus dem Bulk-Ansatz.
caldern$H_bulk <- flux_bulk$sensible_bulk
# LE_bulk ist die latente Wärme als Residualterm der Energiebilanz.
caldern$LE_bulk <- flux_bulk$latent_bulk_residual
# bulk_Ri speichert die Richardson-Diagnose, die vom Guard verwendet wird.
caldern$bulk_Ri <- attr(flux_bulk$sensible_bulk, "bulk_Ri_g")
# bulk_stability speichert die Stabilitätsklasse der jeweiligen Zeitschritte.
caldern$bulk_stability <- attr(flux_bulk$sensible_bulk, "bulk_stability")

# table() zählt Kategorien, hier zum Beispiel Stabilitätsklassen.
table(caldern$bulk_stability, useNA = "ifany")
# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(caldern[, c("H_bulk", "LE_bulk")])

# ---- bulk-plot ----
# Zweck: Bulk-Residual-Ergebnisse für H und LE darstellen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Die bisherigen Grafikeinstellungen werden gespeichert, damit sie nach dem Mehrfachplot wiederhergestellt werden können.
op <- par(mfrow = c(2, 1), mar = c(3.5, 4, 2, 1))

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$H_bulk, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Bulk--Residual: H mit ri_guard")
# Eine Referenzlinie ergänzt den Plot; h = 0 markiert hier meist den Wechsel des Vorzeichens.
abline(h = 0, lty = 2, col = "grey50")

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$LE_bulk, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Bulk--Residual: LE als Rest")
# Eine Referenzlinie ergänzt den Plot; h = 0 markiert hier meist den Wechsel des Vorzeichens.
abline(h = 0, lty = 2, col = "grey50")

# Die alten Grafikeinstellungen werden wiederhergestellt; dadurch beeinflusst dieser Plot spätere Plots nicht.
par(op)

# ---- all-methods ----
# Zweck: Mehrere Wärmeflussmethoden gemeinsam berechnen und Mittelwerte vergleichen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# turb_flux_calc() berechnet mehrere Standardmethoden gemeinsam.
flux_all <- turb_flux_calc(ws)

caldern$H_bowen <- flux_all$sensible_bowen
caldern$LE_bowen <- flux_all$latent_bowen
caldern$H_monin <- flux_all$sensible_monin
caldern$LE_monin <- flux_all$latent_monin
caldern$LE_penman <- flux_all$latent_penman

# Diese Tabelle fasst Methodenmittelwerte kompakt zusammen.
method_summary <- data.frame(
  method = c("Priestley--Taylor", "Bulk--Residual", "Bowen", "Monin/Profile", "Penman"),
  mean_H = c(
    mean(caldern$H_pt, na.rm = TRUE),
    mean(caldern$H_bulk, na.rm = TRUE),
    mean(caldern$H_bowen, na.rm = TRUE),
    mean(caldern$H_monin, na.rm = TRUE),
    NA_real_
  ),
  mean_LE = c(
    mean(caldern$LE_pt, na.rm = TRUE),
    mean(caldern$LE_bulk, na.rm = TRUE),
    mean(caldern$LE_bowen, na.rm = TRUE),
    mean(caldern$LE_monin, na.rm = TRUE),
    mean(caldern$LE_penman, na.rm = TRUE)
  )
)

method_summary[, -1] <- round(method_summary[, -1], 1)
method_summary

# ---- method-comparison-plot ----
# Zweck: Methodenvergleich getrennt für H und LE darstellen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Die bisherigen Grafikeinstellungen werden gespeichert, damit sie nach dem Mehrfachplot wiederhergestellt werden können.
op <- par(mfrow = c(2, 1), mar = c(3.5, 4, 2, 1))

# Für den H-Plot wird ein gemeinsamer y-Achsenbereich über alle H-Methoden berechnet.
h_ylim <- range(caldern[, c("H_pt", "H_bulk", "H_bowen", "H_monin")], na.rm = TRUE)
# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$H_pt, type = "l", ylim = h_ylim,
     xlab = "Zeit", ylab = "W m-2", main = "Fühlbare Wärme H")
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$H_bulk, lty = 2)
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$H_bowen, lty = 3)
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$H_monin, lty = 4)
# Eine Referenzlinie ergänzt den Plot; h = 0 markiert hier meist den Wechsel des Vorzeichens.
abline(h = 0, lty = 2, col = "grey50")
# Die Legende erklärt, welche Linie zu welcher Mess- oder Modellreihe gehört.
legend("topright", legend = c("PT", "Bulk", "Bowen", "Monin"),
       lty = 1:4, bty = "n")

# Für den LE-Plot wird ein gemeinsamer y-Achsenbereich über alle LE-Methoden berechnet.
le_ylim <- range(caldern[, c("LE_pt", "LE_bulk", "LE_bowen", "LE_monin", "LE_penman")], na.rm = TRUE)
# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$LE_pt, type = "l", ylim = le_ylim,
     xlab = "Zeit", ylab = "W m-2", main = "Latente Wärme LE")
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$LE_bulk, lty = 2)
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$LE_bowen, lty = 3)
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$LE_monin, lty = 4)
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$LE_penman, lty = 5)
# Eine Referenzlinie ergänzt den Plot; h = 0 markiert hier meist den Wechsel des Vorzeichens.
abline(h = 0, lty = 2, col = "grey50")
# Die Legende erklärt, welche Linie zu welcher Mess- oder Modellreihe gehört.
legend("topright", legend = c("PT", "Bulk", "Bowen", "Monin", "Penman"),
       lty = 1:5, bty = "n")

# Die alten Grafikeinstellungen werden wiederhergestellt; dadurch beeinflusst dieser Plot spätere Plots nicht.
par(op)

# ---- closure-diagnostics ----
# Zweck: Energiebilanzschließung und offene Residualterme berechnen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Der Closure-Term zeigt, welche Restenergie nach H und LE bleibt.
caldern$closure_pt <- caldern$Q_minus_B - caldern$H_pt - caldern$LE_pt
# Der Closure-Term zeigt, welche Restenergie nach H und LE bleibt.
caldern$closure_bulk <- caldern$Q_minus_B - caldern$H_bulk - caldern$LE_bulk
# Der Closure-Term zeigt, welche Restenergie nach H und LE bleibt.
caldern$closure_bowen <- caldern$Q_minus_B - caldern$H_bowen - caldern$LE_bowen
# Der Closure-Term zeigt, welche Restenergie nach H und LE bleibt.
caldern$closure_monin <- caldern$Q_minus_B - caldern$H_monin - caldern$LE_monin
# Beim Penman-Pfad bleibt nach LE ein offener Term; er ist nicht automatisch eine gemessene fühlbare Wärme.
caldern$penman_open <- caldern$Q_minus_B - caldern$LE_penman

# Diese Tabelle fasst mittlere Closure- oder Offenheitsterme der Methoden zusammen.
closure_summary <- data.frame(
  method = c("PT", "Bulk", "Bowen", "Monin/Profile", "Penman open term"),
  mean_term = c(
    mean(caldern$closure_pt, na.rm = TRUE),
    mean(caldern$closure_bulk, na.rm = TRUE),
    mean(caldern$closure_bowen, na.rm = TRUE),
    mean(caldern$closure_monin, na.rm = TRUE),
    mean(caldern$penman_open, na.rm = TRUE)
  )
)

closure_summary$mean_term <- round(closure_summary$mean_term, 1)
closure_summary

# ---- closure-plot ----
# Zweck: Schließungs- und Offenheitsdiagnostik als Zeitreihe darstellen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$closure_pt, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Schließungs- und Offenheitsdiagnostik")
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$closure_bulk, lty = 2)
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$closure_bowen, lty = 3)
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$closure_monin, lty = 4)
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$penman_open, lty = 5)
# Eine Referenzlinie ergänzt den Plot; h = 0 markiert hier meist den Wechsel des Vorzeichens.
abline(h = 0, lty = 2, col = "grey50")
# Die Legende erklärt, welche Linie zu welcher Mess- oder Modellreihe gehört.
legend("topright", legend = c("PT", "Bulk", "Bowen", "Monin", "Penman offen"),
       lty = 1:5, bty = "n")

# ---- package-closure ----
# Zweck: Optionale Paketfunktion zur Energiebilanzdiagnostik verwenden, falls vorhanden.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Diese Abfrage schützt den Code: Die Paketfunktion wird nur genutzt, wenn sie in der installierten Version vorhanden ist.
if ("energy_balance_closure" %in% getNamespaceExports("fieldClim")) {
  closure_flux <- energy_balance_closure(flux_all)
# head() zeigt die ersten Zeilen und ist ein schneller Format- und Plausibilitätscheck.
  head(closure_flux)
}
