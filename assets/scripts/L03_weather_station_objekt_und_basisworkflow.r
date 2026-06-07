# ============================================================
# Kommentiertes R-Skript zu: L03_weather_station_objekt_und_basisworkflow.qmd
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

# ---- prepare-working-variables ----
# Zweck: Arbeitsvariablen für Netto-Strahlung, Bodenwärmestrom und verfügbare Energie anlegen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Q_star ist die Arbeitsreihe für Netto-Strahlung.
caldern$Q_star <- caldern$rad_net
# B ist die Arbeitsreihe für Bodenwärmestrom.
caldern$B <- caldern$heatflux_soil
# A ist verfügbare Energie: Netto-Strahlung minus Bodenwärmestrom.
caldern$A <- caldern$Q_star - caldern$B

# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(caldern[, c("Q_star", "B", "A")])

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

# ---- object-content ----
# Zweck: Inhalt und Tabellenform des weather_station-Objekts inspizieren.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# class() zeigt, welche Objektklasse R dem Objekt zuweist.
class(ws)
# Spaltennamen prüfen, damit spätere Zugriffe auf Messgrößen nachvollziehbar sind.
names(ws)
# head() zeigt die ersten Zeilen und ist ein schneller Format- und Plausibilitätscheck.
head(as.data.frame(ws))

# ---- inspect-weather-station ----
# Zweck: Prüfen, welche Eingaben im Objekt vorhanden sind und welche Methoden strukturell bereit sind.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# inspection sammelt Diagnosen dazu, ob das Stationsobjekt die nötigen Eingaben enthält.
inspection <- inspect_weather_station_inputs(ws)

# Spaltennamen prüfen, damit spätere Zugriffe auf Messgrößen nachvollziehbar sind.
names(inspection)
inspection$summary

# subset() filtert hier gezielt problematische oder fehlende Felder aus der Diagnosetabelle.
subset(
  inspection$fields,
  source_status != "present"
)

inspection$method_readiness[, c(
  "method",
  "missing_fields",
  "partial_fields",
  "ready",
  "notes"
)]

# ---- modeled-radiation ----
# Zweck: Modellierte Strahlung als Plausibilitätsvergleich berechnen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

lon <- 8.6832
lat <- 50.8405
elev <- 261
slope <- 0
exposition <- 0
valley <- FALSE
surface_type <- "field"

# K_down ist die einfallende kurzwellige Strahlung.
caldern$K_down_model <- rad_sw_in(
  caldern$datetime,
  lon, lat, elev, caldern$Ta_2m,
  slope, exposition
)

# Q_star ist die Arbeitsreihe für Netto-Strahlung.
caldern$Q_star_model <- rad_bal(
  caldern$datetime, lon, lat, elev,
  caldern$Ta_2m, caldern$Huma_2m,
  slope, exposition, valley, surface_type,
  caldern$Ts
)

# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(caldern[, c("rad_sw_in", "K_down_model", "rad_net", "Q_star_model")])

# ---- modeled-radiation-plot ----
# Zweck: Gemessene und modellierte Strahlung gemeinsam darstellen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Die bisherigen Grafikeinstellungen werden gespeichert, damit sie nach dem Mehrfachplot wiederhergestellt werden können.
op <- par(mfrow = c(2, 1), mar = c(3.5, 4, 2, 1))

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$rad_sw_in, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Kurzwellige Einstrahlung")
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$K_down_model, lty = 2)
# Die Legende erklärt, welche Linie zu welcher Mess- oder Modellreihe gehört.
legend("topright", legend = c("gemessen", "modelliert"),
       lty = c(1, 2), bty = "n")

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$rad_net, type = "l",
     xlab = "Zeit", ylab = "W m-2", main = "Netto-Strahlung")
# lines() ergänzt eine weitere Zeitreihe in einen bereits vorhandenen Plot.
lines(caldern$datetime, caldern$Q_star_model, lty = 2)
# Die Legende erklärt, welche Linie zu welcher Mess- oder Modellreihe gehört.
legend("bottomleft", legend = c("gemessen", "modelliert"),
       lty = c(1, 2), bty = "n")

# Die alten Grafikeinstellungen werden wiederhergestellt; dadurch beeinflusst dieser Plot spätere Plots nicht.
par(op)

# ---- helper-variables ----
# Zweck: Zwischengrößen wie absolute Feuchte, Luftdichte und potentielle Temperatur berechnen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Absolute Feuchte übersetzt relative Feuchte und Temperatur in eine physikalisch direktere Feuchtegröße.
caldern$hum_abs <- hum_absolute(caldern$Huma_2m, caldern$Ta_2m)
# Verdampfungswärme ist ein temperaturabhängiger Umrechnungsfaktor für latente Wärme.
caldern$evap_heat <- hum_evap_heat(caldern$Ta_2m)
# Der Feuchtegradient beschreibt die Änderung der Feuchte zwischen zwei Messhöhen.
caldern$hum_gradient <- hum_moisture_gradient(
  caldern$Huma_2m,
  caldern$Huma_10m,
  caldern$Ta_2m,
  caldern$Ta_10m,
  2, 10, 261
)
# Luftdichte wird für sensible Wärmeflüsse benötigt, weil diese von Masse und Wärmekapazität der Luft abhängen.
caldern$air_density <- pres_air_density(261, caldern$Ta_2m)
# Potentielle Temperatur macht Lufttemperaturen aus unterschiedlichen Höhen/Druckniveaus vergleichbarer.
caldern$theta <- temp_pot_temp(caldern$Ta_2m, 261)

# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(caldern[, c("hum_abs", "evap_heat", "hum_gradient", "air_density", "theta")])

# ---- helper-plot ----
# Zweck: Ausgewählte Hilfsgrößen als Zeitreihe prüfen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Die bisherigen Grafikeinstellungen werden gespeichert, damit sie nach dem Mehrfachplot wiederhergestellt werden können.
op <- par(mfrow = c(2, 1), mar = c(3.5, 4, 2, 1))

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$hum_abs, type = "l",
     xlab = "Zeit", ylab = "", main = "Absolute Feuchte")

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$air_density, type = "l",
     xlab = "Zeit", ylab = "kg m-3", main = "Luftdichte")

# Die alten Grafikeinstellungen werden wiederhergestellt; dadurch beeinflusst dieser Plot spätere Plots nicht.
par(op)

# ---- turbulence-screening ----
# Zweck: Windprofil- und Stabilitätsgrößen für spätere Profilmethoden berechnen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

z0 <- turb_roughness_length("field")
z0

# u_star ist die Schubspannungsgeschwindigkeit, eine zentrale Turbulenzkenngröße.
caldern$u_star <- turb_ustar(caldern$Windspeed_2m, 2, surface_type = "field")

# Die Richardson-Zahl beschreibt das Verhältnis von thermischer Stabilität zu mechanischer Durchmischung.
caldern$richardson <- turb_flux_grad_rich_no(
  caldern$Ta_2m,
  caldern$Ta_10m,
  2,
  10,
  caldern$Windspeed_2m,
  caldern$Windspeed_10m,
  261
)

# exchange_temp ist eine Diagnose für den turbulenten Austausch im Temperaturprofil.
caldern$exchange_temp <- turb_flux_ex_quotient_temp(
  caldern$Ta_2m,
  caldern$Ta_10m,
  2,
  10,
  caldern$Windspeed_2m,
  caldern$Windspeed_10m,
  261,
  "field"
)

# summary() zeigt einfache Kennwerte und hilft, Ausreißer, NA-Werte oder falsche Größenordnungen zu erkennen.
summary(caldern[, c("u_star", "richardson", "exchange_temp")])

# ---- turbulence-plot ----
# Zweck: Turbulenz- und Stabilitätsdiagnosen grafisch prüfen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Die bisherigen Grafikeinstellungen werden gespeichert, damit sie nach dem Mehrfachplot wiederhergestellt werden können.
op <- par(mfrow = c(3, 1), mar = c(3.5, 4, 2, 1))

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$u_star, type = "l",
     xlab = "Zeit", ylab = "m s-1", main = "u*: Schubspannungsgeschwindigkeit")

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$richardson, type = "l",
     xlab = "Zeit", ylab = "Ri", main = "Gradient-Richardson-Zahl")
# Eine Referenzlinie ergänzt den Plot; h = 0 markiert hier meist den Wechsel des Vorzeichens.
abline(h = 0, lty = 2, col = "grey50")

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(caldern$datetime, caldern$exchange_temp, type = "l",
     xlab = "Zeit", ylab = "", main = "Austauschdiagnose Temperatur")

# Die alten Grafikeinstellungen werden wiederhergestellt; dadurch beeinflusst dieser Plot spätere Plots nicht.
par(op)

# ---- object-output ----
# Zweck: Ergebnisobjekt nach einer Beispielrechnung in eine Tabelle überführen.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Eine Beispielrechnung wird auf dem Stationsobjekt ausgeführt; das Ergebnis bleibt als Objekt erhalten.
ws_pt <- turb_flux_calc(ws, pt_only = TRUE)
# Das Ergebnisobjekt wird in eine normale Tabelle umgewandelt.
ws_table <- as.data.frame(ws_pt)

# Spaltennamen prüfen, damit spätere Zugriffe auf Messgrößen nachvollziehbar sind.
names(ws_table)
# head() zeigt die ersten Zeilen und ist ein schneller Format- und Plausibilitätscheck.
head(ws_table)

# ---- object-simple-plot ----
# Zweck: Eine einfache Zeitreihe aus der Objekttabelle plotten.
# Hinweis: Dieser Abschnitt stammt aus dem QMD-Dokument und ist hier als eigenständiger, kommentierter R-Code abgelegt.

# Eine einfache base-R-Grafik erzeugen; type = 'l' bedeutet meist Linienplot, type = 'p' Punktplot.
plot(ws_table$datetime, ws_table$temp, type = "l",
     xlab = "Zeit", ylab = "°C", main = "Temperatur aus dem weather_station-Objekt")
