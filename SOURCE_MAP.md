# Quellen- und Integrationslogik

Die vier Kurseinheiten wurden aus den hochgeladenen Rmd-Dateien neu komponiert.

## Hauptquellen

- `fieldclim_missing_data(3).Rmd` und `fieldclim_missing_data_en(1).Rmd`: Datenprüfung, Missingness, Strahlung, Bodenwärmestrom, verfügbare Energie.
- `fieldclim_workflow_steps(3).Rmd` und `fieldclim_workflow_steps_en(2).Rmd`: `weather_station`-Objekt, Default- vs. Objektmethode, Radiation/Soil/Helper/Stability-Workflows.
- `fieldclim_flux_workflow(3).Rmd` und `fieldclim_flux_workflow_en(3).Rmd`: Wärmeflussmethoden und Paketworkflow.
- `fieldclim_m2m_en.Rmd`: Messarchitektur, Methodenwahl, reportbare Aussagen.
- `fieldclim_formula_reference_vignette(1).Rmd`: Referenznotation, Package-Mapping, Closure-Semantik.

## Didaktische Entscheidung

Die deutschen Dateien wurden für Sprache und Illustration stärker berücksichtigt. Die englischen Dateien wurden als fachlich aktuellere Referenzfassung behandelt. Bei Abweichungen wurde die englische spätere Fassung als inhaltliche Kontrollspur genutzt.

Der Code wurde nicht als 1:1-Kopie übernommen, sondern für den Kursstil vereinfacht: base R, explizite Spaltenzuordnung, keine Shiny-/Fancy-Elemente, relative Pfade mit `here::here()`.
