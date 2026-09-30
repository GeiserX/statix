# Configuration

StatiX has no configuration file. Every setting is a value written into a script, and this page lists where each one is. Line numbers are for the current `main`. `scriptServicioBBDDHSdelay.R` is `scriptServicioBBDDHS.R` with one extra line near the top (line 5, the five-minute wait), so from there on its line numbers are one higher.

## Paths

| Setting | Where | In the code |
|---|---|---|
| The directory the scripts run from | `AtStartup*.sh` line 1; `scriptServicioBBDD.R` line 20; `scriptServicioBBDDHS.R` lines 16, 43, 72 and 194 | `/home/tecnico/EstadisticasWEB` |
| An extra R library directory | line 1 of `scriptServicioBBDD.R`, `ScriptTrafico.R`, `scriptServicioBBDDHS.R` and `scriptServicioBBDDHSdelay.R` | `/home/tecnico/R/x86_64-pc-linux-gnu-library/3.2` |

`grep -rn '/home/tecnico' .` lists every line. The library line only adds a directory to R's search path; R ignores it when the directory does not exist, so it can stay.

`server.R` reads `listaMKT.csv` and `listaHS.csv` from the app's own directory, and the collectors write them to the path above: both must be the same directory.

## RADIUS

Used by `scriptServicioBBDD.R`, which feeds the PPPoE and service tabs.

| Setting | Where | In the code |
|---|---|---|
| MySQL user, password, database and host | line 13 | `root`, `PASSWORD`, `radius`, `X.X.X.X` |
| The PPPoE service name for phone | line 28 | `telefonia` |
| The PPPoE service name for data | line 29 | `datos` |
| The suspension pool for unpaid and cancelled subscribers | line 32 | the first two numbers of the pool's addresses, as text |

The collector runs three queries and nothing else:

```sql
SELECT nasname, shortname FROM nas ORDER BY shortname;
SELECT nasipaddress FROM radacct WHERE acctstoptime IS NULL;
SELECT calledstationid, framedipaddress FROM radacct WHERE acctstoptime IS NULL;
```

So the MySQL user needs `SELECT` on `nas` and `radacct` and nothing more. Each router's `nasname` in the `nas` table must be its address exactly as it appears in `radacct.nasipaddress`: the collector stores counts under the address and the app looks them up under `nasname`.

The service names are compared with `Called-Station-Id`, which MikroTik fills with the PPPoE service name. The pool is compared on the first two numbers of the address only, so it must be a `/16`.

## SNMP traffic

Used by `ScriptTrafico.R`, which feeds the Tráfico tab. It reads three border routers: one for the first upstream link, and two whose traffic is added up as the second link.

| Setting | Where |
|---|---|
| First link's router address | lines 12-13, 28-29 and 73-74 |
| Second link's first router address | lines 16-17, 32-33 and 77-78 |
| Second link's second router address | lines 20-21, 36-37 and 81-82 |
| SNMP version and community | the same lines: `-v1 -c public` |
| Which interface is read | the `.1` at the end of each OID, the interface index |
| Capacity (purple) and warning (orange) lines | `server.R` lines 251-260 |
| The labels in the **Escoja ABR** list | `ui.R` lines 77-79 |

Every address is written three times (the first reading, the second reading 30 seconds later, and the loop). Change all three. The OIDs are `ifHCInOctets` (`1.3.6.1.2.1.31.1.1.1.6`) and `ifHCOutOctets` (`1.3.6.1.2.1.31.1.1.1.10`), the 64-bit interface byte counters; to read another interface, change the last number of both. Most SNMP agents serve 64-bit counters over `-v2c` only.

The values in the `ui.R` list are MongoDB collection names (`Total`, `Ono`, and `Aire` for the sum of `Aire1` and `Aire2`); change the labels freely, but the values must match what the collector writes. Adding or removing a link means editing the readings, the rates and the inserts in `ScriptTrafico.R`, the list in `ui.R`, and the collection reads (lines 221-238) and limits (lines 251-260) in `server.R`.

## HotSpot routers

Used by the three Python helpers that `scriptServicioBBDDHS.R` runs.

| Setting | Where | In the code |
|---|---|---|
| The main router's address | `MACsPrincipal.py` line 138, `IPsPrincipal.py` line 138 | `x.x.x.x` |
| The RouterOS API port | line 138 of `MACsPrincipal.py`, `IPsPrincipal.py` and `scriptHotSpot.py` | `8728` |
| The RouterOS user and password | line 157 of the same three files | `admin`, `PASSWORD` |
| How long each hotspot router gets to answer | `scriptHotSpot.py` lines 172-179 | 2 seconds |

The main router gives two lists: the active HotSpot users (`/ip/hotspot/active/print`) and its EoIP tunnels (`/interface/eoip/print`). Every EoIP tunnel's remote address is taken to be a hotspot router, and `scriptHotSpot.py` reads that router's wireless registration table. The tunnel's name becomes the hotspot's label with its first word dropped and hyphens turned into spaces: a tunnel named `eoip-Plaza-Mayor` shows as `Plaza Mayor`.

The helpers use the plain API port without TLS. They log in the way RouterOS did before version 6.43; see [Troubleshooting](troubleshooting.md#the-hotspot-charts-get-no-new-points).

## UniFi controllers

`scriptServicioBBDDHS.R` reads two UniFi controllers through `rPython` and the Python `unifi` module (`from unifi.controller import Controller`).

| Setting | Where |
|---|---|
| First controller | line 141: `Controller('UNIFI.IP', 'admin', 'PASSWORD', '8443', 'v4', '<site id>')` |
| Second controller | line 152, the same arguments |

The arguments are the controller's address, user, password, port, controller version and site id. The collector only lists access points and clients. With one controller, the rest of the script still expects the second controller's results, so replace lines 150-159 (the second `python.exec(...)` block and the two lines that read its results) with an empty result:

```r
NameAPs2 <- NameAPs1[0, , drop = FALSE]
clients2 <- list()
```

Without any UniFi controller the script needs more changes than that, because the UniFi results also feed each user's location.

## MongoDB

Every script connects to MongoDB on `localhost`, port `27017`, with no user and no password: `server.R` lines 71, 150, 219, 315, 374 and 387; `scriptServicioBBDD.R` line 37; `ScriptTrafico.R` lines 48 and 86; `scriptServicioBBDDHS.R` lines 213 and 228. Keep MongoDB bound to `localhost` only. [How it works](how-it-works.md#what-is-stored) lists the databases.

## The web app

| Setting | Where | In the code |
|---|---|---|
| Address and port | `AtStartupWEB.sh` | `127.0.0.1`, `8081` |
| The earliest date each tab lets you pick | `server.R` lines 16, 25, 34 and 43 | dates in 2015 |
| The time zone used to find the chosen days | `server.R` lines 130-131, 198-199 and 360-361 | `Europe/Madrid` |
| How often a chart redraws | `server.R` lines 64, 143, 212 and 308 | `1000*60*5`, five minutes |
| How often the date pickers reset | `server.R` lines 14, 23, 32 and 41 | `1000*60*60`, one hour |
| Hidden error messages | `ui.R` lines 14-16 | on |
| The page theme | `www/bootstrap.css` | Bootswatch 3.3.5 |

## Intervals

| Collector | Where | Every |
|---|---|---|
| PPPoE and services | `scriptServicioBBDD.R` line 65 | 5 minutes |
| Traffic | `ScriptTrafico.R` line 115, plus 5 more minutes after the error on line 117 | 10 minutes in practice |
| HotSpot | `scriptServicioBBDDHS.R` line 246 | 10 minutes |
| Retry after an error | the last `Sys.sleep` of each script | 5 minutes |

Each sleep subtracts the time the pass took, so points stay on a steady beat. Changing an interval changes only how far apart the points are; the charts need no edit. The traffic rates divide by the real time between readings, so they stay right whatever the interval.
