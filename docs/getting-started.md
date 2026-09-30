# Getting started

StatiX is a Shiny app and three collector scripts, written in 2015 and 2016 for one ISP's network. There is no package or image: you copy the repository onto a Linux server that can reach your RADIUS database and your routers, put your own values into the scripts, start the collectors for the tabs you want, and then start the app.

## What you need

| For | You need |
|---|---|
| Every tab | A Linux server, R, a JDK (for `rJava`), MongoDB listening on `localhost:27017` with no login, and the R packages `shiny`, `dplyr`, `ggplot2`, `scales`, `stringr` and `RMongo` |
| Usuarios PPPoE and Usuarios por Servicio | Read access to the FreeRADIUS MySQL database (tables `nas` and `radacct`), and the R packages `RMySQL` and `rjson` |
| Tráfico | `snmpget` from Net-SNMP, SNMP read access to your border routers, and the R package `rmongodb` |
| Usuarios HotSpot and Búsqueda de usuarios HotSpot | RouterOS API access (port 8728) to your main router and to every hotspot router, Python 3, and the R package `rPython`. For UniFi access points, also the Python `unifi` module and admin access to the UniFi controllers |

!!! warning "Old code, old packages"
    `RMongo`, `rmongodb` and `rPython` are no longer on CRAN; the commands below install their last versions from the CRAN archive. `RMongo` was archived in 2018 because it was never updated for MongoDB 3.6 and 4.0. StatiX has not been tested on a current R or a current MongoDB. The scripts last ran on R 3.2.

## Install the R packages

```r
install.packages(c("shiny", "dplyr", "ggplot2", "scales", "stringr", "rjson", "RMySQL", "rJava"))
install.packages("https://cran.r-project.org/src/contrib/Archive/RMongo/RMongo_0.0.25.tar.gz",
                 repos = NULL, type = "source")

# Only for the Tráfico tab
install.packages("https://cran.r-project.org/src/contrib/Archive/rmongodb/rmongodb_1.8.0.tar.gz",
                 repos = NULL, type = "source")

# Only for the HotSpot tabs
install.packages("https://cran.r-project.org/src/contrib/Archive/rPython/rPython_0.0-6.tar.gz",
                 repos = NULL, type = "source")
```

If `rJava` fails to install, run `sudo R CMD javareconf` with the JDK installed and try again.

## Put the code in place

The scripts call each other and write their lists by absolute path, `/home/tecnico/EstadisticasWEB`. The shortest path is to clone the repository there:

```bash
git clone https://github.com/GeiserX/statix.git /home/tecnico/EstadisticasWEB
cd /home/tecnico/EstadisticasWEB
```

To use another directory, clone it there and change every line this command lists:

```bash
grep -rn '/home/tecnico/EstadisticasWEB' .
```

## First run: the PPPoE tabs

1. Put your RADIUS database's user, password and host into `scriptServicioBBDD.R` line 13. A MySQL user with `SELECT` on `nas` and `radacct` is enough. If your PPPoE service names are not `datos` and `telefonia`, or your suspension pool is different, see [Configuration](configuration.md#radius).
2. Start the collector in the background. It loops forever:

    ```bash
    Rscript scriptServicioBBDD.R &
    ```

3. Start the app:

    ```bash
    Rscript -e 'shiny::runApp(".", port = 8081, host = "127.0.0.1")'
    ```

4. Open <http://127.0.0.1:8081>.

It works when the **Usuarios PPPoE** tab shows a blue line for **Gran total** with a red trend line through it, and the text under the chart reads `Usuarios activos actuales: N`, where N is the number of open sessions in `radacct`. The collector writes a point every five minutes, so the first line appears after its second pass, about ten minutes after it starts. The **Escoja MikroTik** list holds every NAS in the `nas` table by its short name.

A blank chart with no error means something failed: Shiny's error messages are hidden. See [Troubleshooting](troubleshooting.md#a-chart-is-blank-and-shows-no-error).

## Add the Tráfico tab

1. Put your border routers' addresses and SNMP community into `ScriptTrafico.R`. Each address is written three times; [Configuration](configuration.md#snmp-traffic) lists the lines.
2. Check that one reading works by hand, with your router's address and community:

    ```bash
    snmpget -v1 -c public 192.0.2.1 1.3.6.1.2.1.31.1.1.1.6.1
    ```

    If it returns nothing, try `-v2c`, and change `-v1` to `-v2c` in the script too.

3. Start it: `Rscript ScriptTrafico.R &`. The first point is written 30 seconds after the start, the second about five minutes later, then one every ten minutes.

## Add the HotSpot tabs

1. Set the main router's address and the RouterOS user and password in `MACsPrincipal.py`, `IPsPrincipal.py` and `scriptHotSpot.py`, and the UniFi controllers in `scriptServicioBBDDHS.R`. [Configuration](configuration.md#hotspot-routers) lists every line.
2. Check the main router by hand: `python3 MACsPrincipal.py` must print one `=user=` line per active HotSpot user.
3. Start it: `Rscript scriptServicioBBDDHS.R &`. One pass connects to every hotspot router in turn, then writes a point; after that, one pass every ten minutes. The **Usuarios HotSpot** list fills after the first pass.

`scriptServicioBBDDHSdelay.R` is the same collector with a five-minute wait before its first pass, for starting at boot before the network is up. Run one of the two, never both: both write to the same collections.

## Start everything at boot

Each `AtStartup*.sh` file is one line that starts one process by its absolute path:

| Script | Starts |
|---|---|
| `AtStartup.sh` | the PPPoE collector |
| `AtStartupTraffic.sh` | the traffic collector |
| `AtStartupHS.sh` | the HotSpot collector |
| `AtStartupHSdelay.sh` | the HotSpot collector, five minutes late |
| `AtStartupWEB.sh` | the app on `127.0.0.1:8081` |

How they were hooked to boot is not part of the repository. One way is a `crontab -e` entry per script for the user that owns the directory:

```text
@reboot sh /home/tecnico/EstadisticasWEB/AtStartup.sh >> /home/tecnico/statix-pppoe.log 2>&1
@reboot sh /home/tecnico/EstadisticasWEB/AtStartupWEB.sh >> /home/tecnico/statix-web.log 2>&1
```

The log files keep the collectors' `ERROR :` lines, which [Troubleshooting](troubleshooting.md) refers to.

Next: [Usage](usage.md) explains what each tab shows.
