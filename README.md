<p align="center">
  <img src="docs/images/banner.svg" alt="StatiX" />
</p>

<h1 align="center">StatiX</h1>

<p align="center">
  <a href="LICENSE"><img src="https://img.shields.io/github/license/GeiserX/statix" alt="License" /></a>
</p>

<p align="center">ISP network statistics and monitoring dashboard</p>

---

An R/Shiny webapp that shows graphics for an ISP. It includes:

1) Graphics for connected PPPoE users in the entire network
2) Graphics for connected users split by service
3) Graphics for inbound/outbound traffic in the Area Border Routers
4) Graphics for HotSpot users
5) A search tool to find issues of HotSpot users: where the user is located and the stats of the connection.

## Quick start

You need MongoDB on `localhost:27017`, MySQL access to the RADIUS server, R with `shiny`, `RMongo`, `RMySQL`, `dplyr`, `ggplot2`, `scales`, `stringr` and `rjson`, and Python 2 plus PHP for the HotSpot collectors. The collector writes `listaMKT.csv` to `/home/tecnico/EstadisticasWEB`, where the app reads it, so clone the repo there or edit `scriptServicioBBDD.R` line 20. Set the RADIUS host and password in the collector, start it, then the web app:

```bash
Rscript scriptServicioBBDD.R
Rscript -e 'shiny::runApp(".", port = 8081, host = "127.0.0.1")'
```

Open http://127.0.0.1:8081. The `AtStartup*.sh` scripts show which collector feeds which tab.

## Related projects

- [genieacs-container](https://github.com/GeiserX/genieacs-container): Helm chart and container for GenieACS TR-069
- [router-express](https://github.com/GeiserX/router-express): auto-configures client routers and syncs databases
- [services-isp](https://github.com/GeiserX/services-isp): automates common ISP operational tasks
- [ScriptPoblar](https://github.com/GeiserX/ScriptPoblar): adopts a whole network of devices into CRM Control in parallel

## License

[GPL-3.0-or-later](LICENSE)
