---
hide:
  - navigation
---

# StatiX { .sx-visually-hidden }

<p align="center">
  <img src="images/banner.svg" alt="StatiX: every subscriber, router and hotspot, graphed" width="100%">
</p>

<p align="center">
  <a href="https://github.com/GeiserX/statix/stargazers"><img alt="GitHub Stars" src="https://img.shields.io/github/stars/GeiserX/statix?style=flat-square&logo=github"></a>
  <a href="https://github.com/GeiserX/statix/releases"><img alt="Release" src="https://img.shields.io/github/v/release/GeiserX/statix?style=flat-square"></a>
  <a href="https://github.com/GeiserX/statix/blob/main/LICENSE"><img alt="License: GPL-3.0-or-later" src="https://img.shields.io/github/license/GeiserX/statix?style=flat-square"></a>
</p>

---

**StatiX** is an R Shiny web app that graphs how an ISP's network is used: open PPPoE sessions per router and per service, traffic on the upstream links, and HotSpot users per access point, plus a search that shows where a HotSpot user connected and how good the signal was. Three collector scripts read FreeRADIUS, the routers' SNMP counters and the MikroTik RouterOS API every five or ten minutes and store the counts in MongoDB; the app draws them. RADIUS tells you how many sessions are open right now. StatiX keeps the history, so a drop at 3 a.m. or a link running near its limit is still visible the next morning. It was written for one ISP in 2015 and 2016 and is published as it ran there: the settings are values in the scripts. Start with [Getting started](getting-started.md), then [Usage](usage.md).

<div class="grid cards" markdown>

-   :material-rocket-launch-outline: **[Getting started](getting-started.md)**

    ---

    What it needs, the R packages that now come from the CRAN archive, and the first chart ten minutes after the collector starts.

-   :material-chart-line: **[Usage](usage.md)**

    ---

    The five tabs, what each line on a chart means, and how the date range and the zoom work.

-   :material-tune: **[Configuration](configuration.md)**

    ---

    Every value to set, file by file and line by line: RADIUS, SNMP, RouterOS, UniFi, paths and limits.

-   :material-sitemap-outline: **[How it works](how-it-works.md)**

    ---

    The three collectors, what they store in MongoDB and how often, and what the app trusts.

</div>

## What you see

Five tabs across the top of the app. The labels are in Spanish; [Usage](usage.md) translates each one.

| Tab | What it shows | Read from | New point every |
|---|---|---|---|
| Usuarios PPPoE | Open PPPoE sessions, all routers or one, with a straight trend line | FreeRADIUS accounting | 5 minutes |
| Usuarios por Servicio | Open sessions per service: paying data, unpaid or cancelled data, phone | FreeRADIUS accounting | 5 minutes |
| Tráfico | Inbound and outbound Mbit/s on the upstream links, with capacity and warning lines | SNMP interface counters | 10 minutes |
| Usuarios HotSpot | HotSpot users per hotspot router or UniFi access point | RouterOS API, UniFi controllers | 10 minutes |
| Búsqueda de usuarios HotSpot | One HotSpot user's history: MAC, IP, traffic, where they connected, signal | RouterOS API, UniFi controllers | 10 minutes |

## How it runs

- One Linux server that can reach your RADIUS database and your routers runs everything: MongoDB, three collector processes and the Shiny app.
- Each collector is an R script that loops forever, catches its own errors and tries again five minutes later. Only the ones for the tabs you want need to run.
- The app listens on `127.0.0.1:8081` with the shipped start script and redraws every chart every five minutes.
- The scripts expect to live in `/home/tecnico/EstadisticasWEB`. [Configuration](configuration.md#paths) lists every line to change to put them elsewhere.

## What it does not do

- It sends no alerts. The capacity and warning lines on the Traffic tab are drawn, nothing more.
- It has no configuration file, no installer and no container image. Setting it up means editing the scripts.
- It never deletes data. MongoDB grows by a few hundred documents per router per day for as long as the collectors run.
- The HotSpot tabs work with MikroTik RouterOS and UniFi only. The PPPoE tabs work with any NAS that sends RADIUS accounting, and the Traffic tab with any router that serves the standard interface counters over SNMP.
- It has not been tested on a current R or MongoDB. The scripts last ran on R 3.2.

## Privacy

- The HotSpot collector saves, every ten minutes and for every logged-in HotSpot user, the user name, MAC address, IP address, login method, session uptime, bytes in and out, the access point or hotspot router, and the signal strength. These are your subscribers' personal data, and nothing deletes them.
- The app has no login. Anyone who can reach its port sees every chart and every HotSpot user's history. Keep it on `127.0.0.1`, as the shipped start script does, and reach it through an SSH tunnel or a reverse proxy that asks for a login.
- The RADIUS, RouterOS and UniFi passwords are written into the scripts. Use read-only accounts ([How it works](how-it-works.md#security-model) says which rights each needs), and never commit your filled-in copies.
- Nothing leaves the server except the queries to your own RADIUS database, routers and UniFi controllers. The browser that opens the app loads the Lato font from Google Fonts.

## Getting help

- Something broken: read [Troubleshooting](troubleshooting.md), then open an [issue](https://github.com/GeiserX/statix/issues) with the details it lists.
- A security problem: follow the [security policy](https://github.com/GeiserX/statix/blob/main/SECURITY.md), never a public issue.
- The code itself: the [repository](https://github.com/GeiserX/statix).

## Related projects

- [genieacs-container](https://github.com/GeiserX/genieacs-container): Helm chart and container for the GenieACS TR-069 server.
- [router-express](https://github.com/GeiserX/router-express): configures client routers and syncs their databases.
- [services-isp](https://github.com/GeiserX/services-isp): automates common ISP operational tasks.
- [ScriptPoblar](https://github.com/GeiserX/ScriptPoblar): adopts a whole network of devices into CRM Control in parallel.

## License

StatiX is released under the [GPL-3.0-or-later](https://github.com/GeiserX/statix/blob/main/LICENSE) license.
