# Usage

The app's labels are in Spanish; this page gives each one with its meaning. Open the app at the address it was started on, <http://127.0.0.1:8081> with the shipped start script. Every chart redraws itself every five minutes; there is nothing to refresh.

## The date range

Every chart tab has **Escoja rango de fechas** (choose a date range) under the chart. It starts at yesterday and ends at tomorrow, which means "up to the latest point". Pick an earlier end date to stop the chart there. The earliest date each tab lets you pick is fixed in `server.R` (see [Configuration](configuration.md#the-web-app)).

The pickers are rebuilt every hour so their dates follow the calendar. That also puts them back to yesterday and tomorrow: a range you picked lasts until an hour after you opened the page, and then resets every hour.

The vertical axis zooms to the lowest and highest value inside the chosen days, with a margin: 9 sessions above and below on the PPPoE tabs, 4 below and 2 above on the HotSpot tab. On the Traffic tab it runs from 0 to the highest inbound value plus 49 Mbit/s, so outbound traffic above that is cut off at the top. When the first or the last chosen day has no point, the axis does not zoom.

## Usuarios PPPoE (PPPoE users)

The number of open PPPoE sessions over time.

- **Escoja MikroTik** (choose a MikroTik): **Gran total** (all routers together) or one router, listed by its short name from the RADIUS `nas` table. Any NAS in that table appears, whatever its make.
- The blue line is the session count, one point every five minutes.
- The red line is a straight-line fit over the days shown: a quick read of whether the number of connected subscribers is growing or shrinking.
- **Usuarios activos actuales** (current active users) under the chart is the latest count for the selection.

A session counts as open while its `radacct` row has no stop time. A router that stops sending accounting leaves its sessions open in `radacct`, and they stay in the count until RADIUS closes them.

## Usuarios por Servicio (users by service)

The same count, split by service. **Escoja Servicio** (choose a service):

- **Activos Internet** (active internet): open data sessions whose address is outside the suspension pool.
- **Impagos y Bajas** (unpaid and cancelled): open data sessions whose address is in the suspension pool, where the network puts subscribers who have not paid or have left.
- **Telefonia** (phone): open phone sessions.

Data and phone are told apart by the RADIUS `Called-Station-Id`, which MikroTik fills with the PPPoE service name: `datos` for data and `telefonia` for phone in the original network. [Configuration](configuration.md#radius) says how to change the names and the pool. The red trend line and the current count work as on the first tab.

## Tráfico (traffic)

Traffic on the links to your upstream providers, in Mbit/s.

- **Escoja ABR** (choose an area border router): **Total**, or one of the two upstream links. The second link is two border routers added together.
- The **SpeedIN** and **SpeedOUT** lines are inbound and outbound traffic.
- The purple line is the link's capacity and the orange line its warning level: 1560 and 1310 Mbit/s for the total, 1000 and 750 for the first link, 720 and 560 for the second. They are fixed numbers in `server.R`; see [Configuration](configuration.md#snmp-traffic).
- **Tráfico actual - Entrada / Salida** under the chart is the latest inbound and outbound value.

Each point is the average since the previous reading, ten minutes in normal running, so a burst shorter than that shows flattened. A megabit here is 1,048,576 bits, so the values read about 5% lower than the same traffic in the 1,000,000-bit megabits most routers display.

## Usuarios HotSpot (HotSpot users)

The number of users on each hotspot over time, one point every ten minutes. **Escoja HotSpot** (choose a hotspot):

- **Total**: every user logged in to the HotSpot on the main router.
- A hotspot router, shown by its name and address: the logged-in HotSpot users whose device is in that router's wireless registration table.
- A UniFi access point, shown by its name: every client the UniFi controller reports on it, logged in to the HotSpot or not.

**Usuarios activos actuales** under the chart is the latest count. This tab has no trend line.

## Búsqueda de usuarios HotSpot (HotSpot user search)

The history of one HotSpot user. **Usuario Hotspot** lists every HotSpot user name StatiX has ever seen, in alphabetical order. Pick one to see a table of every ten-minute sample of that user, newest first, eight rows a page:

| Column | Holds |
|---|---|
| `macs` | the device's MAC address |
| `address` | the IP address the HotSpot gave it |
| `loginby` | how the user logged in, as RouterOS reports it |
| `uptime` | how long the HotSpot session had lasted |
| `bytesin`, `bytesout` | the session's byte counters, as RouterOS reports them |
| `loc` | the hotspot router or UniFi access point the device was seen on |
| `signal` | the signal strength that router or controller reported |
| `DateTime` | when the sample was taken |

`loc` and `signal` are empty when the device was not seen on any hotspot router or UniFi access point in that pass. The user list is read once, when the tab first opens: reload the page to see users who appeared since.
