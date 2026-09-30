# Troubleshooting

Start with the console. The app prints the selection and the time on every redraw, and each collector prints `ERROR :` and the reason when a pass fails. When they run from the `AtStartup*.sh` scripts, send that output to a log file (see [Getting started](getting-started.md#start-everything-at-boot)).

## A chart is blank and shows no error

`ui.R` lines 14-16 hide every Shiny error message, so a chart that fails is an empty area. Read the app's console, or comment out those three lines while you look for the cause. The usual causes, in order: MongoDB is not running on `localhost:27017`; the collector for that tab has not finished a pass yet; one of the lists below is missing.

## The router list or the hotspot list is empty

The **Escoja MikroTik** list comes from `listaMKT.csv` and the **Escoja HotSpot** list from `listaHS.csv`, both in the app's directory. A collector writes its file at the end of its first pass. If the file is missing after that, the collector writes to a different directory than the one the app runs from: check `scriptServicioBBDD.R` line 20 and `scriptServicioBBDDHS.R` line 194 against the directory in `AtStartupWEB.sh`.

## One router's chart is empty while Gran total has data

The collector stores each router's count under the address in `radacct.nasipaddress`, and the app looks it up under `nasname` in the `nas` table. When a router's `nasname` is a host name, or a different address, the two never meet. Set `nasname` to the address the router sends accounting from.

## The Tráfico tab has no data

Run one reading by hand with your router's address and community:

```bash
snmpget -v1 -c public 192.0.2.1 1.3.6.1.2.1.31.1.1.1.6.1
```

If it prints nothing or an error, try `-v2c`. The counters StatiX reads are 64-bit, and SNMPv1 has no 64-bit type, so many agents only answer them over v2c. When `-v2c` works, change `-v1` to `-v2c` on every `snmpget` line of `ScriptTrafico.R`. If neither works, the router has no interface with index 1, or SNMP is closed to the collector's address.

## The traffic log prints "object 'reboot' not found" every pass

Harmless. Line 117 of `ScriptTrafico.R` ends the loop with the word `reboot`, which R cannot find; the error handler then waits five more minutes. That is why traffic points come every ten minutes instead of five. The rates are still right: they divide by the real time between readings. Deleting the word makes the interval five minutes.

## One traffic point is far too high, or below zero

- **Far too high, after the collector could not store readings for an hour or more.** Line 91 of `ScriptTrafico.R` takes the time since the last reading and lets R choose the unit: minutes under an hour, hours from an hour up, days from a day up. After a gap of an hour or more the first new point is 60 times too high (1440 times after a day). Delete that document from the `Trafico` collections, or change line 91 to `minutosLoop <- as.numeric(difftime(as.POSIXct(DateTime), as.POSIXct(ono[[1]]$DateTime), units = "mins"))`.
- **Below zero, after a border router restarted.** The router's counters start again from zero, so the difference with the last reading is negative for one pass.

Either point also stretches the chart's axis. Pick a date range that leaves it out.

## The HotSpot charts get no new points

Run the helpers by hand from the app directory and read what they print:

```bash
python3 MACsPrincipal.py
python3 IPsPrincipal.py
```

- `NameError: name 'msg' is not defined` means the helper could not connect to that address (a bug in its error message). Check the address, and that the RouterOS API service (port 8728) is enabled and allows the collector's address.
- A `KeyError: '=ret'` or an error inside `login()` means the router runs RouterOS 6.43 or later. The helpers log in the way older versions did: they send `/login`, read a challenge from `=ret`, and answer with an MD5 hash. Newer versions take the user and password in the first `/login` sentence (`=name=` and `=password=`). Change `login()` in all three helpers to send that sentence.
- If both print users and tunnels, the problem is on the UniFi side: `python3 apiUnifi.py`, with your controllers filled in, prints what the controllers return.

## The HotSpot charts have twice as many points as expected

Both `scriptServicioBBDDHS.R` and `scriptServicioBBDDHSdelay.R` are running. They are the same collector; stop one.

## A HotSpot user is missing from the search

The **Usuario Hotspot** list is read once, when the tab first opens. Reload the page.

## My chosen date range went back to yesterday and tomorrow

The date pickers are rebuilt every hour after the page opens, which resets them. Pick the range again, or change `invalidateLater(1000*60*60, session)` on `server.R` lines 14, 23, 32 and 41.

## RMongo, rmongodb or rPython will not install

They are off CRAN and were last built against the R and MongoDB of 2015 to 2018. `RMongo` needs `rJava`: install a JDK, run `sudo R CMD javareconf`, then install `rJava` before `RMongo`. `rmongodb` and `rPython` compile C code and need R's build tools; `rPython` also needs the Python development headers. If a current R refuses them, an R version from their time in a container is the fallback. Which versions still work together has not been tested.

## Reporting a bug

Open an [issue](https://github.com/GeiserX/statix/issues) with:

- which tab or collector fails, and what you expected;
- the output of `R --version` and `sessionInfo()` in R, and your MongoDB version;
- the app's console output and the collector's `ERROR :` lines around the failure;
- your RouterOS version, for HotSpot problems.

Remove passwords, addresses, subscriber names and MAC addresses first.
