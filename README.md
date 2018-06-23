# PDS3 Parser

This module implements a very rudimentary parser for the PDS3 data format, as
specified by NASA [here](https://pds.nasa.gov/datastandards/pds3/standards/)

## Usage

Given a PDS file like this abridged example:

```
PDS_VERSION_ID                         = PDS3
MISSION_NAME                           = "LUNAR ORBITER 2"
LAUNCH_DATE                            = 1966-11-06
FOOTPRINT_POINT_LATITUDE               = (4.217, 3.6209, 3.6514, 4.2473)
FOOTPRINT_POINT_LONGITUDE              = (37.6944, 37.5729, 37.4228, 37.544)
```

We can convert this to a document record using `pds:parse/1`.

```
{ok, Bin} = file:read_file("FRAME_2017_H2.LBL").
{ok, Doc} = pds:parse(Bin).
Lat = proplists:get_value("FOOTPRINT_POINT_LATITUDE", Doc#pds_document.properties).
Lon = proplists:get_value("FOOTPRINT_POINT_LONGITUDE", Doc#pds_document.properties).
{Lat, Lon}.
> {{4.2473,3.6514,3.6209,4.217},
>     {37.544,37.4228,37.5729,37.6944}}
```

Document records contain three lists:
- `properties`, any top-level key-value pair in the document
- `pointers`, a proplist of pointer name to file name
- `objects`, a proplist of object name to a second proplist of object values.

### Type conversions

- Values in parens are converted to Erlang tuples of the contained values.
- Dates and times are parsed to the format `{erlang:date(), erlang:time()}`
- Values with units are represented as a tuple of `{Value, Unit}`
- Comments are ignored

| Pds format | Erlang format |
|------------|---------------|
| 1966-11-06 | {1966, 11, 6} |
| 1966-11-18T15:25:18.22 | {{1966, 11, 18}, {15, 25, 18.22}} |
| -.0345 <km/s> | {-0.0345, "km/s"} |
| (4.217, 3.6209, 3.6514, 4.2473) | {4.217, 3.6209, 3.6514, 4.2473} |
