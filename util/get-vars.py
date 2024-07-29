"""
Extract a column of input data from various sources.
"""
from pathlib import Path

import numpy as np
import xarray as xr
import yaml

HERE = Path(__file__).parent
RUN = Path("/scratch1/NCEPDEV/stmp2/Barry.Baker/FV3_RT/rt_980816/atmaero_control_p8_intel")  # Hera

atm_fp = RUN / "atmf024.nc"
sfc_fp = RUN / "sfcf024.nc"
out_fp = HERE / "../tests" / "col.csv"

# Target column
lat, lon = 38.9721, -76.9245 + 360  # NCWCP
nz = 30  # limit the number of levels (set to ``None`` for all levels)

# Open vars.yml
with open("vars.yml") as f:
    var_info = yaml.safe_load(f)["variables"]

unique_src_ids = {d["src"].split(":")[0] for d in var_info.values()}
print("srcs:", unique_src_ids)

src = {}

src["atm"] = None
if "atm" in unique_src_ids:
    src["atm"] = (
        xr.open_dataset(atm_fp)
        .drop_vars(["lon", "lat"])
        .rename({"grid_xt": "lon", "grid_yt": "lat"})
        .sel(lat=lat, lon=lon, method="nearest")
        .squeeze()
    )

    # Switch to sfc first
    pmid, p = src["atm"]["pfull"].values, src["atm"]["phalf"].values
    assert pmid[0] < pmid[-1] and p[0] < p[-1]
    src["atm"] = src["atm"].isel(pfull=slice(None, None, -1), phalf=slice(None, None, -1))

    # Compute height
    assert "z" not in src["atm"].variables
    assert "zmid" not in src["atm"].variables
    zsfc = src["atm"]["hgtsfc"]
    dz = src["atm"]["delz"]
    if (dz < 0).all():
        dz *= -1
    ztop = (zsfc + dz).cumsum("pfull")
    z = xr.DataArray(
        data=np.concatenate(([zsfc.values], ztop.values)),
        dims="phalf",
        # coords={"phalf": src["atm"].coords["phalf"]},
    )
    zmid = ((z + z.shift(phalf=1)) / 2).isel(phalf=slice(1, None)).swap_dims(phalf="pfull")
    src["atm"] = src["atm"].assign_coords(z=z, zmid=zmid)

    # Select levels
    src["atm"] = src["atm"].isel(pfull=slice(None, nz), phalf=slice(None, nz + 1))

src["sfc"] = None
if "sfc" in unique_src_ids:
    src["sfc"] = (
        xr.open_dataset(sfc_fp)
        .drop_vars(["lon", "lat"])
        .rename({"grid_xt": "lon", "grid_yt": "lat"})
        .sel(lat=lat, lon=lon, method="nearest")
        .squeeze()
    )

    # Switch to sfc first
    pmid, p = src["sfc"]["pfull"].values, src["sfc"]["phalf"].values
    assert pmid[0] < pmid[-1] and p[0] < p[-1]
    src["sfc"] = src["sfc"].isel(pfull=slice(None, None, -1), phalf=slice(None, None, -1))

    # Select levels
    src["sfc"] = src["sfc"].isel(pfull=slice(None, nz), phalf=slice(None, nz + 1))

    # Compute soil moisture variables
    #
    # soilw{1..4}: volumetric (fraction) soil moisture at 10, 30, 60, 100 cm
    # Noah predicts values at layer midpoints:
    # - [0, 10)    5
    # - [10, 40)   25
    # - [40, 100)  70
    # - [100, 200) 150
    #
    # GWETTOP: 0--5 cm avg?
    # GWETROOT: 10--100 cm avg?
    src["sfc"]["gwettop"] = src["sfc"]["soilw1"]  # FIXME: this is value at 5 cm, not 0--5 cm avg
    src["sfc"]["gwetroot"] = (src["sfc"]["soilw2"] * 30 + src["sfc"]["soilw3"] * 60) / 90

das = []
for vn, d in var_info.items():
    if vn in {"z", "zmid"}:
        assert d["src"] == "atm", "diagnosed"
        src_id, src_vn = "atm", vn
    elif vn in {"gwettop", "gwetroot"}:
        assert d["src"] == "sfc", "diagnosed"
        src_id, src_vn = "sfc", vn
    else:
        src_id, src_vn = d["src"].split(":")

    da = src[src_id][src_vn]

    # Maybe convert units
    f = d.get("f")
    if f is not None:
        da *= f

    das.append(da.rename(vn))

# Write to text file
fmt = ".4e"
with open(out_fp, "w") as f:
    for da in das:
        if da.ndim == 0:
            data = f"{da.item():{fmt}}"
        elif da.ndim == 1:
            data = ",".join(f"{x:{fmt}}" for x in da.values)
        else:
            raise AssertionError
        info = ",".join([da.name, str(da.size)])
        f.write(info + "\n")
        f.write(data + "\n")
