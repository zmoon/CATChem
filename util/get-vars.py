"""
Extract a column of input data from various sources.
"""
from pathlib import Path

import xarray as xr
import yaml

RUN = Path("/scratch1/NCEPDEV/stmp2/Barry.Baker/FV3_RT/rt_980816/atmaero_control_p8_intel")  # Hera

atm_fp = RUN / "atmf024.nc"
sfc_fp = RUN / "sfcf024.nc"

# Target column
lat, lon = 38.9721, -76.9245 + 360  # NCWCP

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

    # Compute height
    assert "z" not in src["atm"].variables
    assert "zmid" not in src["atm"].variables
    z = (src["atm"]["hgtsfc"] + src["atm"]["delz"]).cumsum(dim="pfull")  # TODO: result should be phalf dim
    zmid = (z + z.shift(pfull=1)) / 2
    src["atm"] = src["atm"].assign_coords(z=z, zmid=zmid)

src["sfc"] = None
if "sfc" in unique_src_ids:
    src["sfc"] = (
        xr.open_dataset(sfc_fp)
        .drop_vars(["lon", "lat"])
        .rename({"grid_xt": "lon", "grid_yt": "lat"})
        .sel(lat=lat, lon=lon, method="nearest")
        .squeeze()
    )

das = []
for vn, d in var_info.items():
    if vn in {"z", "zmid"}:
        assert d["src"] == "atm"  # diagnosed
        src_id, src_vn = "atm", vn
    else:
        src_id, src_vn = d["src"].split(":")

    da = src[src_id][src_vn]

    # Maybe convert units
    f = d.get("f")
    if f is not None:
        da *= f

    das.append(da)
