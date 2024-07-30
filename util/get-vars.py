"""
Extract a column of input data from various sources.
"""
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr
import yaml

HERE = Path(__file__).parent

with open(HERE / "vars.yml") as f:
    ctl = yaml.safe_load(f)
    var_info = ctl["variables"]

unique_src_ids = {d["src"].split(":")[0] for d in var_info.values()}
print("srcs:", unique_src_ids)

for d in ctl["cases"]:
    lat = d["lat"]
    lon = d["lon"]
    nz = d.get("nz")
    atm_fp = Path(d["atm"])
    sfc_fp = Path(d["sfc"])
    out_fp = Path(d["out"])
    if not out_fp.is_absolute():
        out_fp = HERE / out_fp
    print(f"lat={lat}, lon={lon}, nz={nz}, atm={atm_fp}, sfc={sfc_fp}, out={out_fp}")

    src = {}

    src["atm"] = None
    if "atm" in unique_src_ids:
        ds = (
            xr.open_dataset(atm_fp)
            .drop_vars(["lon", "lat"])
            .rename({"grid_xt": "lon", "grid_yt": "lat"})
            .sel(lat=lat, lon=lon, method="nearest")
            .squeeze()
        )

        # Switch to sfc first
        pmid, p = ds["pfull"].values, ds["phalf"].values
        assert pmid[0] < pmid[-1] and p[0] < p[-1]
        ds = ds.isel(pfull=slice(None, None, -1), phalf=slice(None, None, -1))

        # Compute height
        assert "z" not in ds.variables
        assert "zmid" not in ds.variables
        zsfc = ds["hgtsfc"]
        dz = ds["delz"]
        if (dz < 0).all():
            dz *= -1
        ztop = (zsfc + dz).cumsum("pfull")
        z = xr.DataArray(
            data=np.concatenate(([zsfc.values], ztop.values)),
            dims="phalf",
            # coords={"phalf": ds.coords["phalf"]},
        )
        zmid = ((z + z.shift(phalf=1)) / 2).isel(phalf=slice(1, None)).swap_dims(phalf="pfull")
        ds = ds.assign_coords(z=z, zmid=zmid)

        # Select levels
        if nz is not None:
            ds = ds.isel(pfull=slice(None, nz), phalf=slice(None, nz + 1))

        # Store
        src["atm"] = ds

    src["sfc"] = None
    if "sfc" in unique_src_ids:
        ds = (
            xr.open_dataset(sfc_fp)
            .drop_vars(["lon", "lat"])
            .rename({"grid_xt": "lon", "grid_yt": "lat"})
            .sel(lat=lat, lon=lon, method="nearest")
            .squeeze()
        )

        # Switch to sfc first
        pmid, p = ds["pfull"].values, ds["phalf"].values
        assert pmid[0] < pmid[-1] and p[0] < p[-1]
        ds = ds.isel(pfull=slice(None, None, -1), phalf=slice(None, None, -1))

        # Select levels (for cloud fraction)
        if nz is not None:
            ds = ds.isel(pfull=slice(None, nz), phalf=slice(None, nz + 1))

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
        ds["gwettop"] = ds["soilw1"]  # FIXME: this is value at 5 cm, not 0--5 cm avg
        ds["gwetroot"] = (ds["soilw2"] * 30 + ds["soilw3"] * 60) / 90

        # Combine soil moisture into one array
        ds["soilm"] = xr.concat([ds[f"soilw{i}"] for i in range(1, 5)], dim="soil")

        # sotyp, land, vtype are stored as float, maybe to support null mask?
        # But they seem to be all non-null, though with zeros (land 53.8%, sotyp/vtype 66.3%)
        for vn in ["sotyp", "land", "vtype"]:
            if vn in ds.variables:
                ds[vn] = ds[vn].astype(int)

        # Store
        src["sfc"] = ds

    das = []
    for vn, d in var_info.items():
        if vn in {"z", "zmid"}:
            assert d["src"] == "atm", "diagnosed"
            src_id, src_vn = "atm", vn
        elif vn in {"gwettop", "gwetroot", "soilm"}:
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
    # TODO: if the number of levels is too large it currently writes multiple lines (truncation to next line) in the output
    #       Needs to be on a single line
    with open(out_fp, "w") as f:
        for da in das:
            if pd.api.types.is_float_dtype(da.dtype):
                fmt = ".4e"
            elif pd.api.types.is_integer_dtype(da.dtype):
                fmt = "d"
            else:
                raise AssertionError
            if da.ndim == 0:
                data = f"{da.item():{fmt}}"
            elif da.ndim == 1:
                data = ",".join(f"{x:{fmt}}" for x in da.values)
            else:
                raise AssertionError
            info = ",".join([da.name, str(da.size)])
            f.write(info + "\n")
            f.write(data + "\n")
