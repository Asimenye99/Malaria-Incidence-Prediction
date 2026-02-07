import xarray as xr
import os



url = "http://apdrc.soest.hawaii.edu/dods/public_data/Reanalysis_Data/ERA5/monthly_2d/Surface"
ds = xr.open_dataset(url)

# Subset Malawi
ds_malawi = ds.sel(lat=slice(-18, -8), lon=slice(32, 36), time=slice("2015-01-01","2024-12-31"))

# Save variables
ds_malawi["t2m"].to_netcdf("ERA5_temperature_malawi_2015_2024.nc")
ds_malawi["tp"].to_netcdf("ERA5_precipitation_malawi_2015_2024.nc")
ds_malawi[["u10","v10"]].to_netcdf("ERA5_wind_malawi_2015_2024.nc")



print("Current working directory:", os.getcwd())
