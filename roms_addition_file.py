#!/usr/bin/env python
# coding: utf-8

""" Create the alkalinity addition forcing file
for the ROMS_OAE test case with 3 feedstocks at 3 locations:
Mill Cove (MC), Tufts Cove (TC) and Herring Cove (HC).

The script is executed in the terminal with:
python roms_addition_file.py

If necessary execute the following commands in the terminal
prior to running the script:
pip install numpy
pip install netcdf4
pip install xarray
"""

from datetime import datetime
import numpy as np
import xarray as xr

__author__ = "Arnaud Laurent"
__email__ = "arnaud.laurent@dal.ca"

## Script setup ###############
# Location of grid file and output file
path2input = './'
grfile = 'h2_LaurentEtAl2025.nc'
# 3 sites:    MC   TC   HC
Xposition = [129, 144, 115]
Eposition = [392, 345, 266]
# 3 rates:   MC TC  HC
addrate =   [1,  5, 10] # mol s-1
# 3 vertical ranges for dosing
#       MC  TC  HC
kmin = [40, 20, 0]
kmax = [40, 40, 40]
# 3 feedstock types
#       MC TC  HC
ftype = [1, 2, 3] # parameters are set in reduced_bgc.in
# Same time for all dosing locations
addition_time = [datetime(2017,7,2),datetime(2017,9,1)]
## End of setup ###############

hgrid = xr.open_dataset(f"{path2input}{grfile}")

# Same time for all sites
time_ref = datetime(1980,1,1)
dstart = (addition_time[0]-time_ref).days
dend = (addition_time[1]-time_ref).days

# No dosing is added 100 days before/after the dosing period
time = [dstart-100,dstart-0.0007,dstart,dend,dend+0.0007,dend+100]
mask = hgrid.mask_rho.data*0
mask_type = hgrid.mask_rho.data*0
mask_kmin = hgrid.mask_rho.data*0
mask_kmax = hgrid.mask_rho.data*0
mask[Eposition,Xposition] = 1
mask_type[Eposition[0],Xposition[0]] = ftype[0]
mask_type[Eposition[1],Xposition[1]] = ftype[1]
mask_type[Eposition[2],Xposition[2]] = ftype[2]
mask_kmin[Eposition[0],Xposition[0]] = kmin[0]
mask_kmin[Eposition[1],Xposition[1]] = kmin[1]
mask_kmin[Eposition[2],Xposition[2]] = kmin[2]
mask_kmax[Eposition[0],Xposition[0]] = kmax[0]
mask_kmax[Eposition[1],Xposition[1]] = kmax[1]
mask_kmax[Eposition[2],Xposition[2]] = kmax[2]
mask = mask*hgrid.mask_rho.data
# Create 3D fields
add3d = np.repeat(mask[:, :, np.newaxis], np.size(time), axis=2)
add3d_type = np.repeat(mask_type[:, :, np.newaxis], np.size(time), axis=2)
add3d_kmin = np.repeat(mask_kmin[:, :, np.newaxis], np.size(time), axis=2)
add3d_kmax = np.repeat(mask_kmax[:, :, np.newaxis], np.size(time), axis=2)
for iloc in range(0,np.size(Xposition)):
    iarea = 1/(hgrid.pm[Eposition[iloc],Xposition[iloc]]*hgrid.pn[Eposition[iloc],Xposition[iloc]]).values
    add3d[Eposition[iloc],Xposition[iloc],:] = addrate[iloc]

# Create xarray.Dataset
ds = xr.Dataset(
    
data_vars=dict(
    add_dTA=(["add_time", "eta_rho", "xi_rho"], np.transpose(add3d, (2, 0, 1)),{'units':'mol s-1', 'long_name':'dTA addition rate', 'coordinates':'lat_rho lon_rho'}),
    add_Xposition=(["source"], Xposition,{'units':'nondimensional', 'long_name':'addition head ETA-position at RHO-points'}),
    add_Eposition=(["source"], Eposition,{'units':'nondimensional', 'long_name':'addition head XI-position at RHO-points'}),
    add_type=(["add_time", "eta_rho", "xi_rho"], np.transpose(add3d_type, (2, 0, 1)),{'units':'nondimensional', 'long_name':'feedstock type mask [0: no addition, 1: feed1, 2: feed2, 3: feed3]', 'standard_name':'feedstock type', 'coordinates':'lat_rho lon_rho'}),
    add_kmin=(["add_time", "eta_rho", "xi_rho"], np.transpose(add3d_kmin, (2, 0, 1)),{'units':'nondimensional', 'long_name':'minimum s-level for addition', 'standard_name':'minimum s-level', 'coordinates':'lat_rho lon_rho'}),
    add_kmax=(["add_time", "eta_rho", "xi_rho"], np.transpose(add3d_kmax, (2, 0, 1)),{'units':'nondimensional', 'long_name':'maximum s-level for addition', 'standard_name':'maximum s-level', 'coordinates':'lat_rho lon_rho'}),
),
coords=dict(
    add_time=(["add_time"], time),
    lon_rho=(["eta_rho", "xi_rho"], hgrid.lon_rho.values,{'units':'degree_east', 'long_name':'longitude of RHO-points', 'standard_name':'longitude'}),
    lat_rho=(["eta_rho", "xi_rho"], hgrid.lat_rho.values,{'units':'degree_north', 'long_name':'latitude of RHO-points', 'standard_name':'latitude'}),

),
attrs=dict(description="Alkalinity addition forcing file for ROMS_OAE"),
)

reftime = '1980-01-01 00:00:00'
reftime_object = datetime.strptime(reftime, '%Y-%m-%d %H:%M:%S')
time_units = f'days since {reftime}'
time_calendar = 'gregorian'
fill_val = 1.0e35

ds.add_time.attrs['long_name'] = "time since initialization"
ds.add_time.attrs['units'] = time_units
ds.add_time.attrs['calendar'] = time_calendar

# Save to netcdf file
encoding = {
            'add_dTA': {'_FillValue': fill_val},
            'add_type': {'_FillValue': fill_val},
            'add_kmin': {'_FillValue': fill_val},
            'add_kmax': {'_FillValue': fill_val},
            'add_Xposition': {'_FillValue': None},
            'add_Eposition': {'_FillValue': None},
            'lon_rho': {'_FillValue': None},
            'lat_rho': {'_FillValue': None},
            'add_time': {'_FillValue': fill_val},
           }

fout = f'{path2input}h2_addition_LaurentEtAl2025_cst_3locs_multimap.nc'
ds.to_netcdf(fout, 'w', encoding=encoding)
print(f'wrote {fout}')
