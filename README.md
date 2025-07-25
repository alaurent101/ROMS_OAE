# ROMS 3.9

This repository is based on the [official ROMS Version 3.9](https://www.myroms.org/projects/src/ticket/878) (trunk revision `r1054`) downloaded on 20 March, 2021.

The original code was modified for the DalROMS-NWA12 model (Ohashi et al., 2024) to include the following model developments:

* Calculation of wind stress using the [Large and Pond (1981)](https://doi.org/10.1175/1520-0485(1981)011<0324:OOMFMI>2.0.CO;2) scheme
* Calculation of net longwave radiation using specific specific humidity
* Nudging in sponge layer along open boundaries
* Riverine freshwater input scheme of [Ji et al. (2012)](https://doi.org/10.1080/07055900.2011.580165)
* Spectral nudging ([Thompson et al., 2006](https://doi.org/10.1016/j.ocemod.2005.11.003))
* Semi-prognostic method ([Greatbatch et al., 2004](https://doi.org/10.1016/j.csr.2004.07.009))
* Continental freshwater runoff
* Coupling with the [Los Alamos Sea Ice Model, CICE](https://github.com/CICE-Consortium/CICE-svn-trunk) ([Hunke et al., 2015](https://github.com/CICE-Consortium/CICE-svn-trunk/blob/main/cicedoc/cicedoc.pdf)) based on [MetROMS](https://github.com/metno/metroms) ([Kristensen et al., 2017](https://zenodo.org/record/1046114))

## BGC + OAE module

The model was further developed to add a reduced complexity biogeochemical model with a capability for ocean alkalinity enhancement (OAE) (Laurent et al., 2025).

The model configuration files to run the OAE simulations are here in the ROMS source directory:

```
./User/Include/oae.h
./User/External/roms_oae.in
./User/External/reduced_bgc.in
./User/External/varinfo_oae.dat
```

The main CPP options to run the model are (see oae.h):

```c
REDUCED_BGC     /* Main option to use the reduced BGC model */
CARBON          /* This option is mandatory to use the model with addition */
OXYGEN          /* Optional, useful to validate model BGC */
P_PRODUCTION    /* Sources and sinks from primary production */
TALK_BGC        /* BGC feedback on alkalinity */
TALK_ADDITION   /* Turn on alkalinity addition module */
TALK_FILE       /* Use the modified river file to add alkalinity feedstock */
TALK_TRACERS    /* Additional alkalinity addition tracers */
TALK_NONCONSERV /* Background alkalinity is non conservative (not imposed from salinity) */
RW14_OXYGEN_SC  /* Oxygen air-sea gas exchange based on Wanninkhof (2014) */
RW14_CO2_SC     /* CO2 air-sea gas exchange based on Wanninkhof (2014) */

/* Options specific to Halifax Harbour case */
PP_SS       /* Primary production on the Scotian Shelf */
PP_H2       /* Primary production in Halifax Harbour */
WOC_H2      /* Water column respiration in Halifax Harbour */
SOC_H2      /* Sediment respiration in Halifax Harbour */
SOC_ZVAR    /* SOC varies with depth */
SOC_OXYDEP  /* O2 dependency on SOC */

/* Options for atmospheric PCO2 */
PCO2AIR_MAUNALOA     /* Atmospheric pCO2 parameterization for Mauna Loa time series */
PCO2AIR_SABLEISLAND  /* Atmospheric pCO2 parameterization for Scotian Shelf (Sable Island observations) */
```
Note: primary production, water column and sediment respiration are hard coded (with the options above). To relocate the model new rates (parameterizations) need to be provided, otherwise they will be set to zero.

## Model BGC tracers

alkalinity, TIC and oxygen are the 3 tracers of the reduced BGC model. The addition model includes the additional tracers listed below.

* TAp      _Particulate phase of the feedstock_
* dTA      _Dissolved phase of the feedstock + dissolved TAp. This tracer represents the added alkalinity_
* dTIC     _Additional TIC (from net CO2 uptake)_
* TAin     _Optional tracer to record added feedstock (TALK_TRACERS)_
* TArm     _Optional tracer to record dissolved TAp (TALK_TRACERS)_

## River file

The alkalinity addition

## Test forcing files

The model can be run for a 1 month test using the Halifax Harbour set up in July 2017 (constant addition from July 2). The forcing files are available from Zenodo and the setup provided in the files listed above.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16423337.svg)](https://doi.org/10.5281/zenodo.16423337)

## References

* Ohashi, K., Laurent, A., Renkl, C., Sheng, J., Fennel, K., & Oliver, E. (2024). DalROMS-NWA12 v1.0, a coupled circulation–ice–biogeochemistry modelling system for the northwest Atlantic Ocean: development and validation. Geoscientific Model Development, 17(23), 8697–8733. [https://doi.org/10.5194/gmd-17-8697-2024](https://doi.org/10.5194/gmd-17-8697-2024)

* Laurent, A., Wang, B., Atamanchuk, D., Rakshit, S., Azetsu-Scott, K., Algar, C., & Fennel, K. (2025). A high-resolution nested model to study the effects of alkalinity additions in Halifax Harbour, a mid-latitude coastal fjord. EGUsphere. [https://doi.org/10.5194/egusphere-2025-3361](https://doi.org/10.5194/egusphere-2025-3361)
