### Mosquito Control Aid 

This project aims to make a statistical model for mosquito control accessible to individuals interested in mosquito control. The model uses data provided by users in conjunction with temperature, mosquito natural history, and other factors to estimate potential optimal control methods (spraying, fogging, nesting habitat removal, application of larvicide, etc.).

Link to application: https://collin-schwantes.shinyapps.io/MosquitoApp/

### What you will find in this repository

 - Code for R shiny application
 - Code for statistical model underlying application
 - [Project](https://github.com/collinschwantes/MosquitoApp/projects/1) mgmt/group communication 
 - Instructions for using the application 
    - example CSVs for uploading
    - Model interpretation 
    - disclaimer 
    
**Please** use the [Issues tab](https://github.com/collinschwantes/MosquitoApp/issues) to propose enhancements, propose new features, report bugs, or make other project related suggestions. 

## Instructions for using the application

This application accepts CSV and excel files (.xls, .xlsx) as inputs. None of your data are stored long term on the Rshiny servers. At the moment, only a single spreadsheet can be uploaded. If you have multiple years of data, please consolidate to one spread sheet.

- Consistent date formats are required
- [Example Data Sheet](https://github.com/collinschwantes/MosquitoApp/blob/master/SyntheticData/ExampleCulex.csv)
- Minimum data requirements for model:
  - Count data
  - Date Collected
  - Trap type
  - Species
  - Resource constraints

---

This is a summary of the model draft provided by Jeff Demers. 

## Problem:

When is the best time to spray given local mosquito populations?

**Limitations**: Noisy data, irregular sampling effort(?), irregular sampling periodicity(?), May not know when control actions were taken/effort of application, potential limited ability to collect data on covariates (temp, elevation, etc)

## variables:

x(t) = vector population at time t  
Λ(t) = time varying emergence rate (genus or species specific, likely temp dependent as well)    
1/µ = natural vector lifetime (species and temp dependent)  
τ = year/season/period  
N = noisy mosquito pop estimates/impulses  
λ = emergence rate  

populations based soley on females?

## potential pain points

- date formats - 
    - specify date format on entry
    - look for non-sequential entries (provide warning)
    - provide a guess date format option (?)
    - use lubridate to convert to standard date format
- species names
    - use GBIF namesearch 
    - provide warning for unrecognized species name 
    - read in data, check names, allow user to correct 
        - data -> names summary -> provide correction option 
- multi trap type datasets
   - specify trap type or multi
- multi species type datasets 
    - select species of interest
- geocode data - 
    - address lookup if lat long not provided
    - can use geocode to get covariates
- missing time points
    - sharon says interpolate 
    - need to figure out if model is robust to NA's 
  


Use the population data to determine a schedule for Np impulses to occur over a year,
assuming that each impulse reduces the vector population by a fraction ρ. Let z = (z1, z2, ..., zp)
denote the timing of the Np impulses
