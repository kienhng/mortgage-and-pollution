 
Process to build a joined panel data from HMDA data and TRI data
1. Clean HMDA data
2. Clean TRI data
3. Match HMDA data and TRI data based on either county code (FIPS code) or ZIP code.
  3.1. FIPS code: TRI data does not have FIPS code, so must create one based on FIPS code dataset (from US Census Bureau).
  3.2. ZIP code: 
  Note: The TRI data must be grouped by FIPS code (or ZIP code) before matching with the HMDA data.
