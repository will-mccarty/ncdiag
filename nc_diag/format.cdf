netcdf generic_datumfmt {
dimensions:
    n_datum = unlimited ;
variables:
    int Channel(n_datum)
    float Latitude(n_datum);
    float Longitude(n_datum);
    float Temperature(n_datum);
    float Error(n_datum);
}
