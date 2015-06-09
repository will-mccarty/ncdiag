netcdf test_datumfmt {
dimensions:
    people = 10 ;
variables:
    int Person_ID(people);
        Person_ID:hungry = "true";
    int Cookies(people);
        Cookies:taste = "sweet";
    int Cupcakes(people);
        Cupcakes:taste = "sweet";
    int Muffins(people);
        Muffins:taste = "sweet";
    int Bagels(people);
        Bagels:taste = "plain";
    int Donuts(people);
        Donuts:taste = "sweet";
    int Cakes(people);
        Cakes:taste = "sweet";
    int Loaves(people);
        Loaves:taste = "plain";
    int Rolls(people);
        Rolls:taste = "plain";
    int Fried_Chicken(people);
        Fried_Chicken:taste = "savory";
}
