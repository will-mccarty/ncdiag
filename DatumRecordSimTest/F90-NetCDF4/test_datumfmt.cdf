netcdf test_datumfmt {
dimensions:
    dataset = 100;
    people = 10 ;
    longitude = 12 ;
variables:
    int Bakery_Store(dataset);
        Bakery_Store:column1  = "Person_ID";
        Bakery_Store:column2  = "Cookies";
        Bakery_Store:column3  = "Cupcakes";
        Bakery_Store:column4  = "Muffins";
        Bakery_Store:column5  = "Bagels";
        Bakery_Store:column6  = "Donuts";
        Bakery_Store:column7  = "Cakes";
        Bakery_Store:column8  = "Loaves";
        Bakery_Store:column9  = "Rolls";
        Bakery_Store:column10 = "Fried_Chicken";
        
        Bakery_Store:Cookies_Taste = "sweet";
        Bakery_Store:Cupcakes_Taste = "sweet";
        Bakery_Store:Muffins_Taste = "sweet";
        Bakery_Store:Bagels_Taste = "plain";
        Bakery_Store:Donuts_Taste = "sweet";
        Bakery_Store:Cakes_Taste = "sweet";
        Bakery_Store:Loaves_Taste = "plain";
        Bakery_Store:Rolls_Taste = "plain";
        Bakery_Store:Fried_Chicken_Taste = "savory";
    int Person_ID(people);
        Person_ID:hungry = "true";
        
}
