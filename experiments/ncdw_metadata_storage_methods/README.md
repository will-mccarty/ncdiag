nc_diag_write storage method experiment
========================================

Introduction
-------------
This is a simple experiment to test which storage method is faster for
metadata variable data storage:

 * Storing data inside a derived type array instead of in a giant
   variable data storage. The derived type array is now filled
   with the various type arrays. Only one type array is allocated and
   filled so that the array itself has the complete data, and the array
   can be written out directly to NetCDF.
   
 * Data will continue to be stored in giant variable data storage, but
   with an addition of a derived type containing an array of indicies
   referring to the location where the variable's data is stored.

I built minimal programs that access and write the data using the
methods mentionned above. These programs are exactly like the
"real-deal" nc_diag_write ncdw_metadata subroutines, except extremely
simplified.

Source
-------
There are two source files:

 * test_index.f90 - this uses the "array of indicies" method.
 * test_indvarr.f90 - this uses the "derived type array" method, where
   multiple, smaller type arrays are stored inside.

You can compile these program by typing:

    make clean
    make

And you can run these programs by typing:

    ./test_index.x
    ./test_indvarr.x

Results
--------
When unoptimized, the "array of indicies" takes 10-20 seconds longer
than the "derived type array" method.

When compiled with optimizations (`-O2`), both methods take about the
same time. Of course, since optimizations are enabled, both take around
10-20 seconds less than the unoptimized runs.
