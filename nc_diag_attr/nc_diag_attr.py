# nc_diag_attr

from netCDF4 import Dataset, getlibversion
import netCDF4
import argparse
import sys
import traceback
import numpy

try:
    import ujson as json
except:
    import json

def error_msg(msg):
    if args.pretty_output:
        print("\033[31m **   ERROR: %s\033[0m" % msg)
    else:
        print(" **   ERROR: %s" % msg)

def warning_msg(msg):
    if args.verbose:
        if args.pretty_output:
            print("\033[33m ** WARNING: %s\033[0m" % msg)
        else:
            print(" ** WARNING: %s" % msg)

def info_msg(msg):
    if args.verbose:
        if args.pretty_output:
            print("\033[34m **    INFO: %s\033[0m" % msg)
        else:
            print(" **    INFO: %s" % msg)

global current_line
current_line = ""

def line_msg(msg):
    global current_line
    if args.pretty_output:
        sys.stdout.write("\r")
        sys.stdout.write(len(current_line) * " ")
        sys.stdout.write(len(current_line) * "\b")
        sys.stdout.write(msg)
        sys.stdout.write(len(msg) * "\b")
        sys.stdout.flush()
        current_line = msg
    else:
        print(msg)

def line_msg_done():
    global current_line
    if args.verbose and args.pretty_output:
        sys.stdout.write("\n")
        sys.stdout.flush()
        current_line = ""

parser = argparse.ArgumentParser( #prog='ipush',
    description="Tool to add/modify global and variable attributes for NetCDF files")

disable_group = parser.add_mutually_exclusive_group()

parser.add_argument("-v", "--verbose", 
    dest="verbose", action="store_true", default=False,
    help = "enable verbose output")

parser.add_argument("-p", "--pretty", 
    dest="pretty_output", action="store_true", default=False,
    help = "enable colorful, pretty output - don't enable if logging")

disable_group.add_argument("-ng", "--no-global",
    dest="global_attributes", action="store_false", default=True,
    help = "disable global attribute adding/modifying")
disable_group.add_argument("-nv", "--no-var", 
    dest="var_attributes", action="store_false", default=True,
    help = "disable variable attribute adding/modifying")
parser.add_argument("-i", metavar = "RESOURCE_FILE", dest="resource_file",
    help = "input JSON resource file name with attributes to write", required = True)
parser.add_argument("nc4_files", help = "NetCDF4 files to apply attributes to", nargs="+")
args = parser.parse_args()

#print("Resource file: %s" % args.resource_file)
#print("NetCDF4 files: %s" % " ".join(args.nc4_files))
#print("Global attributes: %s" % ("Yes" if args.global_attributes else "No"))
#print("Variable attributes: %s" % ("Yes" if args.var_attributes else "No"))
#print("Pretty output: %s" % ("Yes" if args.pretty_output else "No"))
#print("Verbose output: %s" % ("Yes" if args.verbose else "No"))

# Sanity checks

# Check to make sure that the JSON resource file is valid!
try:
    resource_file_fh = open(args.resource_file, "r")
except IOError:
    error_msg("Resource file '%s' is not accessible or does not exist!" % args.resource_file)
    exit(1)

try:
    resource_data = json.loads(resource_file_fh.read())
except KeyboardInterrupt:
    info_msg("CTRL-C detected, exiting.")
    exit(0)
except:
    error_msg("Resource file '%s' is not a valid JSON file!" % args.resource_file)
    print(traceback.format_exc())
    exit(1)
resource_file_fh.close()

# Check to make sure the NetCDF4 files are legitimate!
if args.verbose:
    info_msg("Using following versions:")
    info_msg("    netcdf4-python v%s" % netCDF4.__version__)
    info_msg("    NetCDF v%s" % getlibversion())
    info_msg("    HDF5 v%s" % netCDF4.__hdf5libversion__)
    info_msg("    Python v%s\n" % sys.version.split("\n")[0].strip())
    info_msg("Reading and validating NetCDF4 files...")

nc4_files_root = []
if args.verbose:
    entry_num = 0
    entry_total = len(args.nc4_files)
for nc4_file in args.nc4_files:
    try:
        open(nc4_file, "r").close()
    except KeyboardInterrupt:
        info_msg("CTRL-C detected, exiting.")
        exit(0)
    except IOError:
        error_msg("The NetCDF4 file '%s' does not exist!" % nc4_file)
        exit(1)
    
    if args.verbose:
        entry_num += 1
        line_msg("Reading/verifying file %i/%i: %s" % (entry_num, entry_total, nc4_file))
        
    try:
        rootgrp = Dataset(nc4_file, "a", format="NETCDF4")
        nc4_files_root.append({ "file" : nc4_file, "group" : rootgrp })
    except KeyboardInterrupt:
        info_msg("CTRL-C detected, exiting.")
        exit(0)
    except:
        error_msg("'%s' is not a valid NetCDF4 file!" % nc4_file)
        exit(1)

line_msg_done()

if args.global_attributes:
    if not "global_attributes" in resource_data:
        warning_msg("Resource file '%s' does not have any global attributes, skipping." % args.resource_file)
    else:
        entry_num = 0
        entry_total = len(nc4_files_root)
        for nc4_entry in nc4_files_root:
            entry_num += 1
            
            if args.verbose:
                line_msg("Applying global attributes to file %i/%i: %s" % (entry_num, entry_total, nc4_entry["file"]))
                
            for global_attr_key in resource_data["global_attributes"]:
                global_attr_val = resource_data["global_attributes"][global_attr_key]
                
                if type(global_attr_val) == unicode:
                    global_attr_val = str(global_attr_val)
                
                # BUG fix - NetCDF really, really, REALLY does not like
                # 64-bit integers. We forcefully convert the value to a 
                # 32-bit signed integer, with some help from numpy!
                if type(global_attr_val) == int:
                    global_attr_val = numpy.int32(global_attr_val)
                
                setattr(nc4_entry["group"], global_attr_key, global_attr_val)
        line_msg_done()

if args.var_attributes:
    if not "variable_attributes" in resource_data:
        warning_msg("Resource file '%s' does not have any variable attributes, skipping." % args.resource_file)
    else:
        entry_num = 0
        entry_total = len(nc4_files_root)
        for nc4_entry in nc4_files_root:
            entry_num += 1
            if args.verbose:
                line_msg("Applying variable attributes to file %i/%i: %s" % (entry_num, entry_total, nc4_entry["file"]))
            
            for var in resource_data["variable_attributes"]:
                if var in nc4_entry["group"].variables.keys():
                    for var_attr_key in resource_data["variable_attributes"][var]:
                        var_attr_val = resource_data["variable_attributes"][var][var_attr_key]
                        var_attr_key = str(var_attr_key)
                        
                        if type(var_attr_val) == unicode:
                            var_attr_val = list(str(var_attr_val))
                        
                        # BUG fix - NetCDF really, really, REALLY does not like
                        # 64-bit integers. We forcefully convert the value to a 
                        # 32-bit signed integer, with some help from numpy!
                        if type(var_attr_val) == int:
                            var_attr_val = numpy.int32(var_attr_val)
                        
                        setattr(nc4_entry["group"].variables[var], var_attr_key, var_attr_val)
                else:
                    warning_msg("Can't find variable %s in file %s!" % (var, nc4_entry["file"]))
        line_msg_done()

# Close everything
entry_num = 0
entry_total = len(nc4_files_root)
for nc4_entry in nc4_files_root:
    entry_num += 1
    if args.verbose:
        line_msg("Saving changes to file %i/%i: %s" % (entry_num, entry_total, nc4_entry["file"]))
    nc4_entry["group"].close()

line_msg_done()

info_msg("Attribute appending complete!")
