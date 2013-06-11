#!/usr/bin/perl
#
# primitve_dumper.pl 

# Stephanie 11/30/2012
#
# Purpose:  Create shape files from an Oracle topology 
#      representing the primitive edges (lines), nodes (points)
#      and faces (polygons) in the topology.
#
# Requires a face feature table with an SDO geometry column
#      called "SDOGEOMETRY."  The face feature table must be named
#      <topology>_<face table name>.
#
# This was assembled as a helper tool specifically for CPB
#      generalized topologies that include a face feature table.

# Need Help?  See Stephanie Spahlinger (wrapper script) or Suzanne McArdle
# (fme portion)

# Usage: 
#       password style...
#           primitive_dumper.pl -d <database> -s <schema> - p <password> -t <topology> -c {face table extension} -o {output directory} -m {mapping file dir} - f {fme application directory} -c {face deature table name extension}
#
#       wallet style... (not implemented) 
#           primitive_dumper.pl -d <database> -w <wallet> -t <topology> -o {output directory} -m {mapping file dir} - f {fme application directory}  -c {face deature table name extension}

# Example usage...
#
# (topology stored in schema we log into...
#      primitive_dumper.pl -d prodbnch -s spahl001 -t z653tm -p mypasswd -o /mtdata004/mapping/gz/dev/prodbnch/spahl001/shapefiles/primitives -m /mtdata004/mapping/gz/dev/devbnch/spahl001/software/data -c clip_face
#
# topology stored in remote schema...
#      primitive_dumper.pl -d prodbnch -s spahl001 -t gzacs12.v653sp -p mypasswd -o /mtdata004/mapping/gz/dev/prodbnch/spahl001/shapefiles/primitives -m /mtdata004/mapping/gz/dev/devbnch/spahl001/software/data -c clip_face


# Requirements:
#   1) You must have password access to a schema which has select priviledges
#      on the primitive tables in the schema 
#      where the topology is stored.
#
#   2) You must have access to the generalization software FME mapping file
#       "primitive_shp_creator.fme."
#       (You can find this in Serena, or usually here on devbnch...
#       /mt/apps/gz/dev/<gz software label>/data/primitive_shp_creator.fme)
#
#   3) FME must be installed on the Linux machine where you are running
#

##############################################################################
#
# Other Perl Modules this program uses
#
# best practice, gives you lots of info about what you are doing wrong, 
# even if Perl could take a pretty good guess.

use strict;
use warnings;

# for getting the current directory and the absolute paths for directories

use Cwd qw(abs_path getcwd);

# for easily reading options in various orders from the user

use Getopt::Long qw(:config no_ignore_case bundling);

##############################################################################

# Set up all our option variables 

my (
  $database,
  $schema,
  $passwd,
  $wallet,
  $topology,
  $outdir,
  $mappingdir,
  $fmedir,
  $facetab
);

# Check input, if no input, prompt user...

if(scalar(@ARGV) == 0) {                  # if no arguments on the command line
   print "\nPlease enter parameters in the format listed below.  Parameters in curly braces are optional." ;
   #   print "\n   password style...";
   print "\n         -d <database> -s <schema> - p <password> -t <topology> -c {face table name} -o {output directory} -m {mapping file dir} - f {fme application directory}" ;
   #  print "\n   wallet style... (not implemented)" ;
   # print "\n         -d <database> -w <wallet> -t <topology> -c {face table name} -c {face table name} -o {output directory} -m {mapping file dir} - f {fme application directory}" ;
   print "\nParameters: " ;
   chomp(my $camps_paras = <STDIN>);      # read one line from standard input
   @ARGV = split /[ \t]+/, $camps_paras;  # parse and store in @ARGV
}

# Get the current working directory...

my $curdir = getcwd;

# Parse entered options and assign their values to variables...

my $rc = GetOptions(
  'database|d=s'    => \$database,       
  'schema|s=s'      => \$schema,         
  'passwd|p=s'      => \$passwd,         
  'wallet|w=s'      => \$wallet,         
  'topology|t=s'    => \$topology,       
  'outdir|o=s'      => \$outdir,     
  'mappingdir|m=s'  => \$mappingdir,
  'fmedir|f=s'      => \$fmedir,
  'facetable|c=s'   => \$facetab
);

# Check optional parameters and assign default values if necessary.
unless (defined $outdir) {$outdir = $curdir};
unless (defined $mappingdir){$mappingdir = "/mt/apps/gz/dev/gzdev20120710/data"};
unless (defined $fmedir) {$fmedir = "/apps/fme/Server/fme"};
unless (defined $facetab) {$facetab = "FACE"};

# Convert directories to their full names for the FME command to work properly.

$outdir = abs_path( $outdir );
$mappingdir = abs_path( $mappingdir );
$fmedir = abs_path( $fmedir );

# Check Options

# If you don't have the required options print a more detailed usage report
# and then quit.
# If you have everything you need just print out the options you will use.

if (
    ## for when we get the wallet working...( (defined $wallet) || (defined $passwd) ) &&
    (defined $passwd) &&
    (defined $database) &&
    (defined $schema) &&
    (defined $topology)
   ) {

    # Print Entered Parameters (except password)

     print "\n DATABASE = $database\n";       
     if (defined $wallet) {print " WALLET   = $wallet\n"};
     print " SCHEMA   = $schema\n";     
     print " TOPOLOGY = $topology\n";     
     print " FACE TABLE SUFFIX = $facetab\n";  
     print " OUTDIR            = $outdir\n";     
     print " MAPPING FILE DIR  = $mappingdir\n";     
     print " FME DIR           = $fmedir\n";

    # set Oracle stuff to upper case just in case

    $schema = uc $schema;
    $database = uc $database;
    $topology = uc $topology;
    $facetab = uc $facetab;

} else {
    print "\n";
    print "ERROR: I don't understand all the options you entered, or some required options are missing.";
    print "\n";
    print "\n";
    print "###############################################\n";
    print "\n";
    print "Call the primitive_dumper.pl program using the following parameters...\n";
    print "   -d <database>      (REQUIRED)\n";
    print "   -s <schema name>   (REQUIRED)\n";
    print "   -p <password>      (REQUIRED)\n"; 
    #print "   -p <password>      (Required unless a wallet is used)\n";
    #print "   -w <wallet name>   (Required unless a password is used)\n";
    print "   -t <topology name> (REQURED)\n";
    print "   -c {face table name}          (OPTIONAL: defaults to 'FACE' and assumes the oracle table is named <topology>_<face_table_name>)\n";
    print "   -o {output directory}         (OPTIONAL: defaults to current directory)\n";
    print "   -m {mapping file directory}   (OPTIONAL: defaults to /mt/apps/gz/dev/gzdev20120710/data)\n";
    print "   -f {fme application directory}(OPTIONAL: defaults to /apps/fme/Server/fme)\n";
    print "\n";
    print "\n";
    print "Here are some example calls...\n";
    print "    primitive_dumper.pl -d devbnch -p mypassword -s bond007 -t topo1 -c clip_face -m /mt/apps/gz/dev/gzdev20120710/data -f /apps/fme/Server/fme\n";
    print "\n";
    print "    primitive_dumper.pl -d devbnch -s prdschema -w PRDSCHEMA_DEVBNCH_1 -t topo1 -m /mt/apps/gz/dev/gzdev20120710/data -f /apps/fme/Server/fme\n\n";
    print "\n";
    die "###############################################\n";
}

# Run the FME primitive dumper.

# Print out the command you are using for the user first.

# if you ever want to get the wallet working, add if wallet, do this..., if not do that here

my $cmd = "$fmedir/runfme $mappingdir/primitive_shp_creator.fme --SourceDataset_ORACLE8I $database --DestDataset_SHAPE $outdir --USERNAME $schema --PASSWORD $passwd --TOPOLOGY $topology --FACE_SUFFIX $facetab";

my $printcmd = "$fmedir/runfme $mappingdir/primitive_shp_creator.fme --SourceDataset_ORACLE8I $database --DestDataset_SHAPE $outdir --USERNAME $schema --PASSWORD **** --TOPOLOGY $topology --FACE_SUFFIX $facetab";

print "\n";
print "Command used to call fme...\n";
print " $printcmd\n";
print "\n";

# run the FME command

print "################# BEGIN FME APPLICATION OUTPUT #################\n";
print "\n";

system $cmd;

print "\n################## END FME APPLICATION OUTPUT ##################\n";

# Tell the user you are finished, and remind them where they directed their shape files.

print "\nprimitive_dumper.pl finished creating primitives, find shapefiles in $outdir.\n"
