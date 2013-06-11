#!/usr/bin/perl

#Matt! 7/21/10
#This is my generalization install script
#Its kind of a joke placeholder until theres a real build script
#Modified 10/18/10
#Modified 10/22/10 based on advice from Sreeni to load code into buffer first, then execute.  Fixes special character interpretation issues
#Modified 10/22/10 to add some logging
#Modified 10/22/10 for gen_clip_parameters blank install option
#Added usage (Stephanie) 11/10 
#Added dos2unix 12/01/10
#Added public execute 12/03/10
#Added table creation as option on command line to avoid question on success
#Fixed table creation log to be different from install log Matt! 05/06/11
#Additional tables and table streamlining Matt! 10/24/11
#Made -t option match real inputs B,A,O,N ! 10/26/11
#1/25/12 Major overhaul for production installation plans
#3/23/12 Backup tables on dumpfile option, added R option to Replace tables in dumpfile option
#3/26/12 Optional input log directory
#3/26/12 Added source_location_tbls to parameter file
#4/23/12 Fixed imp syntax in new environment. Better error checking on imp
#5/29/12 Removed dos2unix
#7/13/12 Tidied up table options
#6/5/13 Added quotes around password to hopefully handle special characters in sqlplus system call

#usage:  gz_build.pl -d <database> -s <schema> -p <password> -r <path to PLSQL packages to install> -t <optional install tables (B|A|O|N|D|R)> -l <optional log dir>
#sample development: gz_build.pl -d devbench -s gen_acs07_gh5 -p xxxxx -r /mapdata2/generalization/programs/matt/generalization
#sample production:  gz_build.pl –d prodbnch –w GZDEC10_04_PRODBNCH –f gz_install_params_dec10_04.txt –t D -l /prodmtdata001/camps/logs/GZ_DEC10_04/1_1
#sample production testing: gz_build.pl -d devbench -s gzcpb14 -p xxxxx -f gz_install_params.txt -t R -l /mapdata4/cpb/generalization/dev/devbench/matt/logs/GZ_DEC10_04/1_1

#sample prep exp
#exp gzcpb_1/gzcpb_1_2010@prodbnch FILE=/mapdata2/generalization/programs/matt/generalization/data/gz_tables_0131.dmp indexes=Y statistics=none tables=(ACS11_TU_Z6_IN,ACS11_TH_Z6_IN,SMALL_POLYGON_PARAMETERS,LINE_SIM_PARAMETERS,TOPO_FIELD_DEF,LUT_LSAD,GZ_SHAPEFILE_PARAMETERS,GZ_CLIP_SETUP,GZ_FSLBUILD_SETUP,GZ_JOB_SETUP,GZ_LINESIM_SETUP,GZ_MERGE_SETUP,GZ_QA_SETUP,GZ_SHAPEFILE_SETUP,GZ_SMPOLY_SETUP,GZ_TOPOBUILD_SETUP,GEN_CLIP_PARAMETERS,GEN_MERGE_PARAMETERS,STEDZ6V4,STEDZ8V3,STEDZ9V2,ACS11_SL040,REFERENCE_SCHEMAS,REFERENCE_FACE_FIELDS)


use strict;
use warnings;
use Getopt::Long qw(:config no_ignore_case bundling);
use FindBin qw($Bin);
use File::Path;



=head1 NAME

  gz_build.pl

  created: 07/21/2010
  last modified (unlikely!): 3/26/2012

=head1 SYNOPSIS

  Attempts to compile all the PLSQL like code in a directory in the specified schema
  Installs parameter tables maybe. Other stuff, dos2unix, updates schema specific columns, etc

=cut

my $debug = 0;
my $build_name = 'HOTCAKES'; 

print STDOUT "\n";
print STDOUT "Good day to you from the Generalization Build Script, lets install the build named: " . $build_name ."\n\n";

sleep (1.5);


#option rundown
#Brain only works with explicit examples
#lead with slashes, close with free love

my (
  $opt_database,                 #always a command long option
  $opt_schema,                   #always a command long option or NA, built into wallet
  $opt_password,                 #always a command long option or NA, built into wallet
  $opt_install_tables,           #always a command long option
  $opt_repository,               #optional command long option. Alternative is included in file as SRC_DIR
  $opt_wallet,                   #optional command long option. Or NA, use schema + password options
  $opt_logdir,                   #optional log directory for production.  Write temp files and logs
  $opt_file,                     #optional command long option. Includes the following
  $file_parm_hash
  #$file_parm_hash->{'APP_DIR'}              #Application directory. Ex /mt/apps/gz/gz_dec10_04
  #$file_parm_hash->{'LOG_DIR'}              #Log directory. Ex /src
  #$file_parm_hash->{'BIN_DIR'}              #Bin directory. Ex /bin
  #$file_parm_hash->{'DATA_DIR'}             #Data directory Ex /data
  #$file_parm_hash->{'SRC_DIR'}              #plsql directory Ex /src
  #$file_parm_hash->{'DMP_FILE'}             #dump file Ex /data/gz_tables_dec10_04.dmp
  #$file_parm_hash->{'FROM_USR'}             #dump file from user Ex GZCPB_9
  #$file_parm_hash->{'DMP_TBLS'}             #list of tables Ex ACS11_TU_Z6_IN,ACS11_TH_Z6_IN,SMALL_POLYGON_PARAMETERS,LINE_SIM_PARAMETERS,TOPO_FIELD_DEF,LUT_LSAD,GZ_SHAPEFILE_PARAMETERS,GZ_CLIP_SETUP,GZ_FSLBUILD_SETUP,GZ_JOB_SETUP,GZ_LINESIM_SETUP,GZ_MERGE_SETUP,GZ_QA_SETUP,GZ_SHAPEFILE_SETUP,GZ_SMPOLY_SETUP,GZ_TOPOBUILD_SETUP,GEN_CLIP_PARAMETERS,GEN_MERGE_PARAMETERS,STEDZ6V4,STEDZ8V3,STEDZ9V2,ACS11_SL040,REFERENCE_SCHEMAS,REFERENCE_FACE_FIELDS
  #$file_parm_hash->{'DEPLOY_TBLS'}          #list of tables to update DEPLOY column to current user ex ACS11_TU_Z6_IN
  #$file_parm_hash->{'GEN_SCHEMA_TBLS'}      #IA release list of tables to update GEN_SCHEMA column ex GZ_JOB_SETUP,GZ_TOPOBUILD_SETUP,GZ_SHAPEFILE_SETUP,GZ_QA_SETUP
  #$file_parm_hash->{'UNGEN_SCHEMA_TBLS'}    #IA release list of tables to update UNGEN_SCHEMA column ex GZ_QA_SETUP
  #$file_parm_hash->{'SOURCE_LOCATION_TBLS'} #List of tables to update database after @.  ex TOPO_UNIVERSE
  #$file_parm_hash->{'STATUS'}               #current status of the install. PASS or FAIL
  #$file_parm_hash->{'MESSAGE'}              #if status is fail, a reason
);

#sample file
#   APP_DIR=/mt/apps/gz/gz_dec10_04
#   LOG_DIR=/src
#   BIN_DIR=/bin
#   DATA_DIR=/data
#   SRC_DIR=/src
#   DMP_FILE=/data/gz_tables_dec10_04.dmp
#   FROM_USR=GZCPB_9
#   DMP_TBLS=ACS11_TU_Z6_IN,ACS11_TH_Z6_IN,SMALL_POLYGON_PARAMETERS,LINE_SIM_PARAMETERS,TOPO_FIELD_DEF,LUT_LSAD,GZ_SHAPEFILE_PARAMETERS,GZ_CLIP_SETUP,GZ_FSLBUILD_SETUP,GZ_JOB_SETUP,GZ_LINESIM_SETUP,GZ_MERGE_SETUP,GZ_QA_SETUP,GZ_SHAPEFILE_SETUP,GZ_SMPOLY_SETUP,GZ_TOPOBUILD_SETUP,GEN_CLIP_PARAMETERS,GEN_MERGE_PARAMETERS,STEDZ6V4,STEDZ8V3,STEDZ9V2,ACS11_SL040,REFERENCE_SCHEMAS,REFERENCE_FACE_FIELDS
#   DEPLOY_TBLS=TOPO_UNIVERSE
#   GEN_SCHMA_TBLS=GZ_JOB_SETUP,GZ_TOPOBUILD_SETUP,GZ_QA_SETUP
#   UNGEN_SCHEMA_TBLS=GZ_QA_SETUP
#   SOURCE_LOCATION_TBLS=TOPO_UNIVERSE


my $rc = GetOptions(
  'database|d=s'        => \$opt_database,
  'schema|s=s'          => \$opt_schema,
  'password|p=s'        => \$opt_password,
  'repository|r=s'      => \$opt_repository,
  'install_tables|t=s'  => \$opt_install_tables,
  'wallet|w=s'          => \$opt_wallet,
  'file|f=s'            => \$opt_file,
  'logdir|l=s'          => \$opt_logdir
);


# display usage if options not entered...

my $usage = "Development usage:  gz_build.pl -d <database> -s <schema> -p <password>  -r <path to PLSQL packages to install>  -t <optional install tables (B/A/O/N)>";
   $usage.= "Production usage: gz_build.pl -d <database> -w <wallet> -f <install file> -t <tables (D|N)> -l <log dir>\n";
   $usage.= "Production test usage: gz_build.pl -d <database> -s <schema> -p <password> -f <install file> -t <tables (D|N)> -l <log dir>\n";

unless ( ($opt_database && $opt_schema && $opt_password && $opt_repository) or  #development
         ($opt_database && $opt_wallet && $opt_file) or                         #production
         ($opt_database && $opt_schema && $opt_password && $opt_file)           #production test, no wallet
       ) 
{
   print STDOUT "$usage";
   die "Bailing out, all required arguments are required.\n\n";
}

if (defined $opt_install_tables) {
   
   if (uc($opt_install_tables) !~ m/^B|A|O|N|D|R$/) {
      print STDOUT $usage."\n";
      die "Bailing out, check -t options\n";
   }
}


#one of these is always undef, one always def
if ( defined $opt_file ) {
   
   $file_parm_hash = get_file_parms($opt_file, 'PROD');

} else {
   
   $file_parm_hash = get_file_parms($opt_repository, 'DEV');
   
}

if ( $file_parm_hash->{'STATUS'} eq 'FAIL' ) {
   die "Failed to read inputs: ".$file_parm_hash->{'MESSAGE'};
}

   ##################
   #Deal with log dir
   ##################

if (defined $opt_logdir) {
   
   #test it or make it
   
   if ( -d $opt_logdir ) {
      
      #log dir exists
      
      unless ( -w $opt_logdir ) {
         
               print STDOUT "\n";
               die "I dont have permission to write files in pre-existing ".$opt_logdir."\n\n\n";

            }
            
   } else {
    
      #need to create own log dir
      print STDOUT "   Making writeable log directory ".$opt_logdir."\n";
      
      eval {
         mkpath($opt_logdir, 1, 0777)  
      };
      
      if ($@) {
        print STDOUT "\n";
        print STDOUT "   Couldn't create " .$opt_logdir."\n";
        die "   Specifically, error is: \"". $@ ."\"\n\n";
        
      }
   }
   
   #override with passed in log dir if passed in
   #no matter what user has in param file
   $file_parm_hash->{'LOG_DIR'} = $opt_logdir;
   
} else {
   
   #still need write access to this location. Usually just xx/src
   
   unless ( -w $file_parm_hash->{'LOG_DIR'} ) {
      
         print STDOUT "\n";
         die "I dont have permission to write files in pre-existing ".$file_parm_hash->{'LOG_DIR'}."\n\n\n";
      
   } 
   
}

if (defined $file_parm_hash->{'DMP_FILE'}) {
   
   #check that we can find dmp file
   unless (-e $file_parm_hash->{'DMP_FILE'}) {
      
      print STDOUT "\n";
      print STDOUT "   I cant find a dump file at ".$file_parm_hash->{'DMP_FILE'}."\n";
      print STDOUT "   Maybe check the DMP_FILE vs APP_DIR values in ".$opt_file."\n";      
      die "\n\n";
   }
}

      

my $globstring;

   ####################
   #Dos2unix Axed
   ####################



#dos2unix .ksh files and .pl files in /bin
#DO NOT dos2unix .dmp files! Yikes

#$globstring = $file_parm_hash->{'BIN_DIR'} . "/*.ksh";
#my @kshfiles = glob $globstring;

#foreach my $ksh (@kshfiles) {
   
   #I think this is safe no matter what
   #Dos2unix should preserve file permissions no matter what
   #If the DBA running this in prod doesn't have write privileges to the file we will just get an onscreen warning
   #not true, seems to change ownerships and I cant control it
   
   #system ("dos2unix ".$ksh); 
   
#}

#$globstring = $file_parm_hash->{'BIN_DIR'} . "/*.pl";

#my @plfiles = glob $globstring;

#foreach my $pl (@plfiles) {
   
   #I think this is safe no matter what
   #Dos2unix should preserve file permissions no matter what
   #If the DBA running this in prod doesn't have write privileges to the file we will just get an onscreen warning

   #system ("dos2unix ".$pl); 
   
#}

   ####################
   #PLSQL code start
   ####################

#get specs
$globstring = $file_parm_hash->{'SRC_DIR'} . "/*.pks";   
my @pksfiles = glob $globstring;


#get bodies
$globstring = $file_parm_hash->{'SRC_DIR'} . "/*.pkb";   
my @pkbfiles = glob $globstring;

#get standalone fns
$globstring = $file_parm_hash->{'SRC_DIR'} . "/*.fnc";   
my @fncfiles = glob $globstring;

#get standalone prc
$globstring = $file_parm_hash->{'SRC_DIR'} . "/*.prc";   
my @prcfiles = glob $globstring;


my $schematag; #use to differentiate logs and .sql files we spit out
if (defined $opt_schema) {
   
   $schematag = $opt_schema;
   
} else {
   
   #production.  Close enough
   $schematag = $opt_wallet;
   
}
   
my $install_src  = $file_parm_hash->{'LOG_DIR'} . "/".$schematag."_install_src.sql";
my $logfileloc   = $file_parm_hash->{'LOG_DIR'} . "/".$schematag."_install_log.txt";

#vars to hold actual package name

my $object;
my $object_file;
my $object_comploc;


   ##############################
   #Write compilation script
   ##############################


my $FH; 
open($FH, ">" . $install_src) or die("   Invalid Path? Can't write compilation script at " . $install_src."\n\n");
print $FH "/* SQL INSTALL SCRIPT FOR GENERALIZATION*/\n";

print $FH "set serveroutput ON size 1000000 format word_wrapped;\n";
print $FH "set feedback on;\n";
print $FH "set pagesize 0;\n";
print $FH "set define off;\n";  #disable ampersand substitution variable prompting
print $FH "set scan off;\n";
print $FH "timing start;\n";

#kinda hard coded log
#this writing to a file the setp for the subsequent sqlplus call to spool a log.  Meta
print $FH "spool \"".$logfileloc."\";\n";


foreach my $pks (@pksfiles) {
   $pks =~ s/^\s*//;
   $pks =~ s/\s*$//;
   if (($pks !~ m/^\#/) && (defined($pks)) ) { ### SKIP COMMENTS
      
      #dos2unix the bad boy
      #again, should be safe, worst case is warning if DBA no permission on CM
      #nope
      #system ("dos2unix ".$pks); 
      
      #substr /xxx/xxx/yyy.zzz  to get yyy.zzz Ie final slash to end
      $object_file = substr($pks,((rindex($pks, '/'))+1));

      
      #clean package removes pound signs in first line and checks authid
      #if production, writes the output file to the log directory
      #returns final location of the clean file, will be the same as one of the inputs
      
      $object_comploc = clean_package($pks, 
                                      $file_parm_hash->{'LOG_DIR'} . "/" . $object_file
                                      ); 
      
      print $FH "begin \n  DBMS_OUTPUT.PUT_LINE('Installing " . $object_comploc . "...')\;\nend\;\n\/\n";
      print $FH "\@\"" . $object_comploc . "\";\n";        #Likey now. --NOOOO cant handle special chars
      #print $FH "get\"" . $object_comploc . "\" NOLIST\n";  #nolist suppresses the screen output of every line of code. Read into buffer
      print $FH "\/\n";                           #now compile
      print $FH "show errors;\n";
      
      #substr /xxx/xxx/yyy.zzz  to get 'yyy'. Ie final slash to final dot
      $object = substr($object_comploc,((rindex($object_comploc, '/'))+1),((rindex($object_comploc, '.'))-(rindex($object_comploc, '/'))-1));
      
      print $FH "GRANT EXECUTE ON " .$object. " TO \"PUBLIC\";\n";
      print $FH "\n";
      print $FH "timing show;\n";
      print $FH "\n";
    }
}

foreach my $pkb (@pkbfiles) {
   $pkb =~ s/^\s*//;
   $pkb =~ s/\s*$//;
   if (($pkb !~ m/^\#/) && (defined($pkb)) ) { ### SKIP COMMENTS
      
      #dos2unix the bad boy
      #again, should be safe, worst case is warning if DBA no permission on CM
      #system ("dos2unix ".$pkb); 
      
      #substr /xxx/xxx/yyy.zzz  to get yyy.zzz Ie final slash to end
      $object_file = substr($pkb,((rindex($pkb, '/'))+1));
      
      #clean package removes pound signs in first line and checks authid
      #if production, writes the output file to the log directory
      #returns final location of the clean file, will be the same as one of the inputs
      
      $object_comploc = clean_package($pkb, 
                                      $file_parm_hash->{'LOG_DIR'} . "/" . $object_file
                                      ); 
      
      print $FH "begin \n  DBMS_OUTPUT.PUT_LINE('Installing " . $object_comploc . "...')\;\nend\;\n\/\n";
      print $FH "\@\"" . $object_comploc . "\";\n"; 
      #print $FH "get\"" . $pkb . "\" NOLIST\n";
      print $FH "\/\n";
      print $FH "show errors;\n";
      
      $object = substr($object_comploc,((rindex($object_comploc, '/'))+1),((rindex($object_comploc, '.'))-(rindex($object_comploc, '/'))-1));
      
      print $FH "GRANT EXECUTE ON " .$object. " TO \"PUBLIC\";\n";
      print $FH "\n";
      print $FH "timing show;\n";
      print $FH "\n";
    }
}

foreach my $fnc (@fncfiles) {
   $fnc =~ s/^\s*//;
   $fnc =~ s/\s*$//;
   if (($fnc !~ m/^\#/) && (defined($fnc)) ) { ### SKIP COMMENTS
   
      #dos2unix the bad boy
      #again, should be safe, worst case is warning if DBA no permission on CM
      #system ("dos2unix ".$fnc); 
      
      #substr /xxx/xxx/yyy.zzz  to get yyy.zzz Ie final slash to end
      $object_file = substr($fnc,((rindex($fnc, '/'))+1));
      
      #clean package removes pound signs in first line and checks authid
      #if production, writes the output file to the log directory
      #returns final location of the clean file, will be the same as one of the inputs
      
      $object_comploc = clean_package($fnc, 
                                      $file_parm_hash->{'LOG_DIR'} . "/" . $object_file
                                      ); 
 
      
      print $FH "begin \n  DBMS_OUTPUT.PUT_LINE('Installing " . $object_comploc . "...')\;\nend\;\n\/\n";
      print $FH "\@\"" . $object_comploc . "\";\n"; 
      #print $FH "get\"" . $fnc . "\" NOLIST\n";
      print $FH "\/\n";
      print $FH "show errors;\n";
      
      $object = substr($object_comploc,((rindex($object_comploc, '/'))+1),((rindex($object_comploc, '.'))-(rindex($object_comploc, '/'))-1));
      
      print $FH "GRANT EXECUTE ON " .$object. " TO \"PUBLIC\";\n";
      print $FH "\n";
      print $FH "timing show;\n";
      print $FH "\n";
    }
}


foreach my $prc (@prcfiles) {
   $prc =~ s/^\s*//;
   $prc =~ s/\s*$//;
   if (($prc !~ m/^\#/) && (defined($prc)) ) { ### SKIP COMMENTS
   
      #dos2unix the bad boy
      #again, should be safe, worst case is warning if DBA no permission on CM
      #system ("dos2unix ".$prc);  
      
      #substr /xxx/xxx/yyy.zzz  to get yyy.zzz Ie final slash to end
      $object_file = substr($prc,((rindex($prc, '/'))+1));
      
      #clean package removes pound signs in first line and checks authid
      #if production, writes the output file to the log directory
      #returns final location of the clean file, will be the same as one of the inputs
      
      $object_comploc = clean_package($prc, 
                                      $file_parm_hash->{'LOG_DIR'} . "/" . $object_file
                                      ); 
      
      print $FH "begin \n  DBMS_OUTPUT.PUT_LINE('Installing " . $object_comploc . "...')\;\nend\;\n\/\n";
      print $FH "\@\"" . $object_comploc . "\";\n"; 
      #print $FH "get\"" . $object_comploc . "\" NOLIST\n";
      print $FH "\/\n";
      print $FH "show errors;\n";
      
      $object = substr($object_comploc,((rindex($object_comploc, '/'))+1),((rindex($object_comploc, '.'))-(rindex($object_comploc, '/'))-1));
      
      print $FH "GRANT EXECUTE ON " .$object. " TO \"PUBLIC\";\n";
      print $FH "\n";
      print $FH "timing show;\n";
      print $FH "\n";
    }
}

   #recompile the whole deal
   #F (forget) this, its too slow
   #print $FH "begin \n  DBMS_OUTPUT.PUT_LINE('Recompiling all schema objects')\;\n";
   #print $FH "DBMS_OUTPUT.PUT_LINE('Ala DBMS_UTILITY.compile_schema(SYS_CONTEXT(''USERENV'', ''SESSION_USER''))')\;\nend\;\n\/\n";
   #print $FH "\/\n";
   #print $FH "show errors;\n";
   #print $FH "EXEC DBMS_UTILITY.compile_schema(SYS_CONTEXT('USERENV', 'SESSION_USER')); \n";
   

   print $FH "commit;\n";
   print $FH "timing stop;\n";
   print $FH "spool off;\n";
   print $FH "exit;\n";

   close($FH);

   print STDOUT "  Created a src file named: " . $install_src . "\n";

   
my $login_clause;


   ##############################
   #Compile Oracle code 2x
   ##############################

   
#set up the login clause.  We will reuse this variable a lot below
if ( (defined $opt_schema) && (defined $opt_password) ) {
   
   #need some // and ' here?  Added to gz_exp_imp_topo.pl if problems here 
   #done, I hope.  Should look like     sqlplus user/"password"@database
   $login_clause = $opt_schema . "/\"" . $opt_password . "\"\@" . $opt_database;
} elsif (defined $opt_wallet) {
   $login_clause = "/@" .$opt_wallet;
} else {
   print STDOUT "$usage";
   die "Bailing out, unknown options.\n\n";
}


# This is it here, the work
# Check this one for successful return code, later sqlplus calls eq whatever   

system ("sqlplus " . $login_clause . " \@" . $install_src) == 0 
        or die "\nFail, sorry, couldnt execute sqlplus, is ORACLE_HOME set?  Or is that my job?  Have your people call my people\n\n";


# Do it again for compilation order issues.  And for fun
# The spool to log on this run will overwrite the first one, and we will only check spewl 2

system "sqlplus " . $login_clause . " \@" . $install_src;


#check log for compilation errors
my $badcompchek = 0;


my $FH2;
open ($FH2, "<", "$logfileloc") or die "Can't open file ".$logfileloc;   #xx_install_log.txt
my @lines = <$FH2>; #eh shouldnt be too big
close $FH2;
         
foreach my $newline (@lines) {
   if ($newline =~ 'Warning: Package Body created with compilation errors') {
      
      $badcompchek = 1;
      print STDOUT "\n";
      print STDOUT "    YO! A PLSQL package body did not compile successfully.  Check screen output or ".$logfileloc."\n";
   }
   if ($newline =~ 'Warning: Package created with compilation errors') {
      
      $badcompchek = 1;
      print STDOUT "\n";
      print STDOUT "    YO! A PLSQL package did not compile successfully.  Check screen output or ".$logfileloc."\n";
   }
   if ($newline =~ 'SP2-') {
      
      $badcompchek = 1;
      print STDOUT "\n";
      print STDOUT "    YO! A PLSQL package did not compile successfully.  Check screen output or ".$logfileloc."\n";
   }
   if ($newline =~ 'ORA-04042') {
      
      #GRANT EXECUTE ON GZ_GEODESIC TO "PUBLIC"
      #ERROR at line 1:
      #ORA-04042: procedure, function, package, or package body does not exist 
      #sometimes Mr. Timmins names his package files differently from their compiled names
      
      $badcompchek = 1;
      print STDOUT "\n";
      print STDOUT "    YO! A Grant appears to have failed. Perhaps a package has a different file name than its compiled name?  Check screen output or ".$logfileloc."\n";
   }
}  


print STDOUT "\n";
print STDOUT "The SQLPlus screen output above is logged at ".$logfileloc."\n";
print STDOUT "\n";



   ##############################
   #Table install and etc
   ##############################

      

if ($badcompchek == 1) {
   
   print STDOUT "We better quit now since some of our code is not compiled.\n";
   print STDOUT "Check ".$logfileloc."\n";
   print STDOUT "Cheerio!\n";
   exit 1;
   
} else { 
   
   print STDOUT "I think we have completed building the generalization code base\n";
  
   my $answer;
   my $schema;

   if ( (defined $opt_install_tables) ) { 
      
      #checked this was a valid input at the start     
      $answer = uc($opt_install_tables);     
        
   } else {
      
      #prompt for tables if not explicitly passed in
      print STDOUT "\nBuild all parameter and reference tables from (B)LANK | (A)dd | (O)THERSCHEMA | (N)O?\n";
      print STDOUT "Choose (B)LANK or (O)ther for fresh install, or (N)O to leave tables alone: ";
      chomp($answer = <STDIN>);
      
      $answer = uc($answer);
      
   }
      
   if ($answer =~ m/^o|O/) {
      print STDOUT "Enter (O)THERSCHEMA Schema from which to steal parameters: ";
      chomp($schema = <STDIN>);
   } else {
      $schema = 'NULL';
   }
         
   
   #shared check for all methods
   my $goodtables = 1;
   my $goodpermissions = 1;
   #shared log names
   my $load_src_tab =  $file_parm_hash->{'LOG_DIR'}  . "/".$schematag."_install_tab.sql";        #both below execute an SQL.  Style 1 does tables in the sql. Style 2 does less
   my $table_logfile = $file_parm_hash->{'LOG_DIR'} . "/".$schematag."_install_tables_log.txt";  #both below execute a sql that logs something
   my $imp_logfile = $file_parm_hash->{'LOG_DIR'} . "/".$schematag."_install_imp_log.txt";       #only -D option needs a imp log
   
   if ($answer eq 'B' or $answer eq 'O' or $answer eq 'A') {
      
      #Style 1, not sure how often used in these modern times
      
      print STDOUT "\nLegalese warning: You've chosen a table install option that isn't 100% supported, but we are close.\n";
      print STDOUT "                  I'm gonna build tables based on the hard coded list in GZ_TYPES.LEGAL_GZ_ALL_TABLES.\n";
      print STDOUT "                  Please acknowledge you are ok with this by nodding your head.\n\n";
      sleep 3;
      
      #sub builds the script
      my $table_install_file = build_table_file($opt_schema,
                                                $load_src_tab,
                                                $table_logfile,
                                                $answer,
                                                $schema);

      ## Do all the work here
      system "sqlplus " . $login_clause . " \@" . $table_install_file;
      ##
      
      #Check the log for errors
      my $FH;
      #my $path = $opt_repository . "/".$schematag."_install_tables_log.txt";
      open ($FH, "<", "$table_logfile") or die "Can't open file ".$table_logfile;
      my @lines = <$FH>;         
      close $FH;
         
      foreach my $newline (@lines) {
            
            if ($newline =~ 'SP2' or $newline =~ 'ORA-') {
               $goodtables = 0;
            
            }
            
         } 
         
      if ($goodtables == 1) {  
      
         print STDOUT "\n";
         print STDOUT "   Tables are installed, check the log if you like: ".$table_logfile."\n";
         
      } else {
      
         print STDOUT "\n";
         print STDOUT "   Looks like we have errors in the table install.  Check ".$table_logfile. "\n";
         
      }
      
   } elsif ($answer eq 'D' or $answer eq 'R') {
      
      
      ###################################
      #Winning: dump file install style 2
      ###################################
      
      #####################################
      #The people demand table backup first
      #####################################
      
      #sub builds sql script 
      #so far it just backs up existing tables
      my $table_install_file = build_dmp_table_file($opt_schema,
                                                    $load_src_tab,
                                                    $table_logfile,
                                                    $file_parm_hash->{'DMP_TBLS'},   #comma delimited list of tables
                                                    $answer);                        #D or R
                                                    
               
      print STDOUT "\n";
      print STDOUT "   Backing up all tables before IMPing\n";
      if ($answer eq 'R') {
         print STDOUT "   Also dropping all existing tables before IMPing\n";
      }
                                           
      ## Do all the work here
      system "sqlplus " . $login_clause . " \@" . $table_install_file;
      ##
      
      #Check the log for errors
      my $FHbak;
      #my $path = $opt_repository . "/".$schematag."_install_tables_log.txt";
      open ($FHbak, "<", "$table_logfile") or die "Can't open file ".$table_logfile;
      my @linesbak = <$FHbak>;         
      close $FHbak;
         
      foreach my $newlinebak (@linesbak) {
            
            if ($newlinebak =~ 'SP2' or $newlinebak =~ 'ORA-') {
               $goodtables = 0;            
            }            
         } 
         
      if ($goodtables == 1) {  
      
         print STDOUT "\n";
         print STDOUT "   Backed up parameter tables to _Xs, check the log if you like: ".$table_logfile."\n";
         
      } else {
      
         print STDOUT "\n";
         print STDOUT "   Looks like we have errors in the parameter table backup.  Check ".$table_logfile. "\n";
         print STDOUT "   Gonna exit so we dont accidentally break stuff\n\n";
         exit 1;
         
      }
      
      ##############
      #IMP DA DMP
      ##############
      
      my $imp_stmt;

      print STDOUT "\n";
      print STDOUT "   Calling imp on dump file\n";

      #figured out login clause way above
      $imp_stmt = "imp ".$login_clause." FILE=".$file_parm_hash->{'DMP_FILE'}." ";

      #ignore=N meaning don't just pile more records in on reruns
      $imp_stmt.= "indexes=Y ignore=N grants=N tables=\\(".$file_parm_hash->{'DMP_TBLS'}."\\) ";

      print STDOUT "   Heres a printout of the imp we are about to send to the system:\n\n";
      print STDOUT "      ".$imp_stmt."\n\n"; 

      my $sys_return;
      
      eval {
         
         #my $sys_return = system ($imp_stmt);
         
         #execute with back tics
         #use file-descriptor redirection, stderr into stdout and back to variable
         #perl backtick and pipe opens all use the Bourne shell
         $sys_return = `$imp_stmt 2>&1`;
         
      };
      

    
      if ($@) {
         #this is a total failure, like imp executable doesnt exist
         print STDOUT "   Looks like import failed and I dont know how to recover.  Sorry bub\n";
         exit 1;
      }
      
      #write out the imp log regardless      
      
      my $FHimp;
      open ($FHimp, ">", "$imp_logfile") or die "Can't write file ".$imp_logfile;
      print $FHimp $sys_return;        
      close $FHimp; 
      
      #there may be multiple errors, look for the most serious first
      if ($sys_return =~ 'IMP-00015:') {
         
         #IMP-00015: following statement failed because the object already exists:         
         print STDOUT "\n";
         print STDOUT "   Fail: Looks like some of the tables in the dmp file were already present in the schema\n";
         print STDOUT "   Check the full imp log at ".$imp_logfile."\n";
         print STDOUT "\n";
         exit 1;
         
      } elsif ($sys_return =~ 'IMP-00003: ORACLE error 29855 encountered') {
         
         #spatial index creation failed due to usual metadata error
         #NB the index will be present but domidx_opstatus will be failed
         
         print STDOUT "\n";
         print STDOUT "Fail: Looks like you had spatial indexes in the dmp file.  These cant be IMPed (easily, at least)\n";
         print STDOUT "         Theyll be present in the schema but with domidx_opstatus of FAIL.\n";
         print STDOUT "         You should probably check the full imp log at ".$imp_logfile."\n";
         print STDOUT "         I'm gonna fail here without continuing out of an abundance of caution.\n";
         print STDOUT "\n";
         exit 1;
         
      } elsif ($sys_return =~ 'ORA-01950: no privileges on tablespace') {
         
         #Some sort of tablespace issue
         #Stephanie saw this I've never seen it in the wild
         
         print STDOUT "\n";
         print STDOUT "Fail: Looks like we had a tablespace meltdown in the IMP\n";
         print STDOUT "         You should probably check the full imp log at ".$imp_logfile."\n";
         print STDOUT "         I'm gonna fail here without continuing out of an abundance of caution.\n";
         print STDOUT "\n";
         exit 1;
         
      } elsif ($sys_return =~ 'sh:' or $sys_return =~ 'syntax error') {
         
         #sh: -c: line 0: syntax error near unexpected token `('
         #sh: -c: line 0: `imp schel010/ ....
         
         print STDOUT "\n";
         print STDOUT "Fail: Looks like a syntax problem in the imp call\n";
         print STDOUT "         Check the imp statement printed above.\n";
         print STDOUT "         Or check the full imp log at ".$imp_logfile."\n";
         print STDOUT "\n";
         exit 1;
         
         
      #} NEW ERRORS GO HERE (probably)
         
      } else {
         
         #one final triple check since I seem to be having trouble capturing system errors
         if ($sys_return =~ 'Import terminated successfully') {
         
            print STDOUT "\n";
            print STDOUT "   Apparent success on dmp file import\n";
            print STDOUT "   Check the full imp log if you like at ".$imp_logfile."\n";
            print STDOUT "\n";
         
         } else {
         
            print STDOUT "\n";
            print STDOUT "Possible Fail: I dont see \"no errors\" in the imp output\n";
            print STDOUT "         Gonna fail here out of an abundance of neurotic caution.\n";
            print STDOUT "         Check the imp log at ".$imp_logfile."\n";
            print STDOUT "\n";
            exit 1;
            
         }
   
      }
      
      print STDOUT "\n\n";
      
      
      ###########################################
      #Special hard coded parameter table updates
      ###########################################
      
      
      my $update_tab =    $file_parm_hash->{'LOG_DIR'}  . "/".$schematag."_update_tab.sql";
      my $table_logfile = $file_parm_hash->{'LOG_DIR'} . "/".$schematag."_update_tables_log.txt";
      
      #call sub to build .sql
      #will update various annoying columns to the name of the new schema, database, etc
      #If the table list is NULL the sub will skip it
      
      my $table_update_file = build_table_update_file($update_tab,
                                                      $table_logfile,
                                                      $file_parm_hash->{'DEPLOY_TBLS'},
                                                      $file_parm_hash->{'GEN_SCHEMA_TBLS'},
                                                      $file_parm_hash->{'UNGEN_SCHEMA_TBLS'},
                                                      $file_parm_hash->{'SOURCE_LOCATION_TBLS'}
                                                      );
                                                           
      #sub should just return the input /xx/xx/update_tab.sql
      
      #Actual work here
      system "sqlplus " . $login_clause . " \@" . $table_update_file;
      ##
      
      #Check the log for errors
      my $FHtab;
      open ($FHtab, "<", "$table_logfile") or die "Can't open file ".$table_logfile;
      my @tablines = <$FHtab>;         
      close $FHtab;     
      
      foreach my $tabnewline (@tablines) {
            
            if ($tabnewline =~ 'SP2' or $tabnewline =~ 'ORA-') {
               $goodtables = 0;            
            }            
         } 
         
      if ($goodtables == 1) {  
      
         print STDOUT "\n";
         print STDOUT "   Updated hard coded schema name, database, etc in tables, check the log if you like: ".$table_logfile."\n";
         
      } else {
      
         print STDOUT "\n";
         print STDOUT "   ***ERROR***: Looks like we have errors in hard coded table parameters update.  Check ".$table_logfile. "\n";
         
      }
      
      #update permissions
      my $permissions_tab =     $file_parm_hash->{'LOG_DIR'}  . "/".$schematag."_permissions_tab.sql";
      my $permissions_logfile = $file_parm_hash->{'LOG_DIR'} . "/".$schematag."_permissions_tables_log.txt";
      
      #sub will build the .sql
      #Will roll through all the tables we imped and call the priv granter on them, based on reference_schemas ultimately
      my $table_permissions_file = build_table_permissions_file($permissions_tab,$permissions_logfile,$file_parm_hash->{'DMP_TBLS'});
      #sub should just return the input /xx/xx/update_tab.sql
      
      #Actual work here
      system "sqlplus " . $login_clause . " \@" . $table_permissions_file;
      ##
      
      #Check the log for errors
      my $FHpermissions;
      open ($FHpermissions, "<", "$permissions_logfile") or die "Can't open file ".$permissions_logfile;
      my @permissionslines = <$FHpermissions>;         
      close $FHtab;     
      
      foreach my $permissionsnewline (@permissionslines) {
            
            if ($permissionsnewline =~ 'SP2' or $permissionsnewline =~ 'ORA-') {
               $goodpermissions = 0;            
            }            
         } 
         
      if ($goodpermissions == 1) {  
      
         print STDOUT "\n";
         print STDOUT "   Updated permissions, check the log if you like: ".$permissions_logfile."\n";
         
      } else {
      
         print STDOUT "\n";
         print STDOUT "   ***ERROR:*** Looks like we have errors in permissions update.  Check ".$permissions_logfile. "\n";
         print STDOUT "\n";
         
      }
      
   } else {
      
      print STDOUT "   You chose not to install tables so we are done\n";
      print STDOUT "\n";
      
   
   }
   
 
       
   if ($goodtables == 1 && $goodpermissions == 1) {
      
      print STDOUT '  _             '."\n";
      print STDOUT ' ( ((           '."\n";
      print STDOUT '  \ =\          '."\n";
      print STDOUT ' __\_ `-\       '."\n";
      print STDOUT '(____))(  \---- '."\n";  
      print STDOUT '(____)) _       '."\n";
      print STDOUT '(____))         '."\n";
      print STDOUT '(____))____/----'."\n";

   
      print STDOUT "\nThumbs up!\n\n";     
   
   } else {
      
      print STDOUT "\n\n   No thumbs up\n";
      print STDOUT "   Error in table or permissions update, see log above\n\n";
      exit 1;
      
   }  
                          



}

#too cute
exit 0;


##############################################################################
############################## End build script   ############################
############################## Start sub routines ############################
##############################################################################


=head2 test_answer_yes_no

C<test_answer_yes_no> takes a $answer and continues looping ubtil the STDIN value starts with either the letter 'y|Y' or 'n|N'.

=cut

sub test_answer_yes_no {
   ### LOOP UNTIL USER ANSWERS "y" or "n"
   my $answer = shift;
   $answer =~ s/(^\s*)([a-z|A-Z|0-9]+)(\s*$)/$2/;
   until (($answer =~ m/^y|Y/) || ($answer =~ m/^n|N/)) {
      print STDOUT 'Please answer "y" or "n"' . "\n";
      print STDOUT "Continue? (y/n): ";
      chomp($answer = <STDIN>);
      $answer =~ s/(^\s*)([a-z|A-Z|0-9]+)(\s*$)/$2/;
#     print STDOUT "TEST 02: '" . $answer ."'\n";
   }
   return $answer;
}


##############################################################################

=head2 build_table_file

Drops out a file that when executed by SQL plus will install parameter tables

=cut

sub build_table_file {
   my $user_schema      = shift;
   my $install_tab      = shift;
   my $log_file         = shift; 
   my $option           = shift;    #Blank, Add, or Other
   my $schema           = shift;    #'NULL' unless Other, then <some other schema>          

                          
   my $FH;
   open($FH, ">", $install_tab) or die("Can't write yo file! --> " .$install_tab);
   
   print $FH "/* CREATE GZ PARAMETER TABLES */\n";
   print $FH "set serveroutput on;\n"; 
   
   #Log
   print $FH "spool \"".$log_file."\";\n";

   
   print $FH "set termout off;\n"; #too messy
   print $FH "set feedback on;\n";
   
   #Back everything up before we make a mess. These just go to a mess of _X1 style tables
   print $FH "BEGIN\n   GZ_UTILITIES.BACKUP_GZ_TABLES();";
   print $FH "\nEND;\n\/\n";
   
   #Build new empty tables as determined in the proc
   
   if ($option eq 'A') {
      
      #do not nuke any tables
      #all drop flags set to No
      print $FH "BEGIN\n   GZ_UTILITIES.CREATE_GZ_TABLES('N','N');"; 
      print $FH "\nEND;\n\/\n";
      
   } elsif ($option eq 'B') {  
      
      #Blank means kill anything at will and recreate
      print $FH "BEGIN\n   GZ_UTILITIES.CREATE_GZ_TABLES('Y','Y');";
      print $FH "\nEND;\n\/\n";
      
   } elsif ($option eq 'O') {
      
      #other schema means we should be empty then fill er up
      print $FH "BEGIN\n   GZ_UTILITIES.CREATE_GZ_TABLES('Y','Y');";
      print $FH "\nEND;\n\/\n";
      
      #other schema, copy data over
      print $FH "BEGIN\n   GZ_UTILITIES.COPY_GZ_TABLES('". $schema ."');";
      print $FH "\nEND;\n\/\n";
      
   }

   print $FH "show errors;\n";
   print $FH "exit;\n";
  
   return $install_tab;
     
}

##############################################################################

=head2 build_dmp_table_file

Drops out a file that when executed by SQL plus will backup parameter tables before dump file overwrite

=cut

sub build_dmp_table_file {
   my $user_schema      = shift;
   my $install_tab      = shift;  #ex /logpath/logpath/gzcpb14_install_tab.sql 
   my $log_file         = shift;  #ex /logpath/logpath/gzcpb14_install_tab.sql 
   my $table_list       = shift;  #ex /logpath/logpath/gzcpb14_install_tables_log.txt
   my $option           = shift;  #D(ump) or R(eplace) 

                          
   my $FH;
   open($FH, ">", $install_tab) or die("Can't write yo file! --> " .$install_tab);
   
   print $FH "/* BACKUP GZ PARAMETER TABLES */\n";
   print $FH "set serveroutput on;\n"; 
   
   #Log
   print $FH "spool \"".$log_file."\";\n";

   
   print $FH "set termout off;\n"; #too messy
   print $FH "set feedback on;\n";
   
   my @tablearray = split(/,/, $table_list);
   
   foreach my $parmtable (@tablearray) {
      
      #dont forget to trim spaces dummy. Matters here
      print $FH "BEGIN\n  GZ_UTILITIES.COPY_TO_X('".trim($parmtable)."');";
      print $FH "\nEND;\n\/\n";
   
   }
   
   if ( $option eq 'R' ) {
    
      #Replace meaning drop all tables before IMPing
      
      foreach my $parmtable (@tablearray) {
      
         print $FH "DROP TABLE ".trim($parmtable).";\n";
      
      }
   
   }

   print $FH "show errors;\n";
   print $FH "exit;\n";
  
   return $install_tab;
     
}

##############################################################################

sub build_table_update_file {
   my $install_tab            = shift;  #path to .sql
   my $log_file               = shift;  #path to log
   my $deploy_tbls            = shift; 
   my $gen_schema_tbls        = shift;  
   my $ungen_schema_tbls      = shift;  
   my $source_location_tbls   = shift;
     

                          
   my $FH;
   open($FH, ">", $install_tab) or die("Can't write yo file! --> " .$install_tab);
   
   print $FH "/* UPDATE GZ PARAMETER TABLES */\n";
   print $FH "set serveroutput on;\n"; 
   
   #Log
   print $FH "spool \"".$log_file."\";\n";

   
   print $FH "set termout off;\n"; #too messy
   print $FH "set feedback on;\n";
   
   #defined vs null, trying to protect myself from everything here
   
   if (defined $deploy_tbls) {
      
      if ($deploy_tbls ne '') {
      
         print $FH "BEGIN\n   GZ_UTILITIES.UPDATE_HARD_CODED_TABLES('".$deploy_tbls."','DEPLOY_TBLS');";
         print $FH "\nEND;\n\/\n";
      
      }
      
   }
   
   if (defined $gen_schema_tbls) {
      
      if ($gen_schema_tbls ne '') {
      
         print $FH "BEGIN\n   GZ_UTILITIES.UPDATE_HARD_CODED_TABLES('".$gen_schema_tbls."','GEN_SCHEMA_TBLS');";
         print $FH "\nEND;\n\/\n";
         
      }
      
   }
   
   if (defined $ungen_schema_tbls) {
      
      if ($ungen_schema_tbls ne '') {
      
         print $FH "BEGIN\n   GZ_UTILITIES.UPDATE_HARD_CODED_TABLES('".$ungen_schema_tbls."','UNGEN_SCHEMA_TBLS');";
         print $FH "\nEND;\n\/\n";
         
      }
      
   }
   
   if (defined $source_location_tbls) {
      
      if ($source_location_tbls ne '') {
      
         print $FH "BEGIN\n   GZ_UTILITIES.UPDATE_HARD_CODED_TABLES('".$source_location_tbls."','SOURCE_LOCATION_TBLS');";
         print $FH "\nEND;\n\/\n";
         
      }
      
   }

   print $FH "show errors;\n";
   print $FH "exit;\n";
  
   return $install_tab;
     
}

##############################################################################

sub build_table_permissions_file {
   my $install_tab         = shift;  #path to .sql
   my $log_file            = shift;  #path to log
   my $tbls_list           = shift;  #comma delimited list of all tables fromt he dmp file

                      
   my $FH;
   open($FH, ">", $install_tab) or die("Can't write yo file! --> " .$install_tab);
   
   print $FH "/* UPDATE PERMISSIONS ON ALL GZ TABLES */\n";
   print $FH "set serveroutput on;\n"; 
   
   #Log
   print $FH "spool \"".$log_file."\";\n";

   
   print $FH "set termout off;\n"; #too messy
   print $FH "set feedback on;\n";
       
   print $FH "BEGIN\n   GZ_UTILITIES.PRIV_GRANTS_ON_LIST('".$tbls_list."');";
   print $FH "\nEND;\n\/\n";
      
   print $FH "show errors;\n";
   print $FH "exit;\n";
  
   return $install_tab;
     
   
}

##############################################################################

=head2 clean_package


=cut

sub clean_package {
   my $package_in      = shift;
   my $package_out     = shift;
   
   #pushes any pound signs within comment blocks back from the first position 
   #checks authid current user on pks - this causes a failure by order of me
   #what else?
   
   my $authidcheck;
   
   if ($package_in =~ m/.pks$/ ) {    # dollar = end of string 
      $authidcheck = 0;
   } else {
      $authidcheck = 1;
   }
   
   #read in the code
   my $FH;   
   open ($FH, "<", "$package_in") or die "Can't open file ".$package_in;         
   my @lines = <$FH>;         
   
   #delete the old file if we are in read and write development mode
   if ($package_in eq $package_out) {
      unlink $package_in;
   }

   #open file handle for write
   my $FH_write; 
   open($FH_write, ">" . $package_out) or die("Invalid Path? Can't write file" . $package_out);
         
   foreach my $newline (@lines) {      

      
      if ( $newline =~ m/^\#/ ) {
       
         #replace pound with space then whatever
         $newline = " ".$newline;
          
      }
      
      if ( ($authidcheck == 0) && ($newline =~ m/Authid Current_User/i) ) {
         
         #found it
         $authidcheck = 1;
         
      }       

      print $FH_write "$newline";

   }
      
   
   close $FH;
   close($FH_write);
   
   if ($authidcheck == 0) {
      die "Package ".$package_in." is missing authid clause\n";
   }
   
   #caller always gets final location to use in compilation
   return $package_out;

}
  
##############################################################################
 
sub get_file_parms {
   my $path                   = shift; #just the file name. Expected to be in ../data
   my $install_type           = shift; #PROD or DEV
   
   #This sub gets the input parameters from a file
   #We will expect that the input parameters are in the file in this exact order
   #May be possible to search keys looking like certain text
   #But this can get messy when a value includes text that looks like a key
   
   my $kount = 1;
   my($returnhash);
   #optimists
   $returnhash->{'STATUS'} = 'PASS';
   $returnhash->{'MESSAGE'} = "";
   
   if ($install_type eq 'PROD') {
      
      #Production inputs have no knowledge of path. 
      #findbin shenanigans below in case user is sitting somewhere else
      #ex $bin = /mapdata2/generalization/programs/matt/generalization/bin

      #go to that last slash and then add /data/xxx.txt to it
      my $datfilepath = substr($Bin,0,((rindex($Bin, '/'))))."/data/".$path;
      
      unless (-r $datfilepath) {
         $returnhash->{'STATUS'} = "FAIL";
         $returnhash->{'MESSAGE'} = "Cant read yo file at ".$datfilepath."\n";
         return $returnhash;
      }
      
      my $FH;
      open ($FH, "<", $datfilepath) or die "Can't open yo file, ".$datfilepath."\n";
      
      my @lines = <$FH>;         
      close $FH;
      
      #This is some shameful work
      
      foreach my $parmline (@lines) {
         
         if ($kount == 1) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'APP_DIR'} = trim($arr[1]);
         }
         if ($kount == 2) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'LOG_DIR'} = $returnhash->{'APP_DIR'} . trim($arr[1]);
         }
         if ($kount == 3) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'BIN_DIR'} = $returnhash->{'APP_DIR'} . trim($arr[1]);
         }
         if ($kount == 4) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'DATA_DIR'} = $returnhash->{'APP_DIR'} . trim($arr[1]);
         }
         if ($kount == 5) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'SRC_DIR'} = $returnhash->{'APP_DIR'} . trim($arr[1]);
         }
         if ($kount == 6) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'DMP_FILE'} = $returnhash->{'APP_DIR'} . trim($arr[1]);
         }
         if ($kount == 7) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'FROM_USR'} = trim($arr[1]);
         }
         if ($kount == 8) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'DMP_TBLS'} = trim($arr[1]);
         }
         if ($kount == 9) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'DEPLOY_TBLS'} = trim($arr[1]);
         }
         if ($kount == 10) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'GEN_SCHEMA_TBLS'} = trim($arr[1]);
         }
         if ($kount == 11) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'UNGEN_SCHEMA_TBLS'} = trim($arr[1]);
         }
         if ($kount == 12) {
            my @arr = split(/=/,$parmline,2);
            $returnhash->{'SOURCE_LOCATION_TBLS'} = trim($arr[1]);
         }
               
         $kount++;   
      }
   
   } else { #end if installing from a file in production
      
      #development install, hard code paths based on root
      #no dump file mess
      $returnhash->{'APP_DIR'}  = trim($path);
      $returnhash->{'LOG_DIR'}  = $returnhash->{'APP_DIR'}."/src";
      $returnhash->{'BIN_DIR'}  = $returnhash->{'APP_DIR'}."/bin";
      $returnhash->{'DATA_DIR'} = $returnhash->{'APP_DIR'}."/data";
      $returnhash->{'SRC_DIR'}  = $returnhash->{'APP_DIR'}."/src";
      
   }
      
      
   if ($install_type eq 'PROD') {
      
      #problem checks double checks checks for prod
      
      if ( ! defined $returnhash->{'APP_DIR'} ) {
         $returnhash->{'status'} = "FAIL";
         $returnhash->{'message'} = "Cant find APP_DIR in ".$path."\n";
      }
      if ( ! defined $returnhash->{'LOG_DIR'} ) {
         $returnhash->{'status'} = "FAIL";
         $returnhash->{'message'} = "Cant find a LOG_DIR in ".$path."\n";
      }
      if ( ! defined $returnhash->{'BIN_DIR'} ) {
         $returnhash->{'status'} = "FAIL";
         $returnhash->{'message'} = "Cant find a BIN_DIR in ".$path."\n";
      }
      if ( ! defined $returnhash->{'DATA_DIR'} ) {
         $returnhash->{'status'} = "FAIL";
         $returnhash->{'message'} = "Cant find a DATA_DIR in ".$path."\n";
      }
      if ( ! defined $returnhash->{'SRC_DIR'} ) {
         $returnhash->{'status'} = "FAIL";
         $returnhash->{'message'} = "Cant find a SRC_DIR in ".$path."\n";
      }
      if ( ! defined $returnhash->{'DMP_FILE'} ) {
         $returnhash->{'status'} = "FAIL";
         $returnhash->{'message'} = "Cant find a DMP_FILE in ".$path."\n";
      }
      if ( ! defined $returnhash->{'FROM_USR'} ) {
         $returnhash->{'status'} = "FAIL";
         $returnhash->{'message'} = "Cant find a FROM_USR in ".$path."\n";
      }
      if ( ! defined $returnhash->{'DMP_TBLS'} ) {
         $returnhash->{'status'} = "FAIL";
         $returnhash->{'message'} = "Cant find DMP_TBLS in ".$path."\n";
      }
      if ( ! defined $returnhash->{'DEPLOY_TBLS'} ) {
         $returnhash->{'status'} = "FAIL";
         $returnhash->{'message'} = "Cant find DEPLOY_TBLS in ".$path."\n";
      }
      #no check on gen_schema_tbls, we wont be dealing with them soon 
      #no check on ungen_schema_tbls, we wont be dealing with them soon 
      #no check on source_location_tbls we wont have to deal with them some day   
      
   }
   
   return $returnhash;
   
}

##############################################################################

# Perl trim function to remove whitespace from the start and end of the string
sub trim {
   my $string = shift;
   $string =~ s/^\s+//;
   $string =~ s/\s+$//;
   return $string;
}
   
   

############################## END SUB ROUTINES ##############################

__END__