#!/usr/bin/perl
use strict;
use warnings;

use DBI; # 1.607;
use DBD::Oracle; # 1.22;

# Get Options and set defaults if necessary

use Getopt::Long qw(:config no_ignore_case bundling);

my (
   $state,
   $res,
   $pext,
   $instance,
   $mods
   );

my $rc = GetOptions(
   'state|s=s' => \$state,
   'res|r=s'     => \$res,
   'pext|p=s'     => \$pext,
   'instance|i=s'     => \$instance,
   'mods|m=s' => \$mods
);

my $usage = "usage: gzprod_fsl_dec10.pl -s <state> -r <resolution z6|z8|z9> -p ".
            "<pw extension> { -i <instance 1-6>  -m <modules> }\n";

unless ( (defined $state ) && (defined $res ) && (defined $pext) ) {
   print "\n";
   print STDOUT $usage;
   die "Please enter first three arguments at command line.\n";
}


unless (defined $mods) {
   $mods = 4;
}

if ($mods !~ /4/) {
   print "\n";
   print STDOUT $usage;
   print STDOUT "-m option must be 4 (for FSL build)\n";
   die "\n";

}

unless (defined $instance) {
   $instance = 4;
}

if ( ($instance !~ /1|2|3|4|5|6/) || (length($instance) > 1) ) {
   print "\n";
   print STDOUT $usage;
   print STDOUT "-i option must be 1, 2, 3, 4, 5 or 6\n";
   die "\n";
}


# set up variables...

my $database = 'PRODBNCH'.$instance;
my $user = 'GZDEC10ST' . $state;
my $password = $user . $pext;

if ( (length($state) !=  2) || (length($pext) != 1) ) {
  print STDOUT "Either the State or Pext option has the incorrect length. \n";
  die "Please call the program with a valid state and pext.\n";
}

# Login to the DB (copied from CAMPS)

my $connection = [$database,$user,\$password];
our $dbh = get_dbh($connection);

#  I should really check the connect to be smart, see Tracy's test_login in
#  RunCamps

my $sql;
my $sth;
my $chk;

# start dbms ouput

$dbh->do("begin dbms_output.enable(NULL); end;");

# print current schema just to check connection...

$sql = "select USER from DUAL";
$sth = $dbh->prepare($sql);
$sth->execute;
$sth->bind_columns(\($chk));
$sth->fetch();
$sth->finish();

print "\n";
print "------------------------------------------------\n";
print "-- START OUTPUT -------------------------------- \n";
print "------------------------------------------------\n";
print "\n";
print STDOUT "Current Schema = " . $chk . "\n";
$user = uc($user);
print STDOUT "User           = " . $user . "\n";
print STDOUT "Instance       = " . $database . "\n";


##########################################################################################

# RUN each GZ step... (FSL only for this example...)

# with primitive run control

my $status = 'GOOD';

# prepare fsl build call...

$res = uc($res);
my $topo_in = $res . $state . "LS";
my $face_tab_ext = "CLIP_FACE";
my $face_tab = $topo_in . "_" . $face_tab_ext;
my $topo_universe;  # = "TOPO_UNIVERSE_FSL_" . $res;
my $topo_hierarchy; # = "TOPO_HIERARCHY_FSL_" . $res;
my $has_mirrors; 
my $project_z9;
my $st_edges_tab;
my $release = "TAB10";

# assign appropriate arguments based on resolution... all this should go in to the FSL parameter table

if ($res eq "Z6") {
   $has_mirrors = "Y";
   $topo_universe = "TOPO_UNIVERSE_FSL_" . $res;
   $topo_hierarchy = "TOPO_HIERARCHY_FSL_" . $res;
} else {
   $has_mirrors = "N"; 
   $topo_universe = "TOPO_UNIVERSE_" . $res;
   $topo_hierarchy = "TOPO_HIERARCHY_" . $res;
}

if ($res eq "Z9") {
  $project_z9 = "Y"
} else {
  $project_z9 = "N"
}

if ($res eq "Z9") {
   $st_edges_tab = $topo_in . "_STATE_EDGES_Z9_V1";
} else {
   $st_edges_tab = $topo_in . "STATE_EDGES_" . $res . "_V2";
}


if ($mods =~ /4/) {

   # clean up FSLs if heirarchical tables exist  (there should only be two registered tables at this stage)

   $status = &cleanup_fsls($topo_in,$user,$face_tab,$st_edges_tab);

   # if this is the projected resolution, clean up the hard-coded tables from
   # the projection step.

   if (($status eq 'GOOD') && ($res eq 'Z9')) { 
         $status = &cleanup_z9_tmp_tables();
   }

   # run FSL build...

   if ($status eq 'GOOD') {
      $status = &build_fsls($topo_in,$face_tab_ext,$topo_universe,$topo_hierarchy,$has_mirrors,$project_z9,$release,$user); 
   }
}

# check FSL Build Status

if ($status ne 'GOOD') {
   $dbh->disconnect;
   print STDOUT "FSL BUILD Failed, ending program\n";
   exit 1;
}

# If it worked, exit happily

print STDOUT "\n";
print STDOUT "Looks like all the procedures you asked to run passed.\n";
print STDOUT "GZ production script complete.\n";
print STDOUT "\n";

# disconnect from Oracle

$dbh->disconnect;

# return zero if everything passes

exit 0;


##############################################################################
# SUBROUTINES
##############################################################################

=head2 cleanup_fsls

Use this to drop all FSL tables prior to the FSL build process.

Tables to keep are the final two arguments

=cut

sub cleanup_fsls {

   # collect arguments

   my $topo_in = shift;
   my $user  = shift;
   my $face_tab = shift;
   my $st_edges_tab = shift;

   # db variables

   my $sql;
   my $sth;
   my $chk = 0;
   my @lines = [];

   my $clean_stat = 'GOOD';

   print STDOUT "...................CLEANING UP FSLS...................\n";
   print STDOUT "Input Topo       = " . $topo_in . "\n";
   print STDOUT "Schema           = " . $user . "\n";
   print STDOUT "Face Table       = " . $face_tab. "\n";
   print STDOUT "State Edge Tab   = " . $st_edges_tab . "\n";

   my $tracking = $topo_in . '_FSL_TRACKING';

   # Set Action Info for the DBAs

   $sql = "BEGIN " .
          "MT_UTIL_CONN_ID.SET_MODULE('GZ_FSL-ST".
          $state . "-" . $topo_in .  " ' || SYSDATE, 'Start FSL clean up.'); " .
          "END;";
   $sth = $dbh->prepare($sql);
   $sth->execute;
   $sth->finish();

   # truncate the FSL tracking table if it exists...

   $sql = "SELECT COUNT(*) " .
          "FROM user_tables " .
          "WHERE table_name = :1";
 
   print STDOUT "\n";
   print STDOUT "CLEANUP_FSLS: Checking for TRACKING TABLE, SQL = " .
                 $sql . " USING '" .  $tracking . "'\n";
   
   $sth = $dbh->prepare($sql);
   $sth->bind_param( 1, $tracking);
   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();


   unless ($chk == 0) {

      #truncate the tracking table...
      
      $sql = "TRUNCATE TABLE $tracking ";
      print STDOUT "CLEANUP_FSLS: Truncating TRACKING TABLE, SQL = " .
                 $sql . "\n";
      $sth = $dbh->prepare($sql);
      $sth->execute;
      $sth->finish();

   }

   $chk = 0;
 
   # de-register and drop existing FSL tables
 
   # need to check first - if no FSLs exist, the DEREGISTER_FSL procedure
   # crashes

   $sql = "SELECT COUNT(*) " . 
          "FROM user_sdo_topo_info " .
          "WHERE topology = :1";

   print STDOUT "\n";
   print STDOUT "CLEANUP_FSLS: Counting Tables, SQL = " . $sql . " USING '" .
                 $topo_in . "'\n";
   
   $sth = $dbh->prepare($sql);
   $sth->bind_param( 1, $topo_in);
   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();

   if ($chk <= 2 ) {
   
      print STDOUT "CLEANUP_FSLS: Found only $chk registered tables.  Skipping De-registration step.\n";
   
   } else {
   
      print STDOUT "CLEANUP_FSLS: Found $chk registered tables. " .
                   "Continuing with de-registration step.\n";
                   
      $sql = "BEGIN " .
             "GZ_TOPO_HELPER.DEREGISTER_FSL('" .
             $topo_in . "','" .
             $user . "'); " .
             "END;";

      print STDOUT "CLEANUP_FSLS: SQL = " . $sql . "\n";

      # Run the FSL Cleanup

      @lines = [];
      $sth = $dbh->prepare($sql);
      $sth->execute;
      @lines = $dbh->func('dbms_output_get');
      $sth->finish();

      &print_dbms_out(@lines);

      # to get rid of mirrors, you have to drop the non-registered FSL tables,
      # too. This only affects Z6 in Dec10.  The mirrors are not registered
      # with the topology.

      $sql = "BEGIN " . 
             "gz_topo_util.drop_tables_with_prefix('$topo_in" . "_FSL', '$user'); " .
             "END;";
      print STDOUT "CLEANUP_FSLS: SQL = " . $sql . "\n";
      @lines = [];
      $sth = $dbh->prepare($sql);
      $sth->execute;
      @lines = $dbh->func('dbms_output_get');
      $sth->finish();

      &print_dbms_out(@lines); 

   }
   
   # Check Results
   
   $sql = "SELECT COUNT(*) FROM ALL_SDO_TOPO_INFO " . 
          "WHERE owner = '$user' AND topology = '$topo_in'";

   $sth = $dbh->prepare($sql);
   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();

   # if the number of registered tables does not exactly equal 2, there is a problem.

   if ($chk == 2) {

      print STDOUT "CLEANUP_FSLS: PASSED for State " . $state . ".\n";
      $clean_stat = 'GOOD';
  
   } else {

      print STDOUT "CLEANUP_FSLS: ERROR in following check... \n";
      print STDOUT "CLEANUP_FSLS:   SQL = " . $sql . "\n";
      print STDOUT "CLEANUP_FSLS:   resulted in " . $chk . 
                   " registered tables found.\n"; 
      print STDOUT "CLEANUP_FSLS:   I expected to get exactly two registered tables.\n"; 

      print STDOUT "CLEANUP_FSLS: FAILED for State " . $state ."\n";
      $clean_stat = 'BAD';

   }

   print "...............................................\n";

   return $clean_stat;

# end cleanup_fsls subroutine
}

##########################################################

=head2  cleanup_z9_tmp_tables

Use this to drop the three temporary Projection tables if projection bails out
in the middle of Z9 FSL processing

=cut

sub cleanup_z9_tmp_tables {

   my $z9_drop_stat = 'GOOD';
   
   # db variables

   my $sql;
   my $sth;
   my $chk = 0;

   print STDOUT "\n";
   print STDOUT "..............CLEANING UP Z9 TMP TABLES..............\n";
   print STDOUT "CLEANUP_Z9_TMP_TABLES: Begin chekcing for temporary projection tables\n";
   print STDOUT "\n";

   my @tables = qw/XX2003ONLY XXTMPOUT AREA/;
   my $table_name;
   
   # look for each table and if it is there drop it.

   foreach $table_name(@tables){
   
      print STDOUT "CLEANUP_Z9_TMP_TABLES: Checking for $table_name\n";

      $chk = 0;
      $sql = "SELECT COUNT(*) " . 
             "FROM user_tables " .
             "WHERE table_name = :1";
   
      $sth = $dbh->prepare($sql);
      $sth->bind_param( 1, $table_name);
      $sth->execute;
      $sth->bind_columns(\($chk));
      $sth->fetch();
      $sth->finish();

      if ($chk == 1) {

         $sql = "DROP TABLE " . 
                "$table_name " .
                "CASCADE CONSTRAINTS PURGE";
      
         print STDOUT "CLEANUP_Z9_TMP_TABLES: Dropping $table_name with this SQL, '$sql'\n";
         $sth = $dbh->prepare($sql);
         $sth->execute;
         $sth->finish();

         $z9_drop_stat = 'GOOD';

      } elsif ($chk == 0) {

         print STDOUT "CLEANUP_Z9_TMP_TABLES: Did not find a table called $table_name to clean up.\n";
         $z9_drop_stat = 'GOOD';

      } else {   
           print STDOUT "CLEANUP_Z9_TMP_TABLES: Got '$chk' when I ran $sql \n";
           print STDOUT "CLEANUP_Z9_TMP_TABLES: FAILED \n";
           $z9_drop_stat = 'BAD';
      }
      
   }
   print STDOUT "CLEANUP_Z9_TMP_TABLES: Complete, returning status $z9_drop_stat\n";

   return $z9_drop_stat;
# end cleanup_z9_tmp_tables subroutine
}


##########################################################

=head2 print_dbms_out

Use this to print dbms_output messages

=cut

sub print_dbms_out {

   my $line;

   print "-----------------------\n";
   print " DBMSOUT start\n";
   print "-----------------------\n";
   foreach $line(@_){
         print "   " . "$line" ."\n";
   }
   print "-----------------------\n";
   print " DBMSOUT end\n";
   print "-----------------------\n";


}

##########################################################

=head2 build_fsls

Use this to run and check the FSL build process

=cut

sub build_fsls {

   # collect arguments

   my $topo_in = shift;
   my $face_tab_ext = shift;
   my $topo_universe = shift;
   my $topo_hierarchy = shift;
   my $has_mirrors = shift;
   my $project_z9 = shift;
   my $release = shift;
   my $user = shift;

   # db variables

   my $sql;
   my $sth;
   my $chk = 0;
   my @lines = [];
   my $msg = 'BUILD_FSLS: ';

   my $fsl_stat = 'GOOD';

   print STDOUT "...................CREATING FSL TABLES...................\n";
   print STDOUT "Input Topo        = " . $topo_in . "\n";
   print STDOUT "Face Table Ext.   = " . $face_tab_ext. "\n";
   print STDOUT "Topo Universe     = " . $topo_universe . "\n";
   print STDOUT "Topo Hierarchy    = " . $topo_hierarchy . "\n";
   print STDOUT "Create Mirrors    = " . $has_mirrors . "\n";
   print STDOUT "Project Data (Z9) = " . $project_z9 . "\n";
   print STDOUT "Release           = " . $release . "\n";
   print STDOUT "Schema            = " . $user . "\n";

   my $face_tab_name = $topo_in . '_' . $face_tab_ext;
   my $tracking = $topo_in . '_FSL_TRACKING';

   # Set Action Info for the DBAs

   $sql = "BEGIN " .
          "MT_UTIL_CONN_ID.SET_MODULE('GZ_FSL-ST".
          $state . "-" . $topo_in .  "-' || SYSDATE, 'Starting!'); " .
          "END;";
   $sth = $dbh->prepare($sql);
   $sth->execute;
   $sth->finish();

   # Create Log Table if it does not exist.

   $sql = "SELECT count(*) FROM user_tables WHERE table_name = :1";
   $sth = $dbh->prepare($sql);
   $sth->bind_param( 1, $tracking);
   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();

   if ($chk == 0) {

      # create the log table if it does not exist...

      $sql = "BEGIN " . 
             "GZ_FSL.CREATE_FSL_TRACKING('$user', '$tracking'); ".
             "END;";

      $sth = $dbh->prepare($sql);
      $sth->execute;
      $sth->finish();

   }
   
   # Check for Connect By Loop Error AND fix it if it exists...

   # log step...
   $sql = "BEGIN " . 
          " DBMS_APPLICATION_INFO.SET_ACTION('Start Connect By Loop Workaround'); ".
          "GZ_UTILITIES.GEN_FSL_TRACKING_LOG('$topo_in','BUILD_FSLS',p_message=>" . 
          "'Begin Check for Connect By Loop Error and Repair'); ". 
          "END;";
   $sth = $dbh->prepare($sql);
   $sth->execute;
   $sth->finish();
          

   $sql = "BEGIN ".
          " DBMS_APPLICATION_INFO.SET_ACTION('Connect By Loop Workaround'); ".
          "GZ_FSL.FIX_ALL_CONNECT_BY_ERRORS('$topo_in','$face_tab_name','FACE_ID','TOPOGEOM'); ".
          "END;";

   @lines = [];
   $sth = $dbh->prepare($sql);
   $sth->execute;
   @lines = $dbh->func('dbms_output_get');
   $sth->finish();

   &print_dbms_out(@lines);

   # Double check to make sure it is fixed...

   $chk = 0;

   $sql = 'SELECT count(*) from ' . $topo_in . '_relation$ '.
          'where tg_layer_id = topo_id and tg_id = topo_type';
             
   print STDOUT "$msg SQL = $sql.\n";
   $sth = $dbh->prepare($sql);
   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();

   if ($chk == 0) {
      print STDOUT "$msg Connect By Loop Double Check Looks All Right.\n";
          # log step...
          $sql = "BEGIN " . 
          "GZ_UTILITIES.GEN_FSL_TRACKING_LOG('$topo_in','BUILD_FSLS',p_message=>" . 
          "'Connect By Loop Fix Completed Successfully'); ".
          "END;";
          $sth = $dbh->prepare($sql);
          $sth->execute;
          $sth->finish();
   } else {
      print STDOUT "$msg Connect By Loop Double Check FAILED.\n";
      $fsl_stat = 'BAD';
          # log step...
          $sql = "BEGIN " . 
          "GZ_UTILITIES.GEN_FSL_TRACKING_LOG('$topo_in','BUILD_FSLS',p_message=>" . 
          "'Connect By Loop Fix FAILED'); ".
          "END;";
          $sth = $dbh->prepare($sql);
          $sth->execute;
          $sth->finish();
      return $fsl_stat;
   }


   # if you actually have to fix something with Sreeni's CONNECT BY FIXER,
   # permissions get removed.  Re-grant them just in case... 

   print STDOUT "$msg granting permissions to $topo_in.\n";
   $sql = "BEGIN " .
          " DBMS_APPLICATION_INFO.SET_ACTION('Finishing Connect By Loop Work Around'); ".
          " GZ_UTILITIES.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS','$topo_in%'); " .
          "END; ";

   $sth = $dbh->prepare($sql);
   $sth->execute;
   $sth->finish();


   #####################  Start if it looks okay #######################

   $sql = "BEGIN " .
          " DBMS_APPLICATION_INFO.SET_ACTION('START Process_FSL'); ". 
          " DBMS_APPLICATION_INFO.SET_CLIENT_INFO('$state-$topo_in-'".
            "||SYSDATE); ".
          "GZ_TOPO_HELPER.PROCESS_FSL('" .
          $topo_in . "','" .
          $face_tab_ext. "','" .
          $topo_universe . "','" .
          $topo_hierarchy . "','" .
          $has_mirrors . "','" .
          $project_z9 . "','" .
          $release . "','" .
          $user . "'); " .
          "END;";

   print STDOUT "\n";
   print STDOUT "$msg SQL = " . $sql . "\n";


   my $logsql = "BEGIN " . 
          "GZ_UTILITIES.GEN_FSL_TRACKING_LOG('$topo_in','BUILD_FSLS',p_message=>" . 
          "'Running PROCESS_FSL'); ".
          "END;";
   $sth = $dbh->prepare($logsql);
   $sth->execute;
   $sth->finish();


   ###########################################################################
   # Run the FSL BUILD

   @lines = [];
   $sth = $dbh->prepare($sql);
   $sth->execute;
   @lines = $dbh->func('dbms_output_get');
   $sth->finish();

   &print_dbms_out(@lines);

   ###########################################################################

   # GRANTS AND CHECKS

   #GRANT SELECT TO PUBLIC on all the views here (user_views where view_name
   #like ...TOPO_FSL

   $sql = "BEGIN ".
          " DBMS_APPLICATION_INFO.SET_ACTION('Granting View Permissions'); ".
          " GZ_FSL.GRANT_FSL_VIEW_PERMISSIONS('$topo_in','PUBLIC'); ".
          " GZ_FSL.GRANT_FSL_TABLE_PERMISSIONS('$topo_in','PUBLIC'); ".
          "END;";

   print STDOUT "\n";
   print STDOUT "$msg Granting Permissions, SQL = " . $sql . "\n";

   
   $logsql = "BEGIN " . 
          "GZ_UTILITIES.GEN_FSL_TRACKING_LOG('$topo_in','BUILD_FSLS',p_message=>" . 
          "'Granting Permissions on FSL views and Tables', " . 
          "p_sqlstmt=>'GZ_FSL.GRANT_FSL_VIEW_PERMISSIONS(''$topo_in'',''PUBLIC''); ".
          "GZ_FSL.GRANT_FSL_TABLE_PERMISSIONS(''$topo_in'',''PUBLIC'');'); ".
          "END;";
   $sth = $dbh->prepare($logsql);
   $sth->execute;
   $sth->finish();
   
   @lines = [];
   $sth = $dbh->prepare($sql);
   $sth->execute;
   @lines = $dbh->func('dbms_output_get');
   $sth->finish();

   &print_dbms_out(@lines);
   
   ###########################################################################
   # Check Results
   
   # This runs all the checks, read the log table to discover if it passed or
   # failed...

   $msg = "CHECK_FSLS:";

   $sql = "BEGIN ".
          " DBMS_APPLICATION_INFO.SET_ACTION('FSL Quality Check'); ".
          "GZ_FSL.CHECK_FSLS('$user','$topo_in'); ".
          "END;";
   print STDOUT "\n";
   print STDOUT "$msg SQL = " . $sql . "\n";
   
   $logsql = "BEGIN " . 
          "GZ_UTILITIES.GEN_FSL_TRACKING_LOG('$topo_in','BUILD_FSLS',p_message=>" . 
          "'Begin Running Checks on FSL tables', " . 
          "p_sqlstmt=>'GZ_FSL.CHECK_FSLS(''$user'',''$topo_in'')'); ".
          "END;";
   $sth = $dbh->prepare($logsql);
   $sth->execute;
   $sth->finish();

   ###########################################################################
   # Run and then Read results of FSL CHECKS

   @lines = [];
   $sth = $dbh->prepare($sql);
   $sth->execute;
   @lines = $dbh->func('dbms_output_get');
   $sth->finish();
   
   &print_dbms_out(@lines);

   
   # i) check number of tables...

   $chk = 0;
   $sql = "SELECT COUNT(*) FROM " .$topo_in."_FSL_TRACKING ". 
          "WHERE step = :1 AND message = :2";

   $sth = $dbh->prepare($sql);
   $sth->bind_param( 1, 'Check 1');
   $sth->bind_param( 2, 'PASSED');
   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();

   if ($chk == 1 )  {
      print STDOUT "$msg Passed the table count check.\n"; 
      # fsl_stat stays 'GOOD'
   } else {
      print STDOUT "$msg Failed the table count check.\n"; 
      $fsl_stat = 'BAD';
   }

    
   # check for bad gtypes

   $chk = 0;   
   $sql = "SELECT COUNT(*) FROM " .$topo_in."_FSL_TRACKING ". 
          "WHERE process = :2 AND message = :1";
   
   $sth = $dbh->prepare($sql);
   $sth->bind_param( 1, 'Check 2');
   $sth->bind_param( 2, 'FAILED');
   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();
 
   if ($chk) {
         print STDOUT "$msg Failed the GTYPE check. ($chk tables are bad)\n";
         $fsl_stat = 'BAD';
   } else {
         print STDOUT "$msg Passed the GTYPE check.\n";
   }

   # check for validate with context errors

   $chk = 0;
   $sql = "SELECT COUNT(*) FROM " .$topo_in."_FSL_TRACKING ". 
          "WHERE step = :1 AND message = :2";
      
   $sth = $dbh->prepare($sql);
   $sth->bind_param( 1, 'Check 3');
   $sth->bind_param( 2, 'FAILED');
   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();
 
   if ($chk) { 
      print STDOUT "$msg Failed the Validate with Context check. ($chk tables are bad)\n"; 
      $fsl_stat = 'BAD'; 
   } else { 
      print STDOUT "$msg Passed the Validate with Context check.\n"; 
   } 
      
   #######################################################################
   # validate the topoliogy

   # check for validate PASSED line

   $chk = 0;
   $sql = "SELECT COUNT(*) FROM " .$topo_in."_FSL_TRACKING ". 
          "WHERE step = :1 AND message = :2";
      
   $sth = $dbh->prepare($sql);
   $sth->bind_param( 1, 'Check 4');
   $sth->bind_param( 2, 'PASSED');

   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();
 
   if ($chk) { 
      print STDOUT "$msg Passed the Sreeni Style Validate Feature Tables check.\n"; 
   } else { 
      print STDOUT "$msg Failed the Sreeni Style Validate Feature Tables check.\n"; 
      $fsl_stat = 'BAD'; 
   } 
      
   #################################
   # use Oracle's Validator
   
   # check for validate PASSED line

   $chk = 0;
   $sql = "SELECT COUNT(*) FROM " .$topo_in."_FSL_TRACKING ". 
          "WHERE step = :1 AND message = :2";
      
   $sth = $dbh->prepare($sql);
   $sth->bind_param( 1, 'Check 5');
   $sth->bind_param( 2, 'PASSED');

   $sth->execute;
   $sth->bind_columns(\($chk));
   $sth->fetch();
   $sth->finish();
 
   if ($chk) { 
      print STDOUT "$msg Passed the Oracle Style Validate Feature Tables check.\n"; 
   } else { 
      print STDOUT "$msg Failed the Oracle Style Validate Feature Tables check.\n"; 
      $fsl_stat = 'BAD'; 
   } 
      
   print "...............................................\n";

   return $fsl_stat;

# end build fsl subroutine
}


##############################################################################
# COPIED FROM CAMPS
##############################################################################

=head2 get_dbh

Use this to create a database handle using a wallet or password.

=cut

sub get_dbh {
  my $connection_str = shift;
  my $dbh;

  my $def_attrs = {
                  PrintError    => 0, # warns
                  RaiseError    => 1, # dies
                  AutoCommit    => 0,
                  ora_check_sql => 1,
                  LongReadLen   => 1342177280, # < 16 mgb is magic; was=1342177280 
                  # LongTruncOk   => 1, # truncates LONGs to avoid ORA-01062 (NOT)
                  RowCacheSize  => 16
                  };

  if ((scalar(@{$connection_str}) == 3)
      || (scalar(@{$connection_str}) == 4)) {
    # OPEN AN ORACLE SESSION THE OLD FASHIONED WAY
    my $db_connection = "dbi:Oracle:" . $connection_str->[0];
    my $username = $connection_str->[1];
    my $passwd = $connection_str->[2]; ### NOT FOR PRODUCTION? -- hide as a ref!
    if (ref($passwd) eq "SCALAR" ) {
       $passwd = ${$passwd}; ### DEREFERENCE THE PASSWORD HERE BEFORE SENDING
    }

    my $attrs = (ref($connection_str->[3]) eq "HASH" )
                ? $connection_str->[3]
                : $def_attrs;

    $dbh = DBI->connect(
       $db_connection,
       $username,
       $passwd,
       $attrs
    ) || die "Database connection not made: " . $DBI::errstr;

  } elsif ((scalar(@{$connection_str}) == 1)
      || (scalar(@{$connection_str}) == 2)) {
    # OPEN AN ORACLE SESSION WITH A FANCY NEW WALLET
    my $db_connection = "dbi:Oracle:";
    my $wallet = $connection_str->[0];

    my $attrs = (ref($connection_str->[1]) eq "HASH" )
                ? $connection_str->[1]
                : $def_attrs ;

    $dbh = DBI->connect(
       $db_connection,
       $wallet,
       '',
       $attrs
    ) || die "Database connection not made: " . $DBI::errstr;
  } else {
    die "Incorrect number of parameters for perl DBI.\n"
        . "  Try: \$database, \$username, \\\$passwordref\n"
        . "  Or:  /\@<\$wallet>\n";
  }

  return $dbh;
# end get_dbh
}

############  END OF PROGRAM #############################
