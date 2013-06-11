#!/usr/bin/perl
# ------------------------------------------------------------------------------
# Title:       gz_shp_metadata.pl
#
# Author:      Suzanne McArdle
#
# Date:        April 16, 2012
#
# Description: Creates gz_shp_metadata table and performs appropriate clean-up, if necessary. Called from run_gz.ksh
#
# Dependencies:	
#
# Usage:       gz_shp_metadata.pl 
#
# 		-j jobid             
#
# ------------------------------------------------------------------------------
# Modification History:
#    4/16/2012 - Suzanne
#		- Does a metadata table already exist?
#		- If it does exist, has this job run already?
#		- If job has already run, let's clean up shapefiles and metadata
#
# ------------------------------------------------------------------------------
#

use strict;
use warnings;
use DBI;

my($jobid,$schema,$password,$connection,$sql,$sth,$wallet,$dbh,$database,$output_dir,$logfile,$meta_sth,$meta_sql_2);


use Getopt::Long qw(:config no_ignore_case bundling);

my (
  $opt_jobid,
  $opt_database,
  $opt_schema,
  $opt_password,
  $opt_logfile,
  $meta_log_clean_path
);

print STDOUT "\n\n";
   print STDOUT "#" x 68, "\n\n";
   print STDOUT "\n  Welcome to GZ shapefile creation.\n";
   print STDOUT "\n  Usage... gz_shp_metadata.pl \n\n";
   print STDOUT "    -j \$jobid\n";
   print STDOUT "    -d \$database\n";
   print STDOUT "    -s \$schema\n";
   print STDOUT "    -p \$password\n";
   print STDOUT "    -l \$logfile_location\n\n";
   print STDOUT "#" x 68, "\n";
   
my $rc = GetOptions(
  'jobid|j=s'           => \$opt_jobid,
  'database|d=s'		=> \$opt_database,
  'schema|s=s'          => \$opt_schema,
  'password|p=s'        => \$opt_password,
  'logfile|l=s'         => \$opt_logfile
);

if ( defined($opt_jobid) ) {
 $jobid = $opt_jobid;
} else {
 die "\n-j there was a problem with jobid\n\n";
}

if ( defined($opt_database) ) {
 $database = $opt_database;
} else {
 $database = undef;
}

if ( defined($opt_schema) ) {
 $schema = $opt_schema;
} else {
 die "\n-s there was a problem with schema\n\n";
}

if ( defined($opt_logfile) ) {
 $logfile = $opt_logfile;
} else {
 die "\n-l there was a problem reading the fme logfile output location from run_gz.ksh\n\n";
}

if ( defined($opt_password) ) {
 $password = $opt_password;
} else {
 die "\n-p there was a problem with the password\n\n";
}


	if ($password =~ /\/@/) {
    print "We are using a wallet for checking metadata\n";
    $wallet = "w";
	$connection = [$password];
	eval {
		$dbh = get_dbh($connection);
	};

    if ($@) {
       print STDOUT "\n";
       print STDOUT "Wallet login failure\n";
       print STDOUT "Heres the error we caught:\n";
       print STDOUT "\n";
       print STDOUT $@;     
       ;
   	}
    	
   	} else {
	#if password does not start with '/@' use user auth as dbh
	print "We are using user authentication for checking metadata\n";
	$connection = [$database,$schema,$password];
	eval {
		$dbh = get_dbh($connection);
	};

    if ($@) {
       print STDOUT "\n";
       print STDOUT "User authentication login failure\n";
       print STDOUT "Heres the error we caught:\n";
       print STDOUT "\n";
       print STDOUT $@;     
       ;
   	   }
	}
  
#Does metadata table exist?  If not, create it.
#Check for existance of permanent metadata table (stolen from CAMPS update_metadata.pm)

  	my $system_metadata_exists;
  	my $system_metadata_table = "GZ_SHP_METADATA"; # Table name must be all caps!
  		$sql = "SELECT count(*) ";
     	$sql .= "FROM user_tables a ";
     	$sql .= "WHERE a.table_name = :p1";

  	print "SQL to see if metadata table exists: $sql\n";
    $sth = $dbh->prepare($sql);
    $sth->bind_param( 1, $system_metadata_table);
    $sth->execute;
	# gather the data into a Perl variable
	$system_metadata_exists = $sth->fetchrow_array(  );
	$sth->finish;
   

# if PERMANENT METADATA TABLE does not exist create it

    unless ($system_metadata_exists) {
         $sql = "BEGIN ";
         $sql .= "GZ_METADATA.create_gz_shp_metadata_table(:p1);";
         $sql .= "END;";

         $sth = $dbh->prepare($sql);
         $sth->bind_param( 1, $schema);
         unless ($sth->execute) {
            warn"    UPDATING METADATA:  ************  ERROR ************
               Could not create permanent gz_shp_metadata table: $DBI::errstr\n";
            return "fail";
         }

         $dbh->commit;
         $sth->finish;
   } else {   
   
   #Query metadata to determine if this jobid has run before!
	$sql = "SELECT shp_name,shp_type,output_dir,logfile ";
    $sql .= "FROM gz_shp_metadata a ";
    $sql .= "WHERE a.jobid = :p1";

  	$sth = $dbh->prepare($sql);
    $sth->bind_param( 1, $jobid);
    $sth->execute;
	$sth->bind_columns(\my ($meta_shp_name,
							$meta_shp_type,
							$meta_output_dir,
							$meta_logfile));
	
    while($sth->fetch()){
	    
    print "    \nDELETING EXISTING FILES FROM PREVIOUS $jobid JOB RUN\n";
    my $shp_to_delete = $meta_output_dir.'/'.$meta_shp_name.'.*';
    print "remove shapefiles - $shp_to_delete\n";
    print "remove logfiles - $meta_logfile\n";
    system qq(rm $shp_to_delete);
    system qq(rm $meta_logfile);
							   			
    print "     \nREMOVING METADATA RECORDS FROM PREVIOUS $jobid JOB RUN\n";

   	# Delete records in gz_shp_metadata

     $meta_sql_2 = "DELETE ";
     $meta_sql_2 .= "FROM $system_metadata_table a ";
     $meta_sql_2 .= "WHERE a.jobid = '".$jobid."' ";
     $meta_sql_2 .= "AND a.shp_name = '".$meta_shp_name."' ";
     $meta_sql_2 .= "AND a.shp_type = '".$meta_shp_type."'";

     $dbh->do($meta_sql_2);
	 $dbh->commit;
     
  }
  
  #clean up any fme logs that may remain from this particular jobid in case we aren't remaking a shapefile with a given rerun.
  $jobid = lc($jobid);
  my $other_logs = $logfile.'/'.$jobid.'*';
  system qq(rm $other_logs);

  $sth->finish;
  
  }

###############################################################################################
#---------------------------------------------------------------------------------------------#
###############################################################################################

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

    $dbh->disconnect;
	exit 0;