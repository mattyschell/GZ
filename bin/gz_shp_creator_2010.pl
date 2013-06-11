#!/usr/bin/perl
# ------------------------------------------------------------------------------
# Title:       gz_shp_creator.pl
#
# Author:      Suzanne McArdle
#
# Date:        July 23, 2010
#
# Description: Creates polygon shapefiles from Oracle table based upon input input parameters and command
#				line inputs.  
#
# Dependencies:	Oracle table with desired input parameters (GZ_SHAPEFILE_PARAMETERS).  
#				Must modify script to allow this table to be stored in a schema separate from 
#				the source data for shapefile creation.  At the moment this table must exist in same schema
#				as entered at command line.
#
# Usage:       gz_shp_creator_2010.pl <database> <schema> <password> <year> 
#										 <generalization_level> <output>
#   -z <gz_release>				gz_release from gz_job_setup.release
#   -x <gen_project_id>         gen_project_id from gz_job_setup.gen_project_id
#   -a <fme_app_dir>           Path to fme application directory
#   -d <database> 				Name of database instance in which connection is needed. (i.e. DEVBENCH)
#   -s <schema>   				Name of the schema from which data is to be extracted; where the FSL views can be queried
#   -p <password> 				Schema password
#   -t <table_prefix>          Prefix to the type of table you are creating shapefiles from (FSL; Z6 for state), etc.
#	-y <year>					Generalization year (i.e. 2009 or 2010); necessary for filenaming
#	-n <gen_level>	            Level of generalization (i.e. z6); necessary for filenaming
#   -c <gencode>				The letter associated with level of generalization (i.e. g, p, d, etc.); necessary for filenaming
#   -j <projection>            Projection must be defined on the fly now (values can be any FME defined projection: SPHERICAL_MERCATOR, US-Low, LL-83)
#   -o <output>				Base output directory path (i.e. /mt/branch/cpmb/camps/render/prodbnch/yadda/yadda
#   -f <state_topo_flag>		Set to 'S' if you are running against state-based topologies; set to '0' if you are not
#   -e <delete_cpg_file_flag>  Set to 'cpg' if you would like to delete .cpg file as a part of the run; set to '0' if this is not the case; as of 7/6/11 this will always be 0 for production
#   -m <mapping file directory> Full path to directory where the mapping files are stored (if not stored with the perl script)
#   -l <log_output_directory>   Output directory path for FME logs
# ------------------------------------------------------------------------------
# Modification History:
#    12/2/2010 - Suzanne
#		- modified code to run for DEC10 project; geovariant code taken into account for filenaming
#		- calls to new projection file
#
#    12/3/2010 - Stephanie Added data directory as an optional last parameter
#
#	 10/2011 - Suzanne
#		- make output directories	
#		- FME wallet option; get_dbh code copied and pasted from RunCamps.pm - cool!
# 
#    11/04/2011: Stephanie added force to lowercase on filenames in two places.
#
#     3/14/2012 - Suzanne 
#        - Added fme application directory as a parameter 
#        - Added automation to find lib/shpgen.pm - GZ code's own copy of SPSB's
#
#     3/28/2012 - Suzanne
#        - Go back to SPSB version of shpgen
#        - Change fme application directory to an optional parameter
#        - Add parameter for fme logfile output
#        - Enhanced command line call with GetOptions
#
#     4/10/2012 - Suzanne
#		 - Complete rewrite to better control how shapfiles are made
#        - Will only create requested shapes (GZ_SHAPEFILE_SETUP.SHP_UNIT_REQUEST controls whether to make national set,
#			state-based set or both)
#
#	  5/15/2012 - Suzanne
#		- Added release and changed gz_shapefile_parameters table structure
#
#     7/2012 - Matt
#              Added umask stuff
#
#	  8/17/2012 - Stephanie
#		- Fixed features read vs features written comparison
#       - Prevent OGC validation running if no shapes were written
#
#     6/6/2013 - Matt!
#        Double quotes around password for special characters
#
#
# ------------------------------------------------------------------------------
# 

use strict;
use warnings;
use DBI;
use Carp;
use File::Basename;
use File::Copy;
use File::Path;
use DBD::Oracle;
#use FindBin qw($Bin);
use POSIX qw(locale_h);
use Hash::Util qw(lock_keys);

#use FindBin to find our current bin directory and move to our own copy of lib in gz code; moved this down into the code
use lib "/mt/apps/shpgen/lib";
#use lib "$Bin/../lib";
use shpgen;

my $argc;
my $i = 0;
my $startTime = time;
my($sql,$sql2,$sth,$sth2,$wallet,$connection,$dbh,$msg);
my($runfme,$syscmd,$fmeecho,$logfile);
my($database,$schema,$password,$table_prefix,$year,$gen_level, $gencode,$projection,$output);
my($jobid,$shp_output_dir,$state_topo,$cpg_del_flag,$data_dir,$log_output_dir,$shp_unit_request,$gz_release,$gen_project_id);
#my ($sum_lev,$polygon,$line,$drop_outline_flag,$nation,$state,$gvcode);
my ($line_mapping_file,$topology,$table_name,$shapename,$shp_class,$shp_type,$source_shp,$state_code,$del_flag);
my ($whereclause,$shp_request_nation,$shp_request_state,$metadata_dbh,$meta_sql,$feature_count_test,$fme_result,$unit);
my @topo_state_list;
my $mode = 0775;
my ($writtencount, $readcount,$ogc_valid,$ogc_logfile);
	

################# SET ENVIRONMENTALS AND GET OPTIONS HERE ####################
##############################################################################
use Getopt::Long qw(:config no_ignore_case bundling);

my (
  $opt_help,
  $opt_gz_release,
  $opt_gen_project_id,
  $opt_fme_app_dir,
  $opt_shp_type,
  $opt_jobid,
  $opt_database,
  $opt_schema,
  $opt_password,
  $opt_table_prefix,
  $opt_year,
  $opt_gen_level,
  $opt_gencode,
  $opt_projection,
  $opt_output,
  $opt_state_topo_flag,
  $opt_delete_cpg_file,
  $opt_mapping_file_dir,
  $opt_log_output_dir,
  $opt_shp_unit_request
);

   print STDOUT "\n\n";
   print STDOUT "#" x 68, "\n\n";
   print STDOUT "\n  Welcome to GZ shapefile creation.\n";
   print STDOUT "\n  Usage... gz_shp_creator_2010.pl \n\n";
   print STDOUT "    -z \$gz_release\n";
   print STDOUT "    -x \$gen_project_id\n";
   print STDOUT "    -a \$fme_app_dir\n";
   print STDOUT "    -h \$shp_type\n";
   print STDOUT "    -i \$jobid\n";
   print STDOUT "    -d \$database\n";
   print STDOUT "    -s \$schema\n";
   print STDOUT "    -p \$password\n"; 
   print STDOUT "    -t \$table_prefix\n";
   print STDOUT "    -y \$year\n";
   print STDOUT "    -n \$gen_level\n";
   print STDOUT "    -c \$gencode\n";
   print STDOUT "    -j \$projection\n";
   print STDOUT "    -o \$output\n";
   print STDOUT "    -f \$state_topo_flag\n";
   print STDOUT "    -e \$delete_cpg_file\n";
   print STDOUT "    -m \$mapping_file_dir\n";
   print STDOUT "    -l \$log_output_dir\n";
   print STDOUT "    -r \$shp_unit_request\n\n";
   print STDOUT "#" x 68, "\n";
   #die "\n\n";
   #chomp(my $gz_shp_creator_paras = <STDIN>);      # read one line from standard input
   #@ARGV = split /[ \t]+/, $gz_shp_creator_paras;  # parse and store in @ARGV
   #print "\n";


my $rc = GetOptions(
  'help|?'                    => \$opt_help,
  'gz_release|z=s'			  => \$opt_gz_release,
  'gen_project_id|x=s'        => \$opt_gen_project_id,
  'fme_app_dir|a=s'           => \$opt_fme_app_dir,
  'jobid|i=s'           	  => \$opt_jobid,
  'shp_type|h=s'           	  => \$opt_shp_type,
  'database|d=s'              => \$opt_database,
  'schema|s=s'			      => \$opt_schema,
  'password|p=s'              => \$opt_password,
  'table_prefix|t=s'          => \$opt_table_prefix,  
  'year|y=s'                  => \$opt_year,
  'gen_level|n=s'             => \$opt_gen_level,
  'gencode|c=s'               => \$opt_gencode,
  'projection|j=s'            => \$opt_projection,
  'output|o=s'                => \$opt_output,
  'state_topo_flag|f=s'       => \$opt_state_topo_flag,
  'delete_cpg_file|e=s'       => \$opt_delete_cpg_file,
  'mapping_file_dir|m=s'      => \$opt_mapping_file_dir,
  'log_output_dir|l=s'        => \$opt_log_output_dir,
  'shp_unit_request|r=s'      => \$opt_shp_unit_request
);

if ( defined($opt_gz_release) ) {
 $gz_release = $opt_gz_release;
} else {
 die "\n-z release is a required parameter\n\n";
}

if ( defined($opt_gen_project_id) ) {
 $gen_project_id = $opt_gen_project_id;
} else {
 die "\n-x gen_project_id is required\n\n";
}

if ( defined($opt_fme_app_dir) ) {
 $runfme = "$opt_fme_app_dir/runfme";
} else {
 $runfme = "runfme";
}

if ( defined($opt_shp_type) ) {
 $shp_type = $opt_shp_type;
 	if ($shp_type eq 'PRD') {
 		$del_flag = 'Y'
		} else {
		$del_flag = 'N';
	}
} else {
 die "\n-h there was a problem gathering shp type\n\n";
}

if ( defined($opt_jobid) ) {
	$jobid = $opt_jobid;
	#$jobid =~ /\{(.*?)\}/;
	#$jobid = $1; 
} else {
 die "\n-i there was a problem gathering jobid\n\n";
}

if ( defined($opt_database) ) {
 $database = $opt_database;
} else {
 die "\n-d database is a required parameter\n\n";
}

if ( defined($opt_schema) ) {
 $schema = $opt_schema;
} else {
 die "\n-s schema is a required parameter\n\n";
}

if ( defined($opt_password) ) {
 $password = $opt_password;
} else {
 die "\n-p password is a required parameter\n\n";
}

if ( defined($opt_table_prefix) ) {
 $table_prefix = $opt_table_prefix;
} else {
 die "\n-t FSL table prefix is a required parameter\n\n";
}

if ( defined($opt_year) ) {
 $year = $opt_year;
} else {
 die "\n-y year is a required parameter\n\n";
}

if ( defined($opt_gen_level) ) {
 $gen_level = $opt_gen_level;
} else {
 die "\n-n gz level is a required parameter\n\n";
}

if ( defined($opt_gencode) ) {
 $gencode = $opt_gencode;
} else {
 die "\n-c gencode is a required parameter\n\n";
}

if ( defined($opt_projection) ) {
 $projection = $opt_projection;
} else {
 die "\n-j projection is a required parameter\n\n";
}

if ( defined($opt_output) ) {
 $output = $opt_output;
} else {
 die "\n-o output directory is a required parameter\n\n";
}

if ( defined($opt_state_topo_flag) ) {
 $state_topo = $opt_state_topo_flag;
} else {
 die "\n-f state topo list flag is a required parameter\n\n";
}

if ( defined($opt_delete_cpg_file) ) {
 $cpg_del_flag = $opt_delete_cpg_file;
} else {
 die "\n-e delete cpg file flag is a required parameter\n\n";
}

if ( defined($opt_mapping_file_dir) ) {
 $data_dir = $opt_mapping_file_dir;
} else {
 die "\n-m mapping file directory is a required parameter\n\n";
}

if ( defined($opt_log_output_dir) ) {
 $log_output_dir = $opt_log_output_dir;
} else {
 die "\n-l log output directory is a required parameter\n\n";
}

if ( defined($opt_shp_unit_request) ) {
 $shp_unit_request = uc($opt_shp_unit_request);
} else {
 die "\n-r type of shapefiles to be requested is required (NATION,STATE,BOTH)\n\n";
}

# ------------------------------------------------------------------------------
# Get Options and Arguments
# ------------------------------------------------------------------------------

#$argc = @ARGV;
#if ($argc < 14)
#{ usage();
#  die "All arguments are required.\n";
#}

#if ($argc > 14)
#{ usage();
#  die "Too many arguments passed.\n";
#}

#foreach my $parm (@ARGV)
#{
#	print "input parameter ($i): ",$ARGV[$i]."\n";
#	$i++;
#}

#sub usage {	
#	print "Usage: perl gz_shp_creator_2010.pl <fme_app_dir> <database> <schema> <table_prefix> <password> <year> <gen_level> <gencode> <output> <state_topo> <delete_cpg_file_flag> <mapping_file_dir> <log_output_directory>\n";
#}

#if ($argc == 14) 
#	($fme_app_dir,$database,$schema,$password,$table_prefix,$year,$gen_level,$gencode,$projection,$output,$state_topo,$cpg_del_flag,$data_dir) = @ARGV;

	#Implement calls to get_dbh copied into here from runcamps.pm
	#if password starts with /@ then use wallet as dbh
		
	if ($password =~ /\/@/) {
    print "We are using a wallet for shapefile creation\n";
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
	$wallet = "";
	print "We are using user authentication for shapefile creation\n";
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
	
   #umask! 7/23/12
	#set in case the person running GZ is locked down
	#seems that umask and mkpath need to match
	umask 0002;
	
	#Attempt at the correct way of creating directories
    eval { mkpath($output, 1, 0775) unless (-d $output) };
      if ($@) {
        die "Couldn't create $output: $@";
      }
    #chmod $mode, $output; # because mkpath won't
    
    #Create log directory
    eval { mkpath($log_output_dir, 1, 0775) unless (-d $log_output_dir) };
      if ($@) {
        die "Couldn't create $log_output_dir: $@";
      }
    #chmod $mode, $output; # because mkpath won't
    
      #system qq(test -d $output exist);
      #bootleg way of creating directory.. #wrong!
    
      #if (-d $output) {
      #   print "\nOutput directory: $output already exists\n";
      #} else {
      #   print "\nCreating output directory now: $output\n";
      #   system qq(mkdir $output)
      #}
       					
    if ($shp_unit_request eq 'NATION' ) {
    	print "We only requested national set of files for this job run.\n";
    	$shp_request_nation = 'Y';
    	$shp_request_state = 'N';
	} elsif ($shp_unit_request eq 'STATE') {
    	print "We only requested state-based files for this job run.\n";
		$shp_request_nation = 'N';
    	$shp_request_state = 'Y';
	} elsif ($shp_unit_request eq 'BOTH') {
    	print "We requested a national and state-based set of files for this job run.\n";
		$shp_request_nation = 'Y';
    	$shp_request_state = 'Y';
	};
      
    #query oracle table set up by user to determine files that need to be created
    $sql ="SELECT SUM_LEV, POLYGON, LINE, DROP_OUTLINE, NATION, FANOUT, GV_CODE, UNIT_FIELD_NAME, UNIT_DIR_PREFIX, MODTAG, WAVE ";
	$sql.="FROM GZ_SHAPEFILE_PARAMETERS ";
	$sql.="WHERE GEN_PROJECT_ID = :p1 and RELEASE = :p2 ";

	$sth = $dbh->prepare($sql);
	$sth->bind_param(1, $gen_project_id);
	$sth->bind_param(2, $gz_release);
	$sth->execute;
	$sth->bind_columns(\my ($sum_lev,
							$polygon,
							$line,
							$drop_outline_flag,
							$nation,
							$fanout,
							$gvcode,
							$unit_field_name,
							$unit_dir_prefix,
							$modtag,
							$wave));
							   			
	    my $ref = $sth->fetchrow_hashref();
	    if ($ref) {
		  while ($ref) {
		  #while($sth->fetch())
          # rows returned
          #proceed with requested transactions for each record in the GZ_SHAPEFILE_PARAMETERS table as we loop through
          
          if ( defined($unit_field_name) ) {
 			print "unit_field_name is defined: $unit_field_name\n";
		  } else {
 			$unit_field_name = "";
	  	  }
	  	  
	  	  if ( defined($unit_dir_prefix) ) {
 			print "unit_dir_prefix is defined: $unit_dir_prefix\n";
		  } else {
 			$unit_dir_prefix = "";
	  	  }
          
	  	  if ( defined($wave) ) {
 			print "unit_dir_prefix is defined: $wave\n";
		  } else {
 			$wave = "";
	  	  }
	  	  
          #if ($unit_field_name eq undef) {
		  #	    print "unit_field_name is not defined: $unit_field_name\n";
		  #} else {
		  # #if password does not start with '/@' use user auth as dbh
		   #print "apparently unit_field_name is defined: $unit_field_name\n";
		  #};
	
		print "\n";
	    print "===================================================================================\n";
		print "QUERYING GZ_SHAPEFILE_PARAMETERS TABLE TO CREATE DESIRED SHAPEFILES FOR SUMMARY LEVEL $sum_lev\n";
		print "polygon 	    		$polygon\n";
		print "line 				$line\n";
		print "nation 				$nation\n";
		print "fanout 				$fanout\n";
		print "gv_code 				$gvcode\n";
		print "unit_field_name 		$unit_field_name\n";
		print "unit_dir_prefix		$unit_dir_prefix\n";
	    print "modtag				$modtag\n";
		print "wave					$wave\n";
				
		#Setting the gencode is obsolete as of 7/6/11 -- we will be passing this into the program from the command line
		#set generalization code level (d, g, or p) based on input zX level
		#both values necessary for proper output filenames
		#if ($gen_level eq 'z1' or $gen_level eq 'z1_p') {
		#	$gencode = 'd';
		#} elsif ($gen_level eq 'z6') {
		#	$gencode = 'g';
		#} elsif ($gen_level eq 'z8') {
		#	$gencode = 'g';
		#} elsif ($gen_level eq 'z9') {
		#	$gencode = 'p';
		#}
		
		#build appropriate table names here based on if its a state-based topology or not
		
		#print "------------\n";
	    #print "generalization level = $gen_level\n";
	    #print "gencode = $gencode\n";
			
    
    #CREATION OF NATIONAL FILES FOR z6 or z8   
    
   
   	if ($polygon eq 'Y') {
	   	
	   	$shp_class = 'py';
	   	$feature_count_test = 'Y';

		$table_name = $table_prefix.$sum_lev."V";
	   	print "CURRENT SHAPEFILE SOURCE TABLENAME: $table_name\n";
	   	
	   	#Address differences between national and state-based files (output and shapefile name)
	   	#Add optional --whereclause parameter to universal_py_shapefile_creator.fme mapping file and only set if it is creating state-based files
	   	if (($nation eq 'Y') and ($shp_request_nation eq 'Y')) {
		   	print "\n\n******Creating nation-based polygon shapefile for Summary Level $sum_lev*****\n";
		   	$shp_output_dir = $output."/nation";
		   	$unit = 'nation';
		   	$shapename = "geo_".$year."_".$gencode."_00000_".$sum_lev."_".$gvcode."_".$shp_class."_".$gen_level;
	        ## Stephanie added the next line to always force lowercase the file name
	        $shapename = lc($shapename);
	        print "RESULTING SHAPEFILE WILL BE: $shapename\n";
	        
	        $logfile = lc($jobid.'_'.$shapename.'_'.$shp_type.'.log');
		
			$syscmd  = "$runfme $data_dir"."universal_py_shapefile_creator.fme";		
			
			if ($wallet eq "w") {
				$syscmd .= " --SourceDataset $password";
			} else {
				$syscmd .= " --SourceDataset $database";
				$syscmd .= " --USER_NAME $schema --PASSWORD $password";
			}
			
			$syscmd .= " --SchemaName $schema";
			$syscmd .= " --TableName $table_name";
		    	    	    
		    
			#new parameters - build shapefile name before it enters mapping file
			$syscmd .= " --ShapeName $shapename";
			$syscmd .= " --Projection $projection";
			$syscmd .= " --DestDataset $shp_output_dir";
	
			$syscmd .= " --LOG_FILE $log_output_dir/$logfile";

			$logfile = $log_output_dir.'/'.$logfile;
	
			#print "-------------------------------------\n";
			print "\n";
			print "CALL TO FME:\n";
			print "$syscmd\n";
			print "\n";
			#print "LogFile value: $logfile\n";
			#print "-------------------------------------\n";
			
			if (-e $shp_output_dir."/$shapename.shp") {
				
				 die "\n$shp_output_dir/$shapename.shp ALREADY EXISTS! PLEASE MAKE SURE YOU ARE DIRECTING YOUR SHAPEFILE OUTPUT TO THE CORRECT PLACE! \n\n";
		
			} else {
			
			eval {
				&runFMEandMetadata($syscmd,$fmeecho,$jobid,$shp_type,$shp_output_dir,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$feature_count_test,$unit,$modtag,$wave);
			};
			
					#added! 7/24/12
					#FME seems to be creating the nation dir with more restricted permissions if user umask is restricted
					#print STDOUT "HERE HERE HERE \n\n\n";
					#print $shp_output_dir."\n";
					#print STDOUT "HERE HERE HERE \n\n\n";
					chmod 0775, $shp_output_dir; 
					
			}
				  			
		 } 
		 
		 if (($fanout eq 'Y') and ($shp_request_state eq 'Y')) {
		    print "\n\n******Creating state-based polygon shapefile for Summary Level $sum_lev*****\n";
	    
		    $sql2 ="SELECT DISTINCT($unit_field_name) ";
			$sql2.="FROM $table_name ";
			$sql2.="ORDER BY $unit_field_name asc";

			$sth2 = $dbh->prepare($sql2);
	 		#print "$sql2\n";
			$sth2->execute;
			$sth2->bind_columns(\my ($state_code));
			 
			while($sth2->fetch()){
				 
			#print "\nCurrent state code is: $state_code\n";
		 
			$shp_output_dir = $output."/".$unit_dir_prefix.$state_code;
			$unit = $unit_dir_prefix.$state_code;
		    #add optional whereclause to fme call
		    $whereclause = '--ORACLE8I_IN_WHERE_CLAUSE_ORACLE8I_1 "'.$unit_field_name.' = '.$state_code.'"';
		     
		    $shapename = "geo_".$year."_".$gencode."_".$state_code."000_".$sum_lev."_".$gvcode."_".$shp_class."_".$gen_level;
	        ## Stephanie added the next line to always force lowercase the file name
	        $shapename = lc($shapename);
	        print "RESULTING SHAPEFILE WILL BE: $shapename\n";
	        
	        $logfile = lc($jobid.'_'.$shapename.'_'.$shp_type.'.log');
		
			$syscmd  = "$runfme $data_dir"."universal_py_shapefile_creator.fme";		
			
			if ( $wallet eq "w" ) {
				$syscmd .= " --SourceDataset $password";
			} else {
				$syscmd .= " --SourceDataset $database";
				$syscmd .= " --USER_NAME $schema --PASSWORD \"$password\"";
			}
			
			$syscmd .= " --SchemaName $schema";
			$syscmd .= " --TableName $table_name";
		    	    	    
			#new parameters - build shapefile name before it enters mapping file
			$syscmd .= " --ShapeName $shapename";
			$syscmd .= " --Projection $projection";
			$syscmd .= " --DestDataset $shp_output_dir";
	
			$syscmd .= " --LOG_FILE $log_output_dir/$logfile";
			
			if ( defined($whereclause) ) {
	 			$syscmd .= " $whereclause";
			}
			
			$logfile = $log_output_dir.'/'.$logfile;
	
			#print "-------------------------------------\n";
			print "\n";
			print "CALL TO FME:\n";
			print "$syscmd\n";
			print "\n";
			#print "LogFile value: $logfile\n";
			#print "-------------------------------------\n";
					
			if (-e $shp_output_dir."/$shapename.shp") {
				
				 die "\n$shp_output_dir/$shapename.shp ALREADY EXISTS! PLEASE MAKE SURE YOU ARE DIRECTING YOUR SHAPEFILE OUTPUT TO THE CORRECT PLACE! \n\n";
		
			} else {
			
				eval {
				&runFMEandMetadata($syscmd,$fmeecho,$jobid,$shp_type,$shp_output_dir,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$feature_count_test,$unit,$modtag,$unit);
				};
			
			}
		
			#added! 8/22/12
     		#FME seems to be creating the state-based dirs with more restricted permissions if user umask is restricted
     		#This is redundant all the time, should be cleaned up with a check on permissions first. In a hurry today...
		    chmod 0775, $shp_output_dir;
			
	}

		
	    #Reset the value of output before it gets reassigned upon next loop through		
		$shp_output_dir = undef;
		$whereclause = undef;
		$syscmd = undef;
		$feature_count_test = undef;
		
	}

		}
		
		#UPDATE METADATA FOR THE FILE WE JUST CREATED?
	
		if (($line eq 'Y') and ($shp_request_nation eq 'Y')) {
			
			$unit = 'nation';
			$feature_count_test = 'N';			
			$shp_class = 'ln';
			$shp_output_dir = $output."/nation";
		
			if (-e $shp_output_dir."/$shapename.shp") {
				
				if ($drop_outline_flag eq 'Y') {
					$line_mapping_file = "nation_py2ln";
				} else {
					$line_mapping_file = "nation_py2ln_keep_outline";
				}					
			
				print "\n";
				print "Polygon shapefile exists.  Proceed to create line shapefile from existing polygon file.\n";
				print "\n";
		    	print "******Creating Nation-based line shapefile for Summary Level $sum_lev*****\n";
		    	
		    	$source_shp = $shapename;
		    	$shapename = "geo_".$year."_".$gencode."_00000_".$sum_lev."_".$gvcode."_".$shp_class."_".$gen_level;
	            ## Stephanie added the next line to always force lowercase the file name
	            $shapename = lc($shapename);
		    	print "RESULTING SHAPEFILE WILL BE: $shapename\n";
			
				$logfile = lc($jobid.'_'.$shapename.'_'.$shp_type.'.log');
		
				$syscmd  = "$runfme $data_dir"."$line_mapping_file.fme";
				$syscmd .= " --SourceDataset $shp_output_dir/$source_shp.shp";
				$syscmd .= " --SourceFileOnly $source_shp";
				$syscmd .= " --SumLev $sum_lev";
				#new parameters - build shapefile name before it enters mapping file
				$syscmd .= " --ShapeName $shapename";
				$syscmd .= " --Projection $projection";
				$syscmd .= " --DestDataset $shp_output_dir";
		
				$syscmd .= " --LOG_FILE $log_output_dir/$logfile";
			
				$logfile = $log_output_dir.'/'.$logfile;
	
				#print "-------------------------------------\n";
				print "\n";
				print "CALL TO FME:\n";
				print "$syscmd\n";
				#print "LogFile value: $logfile\n";
				print "\n";
				#print "-------------------------------------\n";
	
				if (-e $shp_output_dir."/$shapename.shp") {
				
				 die "\n$shp_output_dir/$shapename.shp\nFILE ALREADY EXISTS! PLEASE MAKE SURE YOU ARE DIRECTING YOUR SHAPEFILE OUTPUT TO THE CORRECT PLACE! \n\n";
		
				} else {
			
					eval {
						&runFMEandMetadata($syscmd,$fmeecho,$jobid,$shp_type,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$feature_count_test,$unit,$modtag,$unit);
					};
					
					#added! 7/24/12
					#FME seems to be creating the nation dir with more restricted permissions if user umask is restricted
					#This is redundant all the time, should be cleaned up with a check on permissions first. In a hurry today...
					chmod 0775, $shp_output_dir; 
			
				}

		
		    } elsif (!-e $shp_output_dir."/$shapename.shp") {
			
		  		print "\n";
		  		print "$shp_output_dir/$shapename\n";
				print "WARNING!!! NO POLYGON SHAPEFILE EXISTS.  EVEN THOUGH A LINE SHP WAS REQUESTED, WE ARE SKIPPING LINE CREATION!!!!!!";
				exit 1;
				
			}
			
	  }

	  $ref = $sth->fetchrow_hashref();
  } 
  
} else {
	  
	  die "********GZ CRASH**************\nNo rows selected from gz_shapefile_parameters.  Make sure that you have records that match the gen_project_id of the job you are running.\n";
  }
		  
###############################################################################################
#---------------------------------------------------------------------------------------------#
###############################################################################################	

sub runFMEandMetadata {

		eval {
			&run_fme($syscmd, $fmeecho, $logfile)
		};
	
		if ($@) {
			$fme_result = $@;
			eval {
				&updateMetadata($jobid,$shp_type,$shp_output_dir,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$fme_result,$unit,$modtag,$unit)
			};
			print $@;	
			exit 1;
		} else {
			print "Ran run_fme successfully.\n";
		}

		if ($feature_count_test eq 'Y') {
		
			eval {
				$readcount = &getReadCount($logfile);
				$writtencount = &getWrittenFeatCount($logfile);
			};
			
			if ($readcount != $writtencount){
				print "Feature count read and feature count written DO NOT MATCH.\n";
				print "There was a problem while creating shapes.\n";
				print $@;
				exit 1;
			} else {
				#print "Read count: $readcount\n";
				#print "Written count: $writtencount\n";
				$fme_result = 'PASS';
				print "Feature count read and feature count written match.\n";
			}
		
		} else {
			
			print "No feature count test was run because this is a line file.\n";
		
		}
		
		#if delete .cpg file is set, delete it! especially before calculating file size for metadata
		if ($cpg_del_flag eq 'cpg') {
		system qq(rm -R $shp_output_dir/*.cpg); 
		}
		
		#call to OGCValidation test here so that we have result for updating metadata once!
		#Send SourceDataset, FEATURE_TYPES (and DestDataset?), LOGFILE
		#$OGCsource = $shp_output_dir.'/'$shapename.'.shp';
		#Don't run if features written = 0 (there won't be a shpaefile)
		if ($writtencount) {
		     eval {
			     $ogc_valid = &runOGCvalidation($logfile);	
		     };
	    } else {
    	     $ogc_valid = 'NA';
	    }
		
		#if ($@) {
		#	print $@;
		#	#print "Check to make sure that there are not pre-existing metadata records for the job you are running\n";
		#	exit 1;
		#} else {
		#	print "$shapename passed OGC Validation check.\n";
		#}
		
		#call to updateMetadata here
		#eval {
		#	&updateMetadata($jobid,$shp_type,$shp_output_dir,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$fme_result,$ogc_valid)
		#};
		
		#if ($@) {
		#	print $@;
		#	#print "Check to make sure that there are not pre-existing metadata records for the job you are running\n";
		#	exit 1;
		#}
	
	
	}

###############################################################################################
#---------------------------------------------------------------------------------------------#
###############################################################################################	

sub getReadCount {

  my ($logfile) = @_;
  my ($count,$rec);

  if (!-e $logfile)
  {
    print STDERR "Error:  Could not find file $logfile\n";
    exit 1;
  }

  if (!open FILE, $logfile)
  {
    print STDERR "Error:  Could not open file $logfile.\n";
    exit 1;
  }

  while ($rec = <FILE>)
  {
    if ($rec =~ /^.+Total\sFeatures\sRead\s+(\d+)$/)
    {
      $count = $1;
    }
  }

  close FILE;

  return $count;
  
}

###############################################################################################
#---------------------------------------------------------------------------------------------#
###############################################################################################

sub getWrittenFeatCount {

  my ($logfile) = @_;
  my ($count,$rec);

  if (!-e $logfile)
  {
    print STDERR "Error:  Could not find file $logfile\n";
    exit 1;
  }

  if (!open FILE, $logfile)
  {
    print STDERR "Error:  Could not open file $logfile.\n";
    exit 1;
  }

  while ($rec = <FILE>)
  {
    if ($rec =~ /^.+Total\sFeatures\sWritten\s+(\d+)$/)
    {
      $count = $1;
    }
  }

  close FILE;

  return $count;
  
}

###############################################################################################
#---------------------------------------------------------------------------------------------#
###############################################################################################

sub runOGCvalidation {

	$ogc_logfile = lc($jobid.'_'.$shapename.'_'.$shp_type.'_ogc.log');
		
		$syscmd  = "$runfme $data_dir"."shp_ogc_validation_test.fme";
		$syscmd .= " --SourceDataset $shp_output_dir/$shapename.shp";
		$syscmd .= " --ShapeName $shapename";
		$syscmd .= " --LOG_FILE $log_output_dir/$ogc_logfile";
		
		$ogc_logfile = $log_output_dir.'/'.$ogc_logfile;

		#print "-------------------------------------\n";
		print "\n";
		print "CALL TO FME:\n";
		print "$syscmd\n";
		#print "LogFile value: $logfile\n";
		print "\n";
		#print "-------------------------------------\n";
		


		eval {
			&run_fme($syscmd, $fmeecho, $ogc_logfile)
		};
	
		if ($@) {
			$ogc_valid = $@;
			eval {
				&updateMetadata($jobid,$shp_type,$shp_output_dir,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$fme_result,$unit,$ogc_valid)
			};
			print $@;	
			exit 1;
		} else {
			print "Ran OGC validation call to FME successfully.\n";
		}
		
		#call the validator test using both GeometryOGCValidatorIsValid and GeometryOGCValidatorIsSimple as a transformer name!
		eval {
			$ogc_valid = &TransformerResults($ogc_logfile);
		};
	
		if ($@) {
			$ogc_valid = $@;
			eval {
				&updateMetadata($jobid,$shp_type,$shp_output_dir,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$fme_result,$unit,$ogc_valid)
			};
			print $@;	
			exit 1;
		} else {
			print "OGC Validation step ran successfully.\n";
			eval {
				&updateMetadata($jobid,$shp_type,$shp_output_dir,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$fme_result,$unit,$ogc_valid)
			};
			system qq(rm $ogc_logfile);
			#if successful run, remove ogc_validation test logfile immediately
		}
	
}

###############################################################################################
#---------------------------------------------------------------------------------------------#
###############################################################################################	

#Send in the name of the transformer and automatically parse through to make sure that the 
#number of features passed is a non-zero number and the number of features failed is 0; if not
#return 'FAIL' value and update metadata accordingly (or fail out before metadata so that
#metadata values are only 'PASS' or 'NOT CHECKED'
##GeometryOGCValidator(TestFactory): Tested 257 input feature(s) -- 256 feature(s) passed and 1 feature(s) failed
sub TransformerResults {
	
  my ($number_features_tested,$features_passed,$features_failed,$rec,$result);

  if (!-e $ogc_logfile)
  {
    print STDERR "Error:  Could not find file $ogc_logfile\n";
    exit 1;
  }

  if (!open FILE, $ogc_logfile)
  {
    print STDERR "Error:  Could not open file $ogc_logfile.\n";
    exit 1;
  }

  while ($rec = <FILE>)
  {

    if ($rec =~ /(GeometryOGCValidatorIsValid\(TestFactory\): Tested )(\d+)(\D*)(\d+)(\D*)(\d+)(.+)$/)  
	{
      $number_features_tested = $2;
      $features_passed = $4;
      $features_failed = $6;
      #print "TransformerResults - number of features tested = $number_features_tested\n";
      #print "TransformerResults - number of features passed = $features_passed\n";
      #print "TransformerResults - number of features failed = $features_failed\n";
      
      if ($features_failed > 0) {
	      
	      $ogc_valid = '$shapename failed the OGC IsValid test.';
	      
	      eval {
				&updateMetadata($jobid,$shp_type,$shp_output_dir,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$fme_result,$unit,$ogc_valid)
			};
			print "$shapename failed the OGC IsValid test.\n";
			exit 1;
	      
      } else {
	  
	      $result = 'PASS';
	          
  	  }
      
    }
    
    if ($rec =~ /(GeometryOGCValidatorIsSimple\(TestFactory\): Tested )(\d+)(\D*)(\d+)(\D*)(\d+)(.+)$/)  
	{
      $number_features_tested = $2;
      $features_passed = $4;
      $features_failed = $6;
      #print "TransformerResults - number of features tested = $number_features_tested\n";
      #print "TransformerResults - number of features passed = $features_passed\n";
      #print "TransformerResults - number of features failed = $features_failed\n";
      
      if ($features_failed > 0) {
	      
	      $ogc_valid = '$shapename failed the OGC IsSimple test.';
  			eval {
				&updateMetadata($jobid,$shp_type,$shp_output_dir,$shapename,$shp_class,$table_name,$sum_lev,$projection,$gen_level,$shp_output_dir,$logfile,$fme_result,$unit,$ogc_valid)
			};
			print "$shapename failed the OGC IsSimple test.\n";
			exit 1;
	      
      } else {
	  
	      $result .= ',PASS';
	          
  	  }
      
    }
    
  }

  close FILE;

  return $result;
  
}	

###############################################################################################
#---------------------------------------------------------------------------------------------#
###############################################################################################		

sub updateMetadata {

	my $result;
	my $shapesize = &getShapeSize($shp_output_dir.'/'.$shapename);
	#print "\nFile size in bytes: $shapesize\n";
	
	my $fme_version = &getFMEVersion($logfile);
	#print "\nMy FME version is: $fme_version\n";
	
	my $fmt = 'MM/DD/YY HH:MI:SS AM';
  	$dbh->do("alter session set nls_date_format = '$fmt'");
    				
	$meta_sql = "insert into gz_shp_metadata (jobid,shp_name,shp_type,shp_class,source,sum_lev,projection,";
	$meta_sql .= "unit,modtag,resolution,del_flag,wave,output_dir,logfile,ogc_valid,shp_size_bytes,shp_creation_date,";
	$meta_sql .= "missing_geoid_list,fme_result,fme_version,qa_status,qa_date,notes,user_last_modified,date_last_modified,release_version) ";
	$meta_sql .= "VALUES ('".$jobid."','".$shapename."','".$shp_type."','".$shp_class."','".$table_name."','".$sum_lev."',";
	$meta_sql .= "'".$projection."','".$unit."','".$modtag."','".$gen_level."','".$del_flag."','".$wave."','".$shp_output_dir."','".$logfile."','".$ogc_valid."',";
	$meta_sql .= "".$shapesize.",to_date(sysdate,'".$fmt."'),NULL,'";
	$meta_sql .= "".$fme_result."','".$fme_version."',NULL,NULL,NULL,NULL,";
	$meta_sql .= "NULL,NULL)";
	
	#print "\nHere is the sql to update the metadata table: $meta_sql\n";
	
	$dbh->do($meta_sql);
	$dbh->commit;
	
	print "\nMetadata record successfully inserted for $shapename.\n";
	
	return $result;
	
}

###############################################################################################
#---------------------------------------------------------------------------------------------#
###############################################################################################


sub getFMEVersion {

  my ($logfile) = @_;
  my ($fme_version,$rec);

  if (!-e $logfile)
  {
    print STDERR "Error:  Could not find file $logfile\n";
    exit 1;
  }

  if (!open FILE, $logfile)
  {
    print STDERR "Error:  Could not open file $logfile.\n";
    exit 1;
  }

  while ($rec = <FILE>)
  {
    #if ($rec =~ /^.+Total\sFeatures\sWritten\s+(\d+)$/)
    #if ($rec =~ /^.+FME_PRODUCT_NAME\s\is\s\'(.*?)\'/) 
    if ($rec =~ /^.+FME_PRODUCT_NAME is \'(.*?)\'/) 
    {
      $fme_version = $1;
      #print "\nThe fme version is $fme_version.\n";
    }
  }

  close FILE;

  return $fme_version;
  
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
    
	print "\n";
	print "\n";
	print "'THAT WAS EASY! - It appears that all desired shapefiles were created successfully.\n";
	print "Scroll through screen output to make sure that there are no warnings.\n";
	print "Logfiles for FME translations are here: $logfile\n";
	print "\n";
	exit 0;

1; 
