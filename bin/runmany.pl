#!/usr/bin/perl
use strict;
use warnings;

use DBI; # 1.607;
use DBD::Oracle; # 1.22;
use Getopt::Long qw(:config no_ignore_case bundling);

####################### #############################################################

# THIS IS JUST A QUICKIE HELPER - be careful!

# usage - enter the pw extension and then a list of states to run

# usage runmany.pl <pext> <state1> <state2> <state3> <state4> ...

# commented out... changing so you can run multiple by entering states at 
# command line ...  my @states = qw/20 25 46/;

#####################################################################################

## see gzprod_dec10.pl for real usage 

my (
   $state,
   $slist,
   $res,
   $pext,
   $instance,
   $mods,
   $failure,
   $resnum
   );


my $rc = GetOptions(
   'states|s=s' => \$slist,
   'res|r=s'     => \$res,
   'pext|p=s'     => \$pext,
   'instance|i=s'     => \$instance,
   'mods|m=s' => \$mods
);

##################################################################################

unless ( (defined $pext) && (defined $slist) && (defined $res) &&
       (defined $instance) && (defined $mods) ) {
   print "usage -s <list of states like 10,15,20> -p <pwext> -r <z6 z8 or z9> -i <instance like 1 or 2> -m <123>\n";
   print "example runmany.pl  -s 10,04,12 -p ***  -r z9 -i 1 -m 23\n";
   die "Please re-try entering all arguments.\n\n";
}

$resnum = substr($res,1,1);

$res = uc($res);

my @states = split(/,/,$slist);

##########################################################################################

foreach $state (@states) {

   # set up variables...
   
   $failure = 1;

   my $database = 'PRODBNCH'.$instance;
   my $user = 'GZDEC10ST' . $state;
   my $password = $user . $pext;
   my $tabname = $res . $state . "CL_MT" . $resnum . $state . "_EWRK" ;
   
   print STDOUT "RUNMANY: --------------------------------------------\n";
   print STDOUT "RUNMANY: Attempting to run state '$state'\n";
   
   if ( (length($state) !=  2) || (length($pext) != 1) ) {
     print STDOUT "State option must be 2 characters.\n";
     print STDOUT "Pext option must be 1 character.\n";
     die "Please call the program with a valid state and pext.\n";
   }

   my $cmd = "gzprod_dec10.pl -s $state ". 
             "-r $res " .
             "-p $pext " .
             "-i $instance " .
             "-m $mods " .
             "-t $tabname " ;


   print STDOUT "RUNMANY: Calling gzprod.\n";
   print STDOUT "RUNMANY:    command = '$cmd'.\n";
   print STDOUT "RUNMANY: -----\n";

   $failure = system($cmd);

   if ($failure) {
 
      print STDOUT "RUNMANY: FAILURE, state $state\n";
 
   } else {

      print STDOUT "RUNMANY: SUCCESS, state $state\n";

   }


} # finish state loop 

##########################################################################################

   print STDOUT "RUNMANY: --------------------------------------------\n";
   print STDOUT "RUNMANY: ---  Completed all state runs\n";
   print STDOUT "RUNMANY: --------------------------------------------\n";


##################### END PROGRAM ###################################

