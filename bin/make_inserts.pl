#!/user/bin/perl

use strict;
use warnings;

#take a list of files and create a set of insert statements for the DEC10
#dadsfile table

my $input = shift(@ARGV);
my $output = shift (@ARGV);

unless ( defined($input) && defined($output)) {
   print "Usage: make_inserts.pl <input list of shps> <output file for inserts>\n";
}

open IN, "<$input";
open OU, ">$output";

my $date = time;
my $res;
my $state;
my $unit;

while (<IN>) {

 chomp $_;
 
 $state = substr($_,11,2);
 
 #print "state = $state\n";

 $res = substr($_,27,2);

 if ($state == '00') {
    $unit = "nation";
    $state = "nation"
 }else{
    $unit = "st$state";
 }

 my $filename = substr($_,0,29);

 my $newline = "insert into dadsfile values ".
               "('DEC10','$state','/mtdata/geo/dads/gen/dec10_01/$unit/".
               "$filename',NULL,'Y',sysdate,'$res');\n";
               
 #print "found a line " . "$_";

 print OU "$newline";

}

print "\nFinished\n";

