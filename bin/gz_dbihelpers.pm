package gz_dbihelpers;

use strict;
use warnings;

use DBI; # 1.607;
use DBD::Oracle; # 1.22;

use Exporter qw(import);
our @ISA = qw(Exporter);

our @EXPORT = qw(
get_dbh
);


=head1 NAME

  gz_dbihelpers.pm

  created: 10/21/2010
  last modified (unlikely): 10/21/2010

=head1 SYNOPSIS

  So far just get_dbh stoled from CAMPS. I dunno, I may chuck this

=cut


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

##############################################################################
