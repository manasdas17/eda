$data_root = "../data/abstracts";

open(OUT, "> orgs_states.tsv");
print OUT "Year\tOrganisation\tGrant Amount\tState\n";
foreach my $part (`ls $data_root`) {
    chomp $part;
    foreach my $year_dir (`ls $data_root/$part`) {
        my $year = "";
        chomp $year_dir;
        if($year_dir =~ /\w+_(\d+)/) {
            $year = $1;
        }
        foreach my $abstract_file (`find $data_root/$part/$year_dir -name \"*.txt\" -print`) {
            chomp $abstract_file;
            open(IN, "$abstract_file");
            my $org = "";
            my $grant = "";
            my $state = "";
            while(<IN>) {
                chomp;
                my $line = $_;
                if($line =~ /NSF Org\s*:\s*(\w+).*/) {
                    $org = $1;
                } elsif($line =~ /Total Amt.\s*:\s*\$(\d+).*/) {
                    $grant = $1;
                } elsif($line =~ /\w+,\s+([A-Z]{2})\s+\d+\s+(\d|\s){3}\/(\d|\s){3}-\d*/) {
                    # maybe this regular expression will work if I'm lucky…
                    $state = $1;
                }
            }
            print OUT "$year\t$org\t$grant\t$state\n";
            close(IN);
        }
    }
}

close(OUT);