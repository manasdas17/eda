$data_root = "../data/abstracts";

open(OUT, "> amounts_dates.tsv");

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
            my $start_date = "";
            my $end_date = "";
            while(<IN>) {
                chomp;
                my $line = $_;
                if($line =~ /NSF Org\s*:\s*(\w+).*/) {
                    $org = $1;
                } elsif($line =~ /Total Amt.\s*:\s*\$(\d+).*/) {
                    $grant = $1;
                } elsif($line =~ /Start Date\s*:\s*(\w+)\s+(\d+),\s+(\d{4}).*/) {
                    $start_date = "$1 $2, $3";
                } elsif($line =~ /Expires\s*:\s*(\w+)\s+(\d+),\s+(\d{4}).*/) {
                    $end_date = "$1 $2, $3";
                }
            }
            print OUT "$year\t$org\t$grant\t$start_date\t$end_date\n";
            close(IN);
        }
    }
}

close(OUT);