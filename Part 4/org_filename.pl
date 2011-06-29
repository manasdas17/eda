$data_root = "../data/abstracts";

open(OUT, "> orgs_filenames.tsv");

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
            $abstract_file =~ /.*\/(\w+).txt/;
            my $filename = $1;
            while(<IN>) {
                chomp;
                my $line = $_;
                if($line =~ /NSF Org\s*:\s*(\w+).*/) {
                    $org = $1;
                } elsif($line =~ /Total Amt.\s*:\s*\$(\d+).*/) {
                    $grant = $1;
                }
            }
            print OUT "$year\t$org\t$grant\t$filename\n";
            close(IN);
        }
    }
}

close(OUT);