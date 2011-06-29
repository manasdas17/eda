$data_root = "../data/abstracts";
open(GRANTS, "> grants.tsv");

foreach my $part (`ls $data_root`) {
    chomp $part;
    foreach my $year_dir (`ls $data_root/$part`) {
        my $year = "";
        chomp $year_dir;
        if($year_dir =~ /\w+_(\d+)/) {
            $year = $1;
        }
        $i = 0;
        FILE: foreach my $abstract_file (`find $data_root/$part/$year_dir -name \"*.txt\" -print`) {
            chomp $abstract_file;
            open(IN, "$abstract_file");
            my $org = "";
            my $grant = "";
            my $reading_abstract = 0;
            
            while(<IN>) {
                chomp;
                my $line = $_;
                if($line =~ /Total Amt.\s*:\s*\$(\d+).*/) {
                        $grant = $1;
                        print GRANTS "$i.txt\t$grant\n";
                }
                if($line =~ /NSF Org\s*:\s*(\w+).*/) {
                    if(!(($1 eq "IIS") or ($1 eq "III"))) {
                        close(IN);
                        close(OUT);
                        next FILE;
                    }
                }  elsif ($reading_abstract == 1) {
                    print OUT "$line\n";
                } elsif ($line =~ /Abstract\s+:/) {
                    $reading_abstract = 1;
                    open(OUT, "> abstracts/$i.txt");
                    $i = $i + 1;
                }
            }
            close(IN);
            close(OUT);
        }
    }
}
close(GRANTS);