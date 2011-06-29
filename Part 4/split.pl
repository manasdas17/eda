$data_root = "../data/abstracts";

open(IN, "orgs_filenames.tsv");

while(<IN>) {
    chomp;
    ($year, $dept, $grant, $path) = split /\t/;
    $path =~ /.*\/(\w+).txt/;
    print "$1\n";
}

close(IN);