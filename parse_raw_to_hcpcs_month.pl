#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;
no autovivification;
binmode STDOUT, ":utf8";
use utf8;
use Text::CSV_XS;
use FindBin;
use File::Spec;
use Data::Printer;

my $file = "data/medicaid-provider-spending.csv";

my $csv = Text::CSV_XS->new({ binary => 1, auto_diag => 1, sep_char => ',' });

open my $fh, "<:encoding(UTF-8)", $file or do {
    warn "Can't open $file: $!";
    next;
};

# Read header and use getline_hr for hashref rows
my $header = $csv->getline($fh);
unless ($header) {
    warn "Empty or invalid CSV: $file\n";
    close $fh;
    next;
}
$csv->column_names(@$header);

my %stats = ();
my ($cpt, $crt) = (0, 0);

while (my $row = $csv->getline_hr($fh)) {
    $cpt++;$crt++;
    if ($crt == 50000) {
        $crt = 0;
        STDOUT->printflush("\rParsing file - [$cpt / 227083361]");
    }
    my $billing_provider_npi_num   = $row->{'BILLING_PROVIDER_NPI_NUM'}   // die;
    my $claim_from_month           = $row->{'CLAIM_FROM_MONTH'}           // die;
    my $hcpcs_code                 = $row->{'HCPCS_CODE'}                 // die;
    my $servicing_provider_npi_num = $row->{'SERVICING_PROVIDER_NPI_NUM'} // die;
    my $total_claims               = $row->{'TOTAL_CLAIMS'}               // die;
    my $total_paid                 = $row->{'TOTAL_PAID'}                 // die;
    my $total_unique_beneficiaries = $row->{'TOTAL_UNIQUE_BENEFICIARIES'} // die;

    $stats{$hcpcs_code}->{$claim_from_month}->{'total_unique_beneficiaries'} += $total_unique_beneficiaries;
    $stats{$hcpcs_code}->{$claim_from_month}->{'total_claims'}               += $total_claims;
    $stats{$hcpcs_code}->{$claim_from_month}->{'total_paid'}                 += $total_paid;
}
close $fh;
STDOUT->printflush("\rParsing file - [$cpt]");

# Printing concatenated stats.
open my $out, '>:utf8', 'data/hcpcs_by_months.csv';
say $out "hcpcs_code,claim_from_month,total_claims,total_paid,total_unique_beneficiaries";
for my $hcpcs_code (sort keys %stats) {
    for my $claim_from_month (sort keys %{$stats{$hcpcs_code}}) {
        my $total_unique_beneficiaries = $stats{$hcpcs_code}->{$claim_from_month}->{'total_unique_beneficiaries'} // die;
        my $total_claims               = $stats{$hcpcs_code}->{$claim_from_month}->{'total_claims'}               // die;
        my $total_paid                 = $stats{$hcpcs_code}->{$claim_from_month}->{'total_paid'}                 // die;
        say $out "$hcpcs_code,$claim_from_month,$total_claims,$total_paid,$total_unique_beneficiaries";
    }
}
close $out;