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

my $data_file   = 'data/hcpcs_by_months.csv';
my $hcpcs_file  = 'data/hcpcs_level_2.csv_0_0_0.csv';
my $cpt_file    = 'data/rvu26ar_1/PPRRVU2026_Jan_nonQPP.csv';
my $rev_file    = 'data/revenue_code_lookup_with_descriptions.csv';

my $csv         = Text::CSV_XS->new({ binary => 1, auto_diag => 1, sep_char => ',' });

my %stats       = ();
my %hcpcs_codes = ();
my %cpt_codes   = ();
my %rev_codes   = ();
my %found       = ();
my %not_found   = ();

load_hcpcs_codes();

load_revenue_codes();

load_cpt_codes();

load_monthly_hcpcs();

sub load_hcpcs_codes {
    open my $fh, "<:encoding(UTF-8)", $hcpcs_file or do {
        warn "Can't open $hcpcs_file: $!";
        next;
    };

    my ($cpt, $crt) = (0, 0);
    while (my $row = $csv->getline($fh)) {
        $cpt++;$crt++;
        if ($crt == 5000) {
            $crt = 0;
            STDOUT->printflush("\rParsing file - [$cpt]");
        }

        my $code   = @$row[0] // die;
        my $seqnum = @$row[1] // die;
        my $recid  = @$row[2] // die;
        my $short  = @$row[3] // die;
        $hcpcs_codes{"$code"} = $short;
    }
    close $fh;
    STDOUT->printflush("\rParsing file - [$cpt]");
}

sub load_cpt_codes {
    open my $fh, "<:encoding(UTF-8)", $cpt_file or do {
        warn "Can't open $cpt_file: $!";
        next;
    };

    my ($cpt, $crt) = (0, 0);
    while (my $row = $csv->getline($fh)) {
        $cpt++;$crt++;
        if ($crt == 5000) {
            $crt = 0;
            STDOUT->printflush("\rParsing file - [$cpt]");
        }

        my $code   = @$row[0] // die;
        next unless $code;
        my $short  = @$row[2] // die;
        $cpt_codes{"$code"} = $short;
    }
    close $fh;
    STDOUT->printflush("\rParsing file - [$cpt]");
}

sub load_revenue_codes {
    open my $fh, "<:encoding(UTF-8)", $rev_file or do {
        warn "Can't open $rev_file: $!";
        next;
    };

    my ($cpt, $crt) = (0, 0);
    while (my $row = $csv->getline($fh)) {
        $cpt++;$crt++;
        if ($crt == 5000) {
            $crt = 0;
            STDOUT->printflush("\rParsing file - [$cpt]");
        }

        my $code   = @$row[0] // die;
        next unless $code;
        my $short  = @$row[1] // die;
        $rev_codes{"$code"} = $short;
    }
    close $fh;
    STDOUT->printflush("\rParsing file - [$cpt]");
}

sub load_monthly_hcpcs {
    open my $fh, "<:encoding(UTF-8)", $data_file or do {
        warn "Can't open $data_file: $!";
        next;
    };

    # Read header and use getline_hr for hashref rows
    my $header = $csv->getline($fh);
    unless ($header) {
        warn "Empty or invalid CSV: $data_file\n";
        close $fh;
        next;
    }
    $csv->column_names(@$header);

    my ($cpt, $crt) = (0, 0);
    while (my $row = $csv->getline_hr($fh)) {
        $cpt++;$crt++;
        if ($crt == 5000) {
            $crt = 0;
            STDOUT->printflush("\rParsing file - [$cpt]");
        }
        my $total_paid                 = $row->{'total_paid'}                 // die;
        my $total_claims               = $row->{'total_claims'}               // die;
        my $hcpcs_code                 = $row->{'hcpcs_code'}                 // die;
        my $claim_from_month           = $row->{'claim_from_month'}           // die;
        my $total_unique_beneficiaries = $row->{'total_unique_beneficiaries'} // die;
        if (exists $hcpcs_codes{$hcpcs_code} || exists $cpt_codes{$hcpcs_code} || exists $rev_codes{$hcpcs_code}) {
            $found{$hcpcs_code}->{'total_unique_beneficiaries'} += $total_unique_beneficiaries;
            $found{$hcpcs_code}->{'total_paid'} += $total_paid;
        } else {
            $not_found{$hcpcs_code}->{'total_unique_beneficiaries'} += $total_unique_beneficiaries;
            $not_found{$hcpcs_code}->{'total_paid'} += $total_paid;
        }

        my ($hcpcs_source, $hcpcs_desc)  = ('not_found', 'not_found');
        if (exists $hcpcs_codes{$hcpcs_code}) {
            ($hcpcs_source, $hcpcs_desc) = ('HCPCS', $hcpcs_codes{$hcpcs_code});
        } elsif (exists $cpt_codes{$hcpcs_code}) {
            ($hcpcs_source, $hcpcs_desc) = ('CPT', $cpt_codes{$hcpcs_code});
        } elsif (exists $cpt_codes{$hcpcs_code}) {
            ($hcpcs_source, $hcpcs_desc) = ('Revenue', $rev_codes{$hcpcs_code});
        }

        my ($year, $month) = split '-', $claim_from_month;
        $stats{$hcpcs_code}->{'hcpcs_source'} = $hcpcs_source;
        $stats{$hcpcs_code}->{'hcpcs_desc'}   = $hcpcs_desc;
        $stats{$hcpcs_code}->{'by_months'}->{$year}->{$month}->{'total_unique_beneficiaries'} += $total_unique_beneficiaries;
        $stats{$hcpcs_code}->{'by_months'}->{$year}->{$month}->{'total_paid'}                 += $total_paid;
        $stats{$hcpcs_code}->{'by_months'}->{$year}->{$month}->{'total_claims'}               += $total_claims;
    }
    close $fh;
    STDOUT->printflush("\rParsing file - [$cpt]");
}

# Printing Missing HCPCS Codes.
open my $out_miss, '>:utf8', 'data/hcpcs_missing.csv';
say $out_miss "hcpcs_code,total_unique_beneficiaries,total_paid";
my %by_amount = ();
for my $hcpcs_code (sort keys %not_found) {
    my $total_unique_beneficiaries = $not_found{$hcpcs_code}->{'total_unique_beneficiaries'} // die;
    my $total_paid = $not_found{$hcpcs_code}->{'total_paid'} // die;
    $by_amount{$total_paid}->{$hcpcs_code}->{'total_unique_beneficiaries'} = $total_unique_beneficiaries;
}
for my $total_paid (sort{$b <=> $a} keys %by_amount) {
    for my $hcpcs_code (sort keys %{$by_amount{$total_paid}}) {
        my $total_unique_beneficiaries = $by_amount{$total_paid}->{$hcpcs_code}->{'total_unique_beneficiaries'} // die;
        say $out_miss "$hcpcs_code,$total_unique_beneficiaries,$total_paid";
    }
}
close $out_miss;

# Printing HCPCS Codes with Extended Data.
open my $out_stats, '>:utf8', 'data/hcpcs_by_months_with_desc.csv';
say $out_stats "hcpcs_code,hcpcs_desc,hcpcs_source,year,month,total_unique_beneficiaries,total_paid,total_claims";
for my $hcpcs_code (sort keys %stats) {
    my $hcpcs_desc   = $stats{$hcpcs_code}->{'hcpcs_desc'}   // die;
    my $hcpcs_source = $stats{$hcpcs_code}->{'hcpcs_source'} // die;
    for my $year (sort{$a <=> $b} keys %{$stats{$hcpcs_code}->{'by_months'}}) {
        for my $month (sort{$a <=> $b} keys %{$stats{$hcpcs_code}->{'by_months'}->{$year}}) {
            my $total_unique_beneficiaries = $stats{$hcpcs_code}->{'by_months'}->{$year}->{$month}->{'total_unique_beneficiaries'} // die;
            my $total_paid                 = $stats{$hcpcs_code}->{'by_months'}->{$year}->{$month}->{'total_paid'}                 // die;
            my $total_claims               = $stats{$hcpcs_code}->{'by_months'}->{$year}->{$month}->{'total_claims'}               // die;
            say $out_stats "$hcpcs_code,$hcpcs_desc,$hcpcs_source,$year,$month,$total_unique_beneficiaries,$total_paid,$total_claims";
        }
    }
}
close $out_stats;

p%not_found;

# p%found;