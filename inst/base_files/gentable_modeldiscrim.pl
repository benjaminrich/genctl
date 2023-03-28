#use strict;
#use warnings;
use YAML::XS;
use Template;
use Math::BigFloat;
use Math::CDF qw(:all);

require "./template.ctl";

#print $models;

my $mod = Load $models;
my $allmod = { models => [] };


# Add useful functions to the hash so that they can be used from templates.
$allmod->{'abs'} = sub { my $x = shift; my $digits = shift; return signif(abs($x), $digits); };
$allmod->{'sqrt'} = sub { my $x = shift; my $digits = shift; return signif(sqrt($x), $digits); };
$allmod->{'pow'} = sub { my $x = shift; my $y = shift; my $digits = shift; return signif(($x**$y), $digits); };
$allmod->{'exp'} = sub { my $x = shift; my $digits = shift; return signif(exp($x), $digits); };
$allmod->{'log'} = sub { my $x = shift; my $digits = shift; return signif(log($x), $digits); };
$allmod->{'expit'} = sub { my $x = shift; my $digits = shift; return signif((exp($x)/(1+exp($x))), $digits); };
$allmod->{'logit'} = sub { my $x = shift; my $digits = shift; return signif(log($x/(1-$x)), $digits); };

sub round {
    my $x = shift;
    my $digits = shift;
    $x = Math::BigFloat->new($x);
    $x->precision(-$digits);
    return "$x";
}

sub signif {
    my $x = shift;
    my $digits = shift;
    $x = Math::BigFloat->new($x);
    $x->accuracy($digits);
    return "$x";
}

$allmod->{'round'}  = \&round;
$allmod->{'signif'} = \&signif;

$allmod->{'any_noconverge'} = 0;
$allmod->{'any_problems'} = 0;
$allmod->{'any_nocovstep'} = 0;

my %MOF;
my %DF;

for my $i (0..$#{$mod}) {
    my $name = $mod->[$i]{'name'};

    $_ = $mod->[$i]{'flags'};
    $_ =~ s/,/ /g;
    my @flags = split;
    for my $flag (@flags) {
        $mod->[$i]{$flag} = 1;
    }

    my %namemap;
    if (open(NAMEMAP, "<", "$name/NAMEMAP")) {
        while (my $line = <NAMEMAP>) {
            $line =~ s/^\s+//;
            $line =~ s/\s+$//;
            my @kvpair = split /\s+=\s+/, $line, 2;
            $namemap{ $kvpair[0] } = $kvpair[1];
            #print "$kvpair[0] = $kvpair[1]\n";
        }
    }

    if (open(DF, "<", "$name/DEGREES_OF_FREEDOM")) {
        while (my $line = <DF>) {
            $line =~ s/^\s+//;
            $line =~ s/\s+$//;
            my @kvpair = split /\s*=\s*/, $line, 2;
            #print "$kvpair[0] = $kvpair[1]\n";
            $namemap{ $kvpair[0] } = $kvpair[1];
            $mod->[$i]{'DF_THETA'} = $kvpair[1] if $kvpair[0] eq "THETA";
            $mod->[$i]{'DF_OMEGA'} = $kvpair[1] if $kvpair[0] eq "OMEGA";
            $mod->[$i]{'DF_SIGMA'} = $kvpair[1] if $kvpair[0] eq "SIGMA";
            $mod->[$i]{'DF'} = $mod->[$i]{'DF_THETA'} + $mod->[$i]{'DF_OMEGA'} + $mod->[$i]{'DF_SIGMA'};
        }
        #print "$mod->[$i]{'name'}: $mod->[$i]{'DF'}\n";
    }

    # Note: Determining convergence by searching for "MINIMIZATION SUCCESSFUL" in the .lst file
    # works, for FOCEI method, but not for other methods like SAEM/IMP.
    #my $converged = 1;
    my $converged = 0;
    if (open(LST, "<", "$name/$name.lst")) {
        while (<LST>) {
            $converged = 1 if /MINIMIZATION SUCCESSFUL/;
            $converged = 2 if /HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION/;
            $converged = 1 if /OPTIMIZATION WAS COMPLETED/;
            $converged = 1 if /BURN-IN WAS NOT TESTED FOR CONVERGENCE/;
            $converged = 1 if /REDUCED STOCHASTIC PORTION WAS COMPLETED/;                                     
            ($mod->[$i]{'Tot_Obs'}) = ($_ =~ /:\s*(\d*)/) if /TOT\. NO\. OF OBS RECS/;
            ($mod->[$i]{'Tot_N'}) = ($_ =~ /:\s*(\d*)/) if /TOT\. NO\. OF INDIVIDUALS/;
        }
    }

    if (open(EXT, "<", "$name/$name.ext")) {
        my $ext_header = <EXT>;
        my ($method) = ($ext_header =~ /1: (.*?):/);
        my ($goal) = ($ext_header =~ /Goal Function=(.*?):/);
        my $line = <EXT>;
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;
        my @fields = split /\s+/, $line;

        my @final;
        my @final_se;
        my @sdcor;
        my @sdcor_se;
        my @fixed;
        while ($line = <EXT>) {
            $line =~ s/^\s+//;
            $line =~ s/\s+$//;
            my @values = split /\s+/, $line;
            @final    = @values if ($values[0] eq '-1000000000');
            @final_se = @values if ($values[0] eq '-1000000001');
            @sdcor    = @values if ($values[0] eq '-1000000004');
            @sdcor_se = @values if ($values[0] eq '-1000000005');
            @fixed    = @values if ($values[0] eq '-1000000006');
        }
        my $covstep = ($converged && @final_se) ? 1 : 0;

        for my $j (0..$#{fields}) {
            #print "$fields[$j] = $final[$j]\n";

            $fields[$j] =~ s/[^a-zA-Z0-9_]/_/g;

            if ($fields[$j] eq 'OBJ' or $fields[$j] eq 'MCMCOBJ' or $fields[$j] eq 'SAEMOBJ') {
                my $mof = $final[$j];
                $mof = Math::BigFloat->new($mof);
                $mof->precision(-3);
                $mof = "$mof";
                $mod->[$i]{ 'Obj' } = $mof;
                $mod->[$i]{ 'AIC' } = $mof + 2*$mod->[$i]{ 'DF' };
            }

            if ($fields[$j] =~ /^THETA|^SIGMA|^OMEGA/) {
                my $estimate = ($fields[$j] =~ /^SIGMA|^OMEGA/) ? $sdcor[$j] : $final[$j];

                if ($covstep && $final_se[$j] < 10000000000) {
                    my $stderr = ($fields[$j] =~ /^SIGMA|^OMEGA/) ? $sdcor_se[$j] : $final_se[$j];
                    if (abs($estimate) < 1e-16) { $estimate = 0; }
                    my $rse = ($estimate eq '0') ? "Inf" : 100 * abs($stderr / $estimate);

                    # Compute confidence interval
                    $confint = sprintf("(%.3f, %.3f)", round($estimate - 1.96*$stderr, 3), round($estimate + 1.96*$stderr, 3));
                    $expconfint = sprintf("(%.3f, %.3f)", round(exp($estimate - 1.96*$stderr), 3), round(exp($estimate + 1.96*$stderr), 3));

                    # Round numbers to 3 significant digits.
                    $stderr = signif($stderr,3);
                    $rse = signif($rse,3);

                    $mod->[$i]{ "se_$fields[$j]" }  = $stderr;
                    $mod->[$i]{ "rse_$fields[$j]" } = $rse;
                    $mod->[$i]{ "confint_$fields[$j]" } = $confint;
                    $mod->[$i]{ "expconfint_$fields[$j]" } = $expconfint;
                }

                # Round numbers to 3 significant digits.
                $estimate = signif($estimate,3);
                $mod->[$i]{ $fields[$j] } = $estimate;

                if ($fixed[$j] > 0) {
                    $mod->[$i]{ "fixed_$fields[$j]" } = 1;
                } else {
                    $mod->[$i]{ "fixed_$fields[$j]" } = 0;
                }

                if (exists $namemap{ $fields[$j] }) {
                    #print "$namemap{ $fields[$j] } = $mod->[$i]{ $fields[$j] } = $final[$j]\n";
                    $mod->[$i]{ $namemap{ $fields[$j] } }       = $mod->[$i]{ $fields[$j] };
                    $mod->[$i]{ "exp_$namemap{ $fields[$j] }" }  = round(exp($mod->[$i]{ $fields[$j] }), 3);
                    if ($covstep && $final_se[$j] < 10000000000) {
                        $mod->[$i]{ "se_$namemap{ $fields[$j] }" }  = $mod->[$i]{ "se_$fields[$j]" };
                        $mod->[$i]{ "rse_$namemap{ $fields[$j] }" } = $mod->[$i]{ "rse_$fields[$j]" };
                        $mod->[$i]{ "confint_$namemap{ $fields[$j] }" } = $mod->[$i]{ "confint_$fields[$j]" };
                        $mod->[$i]{ "expconfint_$namemap{ $fields[$j] }" } = $mod->[$i]{ "expconfint_$fields[$j]" };
                    }
                    $mod->[$i]{ "fixed_$namemap{ $fields[$j] }" }  = $mod->[$i]{ "fixed_$fields[$j]" };
                }
            }
        }

        $mod->[$i]{ 'Min' } = 'FAILED'     if not $converged;
        $mod->[$i]{ 'Min' } = 'SUCCESSFUL' if $converged == 1;
        $mod->[$i]{ 'Min' } = 'PROBLEMS'   if $converged == 2;
        $mod->[$i]{ 'Cov' } = 'OK'         if $covstep;
        if (!$converged) {
            $allmod->{'any_noconverge'} = 1;
        }
        elsif ($converged == 2) {
            $allmod->{'any_problems'} = 1;
        }
        if ($converged && !$covstep) {
            $allmod->{'any_nocovstep'} = 1;
        }

        $MOF{$name} = $mod->[$i]{'Obj'};
        $DF{$name} = $mod->[$i]{'DF'};
        $AIC{$name} = $mod->[$i]{'AIC'};

        #print Dump $mod->[$i];
        push @{$allmod->{'models'}}, $mod->[$i];

        if (open(FINAL, ">", "$name/FINAL_ESTIMATES.txt")) {
            print FINAL "$method\n\n";
            print FINAL "$goal = $mod->[$i]{ 'Obj' }\n\n";
            for my $j (0..$#{fields}) {
                if (exists $namemap{ $fields[$j] }) {
                    #print FINAL "$namemap{ $fields[$j] } = $mod->[$i]{ $namemap{ $fields[$j] } }\n";
                    my $var = "$namemap{ $fields[$j] }";
                    my $rse_var = "rse_$var";
                    if ($mod->[$i]{ $rse_var }) {
                        print FINAL "$var = $mod->[$i]{ $var } (rse: $mod->[$i]{ $rse_var } %)\n";
                    } else {
                        print FINAL "$var = $mod->[$i]{ $var }\n";
                    }
                }
            }
            print FINAL "\n";
            for my $j (0..$#{fields}) {
                if ($fields[$j] =~ /^THETA|^SIGMA|^OMEGA/) {
                    print FINAL "$fields[$j] = $final[$j]";
                    print FINAL "; $namemap{ $fields[$j] }" if exists $namemap{ $fields[$j] };
                    print FINAL "\n";
                }
            }
            print FINAL "\n";
            print FINAL "New inits:\n";
            for my $j (0..$#{fields}) {
                if ($fields[$j] =~ /^THETA/ and exists $namemap{ $fields[$j] }) {
                    print FINAL "INIT_THETA[ ${\( signif($final[$j], 3) )}; $namemap{ $fields[$j] }]";
                    print FINAL "\n";
                }
            }
        }
    }

    if (open(SHK, "<", "$name/$name.shk")) {
        my $shk_header = <SHK>;
        my $line = <SHK>;
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;
        my @fields = split /\s+/, $line;

        my @shrinkage;
        while ($line = <SHK>) {
            $line =~ s/^\s+//;
            $line =~ s/\s+$//;
            my @values = split /\s+/, $line;
            @shrinkage = @values if ($values[0] eq '4');
        }
        print FINAL "\n";
        print FINAL "SHRINKAGE:\n";
        for my $j (0..$#{fields}) {
            if ($fields[$j] =~ /^ETA/) {
                print FINAL "$fields[$j] = $shrinkage[$j]";
                print FINAL "\n";
            }
        }
    }
}

my @letters = ('a', 'b', 'c', 'd', 'e', 'f', 'g');
my $ss_index = 0;
if ($allmod->{'any_noconverge'}) {
    $allmod->{'ss_noconverge'} = $letters[$ss_index++];
}
if ($allmod->{'any_problems'}) {
    $allmod->{'ss_problems'} = $letters[$ss_index++];
}
if ($allmod->{'any_nocovstep'}) {
    $allmod->{'ss_nocovstep'} = $letters[$ss_index++];
}

for my $i (0..$#{$allmod->{'models'}}) {
    if (exists $allmod->{'models'}[$i]{'referencemodel'} and $allmod->{'models'}[$i]{'referencemodel'} ne $allmod->{'models'}[$i]{'name'}) {
        if ($allmod->{'models'}[$i]{'Obj'} ne 'ERROR!' and $MOF{ $allmod->{'models'}[$i]{'referencemodel'} } ne 'ERROR!') {
            my $delta_mof = $allmod->{'models'}[$i]{'Obj'} - $MOF{ $allmod->{'models'}[$i]{'referencemodel'} };
            $delta_mof = Math::BigFloat->new($delta_mof);
            $delta_mof->precision(-3);
            $delta_mof = "$delta_mof";
            $allmod->{'models'}[$i]{'dMOF'} = $delta_mof;

            my $delta_aic = $allmod->{'models'}[$i]{'AIC'} - $AIC{ $allmod->{'models'}[$i]{'referencemodel'} };
            $delta_aic = Math::BigFloat->new($delta_aic);
            $delta_aic->precision(-3);
            $delta_aic = "$delta_aic";
            $allmod->{'models'}[$i]{'dAIC'} = $delta_aic;

            my $delta_df = $allmod->{'models'}[$i]{'DF'} - $DF{ $allmod->{'models'}[$i]{'referencemodel'} };
            $allmod->{'models'}[$i]{'dDF'} = $delta_df;
            #print "$delta_df\n";

            if ($allmod->{'models'}[$i]{'dMOF'} < 0 and $delta_df > 0) {
                my $pvalue = 1.0 - pchisq(-1.0 * $allmod->{'models'}[$i]{'dMOF'}, $delta_df);
                #if ($pvalue < 0.001) {
                #    $pvalue = "< 0.001";
                #    $pvalue = sprintf("%.3e", $pvalue);
                #    $temp = Math::BigFloat->new($pvalue);
                #    $temp = $temp->blog(10);
                #    $temp = $temp->bceil();
                #    $pvalue = "< 10<sup>$temp</sup>";
                #} else {
                    $pvalue = Math::BigFloat->new($pvalue);
                    $pvalue->precision(-3);
                    $pvalue = "$pvalue";
                #}
                $allmod->{'models'}[$i]{'pvalue'} = $pvalue;
            }
            my $estim = $allmod->{'models'}[$i]{ $allmod->{'models'}[$i]{'testing'} };
            $allmod->{'models'}[$i]{'estimate'} = $estim;
            if ($allmod->{'models'}[$i]{'Cov'} eq "OK") {
                my $SE = $allmod->{'models'}[$i]{ "se_$allmod->{'models'}[$i]{'testing'}" };
                $allmod->{'models'}[$i]{'confint'} = sprintf("(%.3f, %.3f)", $estim - 1.96*$SE, $estim + 1.96*$SE);

                #if ($allmod->{'models'}[$i]{'pvalue'} <= 0.05) {
                #    print "$allmod->{'models'}[$i]{'testing'} = $estim\n";
                #}
            }

        } else {
            $allmod->{'models'}[$i]{'dMOF'} = 'ERROR!';
            $allmod->{'models'}[$i]{'dAIC'} = 'ERROR!';
        }
    }
}
#print Dump $allmod;


my $tt = Template->new;

print "Generating table_model_discrimination.html ...\n";
$tt->process("template_table_model_discrimination.html", $allmod, "table_model_discrimination.html") || die $tt->error;
print "Done.\n";

