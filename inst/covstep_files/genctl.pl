#use strict;
#use warnings;
use YAML::XS;
use Template;

use File::Path qw(make_path);
use File::Copy;

my $nmfe = "C:\\nm720\\run\\nmfe72.bat";

require "./template.ctl";

#print $models;

my $mod = Load $models;
my @baseflags;


my $tt = Template->new({POST_CHOMP => 1});


my %THETA_index;
my @THETA_name_order;

sub THETA {
    my $name = shift(@_);

    if (!exists $THETA_index{$name}) {
        # Add a new index
        $THETA_index{$name} = $#THETA_name_order + 2;
        push @THETA_name_order, $name;
    }
    return "THETA($THETA_index{$name})";
}

my %ETA_index;
my @ETA_name_order;

sub ETA {
    my $name = shift(@_);

    if (!exists $ETA_index{$name}) {
        # Add a new index
        $ETA_index{$name} = $#ETA_name_order + 2;
        push @ETA_name_order, $name;
    }
    return "ETA($ETA_index{$name})";
}

sub MU_ {
    my $name = shift(@_);

    if (!exists $ETA_index{$name}) {
        # Add a new index
        $ETA_index{$name} = $#ETA_name_order + 2;
        push @ETA_name_order, $name;
    }
    return "MU_$ETA_index{$name}";
}

my %EPS_index;
my @EPS_name_order;

sub EPS {
    my $name = shift(@_);

    if (!exists $EPS_index{$name}) {
        # Add a new index
        $EPS_index{$name} = $#EPS_name_order + 2;
        push @EPS_name_order, $name;
    }
    return "EPS($EPS_index{$name})";
}

sub SIGMA {
    my $name = shift(@_);

    if (!exists $EPS_index{$name}) {
        # Add a new index
        $EPS_index{$name} = $#EPS_name_order + 2;
        push @EPS_name_order, $name;
    }
    return "SIGMA($EPS_index{$name})";
}


my $THETA_df = 0;
sub INIT_THETA {
    my $init_name = shift(@_);
    my $init_value = shift(@_);
    ++$THETA_df if $init_value !~ "FIX|SAME";
    THETA($init_name);
    return "\$THETA $init_value ;$init_name";
}

my @OMEGA_current_block = (0, 0);

sub INIT_OMEGA_BLOCK {
    $OMEGA_current_block[2] == 0 || die "Cannot have nested OMEGA blocks";
    my $block_size = shift(@_);
    $OMEGA_current_block[1] = $block_size;
    $OMEGA_current_block[2] = $block_size;
    return "\$OMEGA BLOCK($block_size)";
}

my $OMEGA_df = 0;
sub INIT_OMEGA {
    my $init_name = shift(@_);
    my $init_value = shift(@_);
    ++$OMEGA_df if $init_value !~ "FIX|SAME";
    ETA($init_name);
    if ($OMEGA_current_block[2] > 0) {
        --$OMEGA_current_block[2];
        return "$init_value ;$init_name";
    } else {
        return "\$OMEGA $init_value ;$init_name";
    }
}

my @SIGMA_current_block = (0, 0);

sub INIT_SIGMA_BLOCK {
    $SIGMA_current_block[2] == 0 || die "Cannot have nested SIGMA blocks";
    my $block_size = shift(@_);
    $SIGMA_current_block[1] = $block_size;
    $SIGMA_current_block[2] = $block_size;
    return "\$SIGMA BLOCK($block_size)";
}

my $SIGMA_df = 0;
sub INIT_SIGMA {
    my $init_name = shift(@_);
    my $init_value = shift(@_);
    ++$SIGMA_df if $init_value !~ "FIX|SAME";
    EPS($init_name);
    if ($SIGMA_current_block[2] > 0) {
        --$SIGMA_current_block[2];
        return "$init_value ;$init_name";
    } else {
            return "\$SIGMA $init_value ;$init_name";
    }
}



for my $i (0..$#{$mod}) {

    my $modname = $mod->[$i]{'name'};
    my $run_dir = "$modname";
    my $ctl_file = "$run_dir/$modname.ctl";
    my $lst_file = "$modname.lst";
    my $namemap_file = "$run_dir/NAMEMAP";
    my $df_file = "$run_dir/DEGREES_OF_FREEDOM";
    my $bat_file = "nmrun_$modname.bat";

    unless(-d "$run_dir") {
        print "Creating directory $run_dir ...\n";
        mkdir "$run_dir" or die "Error creating directory $run_dir: $!";
    }

    print "Generating $ctl_file ...\n";
    $_ = $mod->[$i]{'flags'};
    $_ =~ s/,/ /g;
    my @flags = split;
    for my $flag (@flags) {
        $mod->[$i]{$flag} = 1;
    }
    if (exists $mod->[$i]{'baseflags'}) {
        $_ = $mod->[$i]{'baseflags'};
        $_ =~ s/,/ /g;
        @baseflags = split;
    }
    for my $flag (@baseflags) {
        $mod->[$i]{$flag} = 1;
    }
    #if ($mod->[$i]{'referencemodel'} eq $mod->[$i]{'name'}) {
    #    @baseflags = @flags;
    #}
    $_ = $mod->[$i]{'unflags'};
    $_ =~ s/,/ /g;
    my @unflags = split;
    for my $flag (@unflags) {
        $mod->[$i]{$flag} = 0;
    }

    %THETA_index = ();
    @THETA_name_order = ();
    %ETA_index = ();
    @ETA_name_order = ();
    %EPS_index = ();
    @EPS_name_order = ();

    $THETA_df = 0;
    $OMEGA_df = 0;
    $SIGMA_df = 0;

    #$mod->[$i]{"THETA"} = \&THETA;
    #$mod->[$i]{"ETA"} = \&ETA;
    #$mod->[$i]{"EPS"} = \&EPS;
    #$mod->[$i]{"SIGMA"} = \&SIGMA;

    my $output;
    $tt->process(\$template, $mod->[$i], \$output) || die $tt->error;

    my $intermediate;
    open INTERMEDIATE, ">", \$intermediate;

    for my $l (split("\n", $output)) {
        $l =~ s/\bINIT_THETA\[(.*?);\s*(\w+?)\s*\]/INIT_THETA($2,$1)/ge;
        $l =~ s/\bINIT_OMEGA_BLOCK\[(\w+?)\]/INIT_OMEGA_BLOCK($1)/ge;
        $l =~ s/\bINIT_OMEGA\[(.*?);\s*(\w+?)\s*\]/INIT_OMEGA($2,$1)/ge;
        $l =~ s/\bINIT_SIGMA_BLOCK\[(\w+?)\]/INIT_SIGMA_BLOCK($1)/ge;
        $l =~ s/\bINIT_SIGMA\[(.*?);\s*(\w+?)\s*\]/INIT_SIGMA($2,$1)/ge;
        print INTERMEDIATE "$l\n";
    }

    my $final;
    open FINAL, ">", \$final;

    for my $l (split("\n", $intermediate)) {
        $l =~ s/(\$DATA\s+)(\S+)/$1..\/$2/g;
        $l =~ s/\bTHETA\[(\w+?)\]/THETA($1)/ge;
        $l =~ s/\bETA\[(\w+?)\]/ETA($1)/ge;
        $l =~ s/\bMU_\[(\w+?)\]/MU_($1)/ge;
        $l =~ s/\bEPS\[(\w+?)\]/EPS($1)/ge;
        $l =~ s/\bSIGMA\[(\w+?)\]/SIGMA($1)/ge;
        print FINAL "$l\n";
    }

    #$final =~ /\$DATA\s+(\S+)/;
    #my $data_file = $1;
    #print "\$DATA: $input_data_file \n\n";

    open OUTFILE, ">", $ctl_file || die "Error opening file $ctl_file in write mode: $!";
    for my $l (split("\n", $final)) {
        print OUTFILE "$l\n";
    }

    print "Generating $namemap_file ...\n";
    open NAMEMAP, ">", $namemap_file || die "Error opening file $namemap_file in write mode: $!";

    for my $i (0 .. $#THETA_name_order) {
        print NAMEMAP "THETA${\($i+1)} = $THETA_name_order[$i]\n";
    }
    for my $i (0 .. $#EPS_name_order) {
        #print NAMEMAP "SIGMA(${\($i+1)},${\($i+1)}) = $EPS_name_order[$i]\n";
        print NAMEMAP "SIGMA_${\($i+1)}_${\($i+1)}_ = $EPS_name_order[$i]\n";
    }
    for my $i (0 .. $#ETA_name_order) {
        #print NAMEMAP "OMEGA(${\($i+1)},${\($i+1)}) = $ETA_name_order[$i]\n";
        print NAMEMAP "OMEGA_${\($i+1)}_${\($i+1)}_ = $ETA_name_order[$i]\n";
    }

    print "Generating $df_file ...\n";
    open DF, ">", $df_file || die "Error opening file $df_file in write mode: $!";
    print DF "THETA=$THETA_df\nOMEGA=$OMEGA_df\nSIGMA=$SIGMA_df\n";
    close DF;


#    $nmrun_bat = <<END_NMRUN_BAT;
#\@echo off
#title Running model $modname
#call nmexec $modname
#END_NMRUN_BAT
#
#    print "Generating $bat_file ...\n";
#    open NMRUNBAT, ">", $bat_file || die "Error opening file $bat_file in write mode: $!";
#    print NMRUNBAT $nmrun_bat;
#    
#        $render_results_bat = <<END_RENDER_RESULTS_BAT;
#    \@echo off
#    copy ..\\results.R .\\results.R
#    echo rmarkdown::render("results.R") | "C:\\Program Files\\R\\R-3.5.0\\bin\\x64\\R.exe" --no-save
#    for %%a in (.) do set rundir=%%~na
#    move results.html %rundir%_results.html
#    END_RENDER_RESULTS_BAT
#    
#    open RENDERRESULTSBAT, ">", "$run_dir/render_results.bat" || die "Error opening file $run_dir/render_results.bat in write mode: $!";
#    print RENDERRESULTSBAT $render_results_bat;

    #$nnodes = int(72 / ($#{$mod} + 1));
    $nnodes = 36;
    if ($nnodes > 1) {
        $run_me_sh = <<END_RUN_ME_SH;
rm -rf modelfit_dir*
execute --run_on_sge --sge_prepend="-pe mpi $nnodes" $modname.ctl --nodes=$nnodes --parafile=/shared/.admin/mpigrid.pnm --nm_version=nm73 --clean=1 --nm_output=ext,shk,phi,phm
#cp modelfit_dir1/NM_run1/*.msf .
rm -rf modelfit_dir*
END_RUN_ME_SH
    } else {
        $run_me_sh = <<END_RUN_ME_SH;
rm -rf modelfit_dir*
execute --run_on_sge $modname.ctl --parafile=/shared/.admin/mpigrid.pnm --nm_version=nm73 --clean=1 --nm_output=ext,shk
#cp modelfit_dir1/NM_run1/*.msf .
rm -rf modelfit_dir*
END_RUN_ME_SH
    }

    open RUNMEDOTSH, ">", "$run_dir/RUN_ME.sh" || die "Error opening file $run_dir/RUN_ME.sh in write mode: $!";
    print RUNMEDOTSH $run_me_sh;


    #if (-f "results.R") {
    #    copy("results.R", "$run_dir/results.R") || die "Copy failed: $!";
    #}
    #if (-f "render_results.bat") {
    #    copy("render_results.bat", "$run_dir/render_results.bat") || die "Copy failed: $!";
    #}
}




$template_menu_run_all_seq = <<'END_TEMPLATE_RUN_ALL_SEQ';
[% FOREACH m IN models %]
cd [% m.name +%]
nohup bash RUN_ME.sh &
cd ..
[% END %]
END_TEMPLATE_RUN_ALL_SEQ

$tt->process(\$template_menu_run_all_seq, { models => $mod }, "nmrun_all_sequential.sh") || die $tt->error;


#$template_menu_run_all_seq = <<'END_TEMPLATE_RUN_ALL_SEQ';
#@echo off
#[% FOREACH m IN models %]
#call nmrun_[% m.name %].bat
#[% END %]
#END_TEMPLATE_RUN_ALL_SEQ
#
#$tt->process(\$template_menu_run_all_seq, { models => $mod }, "nmrun_all_sequential.bat") || die $tt->error;
#
#
#$template_menu_run_all_para = <<'END_TEMPLATE_RUN_ALL_PARA';
#@echo off
#[% FOREACH m IN models %]
#echo Executing [% m.name +%]
#start %SystemRoot%\system32\cmd.exe /k "color 9A & nmrun_[% m.name %].bat"
#[% END %]
#END_TEMPLATE_RUN_ALL_PARA
#
#$tt->process(\$template_menu_run_all_para, { models => $mod }, "nmrun_all_parallel.bat") || die $tt->error;
#
#
#
#
#$template_menu = <<'END_TEMPLATE_MENU';
#@echo off
#
#set wfndir=C:\nm720\wfn7
#
#:menu
#
#echo -
#echo Choose a NONMEM model to run:
#echo -
#[% FOREACH m IN models %]
#echo  [% loop.count %]) [% m.name +%]
#echo      [% m.description +%]
#echo -
#[% END %]
#echo  s) Run all the models sequentially
#echo  p) Run all the models in parallel (caution: don't use if there are many models)
#echo -
#
#set /p userinp="Selection (1-[% models.size %], a for all, or Q to quit): "
#
#[% FOREACH m IN models %]
#if "%userinp%"=="[% loop.count %]" goto run[% loop.count +%]
#[% END %]
#
#if "%userinp%"=="s" goto executeallseq
#if "%userinp%"=="p" goto executeallpara
#if "%userinp%"=="q" goto end
#if "%userinp%"=="Q" goto end
#
#echo Invalid choice.  Try again.
#goto menu
#
#[% FOREACH m IN models +%]
#:run[% loop.count +%]
#set model=[% m.name +%]
#goto execute
#[% END %]
#
#:execute
#echo Executing nmexec %model%
#start %SystemRoot%\system32\cmd.exe /k "color 9A & nmexec %model%"
#goto menu
#
#:executeallseq
#echo Executing menu_run_all_sequential 
#start %SystemRoot%\system32\cmd.exe /k "color 9A & nmrun_all_sequential.bat"
#goto menu
#
#:executeallpara
#echo Executing menu_run_all_parallel 
#call nmrun_all_parallel.bat
#goto menu
#
#:end
#END_TEMPLATE_MENU
#
#print "Generating menu.bat ...\n";
#$tt->process(\$template_menu, { models => $mod }, "menu.bat") || die $tt->error;


print "Done.\n";
