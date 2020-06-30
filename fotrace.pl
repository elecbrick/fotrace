#!/usr/bin/perl

# Copyright (c) 2020, Doug Eaton
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
# 1. Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

$VERSION=0.1;

use Getopt::Std;
use File::Basename;

%prev={};
# Default signals are from FOMU: min vexriscv, litex with verilator tooling
@sig=qw(
  tb.dut.VexRiscv.decode_PC
  tb.dut.VexRiscv.decode_INSTRUCTION
  tb.dut.VexRiscv.decode_arbitration_isValid
  tb.dut.VexRiscv.decode_arbitration_isFiring
  tb.dut.shared_ack
  tb.dut.shared_adr
  tb.dut.shared_dat_r
  tb.dut.shared_we
  tb.dut.shared_dat_w
);

=pod
Additional signals that can be used for further analysis
verilog_6502 signals
  tb.dut.cpu.RDY
  tb.dut.cpu.DO
  tb.dut.cpu.DI
  tb.dut.cpu.WE
  tb.dut.cpu.AB
  tb.dut.cpu.PC

USB processing
  tb.usb_pullup
  tb.usb_tx_en
  tb.usb_d_n
  tb.usb_d_p
  tb.reset

  tb.dut.basesoc_cpu_interrupt
  tb.dut.VexRiscv.reset
  tb.dut.shared_cyc
  tb.dut.shared_stb
  tb.dut.shared_err

Display the following signals to see instruction operands and write-back results
  tb.dut.VexRiscv.execute_REGFILE_WRITE_DATA
  tb.dut.VexRiscv.decode_RegFilePlugin_regFileReadAddress1
  tb.dut.VexRiscv.decode_RegFilePlugin_regFileReadAddress2
  tb.dut.VexRiscv.decode_RegFilePlugin_rs1Data
  tb.dut.VexRiscv.decode_RegFilePlugin_rs2Data
  tb.dut.VexRiscv.lastStageRegFileWrite_payload_address
  tb.dut.VexRiscv.lastStageRegFileWrite_payload_data
  tb.dut.VexRiscv.lastStageRegFileWrite_valid
=cut

foreach $signal (@sig) {
  $sig{$signal}=1;
}

sub HELP_MESSAGE {
  print <<EOF
Read the VCD format waveform dumpfile specified or from standard input and 
trace program and data flow by extracting the Program Counter, Opcode and
memory and I/O reads and writes over the Wishbone bus.

Usage: $0 [-c <csr.csv>] [-e <bios.elf>] [-o <output.log>] [dump.vcd]
  -c  Use I/O registers names and locations from a litex compatable spreadsheet.
  -e  Load function and variable names and locations from an elf file.
  -o  Write to outfile instead of standard output.
EOF
}

sub VERSION_MESSAGE {
  printf "%s version %.1f\n", basename($0, (".pl")), $VERSION;
}
$Getopt::Std::STANDARD_HELP_VERSION=1;

getopts('c:e:o:');  # -c, -e, -o take arguments. Sets $opt_* as a side effect.

if($opt_e) {
  open(SYM, "nm $opt_e|") or die "$!\n";
  while(<SYM>) {
    ($addr, $t, $name)=split;
    $addr=hex($addr);
    $sym{$addr}=$name if defined $name and $t=~/[tTdbB]/;
  }
  close(SYM);

  open(LINE, "objdump -WL $opt_e|") or die "$!\n";
  while(<LINE>) {
    if(/(.*):     file format/) {
      $dir=dirname($1);
      last;
    }
  }
  while(<LINE>) {
    # Process headers to get absolute directory from which relative names follow
    if(/^(CU: )?(.*):$/) {
      $file=$2;
      $base=basename($2),
      $filename[++$fileno]=$file;
      $basename[$fileno]=$base;
      #print "Filename $file, no: $fileno\n";
      next;
    }
    chomp;
    if(substr($_, 0, length($base)) eq $base) {
      ($line, $addr, $view)=split(' ', substr($_, length($base)));
      $addr=hex($addr);
      next if $line<1;
      # Only keep first occurence of an address.  This should be the invocation.
      next if exists $line{$addr};
      $line{$addr}=[$fileno, $line];
      #print "Address: $addr, file $filename[$fileno], line $line\n";
    }
  }
}

sub sourceline {
  my($fileno, $lineno)=@_;
  # Check if file already open
  if(!$source[$fileno]) {
    my($file)=$filename[$fileno];
    #print "File: $file, no: $fileno, base: $dir\n";
    my $filename=(substr($file,0,1) eq '/'? $file : "$dir/$file");
    open($handle, $filename) or warn( "Can't open file $filename $!"),
	$source[$fileno]=[], return;
    my @lines = <$handle>;
    close $handle;
    $source[$fileno]=\@lines;
    $l=@{$source[$fileno]};
    #print "No: $fileno, Lines: $l - $filename | ", $source[$fileno][60], "|";
    #print $lines[60];
  }
  # Return line of source file or undef
  #no warnings;
  #print "Looking for: $fileno:$lineno |", $source[$fileno][$lineno], "|\n";
  return $source[$fileno][$lineno];
}


open(CSR, "build/csr.csv");
while(<CSR>) {
  ($section, $name, $addr, $size, $rw)=split /,/;
  $addr=hex($addr);
  $sym{$addr}=$name if $section eq 'csr_register';
}
#foreach $sym (keys %sym) {
#  printf "%08x: %s\n", $sym, $sym{$sym};
#}

sub bin2dec {
  return unpack("N", pack("B32", substr("0" x 32 . shift, -32)));
}

sub fx8 {
  my($value)=@_;
  #return $value;
  if($value eq 'x') {return 'xxxxxxxx'};
  if($value eq 'z') {return 'zzzzzzzz'};
  if($value eq '')  {return '--------'};
  return sprintf("%08x", $value);
}

sub disp {
  return unless (%current);
  $need_nl=0;
  #foreach $k (keys %current) {
  #  print "$current{$k} ", fx8($current{$k})," $name{$k}\n";
  #}
  if($current{$dv}==1 or ($prev{$dv}==1 and not exists $current{$dv})) {
    # Decode valid
    $pc1=exists $current{$pc}?$current{$pc}:$prev{$pc};
    if(exists($line{$pc1})) {
      ($fileno, $line)=@{$line{$pc1}};
      $source=sourceline($fileno, $line);
      #print("$fileno:$line-1:", sourceline($fileno, $line-1));
      #print("$fileno:$line+0:", sourceline($fileno, $line));
      #print("$fileno:$line+1:", sourceline($fileno, $line+1));
      print "<$basename[$fileno]:$line> $source" if $source;
    }
    printf("%-16s %8s", (exists($sym{$pc1})?$sym{$pc1}:'    '.fx8($pc1)),
	fx8(exists $current{$op}?$current{$op}:$prev{$op}));
    $need_nl=1
  } else {
    if(exists $current{$ack} and $current{$ack}) {
      printf "%10.2fus             ", $time/1e6;
    }
  }
  if($current{$ack}==1 or ($prev{$ack}==1 and not exists $current{$ack})) {
    $address=$prev{$adr};
    $address*=4 if $address>0;
    if($prev{$we}) {
      $value=exists $current{$dw}?$current{$dw}:$prev{$dw};
      printf("            %8s \>%8s", fx8($address), fx8($value));
    } else {
      $value=exists $current{$dr}?$current{$dr}:$prev{$dr};
      printf("  \<%8s %8s          ", fx8($value), fx8($address));
    }
    print("  ", $sym{$address}) if exists $sym{$address};
    $need_nl=1
  };
  print("\n") if $need_nl;
  # Transfer new state to previous retaining any unchanged signals
  foreach $k (keys %current) {
    $prev{$k}=$current{$k};
  }
  %current=();
}

while(<>) {
  @word=split;
  if(@word[0] eq '$scope') {
    push(@scope, $word[2]);
  } elsif(@word[0] eq '$upscope') {
    pop(@scope);
  } elsif(@word[0] eq '$enddefinitions') {
    last;
  } else {
    $abbr=$word[3];
    $signal=join('.', @scope, $word[4]);
    if(exists $sig{$signal}) {
      #print "Found $signal\n";
      $name{$abbr}=$signal;
      $sig{$signal}=$abbr;
      #delete $sig{$signal};
    }
  }
}

#foreach $signal (keys %sig) {
#  printf(STDERR "Signal %s not found:", $signal);
#}
#print "Signals found. Analyzing.\n";
$pc=$sig{'tb.dut.VexRiscv.decode_PC'};
$op=$sig{'tb.dut.VexRiscv.decode_INSTRUCTION'};
$dv=$sig{'tb.dut.VexRiscv.decode_arbitration_isFiring'};
$dr=$sig{'tb.dut.shared_dat_r'};
$dw=$sig{'tb.dut.shared_dat_w'};
$we=$sig{'tb.dut.shared_we'};
$adr=$sig{'tb.dut.shared_adr'};
$ack=$sig{'tb.dut.shared_ack'};
$cyc=$sig{'tb.dut.shared_cyc'};
$stb=$sig{'tb.dut.shared_stb'};
$err=$sig{'tb.dut.shared_err'};
#print $pc,"\n";
#print $op,"\n";

$time=0;
while(<>) {
  strip;
  if(/^\#(\d+)/) {
    disp();
    $time=$1;
  }
  if(/^b(\w+) (.+)$/) {
    ($value, $abbr)=($1, $2);
    if(exists $name{$abbr}) {
      # This is slow but an X could be in any bit of the signal
      if($value=~/x/i) {
	$current{$abbr}='x';
      } elsif($value=~/z/i) {
	$current{$abbr}='z';
      } else {
	#print "Oops: $value\n" unless $value=~/^\d+$/;
	$current{$abbr}=bin2dec($value);
      }
    }
  } elsif(/^([01xz])(.+)$/i) {
    ($value, $abbr)=($1, $2);
    if(exists $name{$abbr}) {
      if($value=~/x/i) {
	$current{$abbr}='x';
      } elsif($value=~/z/i) {
	$current{$abbr}='z';
      } else {
	#print "Oops: $value\n" unless $value==0 or $value==1;
	$current{$abbr}=$value;
      }
    }
  }
  #print $abbr, "  ", $current{$abbr}, "\n";
}
