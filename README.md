# fotrace
fotrace makes it easy to debug the hardware/software interaction captured by a simulation run.
It extract the PC, instruction and Wishbone transactions from a simulation waveform dump and combines this with symbol table and source code line number information from the executable that is running. The result is similar to single stepping every machine instruction in gdb
This tool was created debugging an application

## Usage
fotrace [-c <csr.csv>] [-e <bios.elf>] [-o <output.log>] [dump.vcd]
  -c  Use I/O registers names and locations from a litex compatable spreadsheet.
  -e  Load function and variable names and locations from an elf file.
  -o  Write to outfile instead of standard output.
Standard Input is used for the waveform dump is one is not specified on the command line.

## Caveats
The line number information generated by gcc is intended for source
level debugging by gdb. The lines this program displays may be comments or
braces rather than meaningful code. For this reason, the file and line number
are displayed at the beginning of each line of source code.
