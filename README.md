# Dedumi - approximate read deduplication based on Unique Molecular Identifiers

Dedumi deduplicates read sets from paired end sequencing based on the
Unique Molecular Identifiers (UMIs) ligated to the start of each read.
Deduplication is based on the uniqueness of the UMIs, with some
additional bases for genomic context, with only the first read
encountered output.

## Building

This project has [Nix](https://nixos.org) expressions provided for
building and developing. To build with nix, run `nix build`, and to
drop into a development shell use `nix develop`.

The project can also be built using `hpack` and `cabal`. First
generate a cabal file with `hpack` and then build the binary with
`cabal build`.

## Usage


Run `dedumi --help` for some brief explanation of the parameters. The
input and output are paired end fastq files, so an example invocation
(with default parameters) is:
```
dedumi input_R{1,2}.fastq.gz output_R{1,2}.fastq.gz
```
The output will then contain the deduplicated reads with UMIs stripped
from each read pair.

## Parameters

- umiLength: The number of bases at the start of each read
  corresponding to the UMI
- extraHashBases: Additional bases following the UMI to use as genetic
  context
- filterSize: The size of the Cuckoo filter. If the filter reaches
  capacity this can be increased. Lowering the capacity reduces memory
  consumption.


