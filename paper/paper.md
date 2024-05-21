---
title: 'Dedumi: approximate deduplication of unaligned reads using unique molecular identifiers'
tags:
   - Haskell
   - bioinformatics
   - genomics
authors:
   - name: Justin Bedő
     affiliation: "1, 2"
affiliations:
   - name: The Walter and Eliza Hall Institute of Medical Research
     index: 1
   - name: The University of Melbourne
     index: 2
bibliography: references.bib
---

# Summary

Dedumi is an application for approximately deduplicating unaligned reads with unique molecular identifiers (UMIs).
Deduplication is through a Cuckoo filter and hence the probability of incorrectly filtering a unique read is bounded.
By virtue of operating on unaligned reads, there are significant computational savings as duplicate reads are removed prior to alignment.


# Statement of Need

Short read sequencing provides cost-effective DNA sequencing and forms a fundamental technique in many biological assays and studies.
The technique relies on randomly sequencing short fragments of DNA forming a *library*, with much subsequent analysis depending on counts of how frequently certain sequences are observed.
The formation of a library typically requires amplification of small amounts of DNA through polymerase chain reaction (PCR) prior to sequencing [@Garber2009].
Unfortunately, PCR can preferentially amplify certain strands of DNA [@Aird2011], which leads to a bias during analysis.
One method to reduce this bias is to introduce unique molecular identifiers (UMIs) prior to amplification to uniquely identify DNA fragments after amplification.
These UMIs can then be used to remove fragments (deduplicate) that have been repeatedly sequenced, thus mitigating the bias introduced by PCA.


There are several existing approaches to deduplication based on UMIs, most of which operate post-alignment: reads are first aligned to a reference genome and then duplicate reads mapping to the same position are collapsed to a consensuses sequence.
This is the approach taken by UMI-tools [@Smith2017], and many variants of the core approach exist [@Liu2019].
The downside to an alignment approach is that there is no computational cost reduction at the alignment stage: all reads, regardless of whether they are duplicates, are aligned to the genome.

In contrast, unaligned deduplication approaches reduce the computational cost of alignment as the duplicate reads can be removed prior to alignment.
There are some existing methods that deduplicate unaligned reads based on UMIs [@UMIc], however these are resource intensive: as reported by @UMIc, 1M reads required 2.2 hours to process.
These resource requirements prevent use in sequencing projects where many millions of reads need to be deduplicated.

Dedumi takes an alternative approach and approximately deduplicates unaligned reads.
UMIs are extracted from reads in a stream, with a Cuckoo filter [@Fan2014] used to detect duplicates.
A similar approach has recently been implemented in fastp [@Chen2023], however unlike dedumi a Bloom filter [@Bloom1970] is used which does not guarantee an upper bound on the error rate.
The error rate of the Cuckoo hash is bounded, with memory requirements determined by the chosen error rate.

On synthetic 150 b.p. paired end data generated with a duplication rate of 10% and a base error substitution rate of 0.087% to match the Illumina HiSeq X Ten [@Stoler2021],
dedumi processes 1M reads in 6.852s^[all results are the median of 11 different simulated datasets] on an Intel i7-14700F using a single core.
A total of 903,012 reads were conserved (9.7% duplication rate), closely matching the expected 10% duplication rate.
In comparison, fastp processes 1M reads in 4.259s (multiple threads, 10.667s user+sys) and retains 930,862 reads (6.9% duplication rate).
Dedumi is therefore 36% faster on a single core and more closely matches the expected duplication rate.

Dedumi is open source and published under the BSD3 licence.

# Acknowledgements

We thank Anthony Papenfuss (A.T.P.) for supporting this work and providing feedback on the manuscript.
J.B. was supported by the Stafford Fox Medical Research Foundation, Movember Foundation, and by funding to A.T.P. from a National Health and Medical Research Council (NHMRC) Investigator Grant (2026643).
The research benefitted by support from the Victorian State Government Operational Infrastructure Support and Australian Government NHMRC Independent Research Institute Infrastructure Support.
