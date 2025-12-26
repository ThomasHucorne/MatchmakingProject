# Matchmaking Project — Stable Matching Algorithms

## Overview

This project studies and implements several algorithms to solve the **stable matchmaking problem**
(also known as the **Stable Marriage Problem**) between two equally sized sets (e.g. men and women).
The objective is to compute a **stable matching**, meaning that no blocking pair exists.

The work focuses on:
- algorithmic correctness,
- theoretical and empirical **complexity analysis**,
- **performance comparison between R and C++ (Rcpp)**,
- the impact of **data structures** (lists, buckets, heaps) on execution time.

---

## Package Installation

The algorithms are distributed in the R package **`CHTpackage`**.

```r
if (!requireNamespace("CHTpackage", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("ThomasHucorne/MatchmakingProject/CHTpackage")
}

library(CHTpackage)
```

## Implemented Algorithms

### 1 - Naive Gale-Shapley

A direct implementation of the classical Gale–Shapley algorithm:
- men propose sequentially
- women keep their preferred partner so far
- rejected men continue proposing.

Properties
- Always produces a stable matching
- Man-optimal solution
- Theoretical complexity: O(n²)

### 2 - Gale-Shapley with buckets (R & C++)

This variant reorganizes the proposal process using a bucket-based priority structure:
- each bucket corresponds to a proposal rank (1st choice, 2nd choice, …),
- rejected men are placed into the bucket corresponding to their next proposal,
- proposals are processed in increasing priority order.

Motivation
- Reduce constant factors compared to a naive free-men list,
- Improve cache locality and execution speed,
- Prepare an efficient low-level implementation in C++.

### 3 - Hopcroft-Karp

In the context of blood donation, the Hopcroft–Karp algorithm is used to solve a large-scale compatibility matching problem between two distinct groups:
- Donors (left side of the bipartite graph) and Recipients (right side of the bipartite graph).

We model the situation as a bipartite graph:
- Each donor is a node on the left and each recipient is a node on the right.

An edge exists between a donor and a recipient if and only if the donor’s blood is compatible with the recipient’s medical constraints.

Blood compatibility constraints include:
- Blood groups (O, A, B, AB) and Rh factor (+ / −).

And for example, a donor with blood type O− is compatible with all recipients and a donor with blood type A+ can only donate to A+ or AB+ recipients.

The goal of Hopcroft–Karp is to compute a maximum matching, meaning match as many recipients as possible with compatible donors.
This is particularly important in emergency or shortage scenarios, where the priority is saving the maximum number of patients, not optimizing preferences.

## Complexity Analysis
The file Complexity_reports.Rmd is providing, empirical runtime measurements and comparisons with theoretical growth curves, like n², n log n and n³.
The purpose of this analysis is to calcultae the empirical complexity versus the theoritical complexity and to be able to understand the purpose of each methods and their limitations also.
On this part, there is also a comparison between R and C++ implementation for the same algorithm.


## Project Structure
MatchmakingProject/
├── CHTpackage/              # R package
│   ├── R/                   # R implementations
│   ├── src/                 # C++ (Rcpp) implementations
│   └── tests/               # Unit tests
├── Complexity_reports.Rmd
└── README.md

## Authors
- **Valentin CAPIEU**
- **Thomas HUCORNE**
- **Valentin TOURNIER**