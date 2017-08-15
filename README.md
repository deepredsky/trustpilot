# TrustPilot Challenge Solution

Solution to TrustPilot "[follow the white rabbit](https://followthewhiterabbit.trustpilot.com/cs/step2.html)" challenge in haskell

All three hashes was solved in about 12 mins

## Installation

Basic compile stack

```
stack build
```

## Usage

```
trustpilot <dictionary filepath> <anagram phrase>
```

## Example

```
time trustpilot wordlist "poultry outwits ants"
"Total words: 2486"
"Total anagram words: 1366"
e4820b45d2277f3844eac66c903e84be - "<redacted>"
23170acc097c24edb98fc5488ab033fe - "<redacted>"
665e5bcb0c20062fe8abaaf4628bb154 - "<redacted>"
      705.60 real       676.50 user         8.93 sys
```
