# Filters lines of input (JSON objects or text) matching a given set of tokens.

```
Filters lines of input (JSON objects or text) matching a given set of tokens.

Usage: filter-tokens (-t|--tokens-file-name ARG) [-i|--ignore-case] COMMAND
  Reads input from STDIN. Filters lines of input (JSON objects or text) matching
  a given set of tokens.

Available options:
  -h,--help                Show this help text
  -t,--tokens-file-name ARG
                           Tokens to search for, one per line.
  -i,--ignore-case         Case insensitive match.

Available commands:
  json                     Filter JSON, one object per line.
  text                     Filter lines of text.
```
