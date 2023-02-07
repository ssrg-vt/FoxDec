## Existence Metrics
| Metric Name               | Description   | Notes |
|----                       |----           |-----  |
| `metricFileExists`        | Does a file `metrics.json` exist?| This file can be found in directory `/xyz` |
| `HoareGraphFileExists`    | Does a file `hoaregraph.json` exist?| This file can be found in directory `/xyz` |


## Coverage Metrics
| Metric Name               | Description   |
|----                       |----           |
| `%instructionCoverage`    | Estimate of percentage of covered instructions |
| `#instructions`    | Number of covered instructions |
| `#resolvedIndirectJumps`  | Number of resolved indirect jumps |
| `#unresolvedIndirectJumps`  | Number of unresolved indirect jumps |
| `#resolvedIndirectCalls`  | Number of resolved indirect calls |
| `#unresolvedIndirectCalls`  | Number of unresolved indirect calls |
| `#functions`  | Number of covered functions |

> `%instructionCoverage = X/Y` where `X = #instructions ` and `Y = #guessOfExpectedInstructions`. Here `Y` is computed by taking the size of the text-section of the binary divided by the average instruction size.  

## Verification Metrics
| Metric Name                  | Description   |
|----                          |----           |
| `%resolvedMemWrites`         | Percentage of pointers that are assigned a non-trivial domain |
| `precisionScore`             | A score (0-10) indicating how precise the pointer analysis has been |
| `#functionsWithError`        | Number of covered functions with a verification error |
| `hasBeenExportedToJSON`      | Boolean indicating whether the output has been exported in JSON format |
| `hasBeenVerifiedInIsabelle`  | Boolean indicating whether the output has been formally verified in Isabelle/HOL |


> `%resolvedMemWrites = X/Y` where `X = #resolvedMemWrites ` and `Y = #memWrites`. Here `Y` is the total number of instructions that write to memory, and `X` is those instructions whose destination pointer could be assigned a non-trivial domain.

> `precisionScore` The exact computation of this score is still to be determined, but it will indicate whether pointer analysis has been precise (10) or coarse (0).

## Performance Metrics
| Metric Name               | Description   |
|----                       |----           |
| `timeRunning`             | FoxDec running time in hours |
| `timeManual`              | An estimate of the manual effort in hours |


