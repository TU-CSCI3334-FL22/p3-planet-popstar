|     |                 |               |                                               |
| --- | --------------- | ------------- | --------------------------------------------- |
| 1   | Grammar         | $\rightarrow$ | ProductionList                        |
| 3   | ProductionList  | $\rightarrow$ | ProductionSet  SEMICOLON  ProductionList'     |
| 4   | ProductionList' | $\rightarrow$ | ProductionSet  SEMICOLON  ProductionList'     |
| 5   |                 | $\mid$        | $\epsilon$                                    |
| 6   | ProductionSet   | $\rightarrow$ | SYMBOL  DERIVES RightHandSide  ProductionSet' |
| 7   | ProductionSet'  | $\rightarrow$ | ALSODERIVES  RightHandSide  ProductionSet'    |
| 8   |                 | $\mid$        | $\epsilon$                                    |
| 9   | RightHandSide   | $\rightarrow$ | SymbolList                                    |
| 10  |                 | $\mid$        | EPSILON                                       |
| 12  | SymbolList      | $\rightarrow$ | SYMBOL  SymbolList'                           |
| 13  | SymbolList'     | $\rightarrow$ | SYMBOL  SymbolList'                           |
| 14  |                 | $\mid$        | $\epsilon$                                    |
