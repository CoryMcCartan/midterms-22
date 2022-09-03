# Federal Election Predictions 2022
#### Cory McCartan
<img src="cover.jpg" style="width: 100%;">

## About
A dynamic Bayesian model to forecast the 2022 U.S. House elections.

## Model structure

```mermaid
graph TD
    mod_firms[<font size=5>FIRMS]:::model --> |Prior on firm error| mod_natl[<font size=5>NATIONAL INTENT]:::model
    mod_fund[<font size=5>FUNDAMENTALS]:::model --> |Prior on E-day intent| mod_natl
    d_ret([Historical<br />House returns]):::data -.-> mod_race
    mod_natl --> |Covariate| mod_race[<font size=5>OUTCOMES]:::model
    d_gen([Historical generic<br />ballot polling]):::data -.-> mod_firms
    d_fund([Historical economic<br />and approval data]):::data -.-> mod_fund
    d_22([2022 partisanship<br />and incumbency]):::data -.-> mod_race
    classDef model fill:#aa2,stroke:#000,font-size:16pt,font-weight:bold
    classDef data fill:#efeff4,stroke#aaa,line-height:1.5,font-size:9pt
```

## File structure
- Code for all the analyses in [`R/`](R/)
<!-- - Data, along with details on data sources, in [`data/`](data/) -->
