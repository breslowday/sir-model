## Repository Title
# sir-model
### About This Project

This is a test SIR model based on Ottar's book Epidemics chapter 2.
It's a simple SIR model with births and deaths

```math
\begin{align}
\frac{dS}{dt} &= \mu (N - S) -\beta S \frac{I}{N} \\
\frac{dI}{dt} &= \beta S \frac{I}{N} - \gamma I - \mu I \\
\frac{dR}{dt} &= \gamma I - \mu R
\end{align}
```

```math
\begin{align}
\mu &= \frac{1}{50*52} \\
\beta &= 2 \\
\gamma &= \frac{1}{2} \\\\

N &= 1000 \\
S_0 &= 999.0 \\
I_0 &= 1.0 \\
R_0 &= 0.0
\end{align}
```

### Repository Structure

```
.
├── data/
├── figs/
├── funs/
├── out/
└── src/
```

- `data/` contains ...
- `figs/` contains ...
- `funs/` contains ...
- `out/` contains ...
- `src/` contains ...


### Built With

### Getting Started

### Usage

### License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

### Contact

*Deepit Bhatia*

### Acknowledgements
