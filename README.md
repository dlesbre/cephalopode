# Cephalopode

This repository contains the source code for the Cephalopode processor. Cephalopode is a custom processor designed for IoT. It aims to be a very low power hardware device directly aimed at running a high-level functional language.

Cephalopode is developed as part of the [Octopi Project](https://octopi.chalmers.se/).

This repository also contains the Bifröst compiler, a synthesis tool used to create RTL code for Cephalopode from the eponimous language.

**Contents:**
- [Dependencies](#dependencies)
	- [Bifröst](#bifröst)
	- [Cephalopode](#cephalopode)
- [References](#references)

The main feature of Cephalopode ar

## Dependencies

### Bifröst

The Bifröst compiler is written in Haskell. It requires:
- The Haskell platform
- bnfc ("The BNF Converter", on Ubuntu: `apt install bnfc`)
### Cephalopode

Building and running Cephalopode requires [Voss II](https://github.com/TeamVoss/VossII), which can be downloaded and installed from the linked repository.

Additionnaly, some parts of the processor were designed using [Stately](https://github.com/popje-chalmers/stately), a custom FSM editor. The `.fsm` files should be read with it. We have also provided the exported versions of these files as well (with extension `.fsm.fl`)
 It can also be downloaded and installed by following the link.

## References

1. Jeremy Pope and Jules Saget and Carl-Johan H. Seger, *Cephalopode: A custom processor aimed at functional language execution for IoT devices* in *18th ACM/IEEE International Conference on Formal Methods and Models for System Design*, MEMCODE 2020, Jaipur, India. [https://doi.org/10.1109/MEMOCODE51338.2020.9315094](https://doi.org/10.1109/MEMOCODE51338.2020.9315094).
2. Jeremy Pope and Jules Saget and Carl-Johan H. Seger, *Stately: An FSM Design Tool* in *18th ACM/IEEE International Conference on Formal Methods and Models for System Design*, MEMCODE 2020, Jaipur, India. [https://doi.org/10.1109/MEMOCODE51338.2020.9315130](https://doi.org/10.1109/MEMOCODE51338.2020.9315130).
