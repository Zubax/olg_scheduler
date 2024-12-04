# olga_scheduler

[![Main Workflow](https://github.com/zubax/olga_scheduler/actions/workflows/main.yml/badge.svg)](https://github.com/zubax/olga_scheduler/actions/workflows/main.yml)

Generic single-file implementation of scheduler suitable for deeply embedded systems.
"OLGa" is a reference to the fact that it has a logarithmic asymptotic complexity.
**Simply copy `olga_scheduler.hpp` into your project tree and you are ready to roll.**
The only dependency is the CAVL (`cavl.hpp`) header-only library
(>= [v3.1.0](https://github.com/pavel-kirienko/cavl/tree/3.1.0)).

The usage instructions are provided in the comments.
The code is fully covered by manual tests with full state space exploration.

For development-related instructions please refer to the CI configuration files.
To release a new version, simply create a new tag.

<!--suppress CheckImageSize, HtmlDeprecatedAttribute -->
<p align="center">
    <img src="/docs/St_Olga_by_Nesterov_in_1892_(cropped).jpg" alt="Olga of Kiev" width=256>
</p>