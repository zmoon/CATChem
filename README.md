# Configurable ATmospheric Chemistry

This is the repository for the Configurable ATmospheric Chemistry modelling component.

## Development

If you don't have the command-line tool `pre-commit` available, [install it](https://pre-commit.com/#install).

Install the pre-commit hooks with

```
pre-commit install --install-hooks
```

Now, some checks and auto-formatting will run automatically when you commit.

To test the build locally, first configure, using `FC` to specify the compiler you want to use, then build. For example:

```
FC=gfortran-12 cmake -B build
```
```
cmake --build build -j
```

To clean, you can use

```
cmake --build build --target clean
```

or remove the build directory (`./build`).
