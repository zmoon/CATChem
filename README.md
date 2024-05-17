# Configurable ATmospheric Chemistry

This is the repository for the Configurable ATmospheric Chemistry modelling component.

## Development

### pre-commit

If you don't have the command-line tool `pre-commit` available, [install it](https://pre-commit.com/#install).

Install the pre-commit hooks with

```
pre-commit install --install-hooks
```

Now, some checks and auto-formatting will run automatically when you commit.

> [!NOTE]
> For source files with their own formatting that we don't intend to modify (or only modify slightly),
> e.g. "vendored" modules, we generally don't want `findent` to be applied.
> For such files, update the `exclude` section in `.pre-commit-config.yaml` accordingly.

> [!TIP]
> ```
> pre-commit run --all-files
> ```
> can be used to check the pre-commit config
> (e.g. to make sure that the `findent` `exclude` section is working as you intended)
> or to catch up if commits were made without pre-commit or the pre-commit config was updated.

### Build

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

### Test

To run the tests, after building, use

```
ctest --test-dir build/tests
```

There [are options](https://cmake.org/cmake/help/book/mastering-cmake/chapter/Testing%20With%20CMake%20and%20CTest.html#testing-using-ctest) for selecting specific tests.

Edit `tests/CMakelists.txt` to add new tests.
