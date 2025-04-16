# ReplaceAbsoluteAddress

This project uses CMake to build an application that replaces absolute addresses. It links against Clang and LLVM 19 libraries

## Building the Project

1. **Clone the repository and enter the project directory:**

   ```bash
   git clone https://github.com/MH2033/llvm-abs-addr-removal.git
   cd llvm-abs-addr-removal
   ```

2. **Create and enter a build directory:**

   ```bash
   mkdir -p build && cd build
   ```

3. **Configure the project with CMake:**

   ```bash
   cmake .. -G Ninja
   ```

4. **Build the project:**

   ```bash
   ninja
   ```

   This will generate the executable `replace-absaddr` in the build directory.

## Running the Application

From the build directory, run the executable:

```bash
./replace-absaddr [input_file] -o [output_file]
```

You can also add `--debug-only="abs-addr-replace"` option to see the debug messages.

## Running the Tests

To run the test execute the following command after build:

```bash
./replace-absaddr ../test/device_driver/test.c -o out.c
```
