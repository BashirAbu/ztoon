# Ztoon Project Status ⚠️

**Ztoon is currently in a very early development stage.**

* Core language features and compiler infrastructure are experimental and subject to significant change.
* Stability, performance, and API design are actively under development.

Use Ztoon with caution in production environments and stay tuned for updates as the project matures.

# Ztoon Language Roadmap

The following features are planned for addition to Ztoon as the language evolves:

1. **Debug Symbols**
   Emit DWARF or PDB debug information to enable source-level debugging in IDEs and debuggers.

2. **Compile-Time Functions**
   Support `const fn`-style functions that are evaluated at compile time for optimized performance and metaprogramming.

3. **Procedural Macros**
   Introduce Rust-like procedural macros for code generation and compile-time transformations.

4. **Full UTF-8 Support**
   Extend the type system and I/O facilities to handle full Unicode strings and characters seamlessly.

5. **SOA (Struct-of-Arrays) Datatypes**
   Provide built-in constructs for struct-of-arrays layouts to improve data locality and performance in numeric and game applications.

6. **Conditional Compilation**
   Implement `#if`, `#ifdef`, or `cfg`-style directives to compile code selectively based on platform, build flags, or custom features.

7. **Core Library**
    Develop a foundational core library that offers essential modules (e.g., data structures, math utilities, string manipulation) tightly integrated with the compiler, serving as the base for higher‑level libraries.

8. **Language Server Protocol (LSP) Support**
    Provide a language server implementation for editor integrations: code completion, diagnostics, and refactoring.

# Introduction

Ztoon is a modern alternative to C designed for simplicity and strong typing. It aims to:

* Provide a familiar C-like syntax with modern enhancements
* Ensure strong, static typing to catch errors at compile time
* Maintain the same ABI as C for seamless interoperability with existing C code and libraries
* Focus on a minimal set of keywords and clear, predictable semantics
* Offer performance comparable to C with minimal runtime overhead
* Support a modular system for organizing code into reusable components
* Include basic generics for type-safe code reuse

Ztoon’s design emphasizes ease of learning for C programmers while introducing modern features that improve safety and productivity.

---

## Creating a Workspace & “Hello, World!” Example

### 1. **Create a workspace**

   ```sh
   ztoon -new hello_world_workspace
   ```
   This command generates the directory `hello_world_workspace/` with:

### 2. **Add your project**

   In `hello_world_workspace/ztoon.yaml`, list your project:

   ```yaml
   projects:
     - hello
        relative_path: hello/
        type: exe
   ```

   > All source files for a project must reside under `<project_name>/src/`.

### 3. **Write the program**

   Create the file `hello_world_workspace/hello/src/hello.ztoon` with:

   ```ztoon
   package main_pkg;
   fn printf(str: readonly i8*, ...) -> i32

   fn main() -> i32 {
       printf("Hello, World!\n")
       ret 0
   }
   ```

   > **Note:** Prototypes (`fn printf(...)`) are primarily used for external functions such as C library calls or forward declarations.

### 4. **Build & Run**

   The compiler executable is `ztoon`.

   * Build the entire workspace:

     ```sh
     ztoon -build
     ```

   * Build a specific project:

     ```sh
     ztoon -build -project hello
     ```

   Binaries are placed in `hello_world_workspace/bin/`. Run the program:

   ```sh
   ./bin/hello
   ```
---

# Building the Ztoon Compiler

Follow these steps to build the Ztoon compiler from source in a platform-agnostic way.

## Prerequisites

* **CMake** 
* **C++20** compatible compiler (e.g., GCC, Clang, MSVC)
* **LLVM** (with development files)
* **LLD** (LLVM linker)
* **Git** (for FetchContent dependencies)

Ensure LLVM and LLD are installed and note their installation directories.

## 1. Clone the Repository

```sh
git clone https://github.com/BashirAbu/ztoon.git
cd ztoon
```

## 2. Configure with CMake

Create a build directory and invoke CMake, pointing it to your LLVM installation:

```sh
mkdir build && cd build
cmake .. \
  -DLLVM_DIR="/path/to/llvm/lib/cmake/llvm" \
  -DLLD_DIR="/path/to/llvm/lib/cmake/lld"
```

* `LLVM_DIR` should point to the directory containing `LLVMConfig.cmake`.
* `LLD_DIR` should point to the directory containing `LLDConfig.cmake`.

## 3. Build

Once configured, build the compiler:

```sh
cmake --build . --config Release
```

* On Unix, this invokes `make` or your chosen generator.
* On Windows, it builds the Visual Studio solution.

---
# Running Tests

After building the compiler and enabling tests, run the Google Test binary directly from the `tests/workspaces/` directory:

```sh
cd tests/workspaces/
../../build\tests\ztests
```
# Workspaces & Compiler Arguments

## Workspace Configuration (`ztoon.yaml`)

At the root of your workspace, define `ztoon.yaml`:

```yaml
workspace:
  name: hello_world_workspace
  projects:
    hello:
      relative_path: "hello"
      type: "exe"
```

* **workspace.name**: the workspace directory name
* **workspace.projects**: maps each project name to its settings:

  * **relative\_path**: path to the project folder
  * **type**: one of:

    * `exe` — an ordinary executable
    * `zlib` — a Ztoon library (recommended for Ztoon-to-Ztoon linking)
    * `static-lib` — a static C-compatible library (for other languages)
    * `shared-lib` — a shared/dynamic C-compatible library (for other languages)

## Project Configuration (`<project>/ztoon.yaml`)

Each project’s `ztoon.yaml` resides in its directory. Example (`hello/ztoon.yaml`):

```yaml
hello:
  type: "exe"       # exe, zlib, static-lib, or shared-lib
  debug_flags:
    opt_level: "o0"
  release_flags:
    opt_level: "o3"
  linker_flags:
    exe_type: "console"     # or "window"
    no_crt: false           # omit CRT if true
    crt_link_type: "static" # or "dynamic"
    entry: "main"           # custom entry point
    native_libs:
      - mylib:
          type: "shared-lib"
          relative_path: "libs/mylib"
```

> **Native Libraries**: Specify the library name without extension (e.g., `mylib`). The compiler will append the correct platform-specific extension (`.a`, `.lib`, `.so`, `.dylib`, etc.) at link time.

> **C Runtime Libraries**: Controlled by `no_crt` and `crt_link_type`. If `no_crt` is `false` (default), Ztoon automatically links the C runtime libraries. The `crt_link_type` setting (`static` or `dynamic`) determines whether the CRT is linked statically or dynamically.

## Available Compiler Flags

* `-new <name>`: create a new workspace
* `-build`: build all projects
* `-build -project <project_name>`: build only the specified project
* `-debug`: select Debug mode (uses `debug_flags`)
* `-release`: select Release mode (uses `release_flags`)
* `-ir`: emit intermediate representation after parsing
* `-v`, `-verbose`: enable verbose output
* `-q`, `-quiet`: suppress informational messages

## Workflow

1. Initialize: `ztoon -new <workspace>`
2. Configure: edit `ztoon.yaml` files
3. Build workspace: `ztoon -build`
4. Build project: `ztoon -build -project <name>`

---

# Basic Syntax & Types

## Comments

* Single-line: `// comment`
* Multi-line: `/* comment */`

## String Literals

Ztoon supports two forms of string literals:

* Standard string literals: double-quoted with escape sequences, e.g.:

  ```ztoon
  message: readonly i8* = "Hello, World!\n"
  ```

* Raw string literals: C++-style `R"(...)"` syntax with no escape processing, e.g.:

  ```ztoon
  raw: readonly i8* = R"(Hello\nWorld)"
  ```

### Escape Characters

Common escape sequences:

* `\n` (newline)
* `\t` (tab)
* `\\` (backslash)
* `\"` (double-quote)
* `\'` (single-quote)

## Numbers

Numeric literals in Ztoon follow strict syntax:

```ztoon
// Integer literals
a_decimal: i32 = 42
a_hex: i32 = 0x2A
a_binary: i32 = 0b101010

// Floating-point literals
pi_value: f64 = 3.14
```

## Variable Declarations

```ztoon
x: i32 = 10
ratio: f64 = 2.718
flag: bool = true
```

## Assignment Statements

Ztoon supports simple and compound assignments:

```ztoon
// Simple assignment
x = x + 5;

// Compound assignments
x += 3;
x -= 2;
x *= 4;
y: f64 = 2.5;
y /= 0.5;

// Bitwise compound
mask: u32 = 0xFF
mask &= 0x0F;
mask |= 0xF0;
mask ^= 0xAA;

// Shift compound
count: u32 = 1;
count <<= 2;
count >>= 1;
```

## Constant Declarations

Use `readonly` for immutables:

```ztoon
pi: readonly f64 = 3.1415926535;
```

---

## Control Flow Statements

Parentheses around conditions and loop headers are optional in Ztoon.

### If / Else

```ztoon
if x > 0 {
    printf("positive\n");
} else if x < 0 {
    printf("negative\n");
} else {
    printf("zero\n");
}
```

### Ternary Expression

```ztoon
result: i32 = x > 0 ? 1 : -1;
```

### Switch Statement

Only integer and enum types; cases can list multiple values with a block:

```ztoon
switch code {
    case 1, 2, 3: {
        printf("one-to-three\n");
    }
    case 4: {
        printf("four\n");
    }
    default: {
        printf("other\n");
    }
}
```

### For Loop

```ztoon
for i: i32 = 0; i < 10; i += 1 {
    printf("%d\n", i);
}
```

### While Loop

```ztoon
while n > 0 {
    n -= 1;
}
```

---

## Functions

Ztoon treats functions as first-class values. You can declare prototypes, define functions, and use anonymous functions.

### Function Declaration (Prototype)

Declare external or forward functions:

```ztoon
fn printf(str: readonly i8*, ...) -> i32;
```

> **Note:** Prototypes are primarily used for external functions such as C calls in static libraries or forward declarations.

### Function Definition

```ztoon
fn add(a: i32, b: i32) -> i32 {
    ret a + b;
}
```

### Return Statements

```ztoon
fn square(x: i32) -> i32 {
    ret x * x;
}
```

### Anonymous Functions (Lambdas)

```ztoon
result: i32 = apply(fn(a: i32) -> i32 {
    ret a * a;
}, 5)
```

### Function Types

```ztoon
fn invoke(f: (fn(i32, i32) -> i32), a: i32, b: i32) -> i32 {
    ret f(a, b);
}

sum: i32 = invoke(add, 2, 3);
```

## Casting

Ztoon requires explicit casts between types using the `as` operator. Implicit conversions are limited to safe, widening integer promotions. The semantic analyzer enforces compatibility at compile time.

```ztoon
// Numeric cast: f32 to f64
pi: f32 = 3.14 as f64;

// Truncating cast: f64 to i32 (fractional part discarded)
r_value: f64 = 3.14;
i_result: i32 = r_value as i32;

// Enum cast: enum to integer
enum color { RED, GREEN, BLUE };
col: color = RED;
col_val: i32 = col as i32;  // underlying integer value 0

// Boolean cast: numeric to bool (true if value != 0)
zero: i32 = 0;
flag_zero: bool = zero as bool;  // false

pos: i32 = 5;
flag_pos: bool = pos as bool;    // true

neg: i32 = -1;
flag_neg: bool = neg as bool;    // true
```

---

## Operators

Ztoon supports most standard C operators with identical precedence and associativity. Examples:

### Unary Operators

```ztoon
pos: i32 = +5;
neg: i32 = -5;
flag_not: bool = !false;
bits_not: u32 = ~0u32;
ptr: i32* = &x;
val: i32 = *ptr;
size: u64 = sizeof(i32);
```

### Arithmetic

```ztoon
sum: i32     = 1 + 2;
diff: i32    = 5 - 3;
product: i32 = 4 * 7;
quot: i32    = 10 / 2;
modulo: i32  = 10 % 3;
```

### Bitwise

```ztoon
b: u32 = (0b1010 << 1) & 0x0F;
b |= 0b0101;
b ^= 0b0011;
b >>= 2;
```

### Relational & Equality

```ztoon
lt: bool = a < b;
ge: bool = a >= b;
eq: bool = a == b;
ne: bool = a != b;
```

### Logical

```ztoon
logic: bool = (a > 0) && (b < 0) || true;
```

### Assignment

```ztoon
x: i32 = 1;
x += 4;
x -= 2;
x *= 3;
x /= 2;
x %= 2;
```


## Global Variables

Global variables in Ztoon are declared at the top level, outside of any function or type definition. They are zero-initialized by default if no initializer is provided.

### Declaration & Initialization

```ztoon
// A global 32-bit integer initialized to 100
count: i32 = 100;

// A global boolean, zero-initialized to false
flag: bool;

// A global array, zero-initialized
buffer: u8[256];
```

### Usage

Global variables can be read and written from any function:

```ztoon
fn increment() {
    count += 1;
}

fn main() -> i32 {
    increment();
    printf("count = %d\n", count);
    ret 0;
}
```

# The `defer` Statement

Ztoon supports a `defer` statement for scheduling an expression to execute when the nearest enclosing block exits. This is useful for cleanup or post‑block actions.

## Syntax

```ztoon
defer <expression>;
```

* The deferred expression is executed after the surrounding `{ ... }` block finishes, regardless of how the block is exited.
* Multiple `defer` statements in the same block execute in last‑in, first‑out (LIFO) order.

## Example

```ztoon
package mainPkg;

fn main() -> i32 {
  
    num: i32 = 0;

    {
        // num == 0

        defer num++;

        // num == 0
    }

    // After the inner block, num has been incremented once
    // num == 1

    ret 0;
}
```

## Arrays

Ztoon provides first-class support for fixed-size arrays, offering C-like semantics with strong static typing.

### Declaration & Initialization

```ztoon
arr: i32[10];
c_arr: i32* = arr as i32*;
```

### Typed vs Raw Pointers

```ztoon
// Pointer to fixed-size array (includes size in type)
arr: i32[10];
arr_ptr: i32[10]* = &arr;

// Raw pointer to element type (size not tracked)
arr_ptr_raw: i32* = arr as i32*;
```

* `i32[10]*` retains the array length in its type, enabling compile-time size checks in operations involving `arr_ptr`.
* `i32*` is a raw pointer to `i32`, analogous to C’s `int*`; it does **not** track array length, so bounds must be managed manually.

---

### Indexing

Access and assign elements using bracket syntax:

```ztoon
first: i32 = nums[0];
nums[2] = 42;
```

> The subscript operator `[...]` works on both arrays and pointers (i.e. `ptr[i]` is equivalent to `*(ptr + i)`).

### Array Operations

* **Length**: Compile-time constant via `array.len`.
* **Iteration**:

  ```ztoon
  for i: i32 = 0; i < nums.len; i += 1 {
      printf("%d\n", nums[i]);
  }
  ```
* **Copying**: Assigning arrays copies all elements:

  ```ztoon
  other: i32[5] = nums;
  ```
* **Slices**: Use pointer + length:

  ```ztoon
  slice_ptr: i32* = &nums[1];
  slice_len: i32 = 3;
  for i: i32 = 0; i < slice_len; i += 1 {
      printf("%d\n", slice_ptr[i]);
  }
  ```

### Multidimensional Arrays

```ztoon
matrix: f32[2][3] = {
    {1.0, 2.0, 3.0},
    {4.0, 5.0, 6.0},
};

// Access row 1, col 2
elem: f32 = matrix[1][2];
```

### C Array Compatibility

To interoperate with C, cast an array to a pointer to its first element:

```ztoon
arr: i32[10];
c_arr: i32* = arr as i32*;
```

## Struct Types

Structs in Ztoon are user-defined aggregate types with fields and methods. Struct definitions end with a semicolon.

### Declaration with Methods

```ztoon
struct Point {
    x: f64 = 1.0; //default value
    y: f64 = 3.0; //default value

    fn magnitude(self) -> f64 {
        ret sqrt(self.x * self.x + self.y * self.y);
    }

    fn translate(self, dx: f64, dy: f64) -> Point {
        point: Point = {self.x + dx, self.y + dy};
        ret point;
    }

    fn create(x: f64, y: f64) -> Point {
        point: Point = {x, y};
        ret point;
    }
};
```

### Method Receiver Variants

When declaring methods, `self` can be specified in three ways:

* `self`: method receives a copy of the struct values.
* `self*`: method receives a pointer to the struct, allowing in-place modification.
* `readonly self` or `readonly self*`: method receives a copy or reference of the struct, and its fields cannot be modified.

### Initialization

```ztoon
p: Point = {1.0, 2.0};
```

### Accessing Fields & Calling Methods

```ztoon

// Initialize a struct with field values
p: Point = {1.0, 3.0};

// Reset all fields to zero (default initialization)
p = {};

// Copy one instance to another
p2: Point = {44.0, 33.0};
p = p2;

// Obtain a pointer to a struct
ptr: Point* = &p;

// Access fields with the '.' operator (works for both variables and pointers)
x_val: f64 = p.x;
y_val: f64 = ptr.y;

// Write field
p.y = 3.0;

// Call method by value receiver
mag: f64 = p.magnitude();

// Call method by pointer receiver
p2: Point = p.translate(5.0, -2.0);

// Call static-like method
p3: Point = Point::create(1.0, 2.0);
```

## Union Types

Unions in Ztoon define types whose fields share memory. A `union` declaration ends with a semicolon:

```ztoon
union Value {
    i: i32;
    f: f32;
    b: bool;
};
```

### Initialization

Unions cannot be initialized inline at declaration. Declare an uninitialized union and then assign to one of its fields:

```ztoon
v: Value;
v.i = 10;
```

### Access & Assignment

```ztoon
// Read active field
num: i32 = v.i;

// Overwrite with another field
v.f = 3.14;

// Copy union
v2: Value = v;

// Pointer to union
vp: Value* = &v;

// Access via pointer
val: i32 = vp.i;
```

## Enum Types

Enums in Ztoon define named integer constants with an explicit underlying integer type.

### Example

```ztoon
// Underlying type is i32; RED=0, GREEN=1
enum Color: i32 {
    RED = 0,
    GREEN,
    BLUE,   // = 2
};
```

You can also assign explicit values:

```ztoon
enum ErrorCode: i32 {
    OK        = 0,
    NotFound  = 404,
    Internal  = 500,
};
```

### Casting

Enums can be converted to and from integers:

```ztoon
c: Color = Color::RED;
val: i32   = c as i32;    // 0
c2: Color = 1 as Color;     // GREEN
```

### Usage in switch

```ztoon
switch c {
    case Color::RED, Color::BLUE:   { /* ... */ }
    case Color::GREEN: { /* ... */  }
    default:   { /* ... */ }
}
```
### Bitwise Operations

Enums can participate in bitwise operations via casting to their underlying integer type:

```ztoon
// Define a flags enum
enum Permissions: u32 {
    READ    = 1,
    WRITE   = 2,
    EXECUTE = 4,
};

// Combine flags
perms: Permissions = Permissions::READ | Permissions::WRITE;

// Test a specific flag
has_write: bool = (perms & Permissions::WRITE) != 0;
```

### Type declaration within Function Bodies

In Ztoon, you can declare `struct`, `enum`, and `union` types directly inside function bodies to limit their scope. Examples:

```ztoon
fn example() -> i32 {
    // Struct declaration inside a function
    struct LocalPoint {
        x: f64;
        y: f64;
    };

    // Enum declaration inside a function
    enum Status: i32 {
        OK = 0,
        Error = 1,
    };

    // Union declaration inside a function
    union Value {
        i: i32;
        f: f32;
    };

    // Use the local struct
    p: LocalPoint = { .x = 1.0, .y = 2.0 };

    // Use the local enum
    s: Status = OK;

    // Use the local union
    v: Value;
    v.i = 42;

    ret p.x as i32 + s as i32 + v.i;
}
```
### Anonymous Structs & Unions

Ztoon supports embedding anonymous `struct` and `union` types within aggregate definitions to create compact, convenient layouts.

```ztoon
struct vector2 {
    union {
        elements: f32[2];
        struct {
            x: f32;
            y: f32;
        };
    };
};

fn main() -> i32 {
    v: vector2;
    v.x = 12.0;           // v.elements[0] == v.x
    v.elements[1] = 5.4;  // v.elements[1] == v.y
    ret 0;
}
```

**Key Points:**

* Anonymous `struct` and `union` fields cannot have default initializers; they are zero-initialized.
* Anonymous `struct` types cannot declare methods.
* Only anonymous `union` types may be declared inside `struct` definitions.
* Only anonymous `struct` types may be declared inside `union` definitions.

## Generics

Ztoon’s generics system enables compile-time type-safe code reuse through explicit type parameters. Generics are monomorphized, producing specialized versions for each instantiation. Monomorphization is the process where the compiler generates concrete, type-specific implementations for generic code by replacing type parameters with actual types, eliminating generic overhead at runtime.

---

### Declaration Syntax

* **Functions:** Prefix the function name with `fn<[T, ...]>` where `T` is a type parameter:

  ```ztoon
  // Swaps two values of any type
  fn<[T]> swap(a: T*, b: T*) {
      temp: T = *a;
      *a = *b;
      *b = temp;
  }
  ```

* **Unions (and Structs):** Include `<[T, ...]>` immediately after the type name:

  ```ztoon
  // A generic 2D vector with components of possibly different types
  union Vec<[X, Y]> {
      elements: X[2];
      struct {
          x: X;
          y: Y;
      };
  };
  ```

---

### Instantiation & Calls

When invoking, specify concrete types inside `<[ ... ]>`:

```ztoon
// Instantiate swap for integers
i1: i32 = 10;
i2: i32 = 20;
swap<[i32]>( &i1, &i2 );

// Instantiate Vec with f32 and i32
v: Vec<[f32, i32]>;
v.x = 1.5;
v.y = 2;
```

* Generics support multiple parameters: `<[T, U]>`.
* The compiler generates a specialized `swap_i32`, `Vec_f32_i32`, etc.

---

### Key Characteristics

* **Explicit Parameters:** Requires `<[...]>` for clarity.
* **Compile-Time Checks:** Type mismatches are caught during monomorphization.
* **Applies To:** Functions, `struct`, and `union` definitions.
* **No Implicit Bounds:** Currently lacks trait or constraint syntax.

---

# Packages & Libraries

Ztoon’s packaging system organizes code into reusable modules (packages) and links both Ztoon and native libraries seamlessly. Below is a generic example.

## Source File Packaging

Every Ztoon source file must start with a 'package <name>' declaration at the top. Files sharing the same package name are compiled together as a single unit. This allows organizing code logically within packages:

```ztoon
// packages/core/src/logger.ztoon
package core;

fn log(msg: readonly i8*) {
    // ...
}

// packages/core/src/helpers.ztoon
package core;

fn helper() {
    // ...
}
```

## Workspace Structure

```
my_workspace/
├── ztoon.yaml           # workspace configuration
├── packages/            # reusable modules
│   ├── core/            # core utilities
│   │   └── src/
│   ├── utils/           # helper functions
│   │   └── src/
│   └── net/             # networking package
│       └── src/
├── apps/                # executable projects
│   └── my_app/
│       └── src/
└── libs/                # third-party C libraries
    └── external_lib/
        └── include/, lib/
```

## Consuming Packages

In your application (`my_app`), import package modules:

```ztoon
// apps/my_app/src/main.ztoon
import core;
import utils;
import net;

fn main() -> i32 {
    core::init();
    data: utils::Data = utils::make_data();
    conn: net::Conn = net::connect("example.com");
    core::log("App started
");
    ret 0;
}
```

The compiler builds all `zlib` packages before linking the `exe` project.

## Import Syntax

Ztoon supports several forms of import statements:

* `import <lib_name>::<pkg_name>;`
  Imports a specific package from a library namespace.
* `import <lib_name>;`
  Imports all public packages under the given library.
* `import <pkg_name>;`
  Imports a local package in the workspace.

If you prefer not to use `import`, you can reference packages or members directly:

```ztoon
lib::pkg::member;  // e.g., core::log
```

> Only members declared as `pub` in their package are accessible outside that package; all others are private by default.

## Declaring Public Members

Use the `pub` keyword to expose structs and functions:

```ztoon
// packages/utils/src/math.ztoon
package utils;

pub struct Point {
    x: f64;
    y: f64;
};

pub fn add(x: i32, y: i32) -> i32 {
    ret x + y;
}
```

Non-`pub` items remain private to their package and cannot be imported or accessed externally.

## Linking Native Libraries Linking Native Libraries

For C libraries under `libs/`, reference them in your app’s `ztoon.yaml`:

```yaml
my_app:
  type: "exe"
  linker_flags:
    native_libs:
      - external_lib:
          type: "shared-lib"
          relative_path: "libs/external_lib"
```

The compiler appends the correct platform extension (e.g., `.so`, `.dll`, `.dylib`).

---
# Dependencies

The Ztoon project uses the following third-party libraries:

* **yaml-cpp**: Parses `ztoon.yaml` files for workspace and project configurations.
* **LLVM**: Provides the backend infrastructure (code generation, optimizations, and linking via LLD).
* **Google Test (GTest)**: Framework for unit and integration tests in the `tests/` directory.
