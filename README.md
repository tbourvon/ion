# ion

ion is a multi-purpose, statically-typed programming language with a focus on productivity.

## Specification and examples

### Hello world

Here is a classic hello world in ion:
```
import "std/io"

func main() {
	std::io::println("Hello world!")
}
```

Any ion program starts at the main entry point and then executes linearly.

### Variable declaration
The syntax for variable declarations is:
```
var my_variable: string = "Hello"
```

The assignment part is optional only if the type is a primitive with a default value:

| Type   | Default value |
|--------|---------------|
| string | ""            |
| int    | 0             |
| bool   | false         |
| char   | '\0'          |

### Types

Along with the 4 primitive types previously cited and custom structs, the built-in types are:

| Type      | Syntax               |
|-----------|----------------------|
| array     | []type               |
| map       | [key_type]value_type |
| const ref | &type                |
| mut ref   | @type                |

### Var assignment
```
my_variable = "new text"
my_array[4] = "another value"
my_array[] = "new item in array"
my_ref = @some_var
*my_ref = "new content"
```

### Func declaration
```
func my_function(param1: Type1, param2: Type2 = "default_value") -> ReturnType {
	...
}
```

### Struct declaration
```
struct MyStruct {
	field1: Type1,
	field2: Type2
}
```

### Struct init
```
var my_var: MyStruct = new MyStruct {
	field1: value1,
	field2: value2
}
```

### Func call
```
var my_return: ReturnType = my_function(arg1, arg2)
```

### Struct field access
```
std::io::println(my_struct.field1)
```

### If/else if/else
```
if condition {
	...
} else if other_condition {
	...
} else {
	...
}
```

### While
```
while condition {
	...
}
```

### For..in
```
for item in my_array {
	...
}

for char in my_string {
	...
}
```

### Imports
```
import "relative/path/to/file"

func main {
	relative::path::to::file::my_function()
}
```

### Standard library

#### Global/built-in
 * `print(any)`
 * `readln() -> string`

#### std::io
 * `std::io::println(string)`
 * `std::io::read_int() -> int`

#### std::conv
 * `std::conv::char_to_int(char) -> int`
 * `std::conv::str_to_int(string) -> int`

#### std::math
 * `std::math::pow(int, int) -> int`

## Usage
```
ion my_source_file.ion
```

Note: for now, your files have to be placed alongside the executable, as well as the std folder.
