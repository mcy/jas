`jas` - a JVM bytecode assembler
=====

Note: this project is still far from complete, and may not implement
all of the classfile specification correctly.

`jas` is a simple assembler for the JVM `classfile` format. Classes 
are specified in "Sun syntax" (a pun on Intel syntax) described below. 

To build, simply clone and run `cargo build` (this assumes you have a 
valid Rust installation).

## Sun syntax

An assembly file (prefixed with `.j`, but `jas` doesn't actually care).
consists of zero or more "source lines", of the form

```
label: instruction arg0, arg1, ... ; comment
```

The `label` is optional, as is the comma on the last argument. Comments
start with a semicolon (`;`).

There are four kinds of instructions: item, meta, constant, and
code instructons:
 
- An item instruction indicates the beginning of a class, field, 
or method. 
- A meta instruction declares metadata attached to the most 
recently declared item.
- A constant instruction declares an element of the constant pool 
of the most recently declared class.
- A code instruction declares an opcode and arguments inside a 
method.

Instructions may take zero or more arguments, which may
be a label, a literal string, a numeric expression, or an
address expression of the form `[instruction arg0, arg1, ...]`,
where `instruction` is (for now) limited to constant instructions.

Many instructions will interpret string literals differently,
often automatically generating constants. For example,

```
; the following pairs are equivalent
ldc     "Hello, World!"
ldc     [string "Hello, World!"]
 
new     "java/lang/String"
new     [class_ref "java/lang/String"]
```

### Item instructions

- `class this, super` - declares a new class named `this`, with
superclass `super`.
- `field desc, name` - declares a new field `name` with signature
`desc` (given in classfile style, e.g. `Ljava/lang/Object;`).
Name may instead be given by a label: `name: field desc`.
- `method desc, name` - declares a new method `name` with signature
`desc` (given in classfile style, e.g. `(Ljava/lang/Object;)V`).
Name may instead be given by a label: `name: method desc`.

### Meta instructions

- `impl interface` - specifies that a class implements `interface`.
- `verson minor, major` - specifies the classfile version.
- `flags args...` - specifies flags, such as `public`, `final`, and `synthetic`.
- `stack num` - specifies the max stack size for a method.
- `locals num` - specifies the number of local variables used in a method.
- `catch start, end, handler, ty` - specifies an exception handler
for a method, active from `start` to `end`, and jumping to `handler`
in the event of a thrown `ty`. Order is important.
- `const_val const` - specifies the initial value of a field.
- `source file` - specifies the source file the class was supposedly
compiled from.
- `attr name, bytes` -  specifies a custom attribute named `name`
with data contents given by the base64 string `bytes`.

### Constant instructions

- `class_ref class` - specifies a reference to a class
- `field_ref field` - specifies a reference to a field
- `method_ref method` - specifies a reference to a method
- `imethod_ref method` - specifies a reference to an interface method
- `string val` - specifies a `java.lang.String` literal.
- `int val` - specifies an `int` literal.
- `long val` - specifies a `long` literal.
- `float val` - specifies a `float` literal.
- `double val` - specifies a `double` literal.
- `name_and_type name, desc` - specifies a name/descriptor pair,
for use in `*_ref` constants.
- `method_handle type, method` - specifies a `java.lang.invoke.MethodHandle` 
literal
- `method_type desc` - specifies a method descriptor.
- `dynamic_target boostrap, name, desc` - specifies a target for an 
`invokedynamic` instruction.

### Code instructions

Most instructions are code instructions. We use the same mnemonics
for them as Sun did, which can be found
 [here](https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html).
 

