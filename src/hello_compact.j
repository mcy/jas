class           0x0e, 0x0f,
flags           public, super, final, synthetic
version         0, 51,
                                            ; 0x00
utf8            "Hello"
utf8            "java/lang/Object"
utf8            "<init>"
utf8            "()V"                       ; 0x04
utf8            "main"
utf8            "([Ljava/lang/String;)V"
utf8            "java/lang/System"
utf8            "out"                       ; 0x08
utf8            "Ljava/io/PrintStream;"
utf8            "Hello, World!"
utf8            "java/io/PrintStream"
utf8            "println"                   ; 0x0c
utf8            "(Ljava/lang/String;)V"

class_ref       0x01
class_ref       0x02
class_ref       0x07                        ; 0x10
class_ref       0x0b

name_and_type   0x03, 0x04
name_and_type   0x08, 0x09
name_and_type   0x0c, 0x0d                  ; 0x14

field_ref       0x10, 0x13

method_ref      0x0f, 0x12
method_ref      0x11, 0x14

string          0x0a                        ; 0x18

method          0x04, 0x03
stack           0x01
locals          0x01
aload_0
invokespecial   0x16
return

method          0x06,   0x05
flags           public, static, final
stack           0x02
locals          0x01
getstatic       0x15
ldc             0x18
invokevirtual   0x17
return