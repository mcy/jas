;
; loop tests
;
                class           "Loops", "java/lang/Object",
                flags           public, super,
                version         0, 49,

<init>:         method          "()V"
                stack           0x01
                locals          0x01
                aload_0
                invokespecial   [method_ref "java/lang/Object", "<init>", "()V"]
                return

main:           method          "([Ljava/lang/String;)V"
                flags           public, static,
                stack           0x01
                locals          0x01
                bipush          10
                invokestatic    [method_ref this, "loopNTimes", "(I)V"]
                return

print_ref:      method_ref      this, "print", "(Ljava/lang/String;)V"
print:          method          "(Ljava/lang/String;)V"
                flags           static,
                stack           0x02
                locals          0x01
                getstatic       [field_ref "java/lang/System", "out", "Ljava/io/PrintStream;"]
                aload_0
                invokevirtual   [method_ref "java/io/PrintStream", "println", "(Ljava/lang/String;)V"]
                return


loopForever:    method          "()V"
                flags           static,
                stack           0x01
                locals          0x00
loop:           ldc             [string "loop!"]
                invokestatic    print_ref
                goto            loop
                return

loopNTimes:     method          "(I)V"
                flags           static,
                stack           0x02
                locals          0x01
                iload_0
while_start:    dup
                ifeq            while_break
                ldc             [string "loop!"]
                invokestatic    print_ref
                iconst_1
                isub
                goto            while_start
while_break:    return