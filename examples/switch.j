;[
.   switch tests ;[ also, nested comments ];
];
            class           "Switch", "java/lang/Object",
            flags           public, super, final, synthetic
            version         0, 49,

<init>:     method          "()V"
            stack           0x01
            locals          0x01
            aload_0
            invokespecial   [method_ref "java/lang/Object", "<init>", "()V"]
            return

main:       method          "([Ljava/lang/String;)V"
            flags           public, static,
            stack           0x02
            locals          0x01
            iconst_1
            invokestatic    [method_ref this, "lookup", "(I)I"]
            getstatic       [field_ref  "java/lang/System", "out", "Ljava/io/PrintStream;"]
            swap
            invokevirtual   [method_ref "java/io/PrintStream", "println", "(I)V"]
            return

table:      method          "(I)I"
            flags           static,
            stack           0x01
            locals          0x01
            iload_0
            tableswitch     \
                jmp_d,      \
                0, 4,       \
                jmp_0,      \
                jmp_1,      \
                jmp_2,      \
                jmp_3,      \
                jmp_4,
jmp_0:      bipush          55
            goto            exit
jmp_1:      bipush          57
            goto            exit
jmp_2:      bipush          58
            goto            exit
jmp_3:      bipush          59
            goto            exit
jmp_4:      bipush          60
            goto            exit
jmp_d:      iconst_m1
            goto            exit
exit:       ireturn

lookup:     method          "(I)I"
            flags           static,
            stack           0x01
            locals          0x01
            iload_0
            lookupswitch    \
                l_jmp_d,    \
                0, l_jmp_0, \
                1, l_jmp_1,
l_jmp_0:    bipush          91
            goto            l_exit
l_jmp_1:    bipush          92
            goto            l_exit
l_jmp_d:    bipush          90
            goto            l_exit
l_exit:     ireturn