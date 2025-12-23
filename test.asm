.386                              ; 使用 32 位指令集
.model flat, stdcall              ; 使用平面内存模型和标准调用约定
option casemap:none               ; 区分大小写

include msvcrt.inc                ; 引入 C 运行时库
includelib msvcrt.lib             ; 引入 msvcrt 库文件

include kernel32.inc              ; 引入 Kernel32 库
includelib kernel32.lib           ; 引入 Kernel32 库文件

.data                              ; 数据段开始
    fmt db "the int value is : %d", 0                 ; 格式化字符串，%d 表示整数
    num dd 5                       ; 定义一个整型数，值为 5

.code                              ; 代码段开始

printf PROTO C : ptr byte, : VARARG  ; 声明 printf 函数的原型

_start:                            ; 入口点
    ; 调用 printf 函数输出整型数字 5
    invoke printf, offset fmt, num  ; 调用 printf，传入格式化字符串和整数参数

    ; 退出程序
    call exit_program               ; 调用退出程序的函数

; 退出程序函数
exit_program proc
    invoke ExitProcess, 0           ; 调用 Windows API 退出程序
    ret
exit_program endp

END _start                          ; 程序结束
