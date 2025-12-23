.386
.model flat,stdcall
option casemap:none

include msvcrt.inc
includelib msvcrt.lib
include kernel32.inc
includelib kernel32.lib

printf PROTO C:ptr sbyte,:VARARG

.stack 4096

.data
board_width BYTE 10 ;棋盘宽度
board_height BYTE 10 ;棋盘高度
mine_count BYTE 10 ;地雷数量

largeVal QWORD 0              ; 用于存储查询的高精度计数器值

board_grid BYTE 400 DUP(0); ;棋盘格子数据,每个格子4字节 
game_state BYTE 0 ;0-未开始 1-进行中 2-失败 3-胜利

szMsg BYTE "Welcome to Minesweeper!",0ah,0
MsgSeed BYTE "Random Seed is %d",0ah,0

MsgTest_random BYTE "random() generated: %d",0ah,0
MsgTest_get_time_seed BYTE "get_time_seed() returned: %d",0ah,0
MsgTest_random_mod_100 BYTE "random_mod_100() returned: %d",0ah,0
MsgPlaceMines BYTE "Placing Mines In %d",0ah,0

MsgNewLine BYTE 0ah,0

MsgCell_info_mid BYTE "%d %d %d %d,",0
MsgCell_info_end BYTE "%d %d %d %d",0   ;放在最后一个不加逗号

seed DWORD ? ;随机数种子
location DWORD ? ;地雷埋放位置

.code
;获取系统时间并用作种子
get_time_seed PROC C
    INVOKE QueryPerformanceCounter, OFFSET largeVal
    MOV EAX, DWORD PTR largeVal
    AND EAX, 07FFFFFFFh ;确保种子为正数
    MOV seed, EAX
    RET
get_time_seed ENDP

;伪随机数生成函数（线性同余法）
random PROC C
    MOV EAX, seed
    IMUL EAX, 041C64E6DH ;乘法常数
    ADD EAX, 03039H       ;增量常数
    XOR EDX, EDX
    MOV ECX , 07FFFFFFFh    ;模数=2^31-1
    DIV ECX               ;取模
    MOV seed, EDX           ;余数作为种子
    MOV EAX, EDX           ;返回值
    RET
random ENDP

;伪随机数生成函数（取模100）
random_mod_100 PROC C
    CALL random
    XOR EDX, EDX
    MOV ECX, 100
    DIV ECX
    MOV location, EDX
    MOV EAX, EDX ;返回值为0~99
    RET
random_mod_100 ENDP

;随机布雷
r_places_mines PROC C
    LOCAL Number:BYTE

    INVOKE get_time_seed

    MOV CL, mine_count
    MOV Number,CL   ;保存地雷数量

loop_random:
    INVOKE random_mod_100    ;调用random_mod_100获取0~99的随机数
    MOV EAX, location        ;EAX = location (0-99)
    SUB EAX, 1           ;调整为0-99范围
    ;检查该位置是否已放置地雷
    MOV EBX, OFFSET board_grid  ;board_grid地址
    MOV DL, [EBX + EAX*4]      ;DL存放该格子的一个字节
    CMP DL, 1            ;1表示已放置地雷
    JE loop_random       ;如果已放置地雷，重新生成位置

    ;如果该位置未放置地雷，则放置地雷
    MOV DL, 1            ;1表示放置地雷
    MOV [EBX + EAX*4], DL

    PUSH EAX

    INVOKE printf, OFFSET MsgPlaceMines, EAX
    POP EAX

    SUB Number, 1
    CMP Number, 0
    JNE loop_random
    RET
r_places_mines ENDP


;获取并显示当前游戏状态
get_game_state PROC C
    LOCAL bMine:BYTE, bOpen:BYTE, bFlag:BYTE, bAroundMines:BYTE
    ;bMine:是否有地雷, bOpen:是否打开, bFlag:是否插旗, bAroundMines:周围地雷数
    PUSH EBX
    PUSH ESI
    PUSH EDI 

    MOV ESI, OFFSET board_grid ;board_grid地址
    XOR EDI , EDI         ;row = 0

row_loop:
    XOR EBX, EBX          ;col = 0

col_loop:
    ;offset = row * 40 + col * 4
    IMUL EAX, EDI, 40  ;row*40
    LEA EAX, [EAX + EBX*4] ;offset

    MOV AL, [ESI + EAX] ;取出格子数据的第一个字节
    MOV bMine, AL
    MOV AL, [ESI + EAX +1]; 取出格子数据的第二个字节
    MOV bOpen, AL
    MOV AL, [ESI + EAX +2]; 取出格子数据的第三个字节
    MOV bFlag, AL
    MOV AL, [ESI + EAX +3]; 取出格子数据的第四个字节
    MOV bAroundMines, AL

    CMP EBX, 9
    JE last_in_row
    
    INVOKE printf, OFFSET MsgCell_info_mid, bMine, bOpen, bFlag, bAroundMines
    JMP after_print
last_in_row:
    INVOKE printf, OFFSET MsgCell_info_end, bMine, bOpen, bFlag, bAroundMines
after_print:

    INC EBX
    CMP EBX, 10
    JL col_loop

    INVOKE printf, OFFSET MsgNewLine

    INC EDI
    CMP EDI, 10
    JL row_loop

    POP EDI
    POP ESI
    POP EBX
    RET
get_game_state ENDP

start:

    ;INVOKE printf,OFFSET szMsg

    ;INVOKE get_time_seed
    ;INVOKE printf,OFFSET MsgTest_get_time_seed,seed

    ;INVOKE random
    ;INVOKE printf,OFFSET MsgTest_random,seed

    ;INVOKE random_mod_100
    ;INVOKE printf,OFFSET MsgTest_random_mod_100,location

    INVOKE r_places_mines
    INVOKE get_game_state
    RET
end start
