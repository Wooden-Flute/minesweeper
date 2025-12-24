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
get_time_seed PROC
    INVOKE QueryPerformanceCounter, OFFSET largeVal
    MOV EAX, DWORD PTR largeVal
    AND EAX, 07FFFFFFFh ;确保种子为正数
    MOV seed, EAX
    RET
get_time_seed ENDP

;伪随机数生成函数（线性同余法）
random PROC
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
random_mod_100 PROC
    CALL random
    XOR EDX, EDX
    MOV ECX, 100
    DIV ECX
    MOV location, EDX
    MOV EAX, EDX ;返回值为0~99
    RET
random_mod_100 ENDP

;随机布雷
r_places_mines PROC
    LOCAL Number:BYTE

    INVOKE get_time_seed

    MOV CL, mine_count
    MOV Number,CL   ;保存地雷数量

loop_random:
    INVOKE random_mod_100    ;调用random_mod_100获取0~99的随机数
    MOV EAX, location        ;EAX = location (0-99)
    ;SUB EAX, 1           ;调整为0-99范围
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


;计算每个格子周围的地雷数
calc_around_mines PROC
    LOCAL curOffset:DWORD,curRow:DWORD,curCol:DWORD 
    ;当前格子偏移量,行号,列号
 

    PUSH EBX
    PUSH ESI
    PUSH EDI
 
    MOV ESI, OFFSET board_grid ;board_grid基址
    XOR EDI, EDI         ;row = 0

row_loop_calc:
    XOR EBX, EBX         ;col = 0
col_loop_cals:
    ;记录当前行列到局部变量
    MOV curRow, EDI
    MOV curCol, EBX

    ; curOffset = row*40 + col*4
    IMUL EAX, EDI, 40  ;row*40
    LEA EAX, [EAX + EBX*4] ;offset
    MOV curOffset, EAX

    ;若本格子有地雷，则跳过
    MOV DL, [ESI + EAX]
    CMP DL, 1
    JE next_cell_calc

    XOR ECX, ECX    ;ECX=周围地雷数

    ;上
    ;判断是否越界（行）
    MOV EAX, curRow
    CMP EAX, 0
    JE  done_up  ;跳过上方检查

    DEC EAX          ;row-1

    ;计算offset of (row-1, col)
    IMUL EAX, 40
    MOV EDX, curCol
    LEA EAX, [EAX + EDX*4] ;offset of (row-1, col)

    ;判断是否有地雷
    CMP BYTE PTR [ESI + EAX], 1
    JNE done_up
    INC ECX

done_up:
    ;上左
    ;判断是否越界（行）
    MOV EAX, curRow
    CMP EAX, 0
    JE done_ul

    ;判断是否越界（列）
    MOV EDX, curCol
    CMP EDX, 0
    JE  done_ul

    ;计算offset of (row-1, col-1)
    DEC EAX         ;row-1  
    IMUL EAX, 40
    DEC EDX         ;col-1
    LEA EAX, [EAX + EDX*4] ;offset of (row-1, col-1)

    ;判断是否有地雷
    CMP BYTE PTR [ESI + EAX], 1
    JNE done_ul
    INC ECX

done_ul:
    ;上右
    ;判断是否越界（行）
    MOV EAX, curRow
    CMP EAX, 0
    JE done_ur

    ;判断是否越界（列）
    MOV EDX, curCol
    CMP EDX, 9
    JE  done_ur

    ;计算offset of (row-1, col+1)
    DEC EAX         ;row-1
    IMUL EAX, 40
    INC EDX         ;col+1
    LEA EAX, [EAX + EDX*4] ;offset of (row-1, col+1)
    
    ;判断是否有地雷
    CMP BYTE PTR [ESI + EAX], 1
    JNE done_ur
    INC ECX

done_ur:
    ;左
    MOV EAX, curRow
    
    MOV EDX, curCol
    CMP EDX, 0
    JE done_l

    ;计算offset of (row, col-1)
    IMUL EAX, 40
    DEC EDX         ;col-1
    LEA EAX, [EAX + EDX*4] ;offset of (row, col-1)

    ;判断是否有地雷
    CMP BYTE PTR [ESI + EAX],1
    JNE done_l
    INC ECX

done_l:
    ;右
    MOV EAX, curRow

    ;判断是否越界（列）
    MOV EDX, curCol
    CMP EDX, 9
    JE done_r

    ;计算offset of (row, col+1)
    IMUL EAX, 40
    INC EDX         ;col+1
    LEA EAX, [EAX + EDX*4] ;offset of (row, col+1)

    ;判断是否有地雷
    CMP BYTE PTR [ESI + EAX],1
    JNE done_r
    INC ECX

done_r:
    ;下左
    ;判断是否越界（行）
    MOV EAX, curRow
    CMP EAX, 9
    JE done_dl

    ;判断是否越界（列）
    MOV EDX, curCol
    CMP EDX, 0
    JE done_dl

    ;计算offset of (row+1, col-1)
    INC EAX         ;row+1
    IMUL EAX, 40
    DEC EDX         ;col-1
    LEA EAX, [EAX + EDX*4] ;offset of (row+1, col-1)

    ;判断是否有地雷
    CMP BYTE PTR [ESI + EAX],1
    JNE done_dl
    INC ECX

done_dl:
    ;下
    ;判断是否越界（行）
    MOV EAX, curRow
    CMP EAX, 9
    JE done_d

    MOV EDX, curCol

    ;计算offset of (row+1, col)
    INC EAX         ;row+1
    IMUL EAX, 40
    LEA EAX, [EAX + EDX*4] ;offset of (row+1, col)

    ;判断是否有地雷
    CMP BYTE PTR [ESI + EAX],1
    JNE done_d
    INC ECX

done_d:
    ;下右
    ;判断是否越界（行）
    MOV EAX, curRow
    CMP EAX, 9
    JE done_dr

    ;判断是否越界（列）
    MOV EDX, curCol
    CMP EDX, 9
    JE done_dr

    ;计算offset of (row+1, col+1)
    INC EAX         ;row+1
    IMUL EAX, 40
    INC EDX         ;col+1
    LEA EAX, [EAX + EDX*4] ;offset of (row+1, col+1)

    ;判断是否有地雷
    CMP BYTE PTR [ESI + EAX],1
    JNE done_dr
    INC ECX

done_dr:
    ;将周围地雷数存入格子数据的第四个字节
    MOV EAX, curOffset
    MOV [ESI + EAX + 3], CL

next_cell_calc:
    INC EBX
    CMP EBX, 10
    JL col_loop_cals

    INC EDI
    CMP EDI, 10
    JL row_loop_calc

    POP EDI
    POP ESI
    POP EBX
    RET
calc_around_mines ENDP


;获取并显示当前游戏状态
get_game_state PROC
    LOCAL bMine:BYTE, bOpen:BYTE, bFlag:BYTE, bAroundMines:BYTE
    ;bMine:是否有地雷, bOpen:是否打开, bFlag:是否插旗, bAroundMines:周围地雷数
    ;PUSH EBP
    ;MOV EBP, ESP
    PUSH EBX
    PUSH ESI
    PUSH EDI 

    MOV ESI, OFFSET board_grid ;board_grid地址
    XOR EDI , EDI         ;row = 0

row_loop_get:
    XOR EBX, EBX          ;col = 0

col_loop_get:
    ;offset = row * 40 + col * 4
    IMUL EAX, EDI, 40  ;row*40
    LEA EAX, [EAX + EBX*4] ;offset

    MOV EDX, EAX
    MOV AL, [ESI + EDX] ;取出格子数据的第一个字节
    MOV bMine, AL
    MOV AL, [ESI + EDX +1]; 取出格子数据的第二个字节
    MOV bOpen, AL
    MOV AL, [ESI + EDX +2]; 取出格子数据的第三个字节
    MOV bFlag, AL
    MOV AL, [ESI + EDX +3]; 取出格子数据的第四个字节
    MOV bAroundMines, AL

    CMP EBX, 9
    JE last_in_row
    
    ; 保存 ESI（board_grid 基址）
    PUSH ESI
    
    MOVZX EAX, bAroundMines  ; 注意：参数从右到左压栈
    PUSH EAX
    MOVZX EAX, bFlag
    PUSH EAX
    MOVZX EAX, bOpen
    PUSH EAX
    MOVZX EAX, bMine
    PUSH EAX
    PUSH OFFSET MsgCell_info_mid
    CALL printf
    ADD ESP, 20  ; 清理栈（5个参数 * 4字节）
    
    ; 恢复 ESI
    POP ESI
    JMP after_print

last_in_row:
    ; 保存 ESI
    PUSH ESI
    
    MOVZX EAX, bAroundMines
    PUSH EAX
    MOVZX EAX, bFlag
    PUSH EAX
    MOVZX EAX, bOpen
    PUSH EAX
    MOVZX EAX, bMine
    PUSH EAX
    PUSH OFFSET MsgCell_info_end
    CALL printf
    ADD ESP, 20
    
    ; 恢复 ESI
    POP ESI
    
after_print:

    INC EBX
    CMP EBX, 10
    JL col_loop_get

    INVOKE printf, OFFSET MsgNewLine

    INC EDI
    CMP EDI, 10
    JL row_loop_get

    POP EDI
    POP ESI
    POP EBX
    ;MOV ESP, EBP
    ;POP EBP
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

    ;MOV EAX, OFFSET board_grid
    ;MOV BYTE PTR [EAX + 3], 1
    INVOKE calc_around_mines

    INVOKE get_game_state
    RET
end start
