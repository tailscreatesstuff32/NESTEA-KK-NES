'/###################################\
'#  joypad 1              joypad 2   #
'#  a     ; z             a     ; [  #
'#  b     ; x             b     ; ]  #
'#  select; ctrl          select; -  #
'#  start ; enter         start ; =  #
'#  up    ; up arrow      up    ; i  #
'#  down  ; down arrow    down  ; j  #
'#  left  ; left arrow    left  ; k  #
'#  right ; right arrow   right ; l  #
'#                                   #
'#  esc   ; quit                     #
'\###################################/



_TITLE "NESTEA-KK-NES v0.4"
'from original nesem and ddrnes
'with m6502 microprocessor command <-- i am sure not understand this ;p
'modified but renamed the function and optimized the code
'because the code are too long, really confused :D
'i am sorry no comment, i dont understand about assembly language
'so this is not my original code
'dont worry even the mario.nes run good but the sound using native speaker
'you can use this code free just like i get the nesem free no limitation
'happy coding ;)


'ROMS STATUS-------------------
'super mario bros- WORKS
'donkey kong- WORKS
'tiny toons adventures-WORKS
'dig dug- WORKS
'mario bros.-WORKS

'super mario bros2- RUNS-graphical-issues
'super mario bros3- WORKS-laggy-some graphical issues maybe?
'zelda-SUBSCRIPT OUT OF RANGE
'megaman- SUBSCRIPT OUT OF RANGE
'Mega Man 2- SUBSCRIPT OUT OF RANGE
'Castlevania- SUBSCRIPT OUT OF RANGE



'TODO:-------------------------
'sound needs fixing
'compatibility needs fixing

'------------------------------










DEFLNG A-Z
DIM CART AS STRING

DIM SHARED cw(4)
DIM SHARED dq(3, 255, 257)
DIM SHARED jp(1, 8)
DIM SHARED k0(2047) ' CPU RAM
DIM SHARED k1(4, 8191)
DIM SHARED kb(127)
DIM SHARED mr(8)
DIM SHARED nt(3, 1023)
DIM SHARED op(511)
DIM SHARED p2(31)
DIM SHARED pp(3, 255)
DIM SHARED pr(-1 TO 8)
DIM SHARED pu(9)
DIM SHARED rg(3)
DIM SHARED rm(256)
DIM SHARED sm(21)
DIM SHARED si(48)
DIM SHARED vd(61439)
DIM SHARED vr(16383)
DIM SHARED cd, gn, hn, nx, ny, pc, sp, tc
REDIM SHARED im(0), vm(0)

DIM SHARED zoom
zoom = 3



'IF CART = "" THEN
'    PRINT "no nes cartridge inserted"
'END IF

'CARTRIDGE:

'INPUT "insert nes cartridge to play: ", CART



'IF CART = "" THEN
'    PRINT "no cartridge inserted"
'    GOTO CARTRIDGE

'END IF







SCREEN _NEWIMAGE(zoom * 256, zoom * 240, 256)

'initialize
iz


'load nes file

CART = "mario.nes"


loadnes CART

SYSTEM


'$INCLUDE: 'NESTEA-KK-GUI.BAS'

SUB loadnes (f$)
    OPEN f$ FOR BINARY AS 1
    z$ = SPACE$(16)
    GET 1, , z$
    gn = ASC(MID$(z$, 5, 1)) * 2
    hn = ASC(MID$(z$, 6, 1))
    rc = ASC(MID$(z$, 7, 1))
    IF rc AND 4 THEN
        z$ = SPACE$(512)
        GET 1, , z$
    END IF
    mr(5) = rc AND 1
    mr(1) = mr(5)
    mr(2) = 1 - mr(5)
    mr(4) = mr(mr(5) + 7)

    'qb64 is rich of memory
    'i load all nes data to memory instead of from file
    REDIM im(gn * 8192& - 1), vm(hn * 8192& - 1)
    z$ = SPACE$(gn * 8192&)
    GET 1, , z$
    FOR i = 0 TO LEN(z$) - 1
        im(i) = ASC(MID$(z$, 1 + i, 1))
    NEXT
    z$ = SPACE$(hn * 8192&)
    GET 1, , z$
    FOR i = 0 TO LEN(z$) - 1
        vm(i) = ASC(MID$(z$, 1 + i, 1))
    NEXT
    sk
    kt 0, kv(0, hn) * 8, 8
    CLOSE 1
    ex
END SUB

'display the screen
SUB ds
    WAIT &H3DA, 8 ' finishes whenever the screen isn't being written to



    FOR y = 0 TO 239
        FOR x = 0 TO 255

            LINE (zoom * x, zoom * y)-STEP(zoom OR 1, zoom OR 1), vr(16128 + vd(x + y * 256)), BF
    NEXT x, y
    'WAIT &H3DA, &H8
    WAIT &H3DA, 8, 8 ' finishes whenever the screen is being written to

    _DISPLAY
    ' _LIMIT 50

    _LIMIT 60


END SUB

'system execution
SUB ex
    p = 32
    s = 255
    pc = r6(65532) + r6(65533) * 256
    DO
        cd = r6(pc)
        pc = pc + 1
        tc = tc + (cd AND 7)


        'opcode instructions
        'another part assembly command
        'please compare with original nesem for assembly comments :D
        'i think like jpm, cmp, sta things there ;p
        'i dont understand that
        SELECT CASE op(cd)
            CASE 1
                pc = pc + 1
                w9 s, pc, p, 20, 65534
            CASE 2
                p = p6(a, p, a OR r8(cd, sp))
            CASE 3
                v = r8(cd, sp)
                p = (p AND 254) OR (v \ 128)
                v = (v + v) AND 255
                w6 sp, v, 0
                p = p6(0, p, v)
            CASE 4
                w6 256 + s, p, s
            CASE 5
                p = p6(a, (p AND 254) OR (a \ 128), a + a)
            CASE 6
                IF p AND 128 THEN
                    pc = pc + 1
                ELSE
                    md cd
                    pc = pc + sp
                END IF
            CASE 7
                p = p AND 254
            CASE 8
                p = p6(a, p, a + 1)
            CASE 9
                pc = pc + 1
                w6 s + 256, pc \ 256, s
                w6 s + 256, pc, s
                pc = pc - 1
                md cd
                pc = sp
            CASE 10
                v = r8(cd, sp)
                p = p6(a, p, a AND v)
            CASE 11
                v = r8(cd, sp)
                p = (pp(3 + ((v AND a) = 0), p) AND 63) OR (v AND 192)
            CASE 12
                v = r8(cd, sp)
                h = p AND 1
                p = (p AND 254) OR (v \ 128)
                v = ((v + v) AND 255) OR h
                w6 sp, v, 0
                p = p6(0, p, v)
            CASE 13
                s = (s + 1) AND 255
                p = r6(s + 256) OR 32
            CASE 14
                p = p6(a, (p AND 254) OR (a \ 128), ((a + a) AND 255) OR (p AND 1))
            CASE 15
                IF p AND 128 THEN
                    md cd
                    pc = pc + sp
                    tc = tc + 1
                ELSE
                    pc = pc + 1
                END IF
            CASE 16
                p = p OR 1
            CASE 17
                p = p6(a, p, a - 1)
            CASE 18
                s = (s + 1) AND 255
                p = r6(s + 256) OR 32
                s = (s + 1) AND 255
                pc = r6(s + 256)
                s = (s + 1) AND 255
                pc = pc + r6(s + 256) * 256&
            CASE 19
                p = p6(a, p, a XOR r8(cd, sp))
            CASE 20
                v = r8(cd, sp)
                p = (p AND 254) OR (v AND 1)
                v = (v \ 2) AND 255
                w6 sp, v, 0
                p = p6(0, p, v)
            CASE 21
                w6 256 + s, a, s
            CASE 22
                p = p6(a, (p AND 254) OR (a AND 1), a \ 2)
            CASE 23
                md cd
                pc = sp
            CASE 24
                IF p AND 64 THEN
                    pc = pc + 1
                ELSE
                    md cd
                    pc = pc + sp
                    tc = tc + 1
                END IF
            CASE 25
                p = p AND 251
            CASE 26
                w6 256 + s, ny, s
            CASE 27
                s = (s + 1) AND 255
                pc = r6(s + 256)
                s = (s + 1) AND 255
                pc = pc + r6(s + 256) * 256& + 1
            CASE 28
                v = r8(cd, sp)
                a = a + v
                h = p AND 1
                IF (((a AND 255) + h) AND 255) > 127 THEN p = p OR 64 ELSE p = p AND 191
                a = a + h
                IF a > 255 THEN p = p OR 1 ELSE p = p AND 254
                a = a AND 255
                IF p AND 8 THEN
                    p = p AND 254
                    IF (a AND 15) > 9 THEN a = (a + 6) AND 255
                    IF (a AND 240) > 144 THEN
                        a = (a + 96) AND 255
                        p = p OR 1
                    END IF
                ELSE
                    tc = tc + 1
                END IF
                p = p6(0, p, a)
            CASE 29
                v = r8(cd, sp)
                h = p AND 1
                p = (p AND 254) OR (v AND 1)
                v = (v \ 2) AND 255
                IF h THEN v = v OR 128
                w6 sp, v, 0
                p = p6(0, p, v)
            CASE 30
                s = (s + 1) AND 255
                p = p6(a, p, r6(s + 256))
            CASE 31
                h = p AND 1
                p = (p AND 254) OR (a AND 1)
                a = (a \ 2) AND 255
                IF h THEN a = a OR 128
                p = p6(0, p, a)
            CASE 32
                IF p AND 64 THEN
                    md cd
                    pc = pc + sp
                    tc = tc + 1
                ELSE
                    pc = pc + 1
                END IF
            CASE 33
                p = p OR 4
            CASE 34
                s = (s + 1) AND 255
                p = p6(ny, p, r6(s + 256))
            CASE 35
                md cd
                pc = pc + sp
                tc = tc + 1
            CASE 36
                md cd
                w6 sp, a, 0
            CASE 37
                md cd
                w6 sp, ny, 0
            CASE 38
                md cd
                w6 sp, nx, 0
            CASE 39
                p = p6(ny, p, ny - 1)
            CASE 40
                p = p6(a, p, nx)
            CASE 41
                IF p AND 1 THEN
                    pc = pc + 1
                ELSE
                    md cd
                    pc = pc + sp
                    tc = tc + 1
                END IF
            CASE 42
                p = p6(a, p, ny)
            CASE 43
                p = p6(s, p, nx)
            CASE 44
                p = p6(ny, p, r8(cd, sp))
            CASE 45
                p = p6(a, p, r8(cd, sp))
            CASE 46
                p = p6(nx, p, r8(cd, sp))
            CASE 47
                p = p6(ny, p, a)
            CASE 48
                p = p6(nx, p, a)
            CASE 49
                IF p AND 1 THEN
                    md cd
                    pc = pc + sp
                    tc = tc + 1
                ELSE
                    pc = pc + 1
                END IF
            CASE 50
                p = p AND 191
            CASE 51
                p = p6(nx, p, s)
            CASE 52
                v = r8(cd, sp)
                IF (ny + 256 - v) > 255 THEN p = p OR 1 ELSE p = p AND 254
                p = p6(v, p, ny + 256 - v)
            CASE 53
                v = r8(cd, sp)
                IF (a + 256 - v) > 255 THEN p = p OR 1 ELSE p = p AND 254
                p = p6(v, p, a + 256 - v)
            CASE 54
                w6 sp, r8(cd, sp) - 1, 0
                p = p6(v, p, r6(sp))
            CASE 55
                p = p6(ny, p, ny + 1)
            CASE 56
                p = p6(nx, p, nx - 1)
            CASE 57
                IF p AND 2 THEN
                    pc = pc + 1
                ELSE
                    md cd
                    pc = pc + sp
                END IF
            CASE 58
                p = p AND 247
            CASE 59
                w6 256 + s, nx, s
            CASE 60
                v = r8(cd, sp)
                IF (nx + 256 - v) > 255 THEN p = p OR 1 ELSE p = p AND 254
                p = p6(v, p, nx + 256 - v)
            CASE 61
                v = r8(cd, sp) XOR 255
                h = p AND 1
                a = a + v
                IF (((a AND 255) + h * 16) AND 255) > 127 THEN p = p OR 64 ELSE p = p AND 191
                a = a + h
                IF a > 255 THEN p = p OR 1 ELSE p = p AND 254
                a = a AND 255
                IF p AND 8 THEN
                    a = (a - 102) AND 255
                    p = p AND 254
                    IF (a AND 15) > 9 THEN a = (a + 6) AND 255
                    IF (a AND 240) > 144 THEN
                        a = (a + 96) AND 255
                        p = p OR 1
                    END IF
                ELSE
                    tc = tc + 1
                END IF
                p = p6(0, p, a)
            CASE 62
                w6 sp, r8(cd, sp) + 1, 0
                p = p6(v, p, r6(sp))
            CASE 63
                p = p6(nx, p, nx + 1)
            CASE 64
                IF p AND 2 THEN
                    md cd
                    pc = pc + sp
                    tc = tc + 1
                ELSE
                    pc = pc + 1
                END IF
            CASE 65
                p = p OR 8
            CASE 66
                s = (s + 1) AND 255
                p = p6(nx, p, r6(s + 256))
            CASE ELSE
        END SELECT
        IF tc > 114 THEN
            IF l < 240 THEN
                IF l = 0 THEN
                    pr(6) = pr(8)
                ELSEIF pr(7) = 1 AND ((pu(2) AND 31) > 7) THEN
                    pr(6) = (pr(6) - 1) AND 255
                    IF pr(6) = 0 THEN
                        IF (p AND 4) = 0 THEN
                            w9 s, pc, p, 4, 65534
                            tc = tc + 7
                        END IF
                        pr(6) = pr(8)
                    END IF
                END IF
                rn l
            END IF
            IF l > 239 THEN
                IF l = 240 THEN
                    ds



                    ff% = ff% + 1
                    IF TIMER - start! >= 1 THEN fps% = ff%: ff% = 0: start! = TIMER
                    _TITLE "NESTEA-KK-NES v0.4 - " + "FPS:" + STR$(fps%)
                    '_TITLE STR$(fps!)
                    'fps! = 1 / (TIMER - PreviousTime!)
                    'PreviousTime! = TIMER


                    pr(1) = pr(1) + 1
                    jp(0, 0) = kb(44) 'a     ; z
                    jp(0, 1) = kb(45) 'b     ; x
                    jp(0, 2) = kb(29) 'select; ctrl
                    jp(0, 3) = kb(28) 'start ; enter
                    jp(0, 4) = kb(72) 'up    ; up arrow
                    jp(0, 5) = kb(80) 'down  ; down arrow
                    jp(0, 6) = kb(75) 'left  ; left arrow
                    jp(0, 7) = kb(77) 'right ; right arrow
                    jp(1, 0) = kb(26) 'a     ; [
                    jp(1, 1) = kb(27) 'b     ; ]
                    jp(1, 2) = kb(12) 'select; -
                    jp(1, 3) = kb(13) 'start ; =
                    jp(1, 4) = kb(23) 'up    ; i
                    jp(1, 5) = kb(37) 'down  ; j
                    jp(1, 6) = kb(36) 'left  ; k
                    jp(1, 7) = kb(38) 'right ; l
                    pu(3) = 128
                    IF pu(1) AND 128 THEN
                        w9 s, pc, p, 4, 65530
                        tc = tc + 7
                    END IF
                END IF
            END IF
            IF l = 0 OR l = 131 THEN sw
            IF l = 258 THEN pu(3) = 0
            IF l = 262 THEN
                l = 0
                vk = INP(96)
                IF vk THEN
                    IF vk < 128 THEN
                        kb(vk) = 65
                    ELSE
                        kb(vk - 128) = 64
                    END IF
                END IF
                pu(3) = 0
            ELSE
                l = l + 1
            END IF
            tc = tc - 114
        END IF






    LOOP UNTIL kb(1) = 65
END SUB

SUB iz
    a$ = "1e1e1e0309280a042b17022822001d24010a2404021b09011110010515010117"
    a$ = a$ + "0200140b01111b0000000000000000002f302f051a3712103b23093831062e35"
    a$ = a$ + "0818350d082d1603221e010e240105260602261602222a0b0b0b000000000000"
    a$ = a$ + "3d3d3e122c3e22253f301e3e3a1b3b3d1d2e3d201b3b28113530082635081437"
    a$ = a$ + "110f37250a36371819190101010101013e3e3e2d373e33343f37313e3c303d3d"
    a$ = a$ + "30383d33303d362b3b3926343b272e3c2d2c3c36293c3c323132010101010101"
    FOR i = 0 TO 63
        r = VAL("&h" + MID$(a$, 1 + 6 * i, 2))
        g = VAL("&h" + MID$(a$, 3 + 6 * i, 2))
        b = VAL("&h" + MID$(a$, 5 + 6 * i, 2))
        pn i, r, g, b
        pn 64 + i, r, g, b
        pn 128 + i, r, g, b
        pn 192 + i, r, g, b
    NEXT
    a$ = "0102000000020300040205000002030006020200000203000702080000020300"
    a$ = a$ + "090a00000b0a0c000d0a0e000b0a0c000f0a0a000b0a0c00100a11000b0a0c00"
    a$ = a$ + "12130000001314001513160017131400181313000013140019131a0000131400"
    a$ = a$ + "1b1c0000001c1d001e1c1f00171c1d00201c1c00001c1d00211c2200171c1d00"
    a$ = a$ + "2324000025242600270b28002524260029242400252426002a242b0000240000"
    a$ = a$ + "2c2d2e002c2d2e002f2d30002c2d2e00312d2d002c2d2e00322d33002c2d2e00"
    a$ = a$ + "3435000034353600373538003435360039353500003536003a353b0000353600"
    a$ = a$ + "3c3d00003c3d3e003f3d00003c3d3e00403d3d00003d3e00413d4200003d3e00"
    FOR i = 0 TO 255
        op(i) = VAL("&h" + MID$(a$, i * 2 + 1, 2))
    NEXT
    a$ = "01002220030044405670288009004aa04100222003004440567088800900aaa0"
    a$ = a$ + "01000220030044405670088009000aa0010022200300b440567088800900caa0"
    a$ = a$ + "5100222003004440567088d009004aa03130222003004440567088d00900aa90"
    a$ = a$ + "31002220030044405670088009000aa031002220030044405670088009000aa0"
    FOR i = 0 TO 255
        op(i + 256) = VAL("&h" + MID$(a$, i + 1, 1))
    NEXT
    a$ = "047f067b08770a730c6f0e6b10671263145f165b18571a531c4f1e4b20472243"
    FOR i = 0 TO 31
        si(i + 17) = VAL("&h" + MID$(a$, 1 + 2 * i, 2))
    NEXT
    FOR i = 0 TO 255
        FOR b = 0 TO 1
            dq(b, i, 256) = i AND 15
            dq(2 + b, i, 256) = 6
    NEXT b, i
    FOR i = 0 TO 255
        FOR b = 0 TO 3
            dq(b, i, 257) = i \ 8
    NEXT b, i
    FOR i = 0 TO 255
        FOR b = 0 TO 255
            FOR c = 0 TO 2
                dq(c, b, i) = b + (i AND 7) * 256
    NEXT c, b, i
    FOR i = 0 TO 255
        dq(3, i, i) = (i AND 15) * 128
    NEXT
    FOR i = 0 TO 255
        pp(0, i) = i AND 127
        pp(1, i) = i OR 128
        pp(2, i) = i OR 2
        pp(3, i) = i AND 253
    NEXT
    FOR i = 0 TO 30
        p2(i) = 2 ^ i
    NEXT
    p2(31) = -2147483648#
    FOR i = 0 TO 7
        jp(0, i) = 64
        jp(1, i) = 64
    NEXT
    mr(3) = 1
    mr(7) = 1024
    mr(8) = 2048
    pu(4) = 1
    rg(1) = 1
    rg(2) = 254
    rg(3) = 255
END SUB

SUB kt (b, a, n)
    s = a * 1024&
    d = pr(5) XOR (b * 1024&)
    c = n * 1024& - 1
    FOR i = 0 TO c
        vr(i + d) = vm(i + s)
    NEXT
END SUB

FUNCTION kv (a, b)
    IF b = 0 THEN m = 256 ELSE m = b
    IF (m AND (m - 1)) = 0 THEN
        kv = a AND (m - 1)
    ELSE
        i = 1
        WHILE i < m
            i = i + i
        WEND
        i = a AND (i - 1)
        IF i < m THEN kv = i ELSE kv = m - 1
    END IF
END FUNCTION

SUB md (cd)
    SELECT CASE op(cd + 256)
        CASE 1
            sp = r9(r7(pc, nx))
        CASE 2
            sp = r6(pc) AND 255
            pc = pc + 1
        CASE 3
            sp = pc
            pc = pc + 1
        CASE 4
            sp = r9(pc)
            pc = pc + 2
        CASE 5
            sp = r6(pc)
            sp = sp - ((sp AND 128) \ 128) * 256
            pc = pc + 1
        CASE 6
            sp = r9(r6(pc))
            tc = tc + t9(cd, 5, sp, ny)
            pc = pc + 1
        CASE 7
            sp = r9(r6(pc))
            pc = pc + 1
        CASE 8
            sp = (r6(pc) + nx) AND 255
            pc = pc + 1
        CASE 9
            sp = r9(pc)
            tc = tc + t9(cd, 4, sp, ny)
            pc = pc + 2
        CASE 10
            sp = r9(pc)
            tc = tc + t9(cd, 4, sp, nx)
            pc = pc + 2
        CASE 11
            sp = r9(r9(pc))
            pc = pc + 2
        CASE 12
            sp = r9(r9(pc) + nx)
        CASE 13
            sp = r7(pc, ny)
        CASE ELSE
    END SELECT
END SUB

FUNCTION p6 (a, b, c)
    a = c AND 255
    p6 = pp(a \ 128, pp(3 + (a = 0), b))
END FUNCTION

FUNCTION r6 (a)
    SELECT CASE a
        CASE 0 TO 8191
            r6 = k0(a AND 2047&)
        CASE 8192 TO 16383
            SELECT CASE (a AND 7)
                CASE 0, 1, 5, 6
                    r6 = pu(0)
                CASE 2
                    r6 = (pu(0) AND 31) OR pu(3)
                    pu(4) = 1
                    IF ((pu(0) AND 31) OR pu(3)) AND 128 THEN pu(3) = pu(3) AND 96
                CASE 4
                    r6 = pu(0)
                    pu(0) = rm(rm(256))
                    rm(256) = (rm(256) + 1) AND 255
                CASE 7
                    r6 = pu(0)
                    IF pu(5) > 8191 AND pu(5) < 12288 THEN
                        pu(0) = nt(mr((pu(5) AND 3072&) \ 1024&), pu(5) AND 1023&)
                    ELSE
                        pu(0) = vr(pu(5) AND 16383)
                    END IF
                    IF pu(1) AND 4 THEN pu(5) = pu(5) + 32 ELSE pu(5) = pu(5) + 1
                    pu(5) = pu(5) AND 16383
            END SELECT
        CASE 16384 TO 16403, 16405
            r6 = sm(a - 16384)
        CASE 16406
            r6 = jp(0, jp(0, 8))
            jp(0, 8) = (jp(0, 8) + 1) AND 7
        CASE 16407
            r6 = jp(1, jp(1, 8))
            jp(1, 8) = (jp(1, 8) + 1) AND 7
        CASE 24576 TO 32767
            r6 = k1(4, a AND 8191&)
        CASE 32768 TO 40959
            r6 = k1(0, a AND 8191&)
        CASE 40960 TO 49151
            r6 = k1(1, a AND 8191&)
        CASE 49152 TO 57343
            r6 = k1(2, a AND 8191&)
        CASE 57344 TO 65535
            r6 = k1(3, a AND 8191&)
    END SELECT
END FUNCTION

FUNCTION r7 (a, b)
    r7 = (r6(a) + b) AND 255
    a = a + 1
END FUNCTION

FUNCTION r8 (a, b)
    md a
    r8 = r6(b)
END FUNCTION

FUNCTION r9 (a)
    r9 = r6(a) + r6(a + 1) * 256
END FUNCTION

SUB rn (s)
    mr(1) = mr(5)
    mr(2) = 1 - mr(5)
    mr(4) = mr(mr(5) + 7)
    IF s = 0 THEN
        FOR i = 0 TO 61439
            vd(i) = 16
        NEXT
    END IF
    IF (pu(2) AND 16) = 0 THEN EXIT SUB
    IF s = 0 THEN
        pu(5) = pu(6)
    ELSE
        pu(5) = (pu(5) AND 64480) OR (pu(6) AND 1055)
    END IF
    pu(8) = 8192 + (pu(5) AND 3072)
    n = (pu(8) AND 3072) \ 1024
    pr(2) = 8 * (pu(5) AND 31) + pr(3)
    pr(4) = 8 * (pu(5) \ 32 AND 31) OR ((pu(5) \ 4096) AND 7)
    pr(4) = pr(4) - s
    v = pu(5)
    IF (v AND 28672) = 28672 THEN
        v = v AND 36863
        IF (v AND 992) = 928 THEN
            v = v XOR 2048
            v = v AND 64543
        ELSE
            IF (v AND 992) = 992 THEN
                v = v AND 64543
            ELSE
                v = v + 32
            END IF
        END IF
    ELSE
        v = v + 4096
    END IF
    pu(5) = v AND 65535
    IF s = 239 THEN pu(3) = pu(3) OR 128
    sc = s + pr(4)
    IF sc > 480 THEN sc = sc - 480
    r = (sc \ 8) MOD 30
    ty = sc AND 7
    pu(7) = (pu(1) AND 16) * 256&
    FOR t = pr(2) \ 8 TO 31
        ti = nt(mr(n), t + r * 32&)
        x = 8& * t - pr(2) + 7
        IF x < 7 THEN m = x ELSE m = 7
        x = x + s * 256&
        d = nt(mr(n), (960 + t \ 4) + (r \ 4) * 8&)
        SELECT CASE (t AND 2) OR ((r AND 2) * 2)
            CASE 0
                c = (d * 4) AND 12
            CASE 2
                c = d AND 12
            CASE 4
                c = (d \ 4) AND 12
            CASE 6
                c = (d \ 16) AND 12
        END SELECT
        b = vr(pu(7) + ti * 16 + ty)
        b2 = vr(pu(7) + ti * 16 + 8 + ty)
        a = b * 2048& + 8& * b2
        FOR i = m TO 0 STEP -1
            IF b AND p2(i) THEN
                IF b2 AND p2(i) THEN
                    vd(x - i) = 3 OR c
                ELSE
                    vd(x - i) = 1 OR c
                END IF
            ELSE
                IF b2 AND p2(i) THEN vd(x - i) = 2 OR c
            END IF
    NEXT i, t
    pu(8) = pu(8) XOR 1024&
    n = (pu(8) AND 3072&) \ 1024&
    FOR t = 0 TO pr(2) \ 8
        ti = nt(mr(n), t + r * 32&)
        x = t * 8 + 256 - pr(2) + 7
        IF x > 255 THEN m = x - 255 ELSE m = 0
        x = x + s * 256&
        d = nt(mr(n), (960 + t \ 4 + (r \ 4) * 8&))
        SELECT CASE (t AND 2) OR (r AND 2) * 2
            CASE 0
                c = (d * 4) AND 12
            CASE 2
                c = d AND 12
            CASE 4
                c = (d \ 4) AND 12
            CASE 6
                c = (d \ 16) AND 12
        END SELECT
        b = vr(pu(7) + ti * 16 + ty)
        b2 = vr(pu(7) + ti * 16 + 8 + ty)
        a = b * 2048& + 8& * b2
        FOR i = 7 TO m STEP -1
            IF b AND p2(i) THEN
                IF b2 AND p2(i) THEN
                    vd(x - i) = 3 OR c
                ELSE
                    vd(x - i) = 1 OR c
                END IF
            ELSE
                IF b2 AND p2(i) THEN vd(x - i) = 2 OR c
            END IF
    NEXT i, t
    IF pu(2) AND 16 THEN rs s - 1
END SUB

SUB rs (s)
    DIM m(264)
    r = s \ 8
    IF pu(1) AND 32 THEN h = 16 ELSE h = 8
    IF pu(1) AND 8 THEN
        IF h = 8 THEN pu(7) = 4096&
    ELSE
        IF h = 8 THEN pu(7) = 0
    END IF
    IF pu(2) AND 8 THEN x2 = 0 ELSE x2 = 8
    s2 = s * 256&
    FOR i = 0 TO 63
        i2 = 4& * i
        y = rm(i2) + 1
        IF y <= s AND y > s - h THEN
            e = rm(i2 + 2)
            IF e AND 32 THEN u = 0 ELSE u = 1
            x = rm(i2 + 3)
            IF x >= x2 THEN
                IF x < 248 THEN
                    c = 16 + (e AND 3) * 4
                    ti = rm(i2 + 1)
                    IF h = 16 THEN
                        IF ti AND 1 THEN
                            pu(7) = 4096&
                            ti = ti XOR 1
                        ELSE
                            pu(7) = 0
                        END IF
                    END IF
                    IF e AND 128 THEN v = y - s - 1 ELSE v = s - y
                    v = v AND h - 1
                    IF v > 7 THEN v = v + 8
                    b = vr(pu(7) + ti * 16 + v)
                    b2 = vr(pu(7) + ti * 16 + 8 + v)
                    IF e AND 64 THEN
                        a = s2 + x
                        FOR xx = 0 TO 7
                            IF b AND p2(xx) THEN nc = 1 ELSE nc = 0
                            IF b2 AND p2(xx) THEN nc = nc + 2
                            IF nc THEN
                                IF i = 0 AND (pu(3) AND 64) = 0 THEN
                                    IF (vd(a + xx) AND 3) AND ((pu(3) AND 64) = 0) THEN
                                        pu(3) = pu(3) OR 64
                                        IF u THEN vd(a + xx) = c OR nc
                                    ELSE
                                        vd(a + xx) = c OR nc
                                    END IF
                                    m(x + xx) = 1
                                ELSE
                                    IF m(x + xx) = 0 THEN
                                        IF u THEN
                                            vd(a + xx) = c OR nc
                                        ELSEIF (vd(a + xx) AND 3) = 0 THEN
                                            vd(a + xx) = c OR nc
                                        END IF
                                        m(x + xx) = 1
                                    END IF
                                END IF
                            END IF
                        NEXT
                    ELSE
                        a = s2 + x + 7
                        FOR xx = 7 TO 0 STEP -1
                            IF b AND p2(xx) THEN nc = 1 ELSE nc = 0
                            IF b2 AND p2(xx) THEN nc = nc + 2
                            IF nc THEN
                                IF i = 0 AND (pu(3) AND 64) = 0 THEN
                                    IF vd(a - xx) AND 3 AND (pu(3) AND 64) = 0 THEN
                                        pu(3) = pu(3) OR 64
                                        IF u THEN vd(a - xx) = c OR nc
                                    ELSE
                                        vd(a - xx) = c OR nc
                                    END IF
                                    m(x + 7 - xx) = 1
                                ELSE
                                    IF m(x + 7 - xx) = 0 THEN
                                        IF u THEN
                                            vd(a - xx) = c OR nc
                                        ELSEIF (vd(a - xx) AND 3) = 0 THEN
                                            vd(a - xx) = c OR nc
                                        END IF
                                        m(x + 7 - xx) = 1
                                    END IF
                                END IF
                            END IF
                        NEXT
                    END IF
                END IF
                IF i = 0 THEN IF s = y + h - 1 THEN pu(3) = pu(3) OR 64
            END IF
        END IF
    NEXT
END SUB

SUB sk
    FOR n = 0 TO 3
        IF rg(n) >= gn THEN
            i = 255
            WHILE (rg(n) AND i) >= gn
                i = i \ 2
            WEND
            rg(n) = rg(n) AND i
        END IF
        FOR i = 0 TO 8191
            k1(n, i) = im(i + rg(n) * 8192&)
    NEXT i, n
END SUB

SUB sw
    FOR i = 0 TO 3
        IF si(i + 12) <> 0 AND si(i + 12) <> si(i) THEN
            t = si(i + 12)
            IF t < -128 THEN t = -128
            IF t > 127 THEN t = 127
            'SOUND (t AND 63) * 50 + 40, v / 10

            si(i + 12) = 0
        END IF
    NEXT
    FOR i = 0 TO 3
        t = 0
        n = 0
        ch = i
        v = dq(0, sm(ch * 4), 256)
        l = dq(0, sm(3 + ch * 4), 257)
        f = dq(0, sm(2 + ch * 4), sm(3 + ch * 4))
        IF (cw(4) AND (2 ^ ch)) = 0 THEN
            cw(ch) = 1
            IF si(ch) THEN
                si(ch + 12) = si(ch)
                si(ch) = 0
                si(ch + 4) = 0
            END IF
        ELSE
            IF v < 1 OR f < 2 THEN
                IF si(ch) THEN
                    si(ch + 12) = si(ch)
                    si(ch) = 0
                    si(ch + 4) = 0
                END IF
            ELSEIF cw(ch) THEN
                IF f THEN t = CLNG(LOG(111861 / f / 8.176) * 17.31234)
                n = t
                cw(ch) = 0
                si(ch + 8) = pr(1) + si(l + 17)
                IF n <> si(ch) OR v < si(ch + 4) - 3 OR v > si(ch + 4) OR v = 0 THEN
                    IF si(ch) THEN
                        t = si(ch)
                        IF t < -128 THEN t = -128
                        IF t > 127 THEN t = 127
                        'SOUND (t AND 63) * 50 + 40, v / 10

                        si(ch) = 0
                        si(ch + 4) = 0
                    END IF
                    IF n > 0 AND n < 128 AND v > 0 THEN
                        si(ch) = n
                        si(ch + 4) = v
                        t = n
                        IF t < -128 THEN t = -128
                        IF t > 127 THEN t = 127
                        'SOUND (t AND 63) * 50 + 40, v / 10

                    END IF
                END IF
            END IF
        END IF
        IF (pr(1) >= si(ch + 8)) AND si(ch) THEN
            si(ch + 12) = si(ch)
            si(ch) = 0
            si(ch + 4) = 0
        END IF
    NEXT
END SUB

FUNCTION t9 (a, b, c, d)
    IF (a AND 7) = b THEN t9 = ((c MOD 256) + d) \ 256
    c = c + d
END FUNCTION

SUB w6 (a, v, s)
    b = v AND 255  'Extract only the first 8 bits
    SELECT CASE a
        CASE 0 TO 8191 ' First 8K of memory address space, this is CPU RAM because it's 2K and then it's mirrored as per https://wiki.nesdev.com/w/index.php/CPU_memory_map
            k0(a AND 2047) = b
        CASE 8192 TO 16383 ' These are hardware registers, including PPU, etc
            pu(0) = b
            SELECT CASE (a AND 7)
                CASE 0
                    pu(1) = b
                    pu(6) = (pu(6) AND 62463) OR (b AND 3) * 1024&
                CASE 1
                    pu(2) = b
                CASE 2
                    pu(0) = b
                CASE 3
                    rm(256) = b
                CASE 4
                    rm(rm(256)) = b
                    rm(256) = (rm(256) + 1) AND 255&
                CASE 5
                    IF pu(4) THEN
                        pr(3) = b AND 7
                        pu(6) = (b \ 8) AND 31
                    ELSE
                        pu(6) = (pu(6) AND 35871) OR 4& * (b AND 248&) OR (b AND 7) * 4096&
                    END IF
                    pu(4) = 1 - pu(4)
                CASE 6
                    IF pu(4) THEN
                        pu(5) = (pu(5) AND 255&) OR (b AND 63&) * 256&
                    ELSE
                        pu(5) = (pu(5) AND 32512) OR b
                    END IF
                    pu(4) = 1 - pu(4)
                CASE 7
                    pu(0) = b
                    IF pu(5) > 8190 AND pu(5) < 12288 THEN
                        nt(mr((pu(5) AND 3072&) \ 1024&), pu(5) AND 1023&) = b
                    ELSE
                        IF pu(5) < 16384 THEN vr(pu(5)) = b
                        IF (pu(5) AND 65519) = 16128 THEN vr(pu(5) XOR 16) = b
                    END IF
                    IF pu(1) AND 4 THEN pu(5) = pu(5) + 32 ELSE pu(5) = pu(5) + 1
                    pu(5) = pu(5) AND 16383
                    pu(4) = 1
            END SELECT
        CASE 16384 TO 16403
            sm(a - 16384) = b
            IF a < 16400 THEN cw((a - 16384) \ 4) = 1
        CASE 16404
            FOR i = 0 TO 255
                rm(i) = k0(i + b * 256&)
            NEXT
        CASE 16405
            cw(4) = b
        CASE 24576 TO 32767
            k1(4, a AND 8191) = b
        CASE 32768
            pu(9) = b AND 7
            IF b AND 128 THEN pr(5) = 4096 ELSE pr(5) = 0
            mr(6) = (b AND 64) \ 32
        CASE 32769
            SELECT CASE pu(9)
                CASE 0, 1
                    kt 2 * pu(9), kv(b, hn * 8), 1
                    kt 2 * pu(9) + 1, kv(b + 1, hn * 8), 1
                CASE 2 TO 5
                    kt 2 + pu(9), kv(b, hn * 8), 1
                CASE 6, 7
                    pr(pu(9) = 6) = b
                    rg(mr(6)) = pr(-1)
                    rg(1) = pr(0)
                    rg(2 - mr(6)) = 254
                    rg(3) = 255
                    sk
            END SELECT
        CASE 40960
            mr(5) = 1 - (b AND 1)
            mr(1) = mr(5)
            mr(2) = 1 - mr(5)
            mr(4) = mr(mr(5) + 7)
        CASE 49152
            pr(6) = b
        CASE 49153
            pr(8) = b
        CASE 57344
            pr(6) = pr(8)
            pr(7) = 0
        CASE 57345
            pr(7) = 1
    END SELECT
    s = (s - 1) AND 255
END SUB

SUB w9 (a, b, c, d, e)
    w6 a + 256, b \ 256, a
    w6 a + 256, b, a
    w6 a + 256, c, a
    c = c OR d
    b = r9(e)
END SUB

SUB pn (n, r, g, b)
    OUT 968, n
    OUT 969, r
    OUT 969, g
    OUT 969, b
END SUB

